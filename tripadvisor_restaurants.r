# Extracting restaurant details from Tripadvisor

#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("readr")


# Defining function to get info
get_restaurant <- function(link){

  # Reading HTML
  html <- read_html(link)
  
  # Extracting name
  restaurant_name <- html_node(html, xpath = "//h1[@class='ui_header h1']") %>%
    html_text()
  
  # Extracting restaurant details
  
  # Get top node
  top_node <- html_node(html,xpath = "//div[@id='taplc_resp_rr_top_info_rr_resp_0']")
  
  #Get restaurant overall rating
  restaurant_rating <- html_node(top_node,xpath = "//span[contains(@class,'ui_bubble_rating bubble_')]") %>%
    html_attr(name = "class") %>%
    str_extract(pattern = "[0-9]{2}") %>%
    str_replace(pattern = "([0-9])([0-9])",replacement = "\\1.\\2" ) %>%
    as.numeric()
  
  # Get the number of reviews 
  restaurant_number_reviews <- html_node(top_node,xpath = "//span[@class='reviewCount']") %>%
    html_text() %>%
    str_extract("[0-9]+") %>%
    as.numeric()
  
  # Get the restaurant's rank in town
  restaurant_rank <- html_node(top_node, xpath = "//div[@class='popIndexContainer']//b/span") %>%
    html_text() %>%
    str_extract("[0-9]?.?[0-9]+$") %>%
    str_replace("([0-9]).([0-9]+)","\\1\\2") %>%
    as.numeric()
  
  # Get restaurant's postcode
  restaurant_postcode <- html_node(top_node, xpath = "//span[@class = 'locality']") %>%
    html_text() %>%
    str_extract("[0-9]+") %>%
    as.numeric()
  
  # Get restaurant's price level
  restaurant_price <- html_node(top_node, xpath = "//div[@class = 'header_links']/a") %>%
    html_text() %>%{
      if(str_detect(.,"-")){
        c("Medio")
      } else if(str_count(.,"\200")==1){
        c("Barato")
      } else{
        c("Elegante")
      }
    } 
  
  # Get restaurant's scores (WIP)
  restaurant_score_nodes <- html_nodes(html, xpath = "//div[contains(@class,
                                       'restaurants-detail-overview-cards-RatingsOverviewCard__ratingQuestionRow')]") 
  restaurant_scores <- data.frame()
  if (length(restaurant_score_nodes) == 0){
    restaurant_scores <- data.frame(metric=NA,score=NA)
  } else{
    index <- seq(from = 1, to = length(restaurant_score_nodes), by =1)
    
    for (i in index){
      metric <- html_nodes(html,xpath = "//span[contains(@class,'restaurants-detail-overview-cards-RatingsOverviewCard__ratingText')]")[i] %>%
        html_text()
      score <-  html_nodes(html,xpath = "//span[contains(@class,'restaurants-detail-overview-cards-RatingsOverviewCard__ratingBubbles')]/span")[i] %>%
        html_attrs() %>%
        str_extract(pattern = "[0-9]{2}") %>%
        str_replace(pattern = "([0-9])([0-9])",replacement = "\\1.\\2" ) %>%
        as.numeric()
      new_row <- data.frame(metric = metric,score = score, stringsAsFactors = FALSE)
      restaurant_scores <- bind_rows(restaurant_scores,new_row)
    }
  }
  
  #Consildating everything in a tibble
  restaurant <- data.frame(restaurant_name= restaurant_name,
                           restaurant_rating = restaurant_rating,
                           restaurant_number_reviews = restaurant_number_reviews,
                           restaurant_rank = restaurant_rank,
                           restaurant_postcode = restaurant_postcode,
                           restaurant_price = restaurant_price,
                           restaurant_scores = restaurant_scores,
                           stringsAsFactors = FALSE)
  
  # Tidying the restaurant scores column
  restaurant <- spread(restaurant,key=restaurant_scores.metric,value=restaurant_scores.score,sep = "_") 
  
  #Removing the NA column if it exists
  if(sum(str_detect(names(restaurant),"NA")) != 0){
    restaurant <- select(restaurant,-'restaurant_scores.metric_NA')
  }
  
  return(restaurant)
}

# load RDS file 
restaurant_links <- readRDS(file.path("~","GitHub","ieseDataSciProjectTripadvisor","restaurant_links.rds"))

# Getting and saving restaurant details

tripadvisor_restaurants <- map_dfr(restaurant_links[1:5],get_restaurant) %>%
  mutate(restaurant_price = factor(x=restaurant_price, levels = c("Barato","Medio","Elegante")))


saveRDS(tripadvisor_restaurants, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_restaurants_",as.character(Sys.Date()),".rds")))

write_excel_csv(tripadvisor_restaurants,path=file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_restaurants_",as.character(Sys.Date()),".csv")))


