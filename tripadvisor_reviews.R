# Extracting ratings and reviews from Tripadvisor comments

#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")

get_review <- function(link){
  # Extracting comment ID
  comment_id <- str_match(link,pattern = "r([0-9]+)-") %>%
    .[2]
  
  # Reading HTML
  html <- read_html(link)
  
  # Extracting comment attributes
  restaurant_name <- html_node(html,xpath = "//span[@class='altHeadInline']/a") %>%
    html_text()
  comment_node <- html_node(html,xpath = paste0("//div[@id='review_",comment_id,"']"))
  comment_title <- html_node(comment_node,xpath="//span[@class='noQuotes']") %>%
    html_text()
  rating <- html_node(comment_node,xpath = "//div[@class='rating reviewItemInline']/span") %>%
    html_attrs %>%
    unlist %>%
    str_extract(pattern = "[0-9]{2}") %>%
    str_replace(pattern = "([0-9])([0-9])",replacement = "\\1.\\2" ) 
  comment_text <- html_node(comment_node,xpath = "//div[@class='entry']/p") %>%
    html_text()
  
  #Consildating everything in a tibble
  comment <- tibble(comment_id=comment_id,
                    restaurant_name= restaurant_name,
                    comment_title = comment_title,
                    comment_rating=as.numeric(rating),
                    comment_text = comment_text)
  
  return(comment)
}


# load RDS file 
comment_links <- readRDS(file.path("~","GitHub","ieseDataSciProjectTripadvisor","comment_links.rds"))

# Getting and saving reviews

tripadvisor_reviews <- lapply(comment_links[1],get_review)

sequence <- seq(from=1, to=length(comment_links), by=10)
index<-seq(from = 2, to = 4, by=1) 

for(i in index){
  reviews <- lapply(comment_links[(sequence[i-1]+1):sequence[i]],get_review)
  tripadvisor_reviews <- append(tripadvisor_reviews,reviews)
  saveRDS(tripadvisor_reviews, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews.rds"))
  print(paste0(round((i-1)/(max(index)-1)*100,2),"% complete"))
}

reviews <- lapply(comment_links[(max(sequence)+1):length(comment_links)], get_review)
tripadvisor_reviews <- append(tripadvisor_reviews,reviews)
saveRDS(tripadvisor_reviews, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews.rds"))

#test <- as.tibble(tripadvisor_reviews,.name_repair = "unique")
#write_excel_csv(tripadvisor_reviews,path=file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews.csv"))
