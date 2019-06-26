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
tripadvisor_reviews <- lapply(comment_links,get_review)

saveRDS(tripadvisor_reviews, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews.rds"))
