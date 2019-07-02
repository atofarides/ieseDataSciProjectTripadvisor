# Extracting ratings and reviews from Tripadvisor 

#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("readr")



get_review <- function(link){
  # Extracting review ID
  comment_id <- str_match(link,pattern = "r([0-9]+)-") %>%
    .[2]
  
  # Reading HTML
  html <- read_html(link)
  
  # Extracting review detail
  restaurant_name <- html_node(html,xpath = "//span[@class='altHeadInline']/a") %>%
    html_text()
  comment_node <- html_node(html,xpath = paste0("//div[@id='review_",comment_id,"']"))
  comment_date <- html_node(comment_node,xpath = "//span[@class='ratingDate relativeDate']") %>%
    html_text() %>%
    str_extract(pattern = "[0-9]+ de [a-z]+ de [0-9]{4}") %>%
    str_replace(pattern = "([0-9]+) de ([a-z]+) de ([0-9]{4})","\\1 \\2 \\3") %>%
    parse_date(.,"%d %B %Y",locale=locale("es"))
    
  username <- html_node(comment_node,xpath = "//div[@class = 'username mo']") %>%
    html_text()
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
  comment <- data.frame(review_id=comment_id,
                    restaurant_name= restaurant_name,
                    review_date = comment_date,
                    username = username,
                    review_title = comment_title,
                    review_rating=as.numeric(rating),
                    review_text = comment_text, stringsAsFactors = FALSE)
  #comment <- separate(comment, col=user_location,into=c("user_city","user_country"),sep=", ")
  
  return(comment)
}


# load RDS file 
comment_links <- readRDS(file.path("~","GitHub","ieseDataSciProjectTripadvisor","comment_links.rds"))

# Getting and saving reviews

tripadvisor_reviews <- map_dfr(comment_links[1],get_review)

sequence <- seq(from=1, to=length(comment_links), by=10)
index<-seq(from = 2, to = length(sequence), by=1) 

for(i in index){
  reviews <- map_dfr(comment_links[(sequence[i-1]+1):sequence[i]],get_review)
  tripadvisor_reviews <- bind_rows(tripadvisor_reviews,reviews)
  saveRDS(tripadvisor_reviews, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_",as.character(Sys.Date()),".rds")))
  print(paste0(round((i-1)/(max(index)-1)*100,2),"% complete"))
}

# If for any reason the code stops (usually a review has been deleted), find the missing review and remove from the links vector
# Then resume the extraction of data by starting the index vector from the last i value

# If the loop finishes, complete extraction by running the last few isolated lines of code below
reviews <- map_dfr(comment_links[(max(sequence)+1):length(comment_links)], get_review)
tripadvisor_reviews <- bind_rows(tripadvisor_reviews,reviews)
saveRDS(tripadvisor_reviews, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_",as.character(Sys.Date()),".rds")))

# Check if all values are unique
sum(duplicated(tripadvisor_reviews$review_id))

write_excel_csv(tripadvisor_reviews,path=file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_",as.character(Sys.Date()),".csv")))


