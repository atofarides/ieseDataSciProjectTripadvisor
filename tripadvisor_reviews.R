# Extracting ratings and reviews from Tripadvisor 

#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("readr")
library("tm")
library("SnowballC")

# Writing the main review gathering function
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
  comment <- data.frame(doc_id=as.numeric(comment_id),
                        text = comment_text,
                    restaurant_name= restaurant_name,
                    review_date = comment_date,
                    username = username,
                    review_title = comment_title,
                    review_rating=as.numeric(rating),
                     stringsAsFactors = FALSE)
  
  return(comment)
}

get_termdocument <- function(reviews){
  # Creating the corpus
  corpus_reviews <- reviews %>%
    DataframeSource() %>%
    VCorpus()
  
  # Cleaning the corpus
  corpus_reviews <- corpus_reviews %>%
    tm_map(removePunctuation,ucp=TRUE) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords,c("´","\200",stopwords("spanish"))) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers) %>%
    tm_map(stemDocument,language = "spanish")
  
  # Creating a document term matrix
  reviews_dtm <- DocumentTermMatrix(corpus_reviews, control=list(stemming=TRUE))
  #reviews_dtm <- DocumentTermMatrix(corpus_reviews)
  
  # Return a data frame
  reviews_dtm <- as.matrix(reviews_dtm) %>%
    data.frame(row.names = row.names(.),stringsAsFactors = FALSE) %>%
    .[,-(1:13)] %>%
    mutate(doc_id = as.numeric(rownames(.)))
  
  return(reviews_dtm)
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

# Creating a term document matrix
tm_matrix <- get_termdocument(tripadvisor_reviews)

# Joining the reviews with the term document df
tripadvisor_reviews_tm <- inner_join(tripadvisor_reviews,tm_matrix,by="doc_id")

# Saving the final df
saveRDS(tripadvisor_reviews_tm, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_",as.character(Sys.Date()),".rds")))

write_excel_csv(tripadvisor_reviews_tm,path=file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_",as.character(Sys.Date()),".csv")))


