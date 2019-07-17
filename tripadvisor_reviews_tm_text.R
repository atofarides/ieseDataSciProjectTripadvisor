# Creating a term document matrix from review text

#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("readr")
library("tm")
library("SnowballC")

get_termdocument <- function(reviews){
  # Creating the corpus
  corpus_reviews <- reviews %>%
    DataframeSource() %>%
    VCorpus(readerControl = list(language="spanish"))
  
  # Cleaning the corpus
  corpus_reviews <- corpus_reviews %>%
    tm_map(removePunctuation,ucp=TRUE) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords,c("�","\200",stopwords("spanish"))) %>%
    tm_map(removeNumbers) %>%
    tm_map(stripWhitespace) %>%
    tm_map(content_transformer(str_trim),side="left") 
  
  # Creating a document term matrix
  reviews_dtm <- DocumentTermMatrix(corpus_reviews, control=list(stemming=FALSE))
  
  # Return a data frame
  reviews_dtm <- as.matrix(reviews_dtm) %>%
    data.frame(row.names = row.names(.),stringsAsFactors = FALSE) %>%
    .[,-(1:13)] %>%
    mutate(doc_id = as.numeric(rownames(.))) 
  
  # Choose top 100 words - to be further refined
  reviews_dtm_top <- reviews_dtm %>%
    colSums() %>%
    sort(decreasing = TRUE) %>%
    head(100) %>%
    names()
  
  reviews_dtm <- reviews_dtm[,reviews_dtm_top]
  
  return(reviews_dtm)
}

# Functione to combine synonyms and word variables
combine_words <- function(matrix,cols,name){
  combined_col <- rowSums(subset(matrix,select = cols)) 
  matrix <- subset(matrix,select = !(names(matrix) %in% cols))
  combined_matrix <- cbind(matrix,combined_col) 
  names(combined_matrix)[length(names(combined_matrix))] <- name
  return(combined_matrix)
}

# Read the latest version of review data 
tripadvisor_reviews <- readRDS(file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_",as.character(Sys.Date()),".rds")))

# Creating a term document matrix
tm_matrix <- get_termdocument(tripadvisor_reviews)

# Refining the matrix
# Combining synonyms or word variations
tm_matrix_refined <- combine_words(tm_matrix,c("bien","buen","bueno","buena","buenos"),c("bien")) %>%
  combine_words(c("comer","comida"),c("comida")) %>%
  combine_words(c("japon�s","japonesa","japoneses"),c("japonesa")) %>%
  combine_words(c("mejor","mejores"),c("mejor")) %>%
  combine_words(c("mesa","mesas"),c("mesa")) %>%
  combine_words(c("pedido","pedimos","pedir"),c("pedir")) %>%
  combine_words(c("plato","platos"),c("plato")) %>%
  combine_words(c("precio","precios"),c("precio")) %>%
  combine_words(c("probado","probar"),c("probado")) %>%
  combine_words(c("puede","puedes"),c("puede"))  %>%
  combine_words(c("recomiendo","recomendable"),c("recomendable")) %>%
  combine_words(c("restaurante","restaurantes"),c("restaurante")) %>%
  combine_words(c("vez","veces"),c("vez")) %>%
  combine_words(c("atento","atenci�n"),c("atenci�n"))

# Removing irrelevant words or out of context words
words <- c("adem�s","as�","aunque","cada","decir","dos","hace","ido","mas","menos","puede","ser","sevilla","siempre","solo","tan","vez")
tm_matrix_refined <- tm_matrix_refined[,!(names(tm_matrix_refined) %in% words)]

# Choosing the top 25 words 
top_words <- tm_matrix_refined %>%
  colSums() %>%
  sort(decreasing = TRUE) %>%
  head(50) %>%
  names() 

tm_matrix_refined_top <- tm_matrix_refined[,top_words]

# Joining the reviews with the term document df
tripadvisor_reviews_tm <- inner_join(tripadvisor_reviews,tm_matrix_refined_top,by="doc_id")

# Saving the final df
saveRDS(tripadvisor_reviews_tm, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_tm_text_",as.character(Sys.Date()),".rds")))

write_excel_csv(tripadvisor_reviews_tm,path=file.path("~","GitHub","ieseDataSciProjectTripadvisor",paste0("tripadvisor_reviews_tm_text_",as.character(Sys.Date()),".csv")))

