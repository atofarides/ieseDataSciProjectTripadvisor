# Creating a term document matrix from review text

#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")
library("readr")
library("tm")
library("SnowballC")
library("ggplot2")

get_termdocument <- function(reviews){
  # Creating the corpus
  corpus_reviews <- reviews %>%
    DataframeSource() %>%
    VCorpus(readerControl = list(language="spanish"))
  
  # Cleaning the corpus
  corpus_reviews <- corpus_reviews %>%
    tm_map(removePunctuation,ucp=TRUE) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(removeWords,c("´","\200",stopwords("spanish"))) %>%
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
  
  # Binarising results in term document matrix
  for(i in 1:dim(reviews_dtm)[1]){
    for(j in 2:dim(reviews_dtm)[2]){
      reviews_dtm[i,j] <- ifelse(reviews_dtm[i,j]>=1,1,0)
    }
  }
  
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
tripadvisor_reviews <- readRDS(file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews.rds"))

# Remove entries from restaurants with less than 100 reviews
restaurants_over_100 <- tripadvisor_reviews %>%
  group_by(restaurant_name) %>%
  count(restaurant_name) %>%
  filter(n>100) %>%
  select(restaurant_name) 

tripadvisor_reviews <- tripadvisor_reviews[tripadvisor_reviews$restaurant_name %in% restaurants_over_100$restaurant_name,]

# Creating a term document matrix
tm_matrix <- get_termdocument(tripadvisor_reviews)

# Refining the matrix
# Combining synonyms or word variations
tm_matrix_refined <- combine_words(tm_matrix,c("bien","buen","bueno","buena","buenos"),c("bien")) %>%
  combine_words(c("comer","comida"),c("comida")) %>%
  combine_words(c("japonés","japonesa","japoneses"),c("japonesa")) %>%
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
  combine_words(c("atento","atención"),c("atención"))

# Removing irrelevant words or out of context words
words <- c("además","así","aunque","cada","decir","dos","hace","ido","mas","menos","puede","ser","sevilla","siempre","solo","tan","vez")
tm_matrix_refined <- tm_matrix_refined[,!(names(tm_matrix_refined) %in% words)]

# Choosing the top 50 words and doc_id
top_words <- tm_matrix_refined %>%
  colSums() %>%
  sort(decreasing = TRUE) %>%
  head(51)

top_words_no_doc_id <- top_words[2:51] %>%
  as.data.frame()

# Plotting top words

top_words_plot <- ggplot(top_words_no_doc_id,aes(x=reorder(row.names(top_words_no_doc_id),.),y=.))+
  geom_col() +
  coord_flip() +
  labs(title = "Top words count from review text",y="Count",x="Top words")
top_words_plot

# CHoosing the top 50 words in the tm matrix

tm_matrix_refined_top <- tm_matrix_refined[,names(top_words)]

# Joining the reviews with the term document df
tripadvisor_reviews_tm <- inner_join(tripadvisor_reviews,tm_matrix_refined_top,by="doc_id")

# Saving the final df
saveRDS(tripadvisor_reviews_tm, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews_tm_text.rds"))

write_excel_csv(tripadvisor_reviews_tm,path=file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews_tm_text.csv"))

# Question 1: Which words correlate with higher scores, using a multiplication method

# Multiplying the terms by documents ratings
tripadvisor_reviews_q1 <- tripadvisor_reviews_tm
for(j in 8:dim(tripadvisor_reviews_q1)[2]){
  tripadvisor_reviews_q1[,j] <- tripadvisor_reviews_q1$review_rating*tripadvisor_reviews_q1[,j]
}

# Counting number of reviews with each term
term_counts <- tripadvisor_reviews_tm[,8:57] %>%
  colSums()

# Normalised scoring
tripadvisor_reviews_q1 <- tripadvisor_reviews_q1[,8:57] %>%
  colSums()/term_counts %>%
  sort(decreasing = TRUE) %>%
  as.data.frame()

#Plot
top_score_plot <- ggplot(tripadvisor_reviews_q1,aes(x=reorder(row.names(tripadvisor_reviews_q1),.),y=.))+
  geom_col() +
  coord_flip(ylim = c(3.4,5)) +
  labs(title = "Top words from review text, scored by review ratings",y="Score",x="Top words")
top_score_plot

