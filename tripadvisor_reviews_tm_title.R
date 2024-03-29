# Creating a term document matrix from review titles

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
tripadvisor_reviews <- readRDS(file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews.rds"))

# Putting the title column as text
tripadvisor_reviews <- rename(tripadvisor_reviews,review_text = "text",text = "review_title") %>%
  select(1,"text",everything()) 

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
tm_matrix_refined <- combine_words(tm_matrix,c("bien","buen","bueno","buena","buen�simo"),c("bien")) %>%
  combine_words(c("comer","comida"),c("comida")) %>%
  combine_words(c("japon�s","japones","japonesa","japoneses","jap�n","japo"),c("japonesa")) %>%
  combine_words(c("mejor","mejores"),c("mejor")) %>%
  combine_words(c("rica","rico","riquisimo","riqu�simo"),c("rico")) %>%
  combine_words(c("sabor","sabores"),c("sabor")) %>%
  combine_words(c("sorpresa","sorprendente"),c("sorpresa"))

# Removing irrelevant words or out of context words
words <- c("aunque","siempre","sevilla","vez")
tm_matrix_refined <- tm_matrix_refined[,!(names(tm_matrix_refined) %in% words)]

# Binarising results in term document matrix
for(i in 1:dim(tm_matrix_refined)[1]){
  for(j in 2:dim(tm_matrix_refined)[2]){
    tm_matrix_refined[i,j] <- ifelse(tm_matrix_refined[i,j]>=1,1,0)
  }
}

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
  labs(title = "Top words count from review title",y="Count",x="Top words") +
  ggsave(file.path("~","GitHub","ieseDataSciProjectTripadvisor","top_terms_in_title.pdf"),
         width=30,height=15, units = "cm")
top_words_plot

# Choosing the top 50 words in the tm matrix

tm_matrix_refined_top <- tm_matrix_refined[,names(top_words)]

# Joining the reviews with the term document df
tripadvisor_reviews_tm <- inner_join(tripadvisor_reviews,tm_matrix_refined_top,by="doc_id")

# Saving the final df
saveRDS(tripadvisor_reviews_tm, file = file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews_tm_title.rds"))

write_excel_csv(tripadvisor_reviews_tm,path=file.path("~","GitHub","ieseDataSciProjectTripadvisor","tripadvisor_reviews_tm_title.csv"))

# Question 1: Which words correlate with higher scores, using a multiplication method

# Multiplying the terms by documents ratings
tripadvisor_reviews_q1 <- tripadvisor_reviews_tm
for(j in 8:dim(tripadvisor_reviews_q1)[2]){
  tripadvisor_reviews_q1[,j] <- tripadvisor_reviews_q1$review_rating*tripadvisor_reviews_q1[,j]
}

# Counting number of reviews with each term
term_counts_q1 <- tripadvisor_reviews_tm[,8:57] %>%
  colSums()

# Normalised scoring
tripadvisor_reviews_q1 <- tripadvisor_reviews_q1[,8:57] %>%
  colSums()/term_counts_q1 %>%
  sort(decreasing = TRUE) %>%
  as.data.frame()

#Plot
top_score_plot <- ggplot(tripadvisor_reviews_q1,aes(x=reorder(row.names(tripadvisor_reviews_q1),.),y=.))+
  geom_col() +
  coord_flip(ylim = c(1,5)) +
  labs(title = "Top words from review title, scored by review ratings",y="Score",x="Top words") +
  ggsave(file.path("~","GitHub","ieseDataSciProjectTripadvisor","top_scoring_terms_in_title.pdf"),
         width=30,height=15, units = "cm")
top_score_plot

# Question 2: Which words correlate with higher scores for each restaurant, using a multiplication method

# Multiplying the terms by documents ratings
tripadvisor_reviews_q2 <- tripadvisor_reviews_tm
for(j in 8:dim(tripadvisor_reviews_q2)[2]){
  tripadvisor_reviews_q2[,j] <- tripadvisor_reviews_q2$review_rating*tripadvisor_reviews_q2[,j]
}

# Counting number of reviews for each term for each restaurant
term_counts_q2 <- tripadvisor_reviews_tm[,c(4,8:57)] %>%
  group_by(restaurant_name) %>%
  summarise_at(.vars = names(.)[2:51] ,sum) 

# Normalised scoring for each restaurant 
tripadvisor_reviews_q2 <- tripadvisor_reviews_q2[,c(4,8:57)] %>%
  group_by(restaurant_name) %>%
  summarise_at(.vars = names(.)[2:51] ,sum) 

tripadvisor_reviews_q2_normalised <- tripadvisor_reviews_q2[,2:51]/term_counts_q2[,2:51]

tripadvisor_reviews_q2_normalised <- cbind(tripadvisor_reviews_q2[,1],tripadvisor_reviews_q2_normalised)

# Identify the top 5 and bottom 5 scoring terms for each restaurant

tripadvisor_reviews_q2_topbottom <- gather(tripadvisor_reviews_q2_normalised,key = "term", value = "score", -restaurant_name) %>%
  filter(score != 'NaN') %>%
  group_by(restaurant_name) %>%
  arrange(desc(score), .by_group = TRUE)

tripadvisor_reviews_q2_top <- tripadvisor_reviews_q2_topbottom %>%
  group_by(restaurant_name) %>%
  top_n(5)

tripadvisor_reviews_q2_bottom <- tripadvisor_reviews_q2_topbottom %>%
  group_by(restaurant_name) %>%
  top_n(-5)

#Plot top
top_score_plot_q2 <- ggplot(tripadvisor_reviews_q2_top,aes(x=term,y=score))+
  geom_col() +
  coord_flip() + 
  facet_wrap(~restaurant_name, scales = "free") +
  labs(title = "Top words from review title, scored by review ratings, for each restaurant",y="Score",x="Top words") +
  ggsave(file.path("~","GitHub","ieseDataSciProjectTripadvisor","top_scoring_restaurant_terms_in_title.pdf"),
         width=40,height=23,units="cm")
top_score_plot_q2

#Plot bottom
bottom_score_plot_q2 <- ggplot(tripadvisor_reviews_q2_bottom,aes(x=term,y=score))+
  geom_col() +
  coord_flip() + 
  facet_wrap(~restaurant_name, scales = "free") +
  labs(title = "Bottom words from review title, scored by review ratings, for each restaurant",y="Score",x="Bottom words") +
  ggsave(file.path("~","GitHub","ieseDataSciProjectTripadvisor","bottom_scoring_restaurant_terms_in_title.pdf"),
         width=40,height=23,units="cm")
bottom_score_plot_q2




