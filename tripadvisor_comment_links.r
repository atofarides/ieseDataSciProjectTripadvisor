# Project to source comment links of Tripadvisor trip - Japanese restaurants in Sevilla

#Loading libraries
library("XML")
library("rvest")
library("tidyverse")
library("stringr")

#Defining restaurant URL function
get_restaurant_links <- function(link){
  
  restaurants_html <- read_html(link)
  restaurant_links <- html_nodes(restaurants_html, xpath = "//div[contains(@class, 
                                 'trips-trip-view-page-TripSections-common-CardHeader-BaseRow__title_line--ysGYQ')]/a") %>%
    html_attrs() %>%
    unlist() %>%
    paste("https://www.tripadvisor.es",.,sep="")
  return(restaurant_links)
}

# Defining pagination link url function
get_pagination_links <- function(link){
  max_page <- read_html(link) %>%
    html_nodes(.,xpath = "//div[contains(@class,'pageNumbers')]/a") %>%
    html_attr(name = "data-offset") %>%
    na.omit()%>%
    as.numeric()%>%
    max()
  
  if(is.infinite(max_page)){
    pagination_sequence <- 0
  } else{
    pagination_sequence <- seq(from=0,to=max_page,by=10)
  }
  
  i <- 0
  page_links <- 0
  for(s in pagination_sequence){
    i <- i+1
    if(s==0){
      page_links[i] <- link
    }
    else {
      page_links[i] <- str_replace(link,pattern = "(Reviews-)",paste("\\1or",s,"-",sep=""))
    }
  }
    
  return(page_links)
}

# Defining comment link function
get_comment_links <- function(link){
  comment_links <- read_html(link) %>%
    html_nodes(.,xpath = "//div[contains(@class,'quote')]/a") %>%
    html_attr("href") %>%
    unlist() %>%
    paste("https://www.tripadvisor.es",.,sep="")
  return(comment_links)
}

#Extracting restaurant URLs

url <- c('https://www.tripadvisor.es/Trips/88904583/Data_sev_rest_japo')
restaurant_links <- get_restaurant_links(url)

saveRDS(restaurant_links,file = file.path("~","GitHub","ieseDataSciProjectTripadvisor","restaurant_links.rds"))

# Extracting pagination links
pagination_links <- unlist(lapply(restaurant_links, get_pagination_links))

# Extracting and saving comment links, it's a time-consuming operation
comment_links <- unlist(lapply(pagination_links,get_comment_links))

saveRDS(comment_links,file = file.path("~","GitHub","ieseDataSciProjectTripadvisor","comment_links.rds"))



