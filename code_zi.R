library(dplyr)
library(rvest)
library(stringr)
library(ggplot2)
library(lubridate)

beg_url <- "https://tieba.baidu.com/f?kw=%E5%BE%81%E5%A9%9A&ie=utf-8&pn="

custom_pages <- seq(0, 1500, by=50)

empty_list <- list()

for (i in custom_pages) { 
  post_links <- print(paste0(beg_url, i))
  empty_list <- rbind(empty_list, post_links)}

big_list_of_1000_links <- empty_list

list_of_dfs <- list()

for (i in big_list_of_1000_links){ 
  
  url <- i 
  website <- read_html(i)  
  
  posts <- website %>% 
    html_elements("div.threadlist_abs.threadlist_abs_onlyline") %>% 
    html_text2()
  
  date <- website %>% 
    html_nodes(".threadlist_reply_date")  %>% 
    html_text2()

  page_df <- data.frame(posts = posts,
                        date = date,
                        stringsAsFactors = F)
    
  list_of_dfs <- rbind(list_of_dfs, page_df)
  
  rand_num <- sample(10:20, 1)
  Sys.sleep(rand_num)
  
  print(paste("Sleeping", rand_num, "seconds!")) 
}

#Filtering

posts_clean <- list_of_dfs %>%
  filter(posts != "" & !str_detect(posts, "^\\p{P}+$"))

posts_clean <- posts_clean %>%
  distinct(posts, .keep_all = TRUE)

posts_clean <- posts_clean %>%
  mutate(word_count = str_count(posts, "[\\u4e00-\\u9fff]"))

summary(posts_clean$word_count)
ggplot(posts_clean, aes(x=word_count)) +
  geom_histogram(binwidth=1, fill="blue", color="black") +
  theme_minimal() +
  labs(title="Word Count Distribution",
       x="Word Count",
       y="Frequency")

threshold <- 30
posts_clean <- posts_clean %>%
  filter(word_count > threshold)

posts_clean <- posts_clean %>%
  mutate(date = if_else(str_detect(date, ":"), " 3-11", date))



library(openxlsx)
getwd()
setwd("C:/Users/13201/OneDrive/SSDA/SIMP56")
write.xlsx(posts_clean, "posts_clean.xlsx")
write.csv2(posts_clean, "posts_clean.csv")


