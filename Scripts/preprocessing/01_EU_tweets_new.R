# paper 4 dataset revision


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here"))


# paths -------------------------------------------------------------------

eu_tweets<- here("Data","EU_tweet_data")

eu_meta<- here("Data","EU_meta_data")

# datasets ----------------------------------------------------------------


content_analysis<- readRDS(here(eu_tweets,"EU_comcontent_analysis_data.RDS"))

other_only<- content_analysis
content_analysis<- content_analysis %>% filter(!(status_id%in%other_only$status_id))

tweet_text_data<- readRDS(here(eu_tweets,"EU_data_011219_310720.RDS"))

content_cluster_data<- readRDS(here(eu_tweets,"paper_two_two_clusters_w_ml_data.RDS"))
eu_account_data<- readxl::read_excel(path = here(eu_meta,"general_eu_accounts_information.xlsx"),sheet = 1)


# tweet_content -----------------------------------------------------------

tweet_text_data_trimmed<- tweet_text_data %>% 
  mutate(screen_name = tolower(screen_name)) %>% 
  filter(screen_name%in%content_analysis$screen_name & status_id%in%content_analysis$status_id) %>% 
  distinct(status_id,.keep_all = T) %>% 
  select(screen_name,status_id,text,lang,favorite_count,retweet_count,quote_count,reply_count)

tweet_content_data<- left_join(tweet_text_data_trimmed,content_analysis,by = c("screen_name","status_id")) %>% 
  distinct(screen_name,status_id,.keep_all = T)


#tweet_cluster_info

tweet_meta_info<- tweet_content_data %>% 
  mutate(status_id_n = paste0(screen_name,"_",status_id)) %>% 
  select(screen_name,status_id,status_id_n)

content_cluster_data<- left_join(content_cluster_data,tweet_meta_info,by = c("status_id" = "status_id_n")) %>% 
  select(-status_id) %>% 
  rename(status_id = status_id.y)

tweet_content_data<- left_join(tweet_content_data,content_cluster_data,by = c("screen_name","status_id"))


saveRDS(object = tweet_content_data,file = here("Data","EU_tweet_data","paper4_tweets_analysis_data.RDS"))
