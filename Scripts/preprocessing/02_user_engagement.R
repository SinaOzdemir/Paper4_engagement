# engagement extractor:


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","feather"))



# paths -------------------------------------------------------------------

followers_path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/follower_tweets"

replier_path = "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/replier_tweets"

user_files = c(list.files(followers_path,
                          pattern ="*.RDS",
                          full.names = T ),
               list.files(replier_path,
                          pattern = "*.RDS",
                          full.names = T))

eu_accounts = readxl::read_excel(path = here("Data","EU_meta_data","general_eu_accounts_information.xlsx"),sheet = 1) %>% 
  pull(user_id) %>% gsub("x_","",.)

user_engagemet_data<- here("Data","User_engagement_data")
tweets_data<- here("Data","EU_tweet_data")
cluster_path<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/clustering_results/kmeans_n3_clustering_results.Rdata"

# identify cluster shares -------------------------------------------------
load(cluster_path)
remove(kmeans_n3,centers)

tweet_data<-readRDS(file = here("Data","EU_tweet_data","paper4_tweets_analysis_data.RDS")) %>% distinct(status_id,.keep_all = T)
#TODO:
##1) reply and quote counts must be extracted from engaging user data

fe_data_path<- "C:/Users/sinaoz/OneDrive - NTNU/Work/Trondheim/PHD/Papers/Paper 3 the audience of the EU on SoMe/Data/L1_network_data"

fe_data_dirs<- list.files(path = fe_data_path,pattern = "L1_|.feather",full.names = T) %>% 
  grep(x = .,pattern = "_[[:digit:]]",perl = T,value = T)

clusters<- clusters %>% mutate(user_id = gsub("x_","",user_id))

eu_follower_cluster_info<- data.frame()
for (i in 1:length(fe_data_dirs)) {
  cat(i,"\n")
  a<- read_feather(fe_data_dirs[i])
  b<-left_join(a,clusters, by = "user_id") %>% filter(!is.na(cluster))
  eu_follower_cluster_info<- rbind(eu_follower_cluster_info,b)
  remove(a,b)
  gc()
}

eu_follower_count<- eu_follower_cluster_info %>% 
  group_by(eu_accounts) %>% 
  tally(name = "follower_count")

eu_follower_cluster_counts<- eu_follower_cluster_info %>% 
  group_by(eu_accounts, cluster) %>% 
  tally(name = "cluster_size")

eu_follower_type_share<- left_join(eu_follower_cluster_counts, eu_follower_count,by = "eu_accounts") %>% 
  mutate(perc = cluster_size/follower_count)

eu_follower_type_wide<- eu_follower_type_share %>% 
  mutate(cluster = recode(cluster, "1" = "FCluster_1","2" = "FCluster_2", "3" = "FCluster_3")) %>% 
  pivot_wider(id_cols = eu_accounts, names_from = cluster,values_from = perc) %>% 
  rename(user_id = eu_accounts) 

eu_meta<-readxl::read_excel(path = here("Data","EU_meta_data","general_eu_accounts_information.xlsx"),sheet = 1) %>% 
  select(user_id,screen_name) %>% 
  mutate(screen_name = tolower(screen_name)) %>% 
  mutate(user_id = gsub("x_","",user_id)) 

eu_follower_type_wide<- left_join(eu_follower_type_wide,eu_meta, by = "user_id")

# create dependent variables --------------------------------------------------------------


analysis_data<- left_join(tweet_data,eu_follower_type_wide,by = "screen_name") %>% select(-quote_count,-reply_count)

#extracting quote and reply count of the tweets to analyze from the follower data
user_engagement<- read.table(file = here("Data","User_engagement_data","user_engagements.txt"),header = T,sep = ",")

quoted_tweets<- user_engagement %>% 
  mutate(quoted_status_id = gsub("x_","",quoted_status_id)) %>% 
  filter(quoted_status_id%in%analysis_data$status_id) %>% 
  group_by(quoted_status_id) %>% 
  tally(name = "quote_count") %>% 
  rename(status_id = quoted_status_id) 

analysis_data_b<- left_join(analysis_data,quoted_tweets,by = "status_id")

replied_tweets<- user_engagement %>% 
  mutate(reply_to_status_id = gsub("x_","",reply_to_status_id)) %>% 
  filter(reply_to_status_id%in%analysis_data$status_id) %>% 
  group_by(reply_to_status_id) %>%
  tally(name = "reply_count") %>% 
  rename(status_id = reply_to_status_id)

analysis_data_c<- left_join(analysis_data_b,replied_tweets,by = "status_id")
  
saveRDS(replied_tweets,file = here("Data","EU_tweet_data","reply_analysis.RDS"))


saveRDS(analysis_data_c,here("Data","EU_tweet_data","analysis_data.RDS"))
