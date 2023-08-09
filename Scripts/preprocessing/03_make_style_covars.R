#3 data integrity check


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","Hmisc",
                "psych","quanteda",
                "quanteda.textstats","quanteda.dictionaries","quanteda.sentiment",
                "LSX"))


data<- readRDS(file = here("Data","EU_tweet_data","analysis_data.RDS"))

en_data<- data %>% filter(lang == "en")
# creating missing variables ----------------------------------------------
fk_score<- corpus(x = en_data, text_field = "text",docid_field = "status_id") %>% 
  quanteda.textstats::textstat_readability(measure = "Flesch.Kincaid") %>% 
  rename(status_id = document)

analysis_data<- left_join(en_data,fk_score,by = "status_id")

# semantic scaling --------------------------------------------------------

senti_seeds<- as.seedwords(data_dictionary_LSD2015)

senti_scale_data<- en_data %>% 
  corpus(text_field = "text",docid_field = "status_id") %>% 
  tokens(remove_punct = T,remove_symbols = T,remove_numbers = T,remove_url = T) %>% 
  tokens_remove(stopwords("en",source = "stopwords-iso")) %>% 
  dfm() %>% dfm_remove("")

senti_model<- textmodel_lss(senti_scale_data,seed = senti_seeds,k = 300, cache = T)

senti_score<- data.frame(screen_name = docvars(dfm_group(senti_scale_data))$screen_name,
                         status_id = docid(dfm_group(senti_scale_data)),
                         senti_score = predict(senti_model,newdata = dfm_group(senti_scale_data)))


analysis_data_b<- left_join(analysis_data,senti_score,by  =c("screen_name","status_id"))


# add missing control variables -------------------------------------------


eu_meta<- readxl::read_excel(path = here("Data","EU_meta_data","general_eu_accounts_information.xlsx"),sheet = 1) %>% 
  filter(screen_name_l%in%analysis_data_b$screen_name) %>% 
  select(screen_name,Actor_type,followers_count,Agriculture_Fisheries:total_pol_res) %>% 
  mutate(screen_name = tolower(screen_name))

analysis_data_c<- left_join(analysis_data_b,eu_meta,by = "screen_name")

saveRDS(analysis_data_c,file = here("Data","Regression_data","full_analysis_data.RDS"))
