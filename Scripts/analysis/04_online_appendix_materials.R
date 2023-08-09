#Additional materials for appendix


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here","flextable"))


# data --------------------------------------------------------------------

eu_accounts<- readxl::read_excel(path = here("Data","EU_meta_data","general_eu_accounts_information.xlsx"),sheet = 1)

like_retweet_data<- readRDS(file=here("Data","Regression_data","likeretweet_regression_data.RDS"))

quote_data<- readRDS(file=here("Data","Regression_data","quote_regression_data.RDS"))

reply_data<- readRDS(file=here("Data","Regression_data","reply_regression_data.RDS"))

user_names<- unique(c(like_retweet_data$screen_name.x,quote_data$screen_name,reply_data$screen_name))
# account list ------------------------------------------------------------


eu_meta<- eu_accounts %>% filter(screen_name_l%in%user_names) %>% 
  mutate(user_id = gsub("x_","",user_id)) %>% 
  mutate(screen_name = paste0("@",screen_name)) %>% 
  select(name,Actor_type,screen_name,user_id)

eu_meta %>% qflextable() %>% flextable::save_as_docx(path = here("Data","EU_meta_data","eu_analysis_accounts.docx"))
  
