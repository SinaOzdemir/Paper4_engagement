#google corpus script


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","here",
                "quanteda","sophistication",
                "tidytext","ngramr"))


# data --------------------------------------------------------------------

data<- readRDS(file = here("Data","Regression_data","full_analysis_data.RDS"))

tweets<- data$text
names(tweets)<-data$status_id


# familiarity -------------------------------------------------------------

##try with sophistication

sophistication<- sophistication::covars_make_baselines(x = tweets)

familiarity<- sophistication %>% 
  rownames_to_column(var = "status_id") %>% 
  select(status_id,google_mean_2000) %>% 
  rename(familiarity = google_mean_2000)

data<- left_join(data,familiarity,by = "status_id")

saveRDS(data,file = here("Data","Regression_data","analysis_data_allvars.RDS"))
