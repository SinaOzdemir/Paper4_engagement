# summary stats on the analysis variables:


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("here","tidyverse","psych","crosstable","flextable","patchwork"))



# dataset -----------------------------------------------------------------
data<- readRDS(file = here("Data","Regression_data","analysis_data_allvars.RDS")) %>% drop_na(user_id)

quotes<- data %>% drop_na(quote_count)

replies<- data %>% drop_na(reply_count)
# DVs ---------------------------------------------------------------------

like_count_sumstat <- psych::describe(x = data$favorite_count,skew = T,ranges = T,na.rm = F) %>% select(-trimmed,-mad,-se) %>% 
  mutate(vars = recode(vars, `1` = "Likes"))

retweet_count_stat<- psych::describe(x = data$retweet_count,skew = T,ranges = T,na.rm = F) %>% select(-trimmed,-mad,-se)%>% 
  mutate(vars = recode(vars, `1` = "Retweets"))

quote_count_stat<- psych::describe(x = quotes$quote_count,skew = T,ranges = T,na.rm = F) %>% select(-trimmed,-mad,-se) %>%
  mutate(vars = recode(vars, `1` = "Quotes"))

reply_count_sta<- psych::describe(x = replies$reply_count,skew = T,ranges = T,na.rm = F) %>% select(-trimmed,-mad,-se) %>% 
  mutate(vars = recode(vars, `1` = "Reply"))

sumstats<- rbind(like_count_sumstat,retweet_count_stat,quote_count_stat,reply_count_sta) %>% 
  rename(Variables = vars) %>% 
  mutate(across(mean:kurtosis,~round(.x,2)))
#might be useful for later on.
hist(data$favorite_count)
flextable::qflextable(sumstats) %>%
  flextable::bold(.,bold = T, part = "header") %>%
  save_as_docx(path = here("Results","summary stats","dv_sum_stat.docx"))



# Distributions -----------------------------------------------------------

#log transformation

like<- data %>%
  mutate(logFavorite = log(favorite_count+1),logRetweet = log(retweet_count+1)) %>%
  select(favorite_count,logFavorite,retweet_count,logRetweet) %>% 
  pivot_longer(cols = everything(),names_to = "variable",values_to = "values")

quote<- quotes %>% 
  mutate(logQuote = log(quote_count.x+1)) %>% 
  rename(quote_count = quote_count.x) %>% 
  select(quote_count,logQuote) %>% 
  pivot_longer(cols = everything(),names_to = "variables",values_to = "values")

reply<- replies %>% 
  mutate(logReply = log(reply_count.x+1)) %>% 
  rename(reply_count = reply_count.x) %>% 
  select(reply_count,logReply) %>% 
  pivot_longer(cols = everything(),names_to = "variables", values_to = "values")

#distribution plots

lr<- like %>% 
  ggplot(aes(x = values))+
  geom_density(aes(color = variable))+
  theme_bw()+
  facet_wrap(~variable)

qgr <- quote %>% 
  ggplot(aes(x = values))+
  geom_density(aes(color = variables),show.legend = F)+
  theme_bw()+
  facet_wrap(~variables)

rgr<-reply %>% 
  ggplot(aes(x = values))+
  geom_density(aes(color = variables),show.legend = F)+
  theme_bw()+
  facet_wrap(~variables)
#alternative plots

lgr_e<- data %>% 
  ggplot(aes(x=favorite_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Favorite count", title = "Empirical distribution of Favorites")

rgr_re<-data %>% 
  ggplot(aes(x=retweet_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Retweet count", title = "Empirical distribution of Retweet")

qgr_e<- quotes %>% 
  ggplot(aes(x = quote_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Quote count", title = "Empirical distribution of Quotes")

rgr_e<- replies %>% 
  ggplot(aes(x = reply_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Reply count", title = "Empirical distribution of Replies")


lgr_e+rgr_re+qgr_e+rgr_e

###trimmed distributions
t_lgr_e<- data %>% 
  filter(favorite_count<=13) %>% 
  ggplot(aes(x=favorite_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Favorite count", title = "Empirical distribution of Favorites")

t_rgr_re<-data %>% 
  filter(favorite_count<=38) %>% 
  ggplot(aes(x=retweet_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Retweet count", title = "Empirical distribution of Retweet")

t_qgr_e<- quotes %>% 
  filter(quote_count<=4) %>% 
  ggplot(aes(x = quote_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Quote count", title = "Empirical distribution of Quotes")

t_rgr_e<- replies %>% 
  filter(reply_count<=4) %>% 
  ggplot(aes(x = reply_count))+
  geom_density()+
  theme_bw()+
  labs(x = "Reply count", title = "Empirical distribution of Replies")

t_lgr_e+t_rgr_re+t_qgr_e+t_rgr_e
# I can probably do gamma link even with logged forms

dvs<- data %>% 
  summarise(across(all_of(c("retweet_count","favorite_count","quote_count","reply_count")),list(mean = mean,var = var, sd = sd),.names = "{.col}_{.fn}")) %>% 
  pivot_longer(cols = everything(),names_to = "stat",values_to = "value")
  
##moments don't exhibit poisson characteristics

#logging
logged_dvs<- data %>% 
  dplyr::select(retweet_count,favorite_count,quote_count,reply_count) %>% 
  mutate(across(everything(),~log(.x+1))) %>% 
  pivot_longer(cols = everything(),names_to = "variables",values_to = "value")

logged_dvs %>% ggplot(aes(x=value, group = variables))+
  geom_density(aes(color = variables))

### there is also something wrong with flesch score... it has values like -500 (which is not really realistic)


#GAMMA test

shape_rate<- function(variable){
  x = as.numeric(variable)
  
  mu_x<- mean(x,rm.na =T)
  var_x<- var(x,na.rm = T)
  
  Alpha.shape = (mu_x^2)/var_x
  Beta.rate = (mu_x)/var_x
  return(list(alpha = Alpha.shape, beta = Beta.rate,theta = 1/Beta.rate))
}

observed<- shape_rate(data$favorite_count)


td_lr<- data.frame(theoretical_favorite = rgamma(n = nrow(data),shape = shape_rate(data$favorite_count)$alpha,rate = shape_rate(data$favorite_count)$beta),
                                       theoretical_retweet = rgamma(n = nrow(data),shape = shape_rate(data$retweet_count)$alpha,rate = shape_rate(data$retweet_count)$beta))



td_q<-data.frame(theoretical_quote = rgamma(n = nrow(quotes),shape = shape_rate(quotes$quote_count.x)$alpha,rate = shape_rate(quotes$quote_count.x)$beta))

td_r<-data.frame(theoretical_reply = rgamma(n = nrow(replies),shape = shape_rate(replies$reply_count.x)$alpha,rate = shape_rate(replies$reply_count.x)$beta))

td_lgr<- td_lr %>% 
  ggplot(aes(x=theoretical_favorite))+
  geom_density()+
  theme_bw()+
  labs(title = "Theoretical gamma distribution for Favorites distribution", x = "Value",
       subtitle = paste0("Shape(alpha)=",round(shape_rate(data$favorite_count)$alpha,4),"\n","Rate(beta)=",round(shape_rate(data$favorite_count)$beta,5)))


td_rgr<- td_lr %>% 
  ggplot(aes(x=theoretical_retweet))+
  geom_density()+
  theme_bw()+
  labs(title = "Theoretical gamma distribution for Retweet distribution",x = "Value",
       subtitle = paste0("Shape(alpha)=",round(shape_rate(data$retweet_count)$alpha,4),"\n","Rate(beta)=",round(shape_rate(data$retweet_count)$beta,5)))

td_qgr<- td_q %>% 
  ggplot(aes(x = theoretical_quote))+
  geom_density()+
  theme_bw()+
  labs(title = "Theoretical gamma distribution for Quote distribution",x = "Value",
       subtitle = paste0("Shape(alpha)=",round(shape_rate(td_q$theoretical_quote)$alpha,4),"\n","Rate(beta)=",round(shape_rate(td_q$theoretical_quote)$beta,5)))


td_regr<- td_r %>% 
  ggplot(aes(x = theoretical_reply))+
  geom_density()+
  theme_bw()+
  labs(title = "Theoretical gamma distribution for Reply distribution",x = "Value",
       subtitle = paste0("Shape(alpha)=",round(shape_rate(td_r$theoretical_reply)$alpha,4),"\n","Rate(beta)=",round(shape_rate(td_r$theoretical_reply)$beta,5)))


td_lgr+td_rgr+td_qgr+td_regr
#it is gamma after all
# see https://stats.stackexchange.com/questions/96972/how-to-interpret-parameters-in-glm-with-family-gamma?fbclid=IwAR0htuAAGjI2o2e1K8X4aoGVd0eAO2_1OZzVDPeF4upDitx2wptiuGcQMh8

