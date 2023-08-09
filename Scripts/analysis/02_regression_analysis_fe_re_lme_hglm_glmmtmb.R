#Regression analysis


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","glmmTMB","lme4","hglm","margins","prediction","here","misty","arm","patchwork"))

#renv::snapshot()


# data --------------------------------------------------------------------

likeRetweetData<- readRDS(file = here("Data","Regression_data","analysis_data_allvars.RDS")) %>% 
  rename(Actor_type = Actor_type.x) %>% 
  dplyr::select(-Actor_type.y) %>% 
  rename(valence = senti_score) %>% 
  rename(t_cluster = cluster_n) %>% 
  mutate(other_actors = case_when(V201_subject_of_publicity == "Compound" ~1,.default = 0)) %>% 
  drop_na(user_id) %>% 
  mutate(Actor_type = recode(Actor_type,"Comissioner" = "Commissioner","Executive vice-president" = "Commissioner")) %>% 
  filter(V301_06_)
  
quoteData<- likeRetweetData %>% drop_na(quote_count)

replyData<- likeRetweetData %>% drop_na(reply_count)

level1Indicators<- c("familiarity","Flesch.Kincaid","valence",
                     "V301_01_Identity_and_mandate","V301_02_Output","V301_03_Activity",              
                     "V301_04_Opinion","V301_05_Input_seeking","V301_06_Other","other_actors")

level2Indicators<-c("followers_count",                "Actor_type",                    
                    "Agriculture_Fisheries",          "Competetiveness",                "Digit_trans",                   
                    "Economic.and.Financial.affairs", "Education_youth",                "Environment",                   
                    "ESHC",                           "Foreign.Affairs",                "General_Affairs",               
                    "Justice_Home_affairs",           "FCluster_1")


indicator_labels = c("(Intercept)" = "Intercept","familiarity" = "Word familiarity",                                     
                     "Flesch.Kincaid" = "Ease-of-Reading",                                   "valence"="Valence",                                         
                     "V301_01_Identity_and_mandateyes" = "Content: Identity and Mandate",                  "V301_02_Outputyes" = "Content: Output",                               
                     "V301_03_Activityyes" = "Content: Activity",                              "V301_04_Opinionyes"="Content: Opinion",                              
                     "V301_05_Input_seekingyes"="Content: Input Seeking",                         "other_actors" = "Other actor mention",                                    
                     "followers_count" = "Follower count",                                  "Actor_typeCommissioner" = "Actor type: Commissioner",                           
                     "Actor_typeDeputy director general" = "Actor type: Deputy director general",                "Actor_typeDirector general"="Actor type: Director general",                      
                     "Actor_typeDirectorate general" = "Actor type: Directorate general",                    "Actor_typeExecutive vice-president"="Actor type: Executive vice-president",              
                     "Actor_typeHigh representative and vice president" = "Actor type: High Representative", "Actor_typeInstitution" = "Actor type: Institution",                           
                     "Actor_typePresident" = "Actor type: President",                              "Actor_typeVice president" = "Actor type: Vice-president",                        
                     "Agriculture_Fisheries1" = "Policy area: Agriculture&Fisheries",                           "Competetiveness1"= "Policy area: Competetiveness",                                
                     "Digit_trans1" = "Policy area: Digital transformation",                                     "Economic.and.Financial.affairs1" = "Policy area: Economy&Finances",                 
                     "Education_youth1" = "Policy area: Education&Youth",                                 "Environment1" = "Policy area: Environment",                                    
                     "ESHC1" ="Policy area: ESHC",                                            "Foreign.Affairs1"="Policy area: Foreign affairs",                                
                     "General_Affairs1" = "Policy area: General affairs",                                 "Justice_Home_affairs1" ="Policy area: Justice and home affairs",                           
                     "FCluster_1" ="Superspreader follower share","V301_06_Other" = "Content: Other")

#log transformations

likeRetweetData<-likeRetweetData %>% 
  mutate(logFavorite = log(favorite_count+1),
         logRetweet = log(retweet_count+1))

quoteData<- quoteData %>% 
  mutate(logQuote = log(quote_count+1))

replyData<- replyData %>% 
  mutate(logReply = log(reply_count+1))

#factorise user names

likeRetweetData<- likeRetweetData %>% 
  mutate(screen_namef = as.factor(screen_name))

quoteData<- quoteData %>% 
  mutate(screen_namef = as.factor(screen_name))

replyData<- replyData %>% 
  mutate(screen_namef = as.factor(screen_name))

#scale numeric variables
likeRetweetData<- likeRetweetData %>% 
  mutate(familiarity = as.vector(scale(familiarity)),
         followers_count = as.vector(scale(followers_count)),
         FCluster_1 = as.vector(scale(FCluster_1)),
         FCluster_2 = as.vector(scale(FCluster_2)),
         FCluster_3 = as.vector(scale(FCluster_3)),
         Flesch.Kincaid = as.vector(scale(Flesch.Kincaid)),
         valence = as.vector(scale(valence)),
         logFavorite = as.vector(scale(logFavorite))) %>% 
  mutate(across(V301_01_Identity_and_mandate:V201_subject_of_publicity,~as.factor(.x))) %>% 
  mutate(Actor_type = as.factor(Actor_type)) %>% 
  mutate(across(Agriculture_Fisheries:Justice_Home_affairs,~as.factor(.x))) %>% 
  mutate(t_cluster = as.factor(t_cluster)) %>% 
  drop_na(familiarity)

quoteData<- quoteData %>%   
  mutate(familiarity = as.vector(scale(familiarity)),
         followers_count = as.vector(scale(followers_count)),
         FCluster_1 = as.vector(scale(FCluster_1)),
         FCluster_2 = as.vector(scale(FCluster_2)),
         FCluster_3 = as.vector(scale(FCluster_3)),
         Flesch.Kincaid = as.vector(scale(Flesch.Kincaid)),
         valence = as.vector(scale(valence)),
         logQuote = as.vector(scale(logQuote))) %>% 
  mutate(across(V301_01_Identity_and_mandate:V201_subject_of_publicity,~as.factor(.x))) %>% 
  mutate(Actor_type = as.factor(Actor_type)) %>% 
  mutate(across(Agriculture_Fisheries:Justice_Home_affairs,~as.factor(.x))) %>% 
  mutate(t_cluster = as.factor(t_cluster)) 

replyData<-replyData %>%   
  mutate(familiarity = as.vector(scale(familiarity)),
         followers_count = as.vector(scale(followers_count)),
         FCluster_1 = as.vector(scale(FCluster_1)),
         FCluster_2 = as.vector(scale(FCluster_2)),
         FCluster_3 = as.vector(scale(FCluster_3)),
         Flesch.Kincaid = as.vector(scale(Flesch.Kincaid)),
         valence = as.vector(scale(valence)),
         logReply = as.vector(scale(logReply))) %>% 
  mutate(across(V301_01_Identity_and_mandate:V201_subject_of_publicity,~as.factor(.x))) %>% 
  mutate(Actor_type = as.factor(Actor_type)) %>% 
  mutate(across(Agriculture_Fisheries:Justice_Home_affairs,~as.factor(.x))) %>% 
  mutate(t_cluster = as.factor(t_cluster))

#create composite and binary indicators

# variance ratios and intraclass correlation ------------------------------------------------------------

likeICC<- multilevel.icc(x = likeRetweetData$logFavorite,cluster = likeRetweetData$screen_namef)
retweetICC<- multilevel.icc(x = likeRetweetData$logRetweet,cluster = likeRetweetData$screen_namef)
quoteICC<-multilevel.icc(x = quoteData$logQuote,cluster = quoteData$screen_namef)
replyICC<- multilevel.icc(x= replyData$logReply,cluster = replyData$screen_namef)

icc_df<- data.frame(variable = c("favorite","retweet","quote","reply"),
                    icc = round(c(likeICC,retweetICC,quoteICC,replyICC),2))


icc_df %>% flextable::qflextable() %>% flextable::save_as_docx(path = here("Results","summary stats","intraclass_correlation.docx"))


# Regression models -------------------------------------------------------

#model variables
fe_vars <- paste0(paste(level1Indicators,collapse = "+"),"+",paste(level2Indicators,collapse = "+"))
re_vars<- paste0("(1+",paste(level1Indicators[-9],collapse = "+"),"|screen_namef)")

#hbn composite engagement
compositeData<- likeRetweetData %>% 
  mutate(quote_count = replace_na(quote_count, 0),
         reply_count = replace_na(reply_count,0)) %>% 
  mutate(total_engagement = (favorite_count+retweet_count+quote_count+reply_count))

nb_glm_eng_formula<- as.formula(paste0("total_engagement","~",fe_vars,"+",re_vars))

nb_eng<- glmmTMB(nb_glm_eng_formula,
                  zi = ~0,
                  family = nbinom2(),
                  data = compositeData,
                  na.action = "na.omit",
                  se = T,
                  verbose = T)

summary(nb_eng)

saveRDS(nb_eng,file = here("Results","regression results","glmmtmb_nb_eng.RDS"))

sjPlot::tab_model(nb_eng,
                  pred.labels = indicator_labels,
                  dv.labels = "Composite engagement",
                  title = "Hierarchical negative binomial regression for composite engagement")


#hnb - like

nb_glm_like_formula<- as.formula(paste0("favorite_count","~",fe_vars,"+",re_vars))

nb_like<- glmmTMB(nb_glm_like_formula,
                    zi = ~0,
                    family = nbinom2(),
                    data = likeRetweetData,
                    na.action = "na.omit",
                    se = T,
                    verbose = T)

summary(nb_like)

saveRDS(nb_like, file = here("Results","regression results","glmmtmb_nb_like.RDS"))

sjPlot::tab_model(nb_like,
                  pred.labels = indicator_labels,
                  dv.labels = "Favorite",
                  title = "Hierarchical negative binomial regression for favorite count")



#hnb - retweet

nb_glm_retweet_formula<- as.formula(paste0("retweet_count","~",fe_vars,"+",re_vars))

nb_retweet<- glmmTMB(nb_glm_retweet_formula,
                  zi = ~0,
                  family = nbinom2(),
                  data = likeRetweetData,
                  se = T,
                  verbose = T)
summary(nb_retweet)
saveRDS(nb_retweet,file = here("Results","regression results","glmmtmb_nb_retweet.RDS"))

sjPlot::tab_model(nb_retweet,
                  pred.labels = indicator_labels,
                  dv.labels = "retweet",
                  title = "Hierarchical negative binomial regression for retweet count")

#hnb - quote

nb_glm_quote_formula<- as.formula(paste0("quote_count","~",fe_vars,"+",re_vars))

nb_quote<- glmmTMB(nb_glm_quote_formula,
                  zi = ~0,
                  family = nbinom2(),
                  data = quoteData,
                  se = T,
                  verbose = T)
summary(nb_quote)
saveRDS(nb_quote,file = here("Results","regression results","glmmtmb_nb_quote.RDS"))

sjPlot::tab_model(nb_quote,
                  pred.labels = indicator_labels,
                  dv.labels = "Quote",
                  title = "Hierarchical negative binomial regression for quote count")

#hnb - reply

nb_glm_reply_formula<- as.formula(paste0("reply_count","~",fe_vars,"+",re_vars))

nb_reply<- glmmTMB(nb_glm_reply_formula,
                   zi = ~0,
                   family = nbinom2(),
                   data = replyData,
                   se = T,
                   verbose = T)
summary(nb_reply)
saveRDS(nb_reply,file = here("Results","regression results","glmmtmb_nb_reply.RDS"))

sjPlot::tab_model(nb_reply,
                  pred.labels = indicator_labels,
                  dv.labels = "Reply",
                  title = "Hierarchical negative binomial regression for reply count")

#finally
save(nb_like,nb_retweet,nb_quote,nb_reply,file = here("Results","regression results","glmmtmb_models.Rdata"))
