# Extracting regression results


# setup -------------------------------------------------------------------

library(pacman)

p_load(char = c("tidyverse","margins","prediction","here","arm","patchwork","flextable","stargazer","sjPlot","gtsummary"))

level1Indicators<- c("familiarity","Flesch.Kincaid","valence",
                     "V301_01_Identity_and_mandateyes","V301_02_Outputyes","V301_03_Activityyes",              
                     "V301_04_Opinionyes","V301_05_Input_seekingyes","other_actors")

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
                     "FCluster_1" ="Superspreader follower share")


# coefficients ------------------------------------------------------------
#Models
nb_eng<- readRDS(file = here("Results","regression results","glmmtmb_nb_eng.RDS"))
nb_like<-readRDS(file=here("Results","regression results","glmmtmb_nb_like.RDS"))
nb_retweet<-readRDS(file=here("Results","regression results","glmmtmb_nb_retweet.RDS"))
nb_quote<-readRDS(file=here("Results","regression results","glmmtmb_nb_quote.RDS"))
nb_reply<-readRDS(file=here("Results","regression results","glmmtmb_nb_reply.RDS"))

##negative binomial composite engagement model
nb_eng_summary<- summary(nb_eng)

nb_eng_fixed_ef<- as.data.frame(nb_eng_summary$coefficients$cond) %>% rownames_to_column(var = "predictors")

colnames(nb_eng_fixed_ef)<- c("predictors","coefs","std_errors","z_value","significance")

nb_eng_fixed_ef<- nb_eng_fixed_ef %>% 
  mutate(oddr = round((exp(coefs)-1),2)) %>% 
  mutate(p_trad = ifelse(significance<=0.001,"***",
                         ifelse(significance>0.001& significance<=0.01,"**",
                                ifelse(significance>0.01&significance<=0.05,"*","")))) %>% 
  mutate(p_five_sig = ifelse(significance<=0.00001,"***",
                             ifelse(significance>0.00001&significance<=0.0001,"**",
                                    ifelse(significance>0.0001&significance<=0.0005,
                                           yes = "*",no = "")))) %>% 
  mutate(oddr_sg = paste0(oddr,p_five_sig)) %>% 
  mutate(level1_indicators = ifelse(predictors%in%level1Indicators,1,0)) %>% 
  mutate(predictors = recode(predictors, !!!indicator_labels))

#


nb_eng_fixef_coef_graph<- nb_eng_fixed_ef %>%
  filter(predictors!="Intercept") %>% 
  ggplot(aes(x=reorder(predictors,level1_indicators),y = oddr,group = as.factor(level1_indicators)))+
  geom_point(aes(color = as.factor(level1_indicators)))+
  geom_text(aes(x = predictors, y = oddr,label = oddr_sg),nudge_x = .3)+
  geom_hline(yintercept = 0,color = "red")+
  guides(color = guide_legend(title = "Indicator levels"))+
  scale_color_discrete(labels = c("level-2","level-1"))+
  theme_bw()+
  labs(y = "Percentage change in incidence rate ratio", x = "Predictors",caption = "significance levels: ‘***’ 0.00001 ‘**’ 0.0001 ‘*’ 0.0005")+
  coord_flip()



nb_eng_bn_ranef<-as.data.frame(ranef(nb_eng)$cond$screen_namef) %>% rownames_to_column(var = "screen_name") %>% 
  pivot_longer(cols = `(Intercept)`:V301_05_Input_seekingyes,names_to = "predictors",values_to = "value") %>% 
  mutate(predictors = recode(predictors,!!!indicator_labels)) %>% 
  left_join(.,nb_eng_fixed_ef,by = "predictors") %>% 
  mutate(ran_slopes = coefs+value) %>% 
  mutate(ran_slopes = exp(ran_slopes)-1)


nb_eng_random_effects_box_plot<- nb_eng_bn_ranef %>% 
  filter(predictors!="Intercept") %>% 
  group_by(predictors) %>% 
  summarise(avg_effect = mean(ran_slopes),
            min_effect = min(ran_slopes),
            max_effect = max(ran_slopes)) %>% 
  ggplot(aes(x = predictors,y = avg_effect,color = predictors))+
  geom_point(aes(size =avg_effect),show.legend = F)+
  geom_errorbar(aes(x = predictors, ymin= min_effect, ymax = max_effect),width = .04,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  theme_bw()+
  coord_flip()+
  labs(x = "Predictors",y="Percentage change in incidence rate ratio",title = "Likes")




##negative binomial like model

nb_like_summary<- summary(nb_like)

nb_like_fixed_ef<- as.data.frame(nb_like_summary$coefficients$cond) %>% rownames_to_column(var = "predictors")

colnames(nb_like_fixed_ef)<- c("predictors","coefs","std_errors","z_value","significance")

nb_like_fixed_ef<- nb_like_fixed_ef %>% 
  mutate(oddr = round((exp(coefs)-1),2)) %>% 
  mutate(p_trad = ifelse(significance<=0.001,"***",
                         ifelse(significance>0.001& significance<=0.01,"**",
                                ifelse(significance>0.01&significance<=0.05,"*","")))) %>% 
  mutate(p_five_sig = ifelse(significance<=0.00001,"***",
                             ifelse(significance>0.00001&significance<=0.0001,"**",
                                    ifelse(significance>0.0001&significance<=0.0005,
                                           yes = "*",no = "")))) %>% 
  mutate(oddr_sg = paste0(oddr,p_five_sig)) %>% 
  mutate(level1_indicators = ifelse(predictors%in%level1Indicators,1,0)) %>% 
  mutate(predictors = recode(predictors, !!!indicator_labels))

#


nb_like_fixef_coef_graph<- nb_like_fixed_ef %>%
  filter(predictors!="Intercept") %>% 
  ggplot(aes(x=reorder(predictors,level1_indicators),y = oddr,group = as.factor(level1_indicators)))+
  geom_point(aes(color = as.factor(level1_indicators)))+
  geom_text(aes(x = predictors, y = oddr,label = oddr_sg),nudge_x = .3)+
  geom_hline(yintercept = 0,color = "red")+
  guides(color = guide_legend(title = "Indicator levels"))+
  scale_color_discrete(labels = c("level-2","level-1"))+
  theme_bw()+
  labs(y = "Percentage change in incidence rate ratio", x = "Predictors",title = "Likes",caption = "significance levels: ‘***’ 0.00001 ‘**’ 0.0001 ‘*’ 0.0005")+
  coord_flip()



nb_like_bn_ranef<-as.data.frame(ranef(nb_like)$cond$screen_namef) %>% rownames_to_column(var = "screen_name") %>% 
  pivot_longer(cols = `(Intercept)`:V301_05_Input_seekingyes,names_to = "predictors",values_to = "value") %>% 
  mutate(predictors = recode(predictors,!!!indicator_labels)) %>% 
  left_join(.,nb_like_fixed_ef,by = "predictors") %>% 
  mutate(ran_slopes = coefs+value) %>% 
  mutate(ran_slopes = exp(ran_slopes)-1)
  
  
nb_likes_random_effects_box_plot<- nb_like_bn_ranef %>% 
  filter(predictors!="Intercept") %>% 
  group_by(predictors) %>% 
  summarise(avg_effect = mean(ran_slopes),
            min_effect = min(ran_slopes),
            max_effect = max(ran_slopes)) %>% 
  ggplot(aes(x = predictors,y = avg_effect,color = predictors))+
  geom_point(aes(size =avg_effect),show.legend = F)+
  geom_errorbar(aes(x = predictors, ymin= min_effect, ymax = max_effect),width = .04,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  theme_bw()+
  coord_flip()+
  labs(x = "Predictors",y="Percentage change in incidence rate ratio",title = "Likes")


##negative binomial retweet model


nb_retweet_summary<- summary(nb_retweet)

nb_retweet_fixed_ef<- as.data.frame(nb_retweet_summary$coefficients$cond) %>% rownames_to_column(var = "predictors")

colnames(nb_retweet_fixed_ef)<- c("predictors","coefs","std_errors","z_value","significance")

nb_retweet_fixed_ef<- nb_retweet_fixed_ef %>% 
  mutate(oddr = round(exp(coefs)-1,2)) %>% 
  mutate(p_trad = ifelse(significance<=0.001,"***",
                         ifelse(significance>0.001& significance<=0.01,"**",
                                ifelse(significance>0.01&significance<=0.05,"*","")))) %>% 
  mutate(p_five_sig = ifelse(significance<=0.00001,"***",
                             ifelse(significance>0.00001&significance<=0.0001,"**",
                                    ifelse(significance>0.0001&significance<=0.0005,
                                           yes = "*",no = "")))) %>% 
  mutate(oddr_sg = paste0(oddr,p_five_sig)) %>% 
  mutate(level1_indicators = ifelse(predictors%in%level1Indicators,1,0)) %>% 
  mutate(predictors = recode(predictors, !!!indicator_labels))



nb_retweet_fixef_coef_graph<- nb_retweet_fixed_ef %>% 
  filter(predictors != "Intercept") %>% 
  ggplot(aes(x=reorder(predictors,level1_indicators),y = oddr,group = as.factor(level1_indicators)))+
  geom_point(aes(color = as.factor(level1_indicators)))+
  geom_text(aes(x = predictors, y = oddr,label = oddr_sg),nudge_x = .3)+
  geom_hline(yintercept = 0,color = "red")+
  guides(color = guide_legend(title = "Indicator levels"))+
  scale_color_discrete(labels = c("level-2","level-1"))+
  theme_bw()+
  labs(y = "Percentage change in incidence rate ratio", x = "Predictors",title = "Retweets",caption = "significance levels: ‘***’ 0.00001 ‘**’ 0.0001 ‘*’ 0.0005")+
  coord_flip()




nb_retweet_bn_ranef<-as.data.frame(ranef(nb_retweet)$cond$screen_namef) %>%
  rownames_to_column(var = "screen_name") %>% 
  pivot_longer(cols = `(Intercept)`:V301_05_Input_seekingyes,names_to = "predictors",values_to = "value") %>% 
  mutate(predictors = recode(predictors,!!!indicator_labels)) %>% 
  left_join(.,nb_retweet_fixed_ef,by = "predictors") %>%
  mutate(ran_slopes = coefs+value)%>% 
  mutate(ran_slopes = exp(ran_slopes)-1)


nb_retweet_random_effects_box_plot<- nb_retweet_bn_ranef %>% 
  filter(predictors != "Intercept") %>% 
  group_by(predictors) %>% 
  summarise(avg_effect = mean(ran_slopes),
            min_effect = min(ran_slopes),
            max_effect = max(ran_slopes)) %>% 
  ggplot(aes(x = predictors,y = avg_effect,color = predictors))+
  geom_point(aes(size =avg_effect),show.legend = F)+
  geom_errorbar(aes(x = predictors, ymin= min_effect, ymax = max_effect),width = .04,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  theme_bw()+
  coord_flip()+
  labs(x = "Predictors",y="Percentage change in incidence rate ratio",title = "Retweet")


##negative binomial quote model

nb_quote_summary<- summary(nb_quote)

nb_quote_fixed_ef<- as.data.frame(nb_quote_summary$coefficients$cond) %>% rownames_to_column(var = "predictors")

colnames(nb_quote_fixed_ef)<- c("predictors","coefs","std_errors","z_value","significance")

nb_quote_fixed_ef<- nb_quote_fixed_ef %>% 
  mutate(oddr = round((exp(coefs)-1),2)) %>% 
  mutate(p_trad = ifelse(significance<=0.001,"***",
                         ifelse(significance>0.001& significance<=0.01,"**",
                                ifelse(significance>0.01&significance<=0.05,"*","")))) %>% 
  mutate(p_five_sig = ifelse(significance<=0.00001,"***",
                             ifelse(significance>0.00001&significance<=0.0001,"**",
                                    ifelse(significance>0.0001&significance<=0.0005,
                                           yes = "*",no = "")))) %>% 
  mutate(oddr_sg = paste0(oddr,p_five_sig)) %>% 
  mutate(level1_indicators = ifelse(predictors%in%level1Indicators,1,0)) %>% 
  mutate(predictors = recode(predictors, !!!indicator_labels))



nb_quote_fixef_coef_graph<- nb_quote_fixed_ef %>% 
  filter(predictors != "Intercept") %>% 
  ggplot(aes(x=reorder(predictors,level1_indicators),y = oddr,group = as.factor(level1_indicators)))+
  geom_point(aes(color = as.factor(level1_indicators)))+
  geom_text(aes(x = predictors, y = oddr,label = oddr_sg),nudge_x = .3)+
  geom_hline(yintercept = 0,color = "red")+
  guides(color = guide_legend(title = "Indicator levels"))+
  scale_color_discrete(labels = c("level-2","level-1"))+
  theme_bw()+
  labs(y = "Percentage change in incidence rate ratio", x = "Predictors",title = "Quote",caption = "significance levels: ‘***’ 0.00001 ‘**’ 0.0001 ‘*’ 0.0005")+
  coord_flip()




nb_quote_bn_ranef<-as.data.frame(ranef(nb_quote)$cond$screen_namef) %>%
  rownames_to_column(var = "screen_name") %>% 
  pivot_longer(cols = `(Intercept)`:V301_05_Input_seekingyes,names_to = "predictors",values_to = "value") %>% 
  mutate(predictors = recode(predictors,!!!indicator_labels)) %>% 
  left_join(.,nb_quote_fixed_ef,by = "predictors") %>%
  mutate(ran_slopes = coefs+value) %>% 
  mutate(ran_slopes = exp(ran_slopes)-1)

nb_quote_random_effects_box_plot<- nb_quote_bn_ranef %>% 
  filter(predictors != "Intercept") %>% 
  group_by(predictors) %>% 
  summarise(avg_effect = mean(ran_slopes),
            min_effect = min(ran_slopes),
            max_effect = max(ran_slopes)) %>% 
  ggplot(aes(x = predictors,y = avg_effect,color = predictors))+
  geom_point(aes(size =avg_effect),show.legend = F)+
  geom_errorbar(aes(x = predictors, ymin= min_effect, ymax = max_effect),width = .04,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  theme_bw()+
  coord_flip()+
  labs(x = "Predictors",y="Percentage change in incidence rate ratio dispersion",title = "Quote")

##negative binomial reply model


nb_reply_summary<- summary(nb_reply)

nb_reply_fixed_ef<- as.data.frame(nb_reply_summary$coefficients$cond) %>% rownames_to_column(var = "predictors")

colnames(nb_reply_fixed_ef)<- c("predictors","coefs","std_errors","z_value","significance")

nb_reply_fixed_ef<- nb_reply_fixed_ef %>% 
  mutate(oddr = round((exp(coefs)-1),2)) %>% 
  mutate(p_trad = ifelse(significance<=0.001,"***",
                         ifelse(significance>0.001& significance<=0.01,"**",
                                ifelse(significance>0.01&significance<=0.05,"*","")))) %>% 
  mutate(p_five_sig = ifelse(significance<=0.00001,"***",
                             ifelse(significance>0.00001&significance<=0.0001,"**",
                                    ifelse(significance>0.0001&significance<=0.0005,
                                           yes = "*",no = "")))) %>% 
  mutate(oddr_sg = paste0(oddr,p_five_sig)) %>% 
  mutate(level1_indicators = ifelse(predictors%in%level1Indicators,1,0)) %>% 
  mutate(predictors = recode(predictors, !!!indicator_labels))



nb_reply_fixef_coef_graph<- nb_reply_fixed_ef %>% 
  filter(predictors != "Intercept") %>% 
  ggplot(aes(x=reorder(predictors,level1_indicators),y = oddr,group = as.factor(level1_indicators)))+
  geom_point(aes(color = as.factor(level1_indicators)))+
  geom_text(aes(x = predictors, y = oddr,label = oddr_sg),nudge_x = .3)+
  geom_hline(yintercept = 0,color = "red")+
  guides(color = guide_legend(title = "Indicator levels"))+
  scale_color_discrete(labels = c("level-2","level-1"))+
  theme_bw()+
  labs(y = "Percentage change in incidence rate ratio", x = "Predictors",title = "Reply",caption = "significance levels: ‘***’ 0.00001 ‘**’ 0.0001 ‘*’ 0.0005")+
  coord_flip()




nb_reply_bn_ranef<-as.data.frame(ranef(nb_reply)$cond$screen_namef) %>% rownames_to_column(var = "screen_name") %>% 
  pivot_longer(cols = `(Intercept)`:V301_05_Input_seekingyes,names_to = "predictors",values_to = "value") %>% 
  mutate(predictors = recode(predictors,!!!indicator_labels)) %>% 
  left_join(.,nb_reply_fixed_ef,by = "predictors") %>%
  mutate(ran_slopes = coefs+value) %>% 
  mutate(ran_slopes = exp(ran_slopes)-1)

nb_reply_random_effects_box_plot<- nb_reply_bn_ranef %>% 
  filter(predictors != "Intercept") %>% 
  group_by(predictors) %>% 
  summarise(avg_effect = mean(ran_slopes),
            min_effect = min(ran_slopes),
            max_effect = max(ran_slopes)) %>% 
  ggplot(aes(x = predictors,y = avg_effect,color = predictors))+
  geom_point(aes(size =avg_effect),show.legend = F)+
  geom_errorbar(aes(x = predictors, ymin= min_effect, ymax = max_effect),width = .04,show.legend = F)+
  geom_hline(yintercept = 0,color = "red")+
  theme_bw()+
  coord_flip()+
  labs(x = "Predictors",y="Percentage change in incidence rate ratio",title = "Reply")

#final plots

##Fixed effects
fixed_effects<- (nb_like_fixef_coef_graph+nb_retweet_fixef_coef_graph)/(nb_quote_fixef_coef_graph+nb_reply_fixef_coef_graph)

fixed_effects+ plot_annotation(
  title ="Negative binomial fixed effects coefficients at 5-sigma significance level"
)

##Random effects

random_effects<- (nb_likes_random_effects_box_plot+nb_retweet_random_effects_box_plot)/(nb_quote_random_effects_box_plot+nb_reply_random_effects_box_plot)

random_effects+plot_annotation(
  title ="Dispersion of random effects across the Executives' Twitter accounts"
)
