#script for the shadowing experiment 
#first, import all the libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(robustlmm)
library(emmeans)
library(ggpubr)
library(data.table)
library(xtable)

#import the data
dill_rep <- read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/dill_repeat/dill_repeat.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)
dill_imi <- read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/dill_shadow/dill_shadow.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)
till_rep <- read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/till_repeat/till_repeat.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)
till_imi <- read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/till_shadow/till_shadow.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)
deer_rep <- read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/deer_repeat/deer_repeat.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)
deer_imi <- read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/deer_shadow/deer_shadow.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)
tier_rep <- read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/data_tier_repeat/tier_repeat.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)
tier_imi <-  read.csv('/Users/lenahuttner/Desktop/Dissertation/Exp4_data/data_tier_shadow/tier_shadow.csv', sep=",", stringsAsFactors=TRUE, header = TRUE)

#step 1: clean the data... combine all the repsonse and stimulus columns into one column/variable, then subset the relevant columns before binding everything together
#add columns to each df, defining the overall group, the bias, the lexical item, the repition vs imitation condition
dill_rep <- dill_rep %>% mutate(gr = "dill_rep", 
                                bias = 'd',
                                mipair = 'tilldill',
                                cond = 'rep')
dill_imi <- dill_imi %>% mutate(gr = "dill_imi", 
                                bias = 'd',
                                mipair = 'tilldill',
                                cond = 'imi')
till_rep<- till_rep %>% mutate(gr = "till_rep", 
                                bias = 't',
                                mipair = 'tilldill',
                                cond = 'rep')
till_imi<- till_imi %>% mutate(gr = "till_imi", 
                               bias = 't',
                               mipair = 'tilldill',
                               cond = 'imi')
deer_rep <- deer_rep %>% mutate(gr = "deer_rep", 
                                bias = 'd',
                                mipair= 'tierdeer',
                                cond = 'rep')
deer_imi <- deer_imi %>% mutate(gr = "deer_imi", 
                                bias = 'd',
                                mipair = 'tierdeer',
                                cond = 'imi')
tier_rep <- tier_rep %>% mutate(gr = "tier_rep", 
                                bias = 't',
                                mipair = 'tierdeer',
                                cond = 'rep')
tier_imi <- tier_imi %>% mutate(gr = "tier_imi", 
                                bias = 't',
                                mipair = 'tierdeer',
                                cond = 'imi')
#make your life easier by combining a few columns into one move repsonses all to one column for every single df 

tier_rep <- tier_rep %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
tier_rep <- tier_rep %>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7, sep = "", remove = TRUE, na.rm = TRUE)
tier_rep <- tier_rep %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22,cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

tier_imi <- tier_imi %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
tier_imi <- tier_imi %>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7, sep = "", remove = TRUE, na.rm = TRUE)
tier_imi <- tier_imi %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22,cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

deer_rep <- deer_rep %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
deer_rep <- deer_rep %>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7, sep = "", remove = TRUE, na.rm = TRUE)
deer_rep <- deer_rep %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22,cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

deer_imi <- deer_imi %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
deer_imi <- deer_imi %>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7, sep = "", remove = TRUE, na.rm = TRUE)
deer_imi <- deer_imi %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22, cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

dill_rep <- dill_rep %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
dill_rep <- dill_rep%>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7, sep = "", remove = TRUE, na.rm = TRUE)
dill_rep <- dill_rep %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22, cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

dill_imi <- dill_imi %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
dill_imi <- dill_imi%>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7,  sep = "", remove = TRUE, na.rm = TRUE)
dill_imi <- dill_imi %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22,cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

till_rep <- till_rep %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
till_rep <- till_rep%>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7, sep = "", remove = TRUE, na.rm = TRUE)
till_rep <- till_rep %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22,cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

till_imi <- till_imi %>% unite("response", perc_pre,resp_training, sep = "", remove = TRUE, na.rm = TRUE)
till_imi <- till_imi%>% unite("minimal_pair", copy_of_minimalpair_df8f,copy_of_copy_of_minimalpair_df8f_88f7, sep = "", remove = TRUE, na.rm = TRUE)
till_imi <- till_imi %>% unite("Vot_level", copy_of_Vot_level_10a8,copy_of_copy_of_Vot_level_10a8_bd22,cftg1_, sep = "", remove = TRUE, na.rm = TRUE)

#now select only the columns that are needed for analysis. the dfs don't bind with all the columns, so we are going to use only the ones of interest
tier_rep_bind <-  tier_rep %>% select(Task_Name,  #name of phase in experiment
                                       Rec_Session_Id, #subject identifier (random number)
                                       response, # perception response
                                       gr, #group
                                       bias, #t or d
                                       mipair, #which word during training
                                       cond, #condition
                                       VOT, #voice onset time 
                                       shadow_stim,#stimulus during shadowing
                                       Prod_Post, #stimulus in the post task
                                       Condition_Id, #indicating the stimulus numerically, may come in handy later
                                       minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                       Vot_level,#Vot_level technically also cftg1_? 
                                      Condition_Id, 
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot
              

tier_imi_bind <-  tier_imi %>% select(Task_Name,  #name of phase in experiment
                                       Rec_Session_Id, #subject identifier (random number)
                                       response, # perception response
                                       gr, #group
                                       bias, #t or d
                                       mipair, #which word during training
                                       cond, #condition
                                       VOT, #voice onset time 
                                       shadow_stim,#stimulus during shadowing
                                       Prod_Post, #stimulus in the post task
                                       Condition_Id, #indicating the stimulus numerically, may come in handy later
                                       minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                       Vot_level,
                                      Condition_Id, 
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot) #Vot_level technically also cftg1_? 
deer_rep_bind <-  deer_rep %>% select(Task_Name,  #name of phase in experiment
                                      Rec_Session_Id, #subject identifier (random number)
                                      response, # perception response
                                      gr, #group
                                      bias, #t or d
                                      mipair, #which word during training
                                      cond, #condition
                                      VOT, #voice onset time 
                                      shadow_stim,#stimulus during shadowing
                                      Prod_Post, #stimulus in the post task
                                      Condition_Id, #indicating the stimulus numerically, may come in handy later
                                      minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                      Vot_level,
                                      Condition_Id, 
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot) #Vot_level technically also cftg1_? 

deer_imi_bind <-  deer_imi %>% select(Task_Name,  #name of phase in experiment
                                      Rec_Session_Id, #subject identifier (random number)
                                      response, # perception response
                                      gr, #group
                                      bias, #t or d
                                      mipair, #which word during training
                                      cond, #condition
                                      VOT, #voice onset time 
                                      shadow_stim,#stimulus during shadowing
                                      Prod_Post, #stimulus in the post task
                                      Condition_Id, #indicating the stimulus numerically, may come in handy later
                                      minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                      Vot_level, 
                                      Condition_Id, 
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot) #Vot_level technically also cftg1_? 
till_imi_bind <-  till_imi %>% select(Task_Name,  #name of phase in experiment
                                      Rec_Session_Id, #subject identifier (random number)
                                      response, # perception response
                                      gr, #group
                                      bias, #t or d
                                      mipair, #which word during training
                                      cond, #condition
                                      VOT, #voice onset time 
                                      shadow_stim,#stimulus during shadowing
                                      Prod_Post, #stimulus in the post task
                                      Condition_Id, #indicating the stimulus numerically, may come in handy later
                                      minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                      Vot_level, 
                                      Condition_Id, 
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot) #Vot_level technically also cftg1_? 
till_rep_bind <-  till_rep %>% select(Task_Name,  #name of phase in experiment
                                      Rec_Session_Id, #subject identifier (random number)
                                      response, # perception response
                                      gr, #group
                                      bias, #t or d
                                      mipair, #which word during training
                                      cond, #condition
                                      VOT, #voice onset time 
                                      shadow_stim,#stimulus during shadowing
                                      Prod_Post, #stimulus in the post task
                                      Condition_Id, #indicating the stimulus numerically, may come in handy later
                                      minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                      Vot_level, 
                                      Condition_Id, 
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot ) #Vot_level technically also cftg1_? 
dill_rep_bind <-  dill_rep %>% select(Task_Name,  #name of phase in experiment
                                      Rec_Session_Id, #subject identifier (random number)
                                      response, # perception response
                                      gr, #group
                                      bias, #t or d
                                      mipair, #which word during training
                                      cond, #condition
                                      VOT, #voice onset time 
                                      shadow_stim,#stimulus during shadowing
                                      Prod_Post, #stimulus in the post task
                                      Condition_Id, #indicating the stimulus numerically, may come in handy later
                                      minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                      Vot_level, 
                                      Condition_Id,
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot) #Vot_level technically also cftg1_? 
dill_imi_bind <-  dill_imi %>% select(Task_Name,  #name of phase in experiment
                                      Rec_Session_Id, #subject identifier (random number)
                                      response, # perception response
                                      gr, #group
                                      bias, #t or d
                                      mipair, #which word during training
                                      cond, #condition
                                      VOT, #voice onset time 
                                      shadow_stim,#stimulus during shadowing
                                      Prod_Post, #stimulus in the post task
                                      Condition_Id, #indicating the stimulus numerically, may come in handy later
                                      minimal_pair,#minimal pair presented as stimulus in the pre-task... technically also encoded in Condition_Id
                                      Vot_level, 
                                      Condition_Id, 
                                      intervall) #what we are going to use to see which word participants were shown on screen because I am an idiot) #Vot_level technically also cftg1_? 

#And bind everything together in one 
df_all <- rbind(tier_imi_bind, tier_rep_bind, till_imi_bind, till_rep_bind, deer_imi_bind, deer_rep_bind, dill_rep_bind, dill_imi_bind)
to_filter <- c(879998, 866480, 869484, 846762,866420, 824488, 875027, 867239 ) #exclude particuipants that had no recording data, but still showed up in th final df, because the script created a production.df file
df_all <- subset(df_all, !(Rec_Session_Id %in% to_filter))


#create new colum transforming the response data into 1s and 0s 
df_all$gr <- as.factor(df_all$gr)
df_all <- df_all %>% mutate(response_num  = case_when(response == "d"~ 1, 
                                                      response == "t" ~ 0))
#Count how many participants are in the df_all after exclusion. I will remove outliers from the production portion later and need to make sure how many participants are in each set. 
group_counts <- df_all %>%
  group_by(gr) %>%
  summarize(Participants = n_distinct(Rec_Session_Id)) 

# Print the result
print(group_counts)

#subset only the response data from the dataset 
df_all_response <-  subset(df_all, Task_Name == "Perception_Pre_tier" | Task_Name == 'Perception_Post_tier' | Task_Name == 'Perception_training')
to_filter <- c(870920, 830608, 907254 ) # Who are these people? They are the ones that are removed from the production data once the outliers have been removed. All of their recordings are faulty, swoing only 3ms annotations.
df_all_response<- subset(df_all_response, !(Rec_Session_Id %in% to_filter)) #filter them out 


#susbet only the pre and post test for intial plot 
df_all_resp_pre_post <- subset(df_all, Task_Name == "Perception_Pre_tier" | Task_Name == 'Perception_Post_tier') 

#count responses per participant
count_resp_pre <- df_all_response %>% group_by(Rec_Session_Id,Task_Name, cond, gr, bias, mipair, minimal_pair, Vot_level, response, .drop = TRUE) %>% tally()
#filter out only t responses
count_resp_pre<- subset(count_resp_pre, response == 't')
count_resp_pre<- count_resp_pre%>% mutate(prop = n * 0.1) #calculate proportion



#clean response and make all stimuli variables correspond to the right stimulus levels. 
df_all_response$response <- as.factor(df_all_response$response)
df_all_response <- df_all_response %>% mutate(stim_level = Condition_Id) #add another column for stimulus, to make sure everything works

# Update stim_level based on conditions
df_all_response <- df_all_response %>%
  mutate(
    stim_level = case_when(
      is.na(minimal_pair) & mipair == "tilldill" & stim_level == 1 ~ 6,
      is.na(minimal_pair) & mipair == "tilldill" & stim_level == 2 ~ 8,
      is.na(minimal_pair) & mipair == "tilldill" & stim_level == 5 ~ 10,
      TRUE ~ stim_level
    )
  )

# Print the updated data frame
print(df_all_response)


# Update stim_level based on conditions
library(dplyr)
df_all_response <- df_all_response %>%
  mutate(
    stim_level = if_else(
      Task_Name == "Perception_training" & mipair == "tilldill" & stim_level == 1, 6,
      if_else(Task_Name == "Perception_training"  & mipair == "tilldill" & stim_level == 2, 8,
              if_else(Task_Name == "Perception_training"  & mipair == "tilldill" & stim_level == 3, 10, stim_level))
    )
  )

df_all_response <- df_all_response %>%
  mutate(
    stim_level = if_else(
      Task_Name == "Perception_training" & mipair == "tierdeer" & stim_level == 1, 5,
      if_else(Task_Name == "Perception_training"  & mipair == "tierdeer" & stim_level == 2, 7,
              if_else(Task_Name == "Perception_training"  & mipair == "tierdeer" & stim_level == 3, 9, stim_level))
    )
  )



df_all_response <- df_all_response %>%
  mutate(
    minimal_pair = if_else(
      Task_Name == "Perception_training" & mipair == "tilldill", "dilltill", 
      if_else(Task_Name == "Perception_training"  & mipair == "tierdeer", "deertier",minimal_pair))
    )
  
#turn everything into factors
df_all_response$stim_level <- as.factor(df_all_response$stim_level)
df_all_response$response <- as.factor(df_all_response$response)
# Print the updated data frame
print(df_all_response)

#add columns with more plotable values so I don't have to fiddle around with remainuing things in ggplot
df_all_response <- df_all_response %>% mutate(condition = case_when(cond == "imi" ~ "imitation", cond == "rep" ~"repetition"))
df_all_response <- df_all_response %>% mutate(lex = case_when(mipair== "tierdeer" ~ "deer/tier", mipair == "tilldill" ~ "dill/till"))

count_resp_pre <- count_resp_pre %>% mutate(condition = case_when(cond == "imi" ~ "imitation", cond == "rep" ~"repetition"))
count_resp_pre<- count_resp_pre %>% mutate(lex = case_when(mipair== "tierdeer" ~ "deer/tier", mipair == "tilldill" ~ "dill/till"))

#plot
ggplot(data = count_resp_pre, aes(x = Vot_level, y = prop, color = interaction(Task_Name, minimal_pair)))+
  geom_smooth(aes(group = interaction (Task_Name, minimal_pair)), se = FALSE) +
  scale_color_discrete(name = "Task and stimulus",breaks = c("Perception_training.", "Perception_Post_tier.deertier", "Perception_Pre_tier.deertier", "Perception_Post_tier.dilltill",  "Perception_Pre_tier.dilltill"), labels=c("exposure","post deer/tier","pre deer/tier","post dill/till","pre dill/till"))+
  xlab(label  = "Stimulus level from /d/ to /t/")+
  ylab(label = "proportion of identification as /t/")+
  scale_x_discrete(labels = c('1','2','3', '4', '5','6','7'))+
  facet_grid(lex ~ bias ~ condition)

#let's start testing - this is just an initial one to get an idea whether a model would even converge. 
m1 <-  glmer(response ~ bias* mipair* gr* cond* Task_Name + (1|Rec_Session_Id), family = binomial, data = df_all_response) #very overspecified fixed effects, underspecified random effect
summary(m1)


#we now need to split off the 3 midpoint stimuli 
df_resp_mid3 <- subset(df_all_response, Vot_level == "VOT_3"|Vot_level == "VOT_4"|Vot_level == "VOT_5")
#turn things tha need to be factors into factors
df_resp_mid3$Condition_Id <- as.factor(df_resp_mid3$Condition_Id)
df_resp_mid3$response<- as.factor(df_resp_mid3$response)
df_resp_mid3$stim_level<- as.factor(df_resp_mid3$stim_level)
df_resp_mid3$Task_Name<- as.factor(df_resp_mid3$Task_Name)
df_resp_mid3$Rec_Session_Id <- as.factor(df_resp_mid3$Rec_Session_Id)
#perception, mid 3 sitmuli

#minimal model 
m_min <- glm(response ~ 1, data = df_resp_mid3) 

#add random effect 
m123 <- glmer(response ~ 1 + (1|Rec_Session_Id),family = binomial, data = df_resp_mid3)

#compare 
anova(m_min,m123) 

#keep going
m234 <-  glmer(response ~ Task_Name + (1|Rec_Session_Id), family = binomial, data = df_resp_mid3)
summary(m234)
anova(m123,m234)
m345 <-  glmer(response ~ bias * Task_Name + (1|Rec_Session_Id), family = binomial, data = df_resp_mid3)
summary(m345)
anova(m234, m345)
m86 <-  glmer(response ~ bias * Task_Name * cond+ (1|Rec_Session_Id), family = binomial, data = df_resp_mid3)
summary(m86)
anova(m345, m86)

#the last one appears to be the best one, in terms of fixed effects... let's do a quick comparison before fiddling around with a random slope
emm2 <- emmeans(m86,  ~ bias * Task_Name * cond )
em2pairs<- pairs(emm2, adjust = "fdr")
print(em2pairs)
write.csv(em2pairs, file = '/Users/lenahuttner/Desktop/Dissertation/Exp4_data/perc_pairs_noslope')

#one issue though, bias and cond are nested in gr... splitting it up creates a lot of NAs in the emmeans dataframe... let's combine the two 
#why no random slope? because there are only 3 stimuli, and the VOT itself isn't a fixed effect
m2 <-  glmer(response ~ gr* Task_Name + (1|Rec_Session_Id), family = binomial, data = df_resp_mid3)
summary(m2)
anova(m86,m2)
#quick check... better, especially if we now add a slope
emm2 <- emmeans(m2,  ~ gr* Task_Name )
em2pairs<- pairs(emm2, adjust = "fdr")
print(em2pairs)



#now adding in a random slope
#be prepared foir this thing to run for ages and ages and ages 


#adding in a random slope for stimulus and also adding stimulus as a fixex effect, because techincally that's be the ideal way to do this
#this one does not converge whatsoever 
m3<-  glmer(response ~ gr* Task_Name * stim_level+ (0+stim_level|Rec_Session_Id), family = binomial, data = df_resp_mid3, control=glmerControl(optimizer="bobyqa"))    #,optCtrl=list(maxfun=1e5)
summary(m3)


#adding minimal pair as a random efect.. only has two levels though, so it's really not informative as a radom effect. 
m71<-  glmer(response ~ bias * Task_Name * cond  + (1+minimal_pair) + (1|Rec_Session_Id), family = binomial, data = df_resp_mid3, control=glmerControl(optimizer="bobyqa"))    #,optCtrl=list(maxfun=1e5)
summary(m71)

#minimal pair as fixed, only slope, no intercept. having a random slope without stim_level in fixed is not ideal, but also not wrong. With 3 levels it also does not matter that much. 
m989<-  glmer(response ~ gr* Task_Name *minimal_pair + (0+stim_level|Rec_Session_Id), family = binomial, data = df_resp_mid3, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))    #,optCtrl=list(maxfun=1e5)
summary(m989)

#slope and intercept.. doesn't converge
m1000<-  glmer(response ~ gr* Task_Name *minimal_pair + (1+stim_level|Rec_Session_Id), family = binomial, data = df_resp_mid3, control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=1e5)))    #,optCtrl=list(maxfun=1e5)
summary(m1000)

#let us go with m989... it converges and has all the things we want. 
#compare levels
emm <- emmeans(m989,  ~ gr* Task_Name *minimal_pair )
em3pairs<- pairs(emm, adjust = "fdr")
print(em3pairs)
as.data.frame(em3pairs)
#write the final file to csv and scroll through it in excel. 
write.csv(em3pairs, file = '/Users/lenahuttner/Desktop/Dissertation/Exp4_data/perception_pairs_m3')

#filter out the contrasts that are actually of interest and convert them to a latex table
target_strings_perc <- c(
  "deer_imi Perception_Post_tier deertier - deer_imi Perception_Pre_tier deertier",
"deer_imi Perception_Post_tier deertier - deer_imi Perception_training deertier",
"deer_imi Perception_Post_tier deertier - deer_imi Perception_Post_tier dilltill", 
"deer_imi Perception_Post_tier deertier - deer_imi Perception_Pre_tier dilltill",
"deer_imi Perception_Pre_tier deertier - deer_imi Perception_training deertier",
"deer_imi Perception_Pre_tier deertier - deer_imi Perception_Pre_tier dilltill",
"deer_imi Perception_training deertier - deer_imi Perception_Post_tier dilltill",
"deer_imi Perception_training deertier - deer_imi Perception_Pre_tier dilltill",
"deer_imi Perception_Post_tier dilltill - deer_imi Perception_Pre_tier dilltill",

"deer_rep Perception_Post_tier deertier - deer_rep Perception_Pre_tier deertier",
"deer_rep Perception_Post_tier deertier - deer_rep Perception_training deertier",
"deer_rep Perception_Post_tier deertier - deer_rep Perception_Post_tier dilltill",
"deer_rep Perception_Post_tier deertier - deer_rep Perception_Pre_tier dilltill",
"deer_rep Perception_Pre_tier deertier - deer_rep Perception_training deertier",
"deer_rep Perception_Pre_tier deertier - deer_rep Perception_Post_tier dilltill",
"deer_rep Perception_Pre_tier deertier - deer_rep Perception_Pre_tier dilltill",
"deer_rep Perception_training deertier - deer_rep Perception_Post_tier dilltill",
"deer_rep Perception_training deertier - deer_rep Perception_Pre_tier dilltill",
"deer_rep Perception_Post_tier dilltill - deer_rep Perception_Pre_tier dilltill",

"dill_imi Perception_Post_tier deertier - dill_imi Perception_Pre_tier deertier",
"dill_imi Perception_Post_tier deertier - dill_imi Perception_Post_tier dilltill",
"dill_imi Perception_Post_tier deertier - dill_imi Perception_Pre_tier dilltill",
"dill_imi Perception_Post_tier deertier - dill_imi Perception_training dilltill",
"dill_imi Perception_Pre_tier deertier - dill_imi Perception_Post_tier dilltill",
"dill_imi Perception_Pre_tier deertier - dill_imi Perception_Pre_tier dilltill",
"dill_imi Perception_Post_tier dilltill - dill_imi Perception_Pre_tier dilltill",
"dill_imi Perception_Post_tier dilltill - dill_imi Perception_training dilltill",
"dill_imi Perception_Pre_tier dilltill - dill_imi Perception_training dilltill",

"dill_rep Perception_Post_tier deertier - dill_rep Perception_Pre_tier deertier",
"dill_rep Perception_Post_tier deertier - dill_rep Perception_Post_tier dilltill",
"dill_rep Perception_Post_tier deertier - dill_rep Perception_Pre_tier dilltill",
"dill_rep Perception_Post_tier deertier - dill_rep Perception_training dilltill",
"dill_rep Perception_Pre_tier deertier - dill_rep Perception_Post_tier dilltill",
"dill_rep Perception_Pre_tier deertier - dill_rep Perception_Pre_tier dilltill",
"dill_rep Perception_Pre_tier deertier - dill_rep Perception_training dilltill",
"dill_rep Perception_Post_tier dilltill - dill_rep Perception_Pre_tier dilltill",
"dill_rep Perception_Post_tier dilltill - dill_rep Perception_training dilltill",
"dill_rep Perception_Pre_tier dilltill - dill_rep Perception_training dilltill",

"tier_imi Perception_Post_tier deertier - tier_imi Perception_Pre_tier deertier",
"tier_imi Perception_Post_tier deertier - tier_imi Perception_training deertier",
"tier_imi Perception_Post_tier deertier - tier_imi Perception_Post_tier dilltill",
"tier_imi Perception_Post_tier deertier - tier_imi Perception_Pre_tier dilltill",
"tier_imi Perception_Pre_tier deertier - tier_imi Perception_training deertier",
"tier_imi Perception_Pre_tier deertier - tier_imi Perception_Post_tier dilltill",
"tier_imi Perception_training deertier - tier_imi Perception_Post_tier dilltill",
"tier_imi Perception_training deertier - tier_imi Perception_Pre_tier dilltill",
"tier_imi Perception_Post_tier dilltill - tier_imi Perception_Pre_tier dilltill",

"tier_rep Perception_Post_tier deertier - tier_rep Perception_Pre_tier deertier",
"tier_rep Perception_Post_tier deertier - tier_rep Perception_training deertier",
"tier_rep Perception_Post_tier deertier - tier_rep Perception_Post_tier dilltill",
"tier_rep Perception_Post_tier deertier - tier_rep Perception_Pre_tier dilltill",
"tier_rep Perception_Pre_tier deertier - tier_rep Perception_training deertier",
"tier_rep Perception_Pre_tier deertier - tier_rep Perception_Post_tier dilltill",
"tier_rep Perception_Pre_tier deertier - tier_rep Perception_Pre_tier dilltill",
"tier_rep Perception_Post_tier dilltill - tier_rep Perception_Pre_tier dilltill",

"till_imi Perception_Post_tier deertier - till_imi Perception_Pre_tier deertier",
"till_imi Perception_Post_tier deertier - till_imi Perception_Post_tier dilltill",
"till_imi Perception_Post_tier deertier - till_imi Perception_Pre_tier dilltill",
"till_imi Perception_Post_tier deertier - till_imi Perception_training dilltill",
"till_imi Perception_Pre_tier deertier - till_imi Perception_Post_tier dilltill",
"till_imi Perception_Pre_tier deertier - till_imi Perception_Pre_tier dilltill",
"till_imi Perception_Pre_tier deertier - till_imi Perception_training dilltill",
"till_imi Perception_Post_tier dilltill - till_imi Perception_Pre_tier dilltill",
"till_imi Perception_Post_tier dilltill - till_imi Perception_training dilltill",
"till_imi Perception_Pre_tier dilltill - till_imi Perception_training dilltill",

"till_rep Perception_Post_tier deertier - till_rep Perception_Pre_tier deertier",
"till_rep Perception_Post_tier deertier - till_rep Perception_Post_tier dilltill",
"till_rep Perception_Post_tier deertier - till_rep Perception_Pre_tier dilltill",
"till_rep Perception_Post_tier deertier - till_rep Perception_training dilltill",
"till_rep Perception_Pre_tier deertier - till_rep Perception_Post_tier dilltill",
"till_rep Perception_Pre_tier deertier - till_rep Perception_Pre_tier dilltill",
"till_rep Perception_Pre_tier deertier - till_rep Perception_training dilltill",
"till_rep Perception_Post_tier dilltill - till_rep Perception_Pre_tier dilltill",
"till_rep Perception_Post_tier dilltill - till_rep Perception_training dilltill",
"till_rep Perception_Pre_tier dilltill - till_rep Perception_training dilltill"
)

em3pairs <- as.data.table(em3pairs)
# Subset the rows based on the specified strings
#pairs_filt <- em87pairs_filt[grepl(paste(target_strings, collapse = "|"), em87pairs_filt$contrast), ]
pairs_filt_perc <- em3pairs[grepl(paste(target_strings_perc, collapse = "|"), contrast)]

# Print the filtered data frame
print(pairs_filt_perc )
print(xtable(pairs_filt_perc , type = "latex"))#, file = "prod_filt.tex")

#let's table that for now and start on production

#first create a df with the tasks of interest
df_prod_prepo <-  df_all%>% subset(Task_Name =='Production_Pre'| Task_Name == 'Production_Post')
df_prod_prepo <-  df_prod_prepo %>% subset(intervall == 'pos') #make sure to exclude any epty files

#now upon initial inspection, exclude any files with extreme extreme outliers
#too short – script didn't work properly – upon manual inspection: there is an issue in the recording
df_prod_prepo <- df_prod_prepo %>% subset(VOT > 0.006)
#and the three files above 1... beacuse that's also unreasonable and and error
df_prod_prepo <- df_prod_prepo %>% subset(VOT < 1)


#count how many participants you have left
group_counts <- df_prod_prepo %>%
  group_by(gr) %>%
  summarize(Participants = n_distinct(Rec_Session_Id)) 
print(group_counts)


# Function to remove any other outliers based on the IQR method
remove_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  return(between(x, lower_bound, upper_bound))
}

# Apply the function by stimulus level (/d/ vs /t/)
filtered_data <- df_prod_prepo  %>%
  group_by(Condition_Id) %>%
  filter(remove_outliers(VOT))

filtered_data <- ungroup(filtered_data)

# Print the resulting data
print(filtered_data)

count
group_counts <- filtered_data %>%
  group_by(gr) %>%
  summarize(Participants = n_distinct(Rec_Session_Id)) 
print(group_counts)

#filtering removes 2 participants from dill_imi
#We end up with 15 participants per condition

#now clean the data up a bit
#first things first, factors
filtered_data$Condition_Id <- as.factor(filtered_data$Condition_Id)

#again, add columns with presentable, plotable names
filtered_data <- filtered_data %>% mutate(task = case_when(Task_Name == "Production_Post" ~ "Post Test",Task_Name == "Production_Pre" ~ "Pre Test" ))
filtered_data <- filtered_data %>% mutate(grp = case_when(gr == "deer_imi" ~ "imitation deer/tier",
                                                           gr == "deer_rep" ~ "repetition deer/tier", 
                                                           gr == "dill_imi" ~ "imitation dill/till",
                                                           gr == "dill_rep" ~ "repetition dill/till",
                                                           gr == "tier_imi" ~ "imitation deer/tier",
                                                           gr == "tier_rep" ~ "repetition deer/tier", 
                                                           gr == "till_imi" ~ "imitation dill/till",
                                                           gr == "till_rep" ~ "repetition dill/till"))
filtered_data  <- filtered_data  %>% mutate(condition = case_when(cond == "imi" ~ "imitation", cond == "rep" ~"repetition"))
filtered_data <- filtered_data  %>% mutate(lex = case_when(mipair== "tierdeer" ~ "deer/tier", mipair == "tilldill" ~ "dill/till"))
filtered_data <- filtered_data  %>% mutate(stimulus = case_when(Condition_Id== 1 ~ "deer",
                                                           Condition_Id== 2 ~ "tier", 
                                                           Condition_Id== 3 ~ "till",
                                                           Condition_Id== 4 ~ "dill"))
filtered_data$task = factor(filtered_data$task, levels=c("Pre Test", "Post Test"))

#plotting time
ggplot(data = filtered_data , aes (x=stimulus, y=VOT, color = stimulus))+
  geom_boxplot(width = 0.2, aes(group = stimulus))+
  facet_grid(condition ~ lex ~ bias ~ task)+
  theme_bw()+
  labs(color ="produced stimulus" )+
  xlab(label = "Stimulus")+
  ylab(label = "VOT in seconds")

#make sure everything that needs to be a factor actually is a factor
df_prod_prepo$Condition_Id <-  as.factor(df_prod_prepo$Condition_Id)
df_prod_prepo$Task_Name <-  as.factor(df_prod_prepo$Task_Name)
df_prod_prepo$gr <-  as.factor(df_prod_prepo$gr)
df_prod_prepo$minimal_pair <- as.factor(df_prod_prepo$minimal_pair)
df_prod_prepo$Rec_Session_Id <- as.factor(df_prod_prepo$Rec_Session_Id)

#analysis time. 

#first try lmer, plot residuals.

#something like this
m12 <- lmer(VOT ~ Condition_Id * Task_Name * gr + (1+Condition_Id|Rec_Session_Id) , data = filtered_data)
summary(m12)

res = summary(m12)$resid
ggqqplot(res)
#not normal,try with log transform

m87_filt <- lmer(log(VOT) ~ Condition_Id * gr * Task_Name + (1|Rec_Session_Id), data = filtered_data)
summary(m87_filt)

res = summary(m87_filt)$resid
ggqqplot(res)
#...nope 

#before trying robust lme again (after all, we went through all the hassle to remove outliers) let's try alternative distributions 

#this ththe model I think fits best in terms of effects structure...  again, mipair kind of a redundant random effect... 
m_9 <- glmer(VOT ~ Condition_Id * Task_Name * gr + (1+Condition_Id|Rec_Session_Id) + (1|mipair) ,  family = Gamma(), data = filtered_data, )
summary(m_9)
res = summary(m_9)$resid
ggqqplot(res)
#it works!!! 
#let's run with this one then. 

#I suck at finding a coherent naming system. Let's compare... 
emm56 <- emmeans(m_9,  ~ Condition_Id * Task_Name * gr ) 
em56pairs <- pairs(emm56, adjust = "fdr")
as.data.frame(em56pairs)

#this creates a data frame that is simply not legible in R. save, inspect in excel. 
write.csv(em56pairs, file = '/Users/lenahuttner/Desktop/Dissertation/Exp4_data/vot_em_pairs')

#ok, so we are only interested in a subportion of the results table... 


#specify the strings to subset. 
target_strings <- c(
  "1 Production_Post deer_imi - 1 Production_Pre deer_imi",
  "2 Production_Post deer_imi - 2 Production_Pre deer_imi",
  "3 Production_Post deer_imi - 3 Production_Pre deer_imi",
  "4 Production_Post deer_imi - 4 Production_Pre deer_imi",
  "1 Production_Post deer_rep - 1 Production_Pre deer_rep",
  "2 Production_Post deer_rep - 2 Production_Pre deer_rep",
  "3 Production_Post deer_rep - 3 Production_Pre deer_rep",
  "4 Production_Post deer_rep - 4 Production_Pre deer_rep",
  "1 Production_Post tier_rep - 1 Production_Pre tier_rep",
  "2 Production_Post tier_rep - 2 Production_Pre tier_rep",
  "3 Production_Post tier_rep - 3 Production_Pre tier_rep",
  "4 Production_Post tier_rep - 4 Production_Pre tier_rep",
  "1 Production_Post tier_imi - 1 Production_Pre tier_imi",
  "2 Production_Post tier_imi - 2 Production_Pre tier_imi",
  "3 Production_Post tier_imi - 3 Production_Pre tier_imi",
  "4 Production_Post tier_imi - 4 Production_Pre tier_imi",
  "1 Production_Post till_imi - 1 Production_Pre till_imi",
  "2 Production_Post till_imi - 2 Production_Pre till_imi",
  "3 Production_Post till_imi - 3 Production_Pre till_imi",
  "4 Production_Post till_imi - 4 Production_Pre till_imi",
  "1 Production_Post till_rep - 1 Production_Pre till_rep",
  "2 Production_Post till_rep - 2 Production_Pre till_rep",
  "3 Production_Post till_rep - 3 Production_Pre till_rep",
  "4 Production_Post till_rep - 4 Production_Pre till_rep",
  "1 Production_Post dill_rep - 1 Production_Pre dill_rep",
  "2 Production_Post dill_rep - 2 Production_Pre dill_rep",
  "3 Production_Post dill_rep - 3 Production_Pre dill_rep",
  "4 Production_Post dill_rep - 4 Production_Pre dill_rep",
  "1 Production_Post dill_imi - 1 Production_Pre dill_imi",
  "2 Production_Post dill_imi - 2 Production_Pre dill_imi",
  "3 Production_Post dill_imi - 3 Production_Pre dill_imi",
  "4 Production_Post dill_imi - 4 Production_Pre dill_imi"
)


em56pairs_filt <- as.data.table(em56pairs)
pairs56_filt <- em56pairs_filt[grepl(paste(target_strings, collapse = "|"), contrast)]

# Print the filtered data frame 
print(pairs56_filt)
#print as latex table
print(xtable(pairs56_filt, type = "latex"))#, file = "prod_filt.tex")



# Try to do the same for between stimuli.... and fail
target_strings_btwnstim<- c(
  "2 Production_Post deer_imi - 3 Production_Post deer_imi",
  "1 Production_Post deer_imi - 4 Production_Post deer_imi",
  "2 Production_Pre deer_imi - 3 Production_Pre deer_imi",
  "1 Production_Pre deer_imi - 4 Production_Pre deer_imi",
  "2 Production_Post deer_rep- 3 Production_Post deer_rep",
  "1 Production_Post deer_rep - 4 Production_Post deer_rep",
  "3 Production_Pre deer_rep - 2 Production_Pre deer_rep",
  "1 Production_Pre deer_rep - 4 Production_Pre deer_rep",
  "2 Production_Post tier_rep- 3 Production_Post tier_rep",
  "1 Production_Post tier_rep - 4 Production_Post tier_rep",
  "2 Production_Pre tier_rep- 3 Production_Pre tier_rep",
  "1 Production_Pre tier_rep - 4 Production_Pre tier_rep",
  "2 Production_Post tier_imi- 3 Production_Post tier_imi",
  "1 Production_Post tier_imi - 4 Production_Post tier_imi",
  "2 Production_Pre tier_imi- 3 Production_Pre tier_imi",
  "1 Production_Pre tier_imi - 4 Production_Pre tier_imi",
  "2 Production_Post till_imi- 3 Production_Post till_imi",
  "1 Production_Post till_imi - 4 Production_Post till_imi",
  "2 Production_Pre till_imi- 3 Production_Pre till_imi",
  "1 Production_Pre till_imi - 4 Production_Pre till_imi",
  "2 Production_Post till_rep- 3 Production_Post till_rep",
  "1 Production_Post till_rep- 4 Production_Post till_rep",
  "2 Production_Pre till_rep- 3 Production_Pre till_rep",
  "1 Production_Pre till_rep - 4 Production_Pre till_rep",
  "2 Production_Post dill_rep- 3 Production_Post dill_rep",
  "1 Production_Post dill_rep- 4 Production_Post dill_rep",
  "2 Production_Pre dill_rep- 3 Production_Pre dill_rep",
  "1 Production_Pre dill_rep - 4 Production_Pre dill_rep",
  "2 Production_Post dill_imi- 3 Production_Post dill_imi",
  "1 Production_Post dill_imi- 4 Production_Post dill_imi",
  "2 Production_Pre dill_imi- 3 Production_Pre dill_imi",
  "1 Production_Pre dill_imi - 4 Production_Pre dill_imi")

em56pairs <- as.data.table(em56pairs)
pairs_filt_btwnstim <- em56pairs[grepl(paste(target_strings_btwnstim, collapse = "|"), contrast)]

# Print the filtered data frame
print(pairs_filt_btwnstim)
print(xtable(pairs_filt_btwnstim, type = "latex"))#, file = "prod_filt.tex")






#give me the files for the production imitation vs shadowing task 

df_manip <- df_all %>% subset(Task_Name == "Production_shadowing")
df_manip$Condition_Id <- as.factor(df_manip$Condition_Id)


#remove the outliers 

#first filter out everything that is too low and therefore most likely an error in annotation
df_manip <- df_manip %>% subset(VOT > 0.005)
#and the ones above 1... beacuse that's also unreasonable 
df_manip <- df_manip %>% subset(VOT < 1)

#we have filtered out 40 rows

remove_outliers <- function(x) {
  q <- quantile(x, c(0.25, 0.75))
  iqr <- q[2] - q[1]
  lower_bound <- q[1] - 1.5 * iqr
  upper_bound <- q[2] + 1.5 * iqr
  return(between(x, lower_bound, upper_bound))
}

# Apply the function by group
filtered_shad<- df_manip  %>%
  group_by(Condition_Id) %>%
  filter(remove_outliers(VOT))

# ungroup
filtered_shad<- ungroup(filtered_shad)

# Print the resulting data
print(filtered_shad)


shad_plot  <- filtered_shad  %>% mutate(condition = case_when(cond == "imi" ~ "imitation", cond == "rep" ~"repetition"))
shad_plot <- shad_plot  %>% mutate(lex = case_when(mipair== "tierdeer" ~ "deer/tier", mipair == "tilldill" ~ "dill/till"))
shad_plot<- shad_plot  %>% mutate(stimulus = case_when(Condition_Id== 1 ~ "VOT level 3",
                                                                Condition_Id== 2 ~ "VOT level 4", 
                                                                Condition_Id== 3 ~ "VOT level 5")
                                                          )
filtered_data$task = factor(filtered_data$task, levels=c("Pre Test", "Post Test"))


#plot
ggplot(data = shad_plot, aes (x=stimulus, y=VOT, color = stimulus))+
  geom_boxplot(width = 0.2, aes(group = stimulus))+
  facet_grid(condition ~ lex ~bias ~ Task_Name)+
  theme_bw()+
  labs(fill ="produced stimulus" )+
  xlab(label = "stimulus level")+
  ylab(label = "VOT in seconds")

summary(df_manip)

#now model the manipulation part
m_shad <-  lmer(log(VOT)~ Condition_Id * cond * bias + (1+Condition_Id|Rec_Session_Id), data = filtered_shad)
summary(m_shad)
resshad = summary(m_shad)$resid
ggqqplot(resshad)
#here it works... 

emmshad <- emmeans(m_shad,  ~ Condition_Id * cond * bias)
 pairs(emmshad, adjust = "fdr")

#collapse cond and bias into gr.
m_shad1 <-  lmer(log(VOT)~ Condition_Id * gr + (1+Condition_Id|Rec_Session_Id), data = filtered_shad)
summary(m_shad1)
res = summary(m_shad1)$resid
ggqqplot(res)
emmshad1 <- emmeans(m_shad1,  ~ Condition_Id * gr)
shad_pairs <- pairs(emmshad1, adjust = "fdr")
print(shad_pairs)

#do the filter thing again
within_con<- c("1 deer_imi - 2 deer_imi",
"1 deer_imi - 3 deer_imi",
"2 deer_imi - 3 deer_imi",

"1 deer_rep - 2 deer_rep",
"1 deer_rep - 3 deer_rep",
"2 deer_rep - 3 deer_rep",

"1 tier_imi - 2 tier_imi", 
"1 tier_imi - 3 tier_imi", 
"2 tier_imi - 3 tier_imi",

"1 tier_rep - 2 tier_rep",
"1 tier_rep - 3 tier_rep",
"2 tier_rep - 3 tier_rep",

"1 dill_imi - 2 dill_imi", 
"1 dill_imi - 3 dill_imi", 
"2 dill_imi - 3 dill_imi",

"1 dill_rep - 2 dill_rep",
"1 dill_rep - 3 dill_rep", 
"2 dill_rep - 3 dill_rep",

"1 till_imi - 2 till_imi",
"1 till_imi - 3 till_imi", 
"2 till_imi - 3 till_imi",

"1 till_rep - 2 till_rep", 
"1 till_rep - 3 till_rep", 
"2 till_rep - 3 till_rep")

shad_pairs <- as.data.table(shad_pairs)
print(shad_pairs)
# Subset the rows based on the specified strings
pairs_shad <- shad_pairs[grepl(paste(within_con, collapse = "|"), shad_pairs$contrast), ]


# Print the filtered data frame
print(pairs_shad)
print(xtable(pairs_shad , type = "latex"))#, file = "prod_filt.tex")




#now for individual level comparisons. 

# first summarize the data... 

summary_data_perc <- df_all_resp %>%
  group_by(Rec_Session_Id, Task_Name) %>%
  summarise(
    Response_Count = n(),
  )


# Count responses for each Rec_Session_Id and Task_Name

df_all_resp <-  as.data.frame(df_all_resp)


summary_data_perc <- df_all_response %>%
  group_by(Rec_Session_Id, Task_Name) %>%
  summarise(
    Count_t = sum(response =="t", na.rm = TRUE),
    Count_d = sum(response == "d", na.rm = TRUE),
    #Mean_VOT = mean(VOT, na.rm = TRUE),
    Bias = first(bias), 
    Cond = first(cond),  
    Gr = first(gr,  
    
  )

# Print the summary data
print(summary_data_perc)

summary_data_prod <- filtered_data%>%
  group_by(Rec_Session_Id, Task_Name, voicing) %>%
  summarise(
    Mean_VOT = mean(VOT, na.rm = TRUE), 
    Bias = first(bias),  
    Cond = first(cond),
    Gr = first(gr),  
  )

print(summary_data_prod)

#create columns and values for pre and post. 
summary_pre_prod <-  summary_data_prod %>% subset(Task_Name == "Production_Pre")
summary_post_prod <-  summary_data_prod %>% subset(Task_Name == "Production_Post")
summary_prod  <- left_join(summary_post_prod, summary_pre_prod, by=c('Rec_Session_Id'='Rec_Session_Id', 'voicing'='voicing'))

    change pre-post 
summary_prod <- summary_prod %>% mutate(change_prod = Mean_VOT.x - Mean_VOT.y)

#claculate change in perception
summary_pre_perc <-  summary_data_perc %>% subset(Task_Name == "Perception_Pre_tier")
summary_post_perc <-  summary_data_perc %>% subset(Task_Name == "Perception_Post_tier")

 
summary_perc <- left_join(summary_post_perc, summary_pre_perc, by=c('Rec_Session_Id'='Rec_Session_Id'))
summary_perc <- summary_perc %>% mutate(change_perc_t = Count_t.x - Count_t.y) #pre-post
summary_perc <- summary_perc%>% mutate(change_perc_d = Count_d.x - Count_d.y) #pre-post

summary_prod_d <-  summary_prod %>% subset(voicing == "d") #split by voicing
summary_prod_t <-  summary_prod %>% subset(voicing == "t")

summary_perc_prod <-left_join(summary_perc, summary_prod_t, by=c('Rec_Session_Id'='Rec_Session_Id')) #everything into one illegible dataframe
summary_perc_prod <-left_join(summary_perc_prod, summary_prod_d, by=c('Rec_Session_Id'='Rec_Session_Id'))

print(summary_perc_prod) #have a look

    #correlate
#perc_prod t-t 
cor.test(summary_perc_prod$change_perc_t, summary_perc_prod$change_prod.x)

#plot the correlation
ggplot(summary_perc_prod, aes(x = change_perc_t, y = change_prod.x)) + 
  geom_point(aes(color = Bias.x.x))+
  stat_smooth(method = "lm", color = "black")+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_vline(aes(xintercept = 0), linetype = "dashed")+
  theme_bw()+
  labs(color ="Bias")+
  xlab(label = "change in categorization between pre-and post test")+
  ylab(label = "change in production between pre and post test")+
  ggtitle("Correlation of categorization and production for /t/ initial stimuli")


#perc_prod d-d
cor.test(summary_perc_prod$change_perc_d, summary_perc_prod$change_prod.y)

ggplot(summary_perc_prod, aes(x = change_perc_d, y = change_prod.y)) + 
  geom_point(aes(color = Bias.x.x))+
  stat_smooth(method = "lm", color = "black")+
  labs(color="Bias")+
  geom_hline(aes(yintercept = 0), linetype = "dashed")+
  geom_vline(aes(xintercept = 0), linetype = "dashed")+
  theme_bw()+
  xlab(label = "change in categorization between pre-and post test")+
  ylab(label = "change in production between pre and post test")+
  ggtitle("Correlation of categorization and production for /d/ initial stimuli")
