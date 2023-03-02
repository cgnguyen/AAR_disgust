#Setup-----------------
  library(tidyverse)
  library(haven)
  library(car)
  library(labelled)
  library(dplyr)
  library(readr)
  library(tidyr)

#Clean Wave 1------------------------------------------------------
 D_1<-read_sav("./DATA/wave_1.sav")
  
  ##Sociodemographics -------------------------------------------------------
    D_1$gender_resp<-as_factor(D_1$v_13)
    D_1$migrant<-as_factor(D_1$c_0013)
    D_1$edu_simple<-as_factor(D_1$v_15)
    D_1$age<-D_1$v_12
    D_1$bundesland<-as_factor(D_1$v_16)
    D_1$lr<-car::recode(D_1$v_102,"98=NA;99=NA")
    D_1$activity<-as_factor(D_1$v_28)
  
    D_1$activity_simple <- recode_factor(D_1$activity,
        "Vollzeit berufstätig" = "Full_Time",
        "Teilzeit berufstätig" = "Part_Time",
        "In Pension/ Rente" = "Retired",
        "Schüler/Student/ in Ausbildung" = "Education",
        "Arbeitslos oder arbeitssuchend" = "Unemployed",
        "Hausmann/Hausfrau" = "At_home",
        "Elternzeit" = "At_home",
        "Arbeitsunfähig" = "Sick_or_disabled",
        "Anderes" = "Other",
        "weiß nicht" = NA_character_,
        "keine Angabe" = NA_character_
      )
    
  
    D_1$edu_simple <- fct_recode(D_1$edu_simple,
      "niedrige" = "niedrige Bildung",
      "mittlere" = "mittlere Bildung",
      "hohe" = "hohe Bildung"
    )
    
  ##Own identity-------------------------
  look_for(D_1,"F11")
  
  D_1<-
    D_1%>%
    mutate(asian_resp=as.factor(case_when(v_47 ==1 ~ "Yes",
                                          TRUE ~ "No")),
           black_resp=as.factor(case_when(v_48 ==1 ~ "Yes",
                                          TRUE ~ "No")),
           white_resp=as.factor(case_when(v_49 ==1 ~ "Yes",
                                          TRUE ~ "No")))

  ##Disgust Sensitivity----------------------
  D_1<-
    D_1 %>% 
      mutate(disgust_1=v_96,
               disgust_2=v_97,
               disgust_3=v_98,
               disgust_4=v_99,
               disgust_5=v_100,
               disgust_6=v_101)%>%
      mutate(disgust_all = rowMeans(dplyr::select(., dplyr::starts_with("disgust")), na.rm = TRUE),
              disgust_disease = rowMeans(dplyr::select(.,disgust_3,disgust_6), na.rm = TRUE),
              disgust_animal = rowMeans(dplyr::select(.,disgust_2,disgust_5), na.rm = TRUE),
              disgust_general = rowMeans(dplyr::select(.,disgust_1,disgust_4), na.rm = TRUE))
      
  
  ##Group Evaluations------------------------
    look_for(D_1, "F31") # note coding error 0=NA?
     
     D_1<-
       D_1%>%
        mutate(eval_white=car::recode(as.numeric(v_184),"0= NA;1=-5;2=-4;3=-3;4=-2;5=-1;6=0;7=1;8=2;9=3;10=4;11=5"),
               eval_asian=car::recode(as.numeric(v_178),"0= NA;1=-5;2=-4;3=-3;4=-2;5=-1;6=0;7=1;8=2;9=3;10=4;11=5"),
               eval_black=car::recode(as.numeric(v_180),"0= NA;1=-5;2=-4;3=-3;4=-2;5=-1;6=0;7=1;8=2;9=3;10=4;11=5"),  
               eval_muslim=car::recode(as.numeric(v_182),"0= NA;1=-5;2=-4;3=-3;4=-2;5=-1;6=0;7=1;8=2;9=3;10=4;11=5"))%>%
       mutate(eval_asian_dif= eval_asian-eval_white,
              eval_black_dif= eval_black-eval_white,
              eval_muslim_dif= eval_muslim-eval_white)
     
     
   
    
    
    ##Modern Racism--------------------------
    #Note- last item reverse coded
    D_1<-
      D_1 %>% 
        mutate(mod_rac_1= car::recode(as.numeric(v_162)," 6= NA;7=NA;98=NA"),
                 mod_rac_2= car::recode(as.numeric(v_163)," 6= NA;7=NA;98=NA"),
                 mod_rac_3= car::recode(as.numeric(v_164)," 6= NA;7=NA;98=NA"),
                 mod_rac_4= car::recode(as.numeric(v_165)," 6= NA;7=NA;98=NA"),
                 mod_rac_5= car::recode(as.numeric(v_166)," 6= NA;7=NA;98=NA"),
                 mod_rac_6= car::recode(as.numeric(v_167)," 6= NA;7=NA;98=NA;
                                        5=1;4=2;3=3;2=4;1=5",))%>%
        mutate(mod_rac_index = rowMeans(dplyr::select(., dplyr::starts_with("mod_rac_")), na.rm = TRUE),
               mod_rac_w1 = rowMeans(dplyr::select(.,mod_rac_1,mod_rac_4,mod_rac_5 ), na.rm = TRUE))
        
     
    
    
     ##Social Distance Measures----------------------------
     look_for(D_1, "F33")
     
     D_1<-
       D_1%>%
        mutate(soc_dist_asian_1=car::recode(v_185,"98=NA;99=NA")-4,
               soc_dist_asian_2=car::recode(v_186,"98=NA;99=NA")-4,
               soc_dist_asian_3=car::recode(v_187,"98=NA;99=NA")-4,
               soc_dist_black_1=car::recode(v_188,"98=NA;99=NA")-4,
               soc_dist_black_2=car::recode(v_189,"98=NA;99=NA")-4,
               soc_dist_black_3=car::recode(v_190,"98=NA;99=NA")-4,
               soc_dist_muslim_1=car::recode(v_191,"98=NA;99=NA")-4,
               soc_dist_muslim_2=car::recode(v_192,"98=NA;99=NA")-4,
               soc_dist_muslim_3=car::recode(v_193,"98=NA;99=NA")-4,
               soc_dist_white_1=car::recode(v_194,"98=NA;99=NA")-4,
               soc_dist_white_2=car::recode(v_195,"98=NA;99=NA")-4,
               soc_dist_white_3=car::recode(v_196,"98=NA;99=NA")-4)%>%
      mutate(soc_dist_asian_index = rowMeans(dplyr::select(., dplyr::starts_with("soc_dist_asian_")), na.rm = TRUE),
             soc_dist_black_index = rowMeans(dplyr::select(., dplyr::starts_with("soc_dist_black_")), na.rm = TRUE),
             soc_dist_muslim_index = rowMeans(dplyr::select(., dplyr::starts_with("soc_dist_muslim_")), na.rm = TRUE),
             soc_dist_white_index = rowMeans(dplyr::select(., dplyr::starts_with("soc_dist_white_")), na.rm = TRUE))
     
     
      ##Big 5--------------
      #Some Items reverse coded)
      D_1<-
        D_1%>%
          mutate(big5_extro_1=(car::recode(v_63,"98=NA;99=NA")*-1+5),
                 big5_agree_1=(car::recode(v_64,"98=NA;99=NA")-1),
                 big5_consc_1=(car::recode(v_65,"98=NA;99=NA")*-1+5),
                 big5_neuro_1=(car::recode(v_66,"98=NA;99=NA")*-1+5),
                 big5_open_1=(car::recode(v_67,"98=NA;99=NA")*-1+5),
                 big5_extro_2=(car::recode(v_68,"98=NA;99=NA")-1),
                 big5_agree_2=(car::recode(v_69,"98=NA;99=NA")*-1+5),
                 big5_consc_2=(car::recode(v_70,"98=NA;99=NA")-1),
                 big5_neuro_2=(car::recode(v_71,"98=NA;99=NA")-1),
                 big5_open_2=(car::recode(v_72,"98=NA;99=NA")-1))%>%
        mutate(big_5_extro = rowMeans(dplyr::select(.,big5_extro_1,big5_extro_2), na.rm=T),
               big_5_agree= rowMeans(dplyr::select(.,big5_agree_1,big5_agree_2), na.rm=T),
               big_5_consc= rowMeans(dplyr::select(.,big5_consc_1,big5_extro_2), na.rm=T),
               big_5_neuro= rowMeans(dplyr::select(.,big5_neuro_1,big5_neuro_2), na.rm=T),
               big_5_open= rowMeans(dplyr::select(.,big5_open_1,big5_open_2), na.rm=T))
      
  
    
   write_rds(D_1, file="./DATA/observational.rds")
   
   
#Clean Wave 3 ------------------------------------------
     D_3<-read_sav("./Data/wave_3.sav")




##Conditions --------------------------------------------------------------
  D_3<-
    D_3%>%
    dplyr::mutate(condition=as.factor(c_0066),
           anger = as.factor(case_when(condition == 1 ~ "Yes",
                                       condition == 5 ~ "Yes", 
                                       TRUE ~"No")),
           anxiety = as.factor(case_when(condition == 2 ~ "Yes",
                                       condition == 6 ~ "Yes", 
                                       TRUE ~"No")),
           disgust = as.factor(case_when(condition == 3 ~ "Yes",
                                       condition == 7 ~ "Yes", 
                                       TRUE ~"No")),
           corona =as.factor(case_when(condition == 5 ~ "Yes",
                                       condition == 6 ~"Yes",
                                       condition == 7 ~ "Yes",
                                       condition == 8 ~ "Yes",
                                       TRUE ~"No")),
           emotion= as.factor(case_when(anger == "Yes" ~ "Anger",
                                        anxiety == "Yes" ~ "Anxiety",
                                        disgust == "Yes" ~ "Disgust",
                                         TRUE ~ "Control")))
                                        
  ## Recoding D_3$condition
  D_3$condition <- fct_recode(D_3$condition,
    "Anger-General" = "1",
    "Anxiety-General" = "2",
    "Disgust-General" = "3",
    "Control" = "4",
    "Anger-Corona" = "5",
    "Anxiety-Corona" = "6",
    "Disgust-Corona" = "7",
    "Corona" = "8"
  )
  
  D_3$condition<-relevel(D_3$condition, ref="Control")
  

## PTV AfD -----------------------------------------------------------------

    D_3<-
     D_3%>%
     dplyr::mutate(ptv_afd_1=car::recode(v_123,"99=0"),
            ptv_afd_2=car::recode(v_627,"99=0"),
            ptv_afd = case_when(is.na(ptv_afd_1) ~ (ptv_afd_2),
                                TRUE ~ ptv_afd_1))
  
     D_3<-
      D_3%>%
      dplyr::mutate(ptv_cdu_1=car::recode(v_113,"99=0"),
            ptv_cdu_2=car::recode(v_617,"99=0"),
            ptv_cdu = case_when(is.na(ptv_cdu_1) ~ (ptv_cdu_2),
                                TRUE ~ ptv_cdu_1))
  
    

## Party Affinity ----------------------------------------------------------
  look_for(D_3, "F3_03")
  
  D_3$v_554
  
  D_3<-
    D_3%>%
      dplyr::mutate(aff_afd=car::recode(v_554,"99=NA; 98=NA"))%>%
      dplyr::mutate(aff_afd=aff_afd-4)

  
## Manipulation Checks -------------------------------------------------------
   D_3<-
    D_3%>%
      dplyr::mutate(emo_anger_1=car::recode(v_581,"98=NA;99=NA"),
             emo_anger_2=car::recode(v_601,"98=NA;99=NA"),
             emo_anger=case_when(is.na(emo_anger_1) ~ (emo_anger_2),
                                TRUE ~ emo_anger_1))
  
  D_3<-
    D_3%>%
      dplyr::mutate(emo_anxiety_1=car::recode(v_582,"98=NA;99=NA"),
             emo_anxiety_2=car::recode(v_602,"98=NA;99=NA"),
             emo_anxiety=case_when(is.na(emo_anxiety_1) ~ (emo_anxiety_2),
                                TRUE ~ emo_anxiety_1))
  
  D_3<-
    D_3%>%
      dplyr::mutate(emo_happy_1=car::recode(v_583,"98=NA;99=NA"),
             emo_happy_2=car::recode(v_603,"98=NA;99=NA"),
             emo_happy=case_when(is.na(emo_happy_1) ~ (emo_happy_2),
                                TRUE ~ emo_happy_1))
  
    D_3<-
    D_3%>%
      dplyr::mutate(emo_sad_1=car::recode(v_584,"98=NA;99=NA"),
             emo_sad_2=car::recode(v_604,"98=NA;99=NA"),
             emo_sad=case_when(is.na(emo_sad_1) ~ (emo_sad_2),
                                TRUE ~ emo_sad_1))
  
  D_3<-
    D_3%>%
      dplyr::mutate(emo_disgust_1=car::recode(v_585,"98=NA;99=NA"),
             emo_disgust_2=car::recode(v_605,"98=NA;99=NA"),
             emo_disgust=case_when(is.na(emo_disgust_1) ~ (emo_disgust_2),
                                TRUE ~ emo_disgust_1))  
    
  D_3<-
    D_3%>%
      dplyr::mutate(emo_guilty_1=car::recode(v_586,"98=NA;99=NA"),
             emo_guilty_2=car::recode(v_606,"98=NA;99=NA"),
             emo_guilty=case_when(is.na(emo_guilty_1) ~ (emo_guilty_2),
                                TRUE ~ emo_guilty_1))   
  
  D_3<-
    D_3%>%
      dplyr::mutate(emo_powerless_1=car::recode(v_587,"98=NA;99=NA"),
             emo_powerless_2=car::recode(v_607,"98=NA;99=NA"),
             emo_powerless=case_when(is.na(emo_powerless_1) ~ (emo_powerless_2),
                                TRUE ~ emo_powerless_1))  



# ## Feeling of Control ------------------------------------------------------
#   look_for(D_3, "F15.1")
#   
# 
# 
#   
#   D_3<-
#     D_3%>%
#       dplyr::mutate(feel_control_1_p=car::recode(v_501,"98=NA;99=NA")-1,
#                     feel_control_2_p=(car::recode(v_502,"98=NA;99=NA")-5)*-1,
#                     feel_control_3_p=car::recode(v_503,"98=NA;99=NA")-1,
#                     feel_control_4_p=car::recode(v_504,"98=NA;99=NA")-1,
#                     feel_control_5_p=(car::recode(v_505,"98=NA;99=NA")-5)*-1)%>%
#          mutate(feel_control_p = rowMeans(dplyr::select(., starts_with("feel_control_")), na.rm = TRUE))
#   
#   
#   D_3<-
#     D_3%>%
#       unite(feel_control, c("v_588","v_608"), na.rm=T)%>%
#       mutate(feel_control=car::recode(v_501,"98=NA;99=NA")-1)
#       
# 

##Modern Rac --------------------------------------------------------------
    select_vec<-look_for(D_3, "F29")$variable


      D_3<-
        D_3%>%
          unite(mod_rac_1, c("v_162","v_634"), na.rm=T)%>%
          unite(mod_rac_2, c("v_163","v_635"), na.rm=T)%>%
          unite(mod_rac_3, c("v_164","v_636"), na.rm=T)%>%
          dplyr::mutate(across(starts_with("mod_rac"),
                        ~as.numeric(.)))

      D_3<-
        D_3%>%
          dplyr::mutate(mod_rac_1=car::recode(mod_rac_1,"6=NA;7=NA"),
                 mod_rac_2=car::recode(mod_rac_2,"6=NA;7=NA"),
                 mod_rac_3=car::recode(mod_rac_3,"6=NA;7=NA"))%>%
          rowwise()%>%
          dplyr::mutate(mod_rac = mean(c(mod_rac_1,mod_rac_2,mod_rac_3), na.rm=T))

# 
# ## Populism Scale 1 Pre-Treatment-----------------------------------------------
# 
#    D_3<-
#         D_3%>%
#           dplyr::rename(pop_p_1=v_537)%>%
#           dplyr::rename(pop_p_2=v_538)%>%
#           dplyr::rename(pop_p_3=v_539)%>%
#           dplyr::rename(pop_p_4=v_540)%>%
#           dplyr::rename(pop_p_5=v_541)%>%
#           dplyr::rename(pop_p_6=v_542)%>%
#           rowwise()%>%
#           dplyr::mutate(pop_people_p = mean(c(pop_p_3,pop_p_4), na_rm=T),
#                  pop_elite_p = mean(c(pop_p_1,pop_p_2), na_rm=T),
#                  pop_will_p = mean(c(pop_p_5,pop_p_6), na_rm=T),
#                  pop_p = mean(c(pop_p_1,pop_p_2,pop_p_3,pop_p_4,pop_p_5, pop_p_6),na.rm=T),
#                  pop_p_min=min(c(pop_people_p,pop_elite_p,pop_will_p),na.rm=T),
#                  pop_p_multip=pop_people_p*pop_elite_p*pop_will_p)
#                  
#                    
#                    
#                  
#                  
# 
# ##Populism Scale 2 --------------------------------------------------------------
#     select_vec<-look_for(D_3, "F99")$variable
# 
#         
#       D_3<-
#         D_3%>%
#           dplyr::mutate(across(select_vec, ~na_if(.,99)))%>%
#           dplyr::mutate(across(select_vec, ~na_if(.,98)))%>%
#           
#           unite(pop_1, c("v_595","v_628"), na.rm=T)%>%
#           unite(pop_2, c("v_596","v_629"), na.rm=T)%>%
#           unite(pop_3, c("v_597","v_630"), na.rm=T)%>%
#           unite(pop_4, c("v_598","v_631"), na.rm=T)%>%
#           unite(pop_5, c("v_599","v_632"), na.rm=T)%>%
#           unite(pop_6, c("v_600","v_633"), na.rm=T)%>%
#         
#           dplyr::mutate(across(starts_with("pop_"),
#                         ~as.numeric(.)))
#       
#      D_3<-
#        D_3%>%
#         mutate(pop_people = rowMeans(dplyr::select(.,pop_1,pop_2), na.rm = TRUE),
#                pop_elite = rowMeans(dplyr::select(.,pop_3,pop_4), na.rm = TRUE),
#                pop_will = rowMeans(dplyr::select(.,pop_5,pop_6), na.rm = TRUE),
#                pop = rowMeans(dplyr::select(.,pop_1,pop_2,pop_3,pop_4,pop_5, pop_6),na.rm=T),
#                pop_p_min=min(c(pop_people_p,pop_elite_p,pop_will_p),na.rm=T),
#                pop_multi=pop_people*pop_elite*pop_will)
#                
     

               
##Conspiracy Theories--------------------
 # note reverse coding on item v_277/640
  look_for(D_3, "F42")
         select_vec<-look_for(D_3, "F42")$variable

       D_3<-D_3%>%
          dplyr::mutate(across(select_vec, ~na_if(.,99)))%>%
          dplyr::mutate(across(select_vec, ~na_if(.,98)))%>%
          dplyr::mutate(across(select_vec, ~na_if(.,9)))%>%
          dplyr::mutate(across(select_vec, ~na_if(.,8)))%>%
          unite(consp_1, c("v_275","v_638"), na.rm=T)%>%
          unite(consp_2, c("v_276","v_639"), na.rm=T)%>%
          unite(consp_3, c("v_277","v_640"), na.rm=T)%>%
          unite(consp_4, c("v_278","v_641"), na.rm=T)%>%
          unite(consp_5, c("v_279","v_642"), na.rm=T)%>%
          unite(consp_6, c("v_280","v_643"), na.rm=T)%>%
          unite(consp_7, c("v_281","v_644"), na.rm=T)%>%
          dplyr::mutate(across(starts_with("consp_"),
                        ~as.numeric(.)))
         
## Data Exclusion --------------------
       
         #Read in handcoded noncompliance data 
  prime_code<-read_csv("./Data/prime_code.csv")


  D_3<-
    prime_code%>%
    dplyr::select(-condition,-text_prime)%>%
    right_join(D_3, by="lfdn")
      
 

  D_3<-
    D_3%>%
    dplyr::mutate(exclude=case_when(non_comp==1 ~ 1,
                             no_experience==1 ~1, 
                             TRUE ~ 0))

#Conjoint Cleaning & Setup-----------------------------------
  ##dplyr::select conditions and pivot longer to have one row per trial
  #Choice conditions are listed in condition 31 to 40 - number represents to condition number
  condition_vec<-paste0("c_00", 31:40)
  
  ubahn_condition<-D_1%>%
    dplyr::select("lfdn",condition_vec)%>%
    pivot_longer(condition_vec, values_to="condition", names_to="trial_condition")%>%
    mutate(condition=as.numeric(as.character(condition)))

  #Also add choices for each condition and pivot longer
  #trial choices are listed on v18 to v27
  trial_vec<-paste("v",18:27, sep="_")
  

  ubahn_choice<-D_1%>%
    dplyr::select("lfdn",trial_vec)%>%
    pivot_longer(trial_vec, values_to="choice",names_to="trial")%>%
    dplyr::select(-"lfdn")
  
  ##combine condition and choice data for all variables then duplicate each row for two rows per trial choice
  ubahn_temp<-cbind(ubahn_condition,ubahn_choice)%>%
    dplyr::select(lfdn,condition,choice)%>%
    group_by(lfdn)%>%
    mutate(trial=row_number())%>%
    slice(rep(1:n(), each = 2))%>%
    group_by(lfdn,trial)%>%
    mutate(position=c("left","right"))
  
    #This should make merging the conditions in "easy" since I can merge on position and condition 
  
  
  
  ##Get back trial conditions from design file
  conditions<-read.csv("./Data/conditions.csv")
  
  #Make conditions comparable by including position as proper factor
  conditions<-conditions %>%
    group_by(condition)%>%
    slice(rep(1:n(), each = 2))%>%
    mutate(position=c("left","right"))
  
  conditions_left<-conditions %>%
    filter(position=="left")%>%
    dplyr::select(condition,left_gender,left_race,left_mask,position)%>%
    mutate(gender=left_gender,race=left_race,mask=left_mask)%>%
    dplyr::select(condition,gender,race,mask,position)
  
  conditions_right<-conditions %>%
    filter(position=="right")%>%
    dplyr::select(condition,right_gender,right_race,right_mask,position)%>%
    mutate(gender=right_gender,race=right_race,mask=right_mask)%>%
    dplyr::select(condition,gender,race,mask,position)
  
  #Recombine conditions and sort them to be easier to read by condition
  conditions_merge<-rbind(conditions_left,conditions_right)%>%
    arrange(condition,position)
  
  
   ubahn<-
      ubahn_temp %>%
      left_join(conditions_merge, by=c("condition","position"))
  
   


##recode choice variable to 0 and 1 variable based on condition -slow 
    ubahn$choice<-as.numeric(ubahn$choice)
    
    ubahn<-
      ubahn %>%
        mutate(choice=as.factor(car::recode(choice,"
                                1 = 'left'; 
                                2 = 'right'")))
    
    
    ubahn<-
      ubahn%>%
      group_by(lfdn,trial)%>%
      mutate(choice_binary = case_when(choice==position ~ 1,
                                       choice!=position ~ 0))
    
    ubahn<-
      ubahn %>%
        dplyr::select(lfdn,choice_binary,trial,gender,mask,race,position)%>%
        mutate(gender=as.factor(gender),
               mask=as.factor(mask),
               race=as.factor(race),
               position=as.factor(position))
    

    #Merge back with main DATA
    ubahn_1<-
      ubahn %>%
        left_join(D_1, by="lfdn")
    
  
  #Define baselines  
  ubahn_1$race<-relevel(ubahn_1$race, ref="white")
  ubahn_1$gender<-relevel(ubahn_1$gender, ref="man")
  ubahn_1$mask<-relevel(ubahn_1$mask, ref="nomask")
  
     
  #Export
  write_rds(ubahn_1, file="./DATA/conjoint.rds")
  
  
  