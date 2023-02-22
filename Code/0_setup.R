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
  
  
  