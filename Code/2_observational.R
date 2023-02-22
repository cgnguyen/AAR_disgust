#Setup----------------------------
  library(tidyverse)
  library(haven)
  
  library(huxtable)
  

  D<-read_rds("./DATA/observational.rds")
  
  D<-
    D%>%filter(white_resp=="Yes")

  
#Main models: Does Disease Disgust correlate with outgroup rejection --------------------
  ##Modern Racism Scale-------------------------
  mod_mod_rac_index_1<-lm(mod_rac_index~disgust_disease, 
                       data=D)
  
  
   mod_mod_rac_index_2<-lm(mod_rac_index~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland, 
                       data=D)
    
    
   
     
  mod_mod_rac_index_3<-lm(mod_rac_index~disgust_disease+ 
                              age+gender_resp+activity_simple+edu_simple+
                              bundesland+
                              lr+
                              big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                       data=D)
  
 
  
  screenreg(list(mod_mod_rac_index_1,mod_mod_rac_index_2,mod_mod_rac_index_3))


  
  screenreg(list(mod_eval_asian,mod_eval_black,mod_eval_muslim,mod_eval_white))
  
  
  ##Social Distance Measures for outgroups-----------------------------------
  mod_soc_dist_asian_index<-lm(soc_dist_asian_index~disgust_disease+ 
                              age+gender_resp+activity_simple+edu_simple+
                              bundesland+
                              lr+
                              big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                              data=D)
    
  
  mod_soc_dist_black_index<-lm(soc_dist_black_index~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
  mod_soc_dist_muslim_index<-lm(soc_dist_muslim_index~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
    
  mod_soc_dist_white_index<-lm(soc_dist_white_index~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
  
    export_summs(mod_soc_dist_asian_index,mod_soc_dist_black_index,mod_soc_dist_muslim_index,mod_soc_dist_white_index,
                 model.names = c("Asian","Black","MENA","White"),
                 coefs = c("Disease Disgust"="disgust_disease",
                          "Age"="age",
                          "Female" = "gender_respweiblich",
                          "Part-Time"="activity_simplePart_Time",
                          "Retired" ="activity_simpleRetired",
                          "Student"= "activity_simpleEducation",
                          "Unemployed"= "activity_simpleUnemployed",
                          "At-Home"= "activity_simpleAt_home",
                          "Sick or Disabled"= "activity_simpleSick_or_disabled",
                          "Other"="activity_simpleOther",
                          "Educational Attainment-Middle"="edu_simplemittlere",
                          "Educational Attainment-High"="edu_simplehohe",
                          "Left-Right Self Placement"="lr",
                          "Extroversion"="big_5_extro",
                          "Agreeableness"="big_5_agree",
                          "Conscienscitousness"="big_5_consc",
                          "Neuroticism"="big_5_neuro",
                          "Openeness"="big_5_open"))
                 
                          
       
                          
                           mutate(big_5_extro = rowMeans(dplyr::select(.,big5_extro_1,big5_extro_2), na.rm=T),
               big_5_agree= rowMeans(dplyr::select(.,big5_agree_1,big5_agree_2), na.rm=T),
               big_5_consc= rowMeans(dplyr::select(.,big5_consc_1,big5_extro_2), na.rm=T),
               big_5_neuro= rowMeans(dplyr::select(.,big5_neuro_1,big5_neuro_2), na.rm=T),
               big_5_open= rowMeans(dplyr::select(.,big5_open_1,big5_open_2), na.rm=T))
                          ""
                 
                 

  
  
#Results for Paper --------------------------------------------------------------------------------
    ##Visualizations ---------------------------------------------------------------
    ###Modern Racism Measure---------------
    
    
    
    plot_summs(mod_mod_rac_index_1,mod_mod_rac_index_2,mod_mod_rac_index_3,
               omit.coefs = "bundeslandBayern")
    
    
    
    
    
    
    
    
    
    ###Social Distance Measures------------------------------------------------------
    plot_summs(mod_soc_dist_asian_index, mod_soc_dist_black_index,mod_soc_dist_muslim_index,mod_soc_dist_white_index,
               omit.coef="bundesland")

    
    
    
  
#Appendix -----------------
    ##Outgroup Evaluations--------------------
    
    
  ##Evaluation of (out) groups---------------------
  mod_eval_asian<-lm(eval_asian~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr, 
                      data=D)
    
  mod_eval_black<-lm(eval_black~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr, 
                     data=D)
      
  
  mod_eval_muslim<-lm(eval_muslim~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr, 
                     data=D)
  
  mod_eval_white<-lm(eval_white~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr, 
                     data=D)
  
  
  
  ##Check sub-dimensions for social distance measures----------------------------
  ###Neighbours------------------------------
  mod_soc_dist_asian_1<-lm(soc_dist_asian_1~disgust_disease+ 
                              age+gender_resp+activity_simple+edu_simple+
                              bundesland+
                              lr+
                              big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                              data=D)
    
  
  mod_soc_dist_black_1<-lm(soc_dist_black_1~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
  mod_soc_dist_muslim_1<-lm(soc_dist_muslim_1~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
    
  mod_soc_dist_white_1<-lm(soc_dist_white_1~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
  
  screenreg(list(mod_soc_dist_asian_1,mod_soc_dist_black_1,mod_soc_dist_muslim_1,mod_soc_dist_white_1))
  
  
  ###Work------------------------------
  mod_soc_dist_asian_2<-lm(soc_dist_asian_2~disgust_disease+ 
                              age+gender_resp+activity_simple+edu_simple+
                              bundesland+
                              lr+
                              big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                              data=D)
    
  
  mod_soc_dist_black_2<-lm(soc_dist_black_2~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
  mod_soc_dist_muslim_2<-lm(soc_dist_muslim_2~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
    
  mod_soc_dist_white_2<-lm(soc_dist_white_2~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
  
  screenreg(list(mod_soc_dist_asian_2,mod_soc_dist_black_2,mod_soc_dist_muslim_2,mod_soc_dist_white_2))
  
  
  ###Family-------------------------------------------------------------
  mod_soc_dist_asian_3<-lm(soc_dist_asian_3~disgust_disease+ 
                              age+gender_resp+activity_simple+edu_simple+
                              bundesland+
                              lr+
                              big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                              data=D)
    
  
  mod_soc_dist_black_3<-lm(soc_dist_black_3~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
  mod_soc_dist_muslim_3<-lm(soc_dist_muslim_3~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+
                               lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
    
  
    
  mod_soc_dist_white_3<-lm(soc_dist_white_3~disgust_disease+ 
                             age+gender_resp+activity_simple+edu_simple+
                             bundesland+lr+
                               big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                      data=D)
  
  screenreg(list(mod_soc_dist_asian_3,mod_soc_dist_black_3,mod_soc_dist_muslim_3,mod_soc_dist_white_3))
  