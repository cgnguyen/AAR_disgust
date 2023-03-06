#Setup----------------------------
  library(tidyverse)
  library(haven)
  library(officer)
  library(huxtable)
  library(psych)
  library(jtools)
  library(rvg)
  library(broom)
  library(dotwhisker)

  D<-read_rds("./DATA/observational.rds")
  
  D<-
    D%>%filter(white_resp=="Yes")

#Summary and Descriptive Statistics -------------------------------------------------------------------
    psych::alpha(x= subset(D, select=c(mod_rac_1,mod_rac_2,mod_rac_3,mod_rac_4,mod_rac_5,mod_rac_6)))
  
  
  
#Main models: Does Disease Disgust correlate with out group rejection --------------------
  ##Modern Racism Scale-------------------------
  mod_mod_rac_index_1<-lm(mod_rac_index~disgust_disease, 
                            data=D)
  
  
  mod_mod_rac_index_2<-lm(mod_rac_index~disgust_disease+ 
                            age+gender_resp+activity_simple+edu_simple+
                            lr+
                            bundesland, 
                            data=D)

  mod_mod_rac_index_3<-lm(mod_rac_index~disgust_disease+ 
                            age+gender_resp+activity_simple+edu_simple+
                            bundesland+
                            lr+
                            big_5_extro+big_5_agree+big_5_consc+big_5_neuro+big_5_open, 
                            data=D)
  
 
  ###Results Table------------------------------
  table_1<-
    export_summs(mod_mod_rac_index_1,mod_mod_rac_index_2,mod_mod_rac_index_3,
               scale=T,
               digits=3,
               model.names = c("Bivariate Model","Socio-Demographic Controls","Full Model"),
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
                          "Conscientiousness"="big_5_consc",
                          "Neuroticism"="big_5_neuro",
                          "Openness"="big_5_open"),
               error_pos = "right")
  
  table_1
    table_1%>%
        set_caption("Impact of Disease Disgust on Modern Racism")%>%
        set_font_size(12)%>%
        quick_docx(file="./Tables/table_modern_rac.docx")
    
    
  ###Results Figure---------------------------------------------
    #Update to use dotwhisker / Handcoded result table figure
    mod_1_temp<-
      mod_mod_rac_index_1%>%
      broom::tidy()%>%
      mutate(model="Bivariate Model")
    
    mod_2_temp<-
      mod_mod_rac_index_2%>%
      broom::tidy()%>%
      mutate(model="Socio-Demographic Controls")
    
    mod_3_temp<-
      mod_mod_rac_index_3%>%
      broom::tidy()%>%
      mutate(model="Full Model")
  
  temp_table<-rbind(mod_1_temp,mod_2_temp,mod_3_temp)
   
  
  
  #Define Brackets-------
  variable_brackets <- list(
    c("Socio-Economic Controls", "Age", "Left-Right Self Placement"),
    c("Education", "Middle", "High"),
    c("Main Activity", "Part-Time Employment", "Stay-At Home"),
    c("Big-5 Personality Traits","Extroversion","Openness")
)

  fig_1_full<-
    {temp_table%>%
      #Clean high variance variables with few observations/ Intercepts
      filter(!grepl('bundesland*', term))%>%
      filter(term != "gender_respdivers")%>%
      filter(term!= "activity_simpleOther")%>%
      filter(term!= "activity_simpleSick_or_disabled")%>%
      #Generate plot and define basic terms
      dwplot(
        by_2sd = T,
        dot_args = list(aes(shape=model,colour=model),size=4), 
        whisker_args = list(size=2),
        model_order = c("Bivariate Model","Socio-Demographic Controls","Full Model"))%>%
          relabel_predictors( c("disgust_disease"="Disease Disgust Sensitivity",
                              "age"="Age",
                              "gender_respweiblich"= "Female",
                               "lr"="Left-Right Self Placement",
                               "edu_simplemittlere"="Middle",
                              "edu_simplehohe"="High",
                              "activity_simplePart_Time"=  "Part-Time Employment",
                              "activity_simpleRetired"  = "Retired",
                              "activity_simpleEducation" = "Education",
                              "activity_simpleUnemployed"="Unemployed",
                              "activity_simpleAt_home"="Stay-At Home",
                              "activity_simpleSick_or_disabled"="Sick or Disabled",
                              "big_5_extro"="Extroversion",
                              "big_5_consc"="Conscientiousness",
                              "big_5_agree"="Agreeableness",
                              "big_5_neuro"= "Neuroticism",
                              "big_5_open" ="Openness"
                             ))+
    #
    #Define Figure to make it more readable
      theme_minimal(base_size = 18)+
      geom_vline(xintercept=0, linetype="dashed")+
      theme(legend.position = "bottom", legend.title = element_blank())+
    #Combine Guides
      guides(
        shape = guide_legend("Model"), 
        colour = guide_legend("Model"))
      }%>%
    #Add Brackets (must be at end + outside bracket)
      add_brackets(variable_brackets, fontSize=1)
   
  fig_1_full     

  
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
                 
                          
       
           
    table_social<-
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
                          "Conscientiousness "="big_5_consc",
                          "Neuroticism"="big_5_neuro",
                          "Openeness"="big_5_open"))
    
    
    
    
    #Style Table
    
    table_social%>%
          set_caption("Impact of Disease Disgust on social acceptance for different racial groups")%>%
          set_font_size(12)%>%
          quick_docx(file="./Tables/social_distance.docx")
          
    
    
    
                 to.file = "docx",
                 file.name="./Tables/social_distance.docx")%>%
                 set_caption("Test")
                 
                         

  
  
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
  