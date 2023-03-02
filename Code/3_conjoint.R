#Setup--------------------------------
  library(tidyverse)
  library(texreg)
  library(estimatr)
  library(ggpubr)
  library(jtools)

  D<-read_rds("./DATA/conjoint.rds")
  
  #Focus on Analysis of white respondents only
  D<-
    D%>%
      filter(white_resp=="Yes")

  
#Main Model: Does disease disgust increase avoidance of out groups?
  
  
  
  
  
  ##Main Effects/Baselines---------------------
  mod_baseline<-
      lm_robust(choice_binary ~ mask+race+gender+position+trial,
      data = D,
      clusters = lfdn,
      se_type = "stata")
    
   screenreg(mod_baseline)
  

  
##Interaction - Race * Disgust Sensitivity--------------------------------------
 
  
  ###disgust_disease------------------
    mod_disgust_disease<-
      lm_robust(choice_binary ~ mask+race*disgust_disease+gender+position+trial,
      data = D,
      clusters = lfdn,
      se_type = "stata")
   screenreg(mod_disgust_disease)
  
   plot_summs(mod_disgust_disease, 
                omit.coefs = c("maskmask","(Intercept)","genderwoman","positionright","trial"))
  

   
   ###Visualize Results: Predicted Prob Simple---------------------------------------------
   scenarios<-expand.grid(
     levels(D$race),
     seq(from=0, to=10, by=0.1))
   
   
    names(scenarios)<-c("race","disgust_disease")
    
    scenarios$position<-"left"
    scenarios$mask<-"nomask"
    scenarios$gender<-"man"
    scenarios$trial<-5
  
    
  #Generate Outcomes
      scenarios$results<-predict(mod_disgust_disease, newdata = scenarios, se.fit=T, type = "response"  )$fit
      scenarios$se<-predict(mod_disgust_disease, newdata = scenarios, se.fit=T,type = "response")$se.fit
  
  #Generate Plot
      scenarios %>%
          ggplot()+
          aes(x=disgust_disease, y=results, ymin=results-1.96*se, ymax=results+1.96*se)+
          geom_line()+
          geom_errorbar(width= 0.2, alpha=0.1)+
          facet_grid(.~race)+
          theme_bw()
   
   

  
  
  
#Appendix and Expanded Models--------------------------
  ##Alternative Measurements of Disgust ----------------------
      
      
      
   ###disgust_all------------------
    mod_disgust_all<-
      lm_robust(choice_binary ~ mask+race*disgust_all+gender+position+trial,
      data = D,
      clusters = lfdn,
      se_type = "stata")
  
  screenreg(mod_disgust_all)
  
  plot_summs(mod_disgust_all, 
                omit.coefs = c("maskmask","(Intercept)","genderwoman","positionright","trial"))
    
  ###disgust_general------------------
    mod_disgust_general<-
      lm_robust(choice_binary ~ mask+race*disgust_general+gender+position+trial,
      data = D,
      clusters = lfdn,
      se_type = "stata")
  
  screenreg(mod_disgust_general)
  
    plot_summs(mod_disgust_general, 
                omit.coefs = c("maskmask","(Intercept)","genderwoman","positionright","trial"))
    
    
  ###disgust_animal------------------
    mod_disgust_animal<-
      lm_robust(choice_binary ~ mask+race*disgust_animal+gender+position+trial,
      data = D,
      clusters = lfdn,
      se_type = "stata")
   screenreg(mod_disgust_animal)
  
       plot_summs(mod_disgust_animal, 
                omit.coefs = c("maskmask","(Intercept)","genderwoman","positionright","trial"))
       
       
       
##Interactions of choice characteristics ---------------------------------------
    ###Gender-------------------------------------
    mod_disgust_disease_gender<-
      lm_robust(choice_binary ~ mask+race*gender*disgust_disease+position+trial,
      data = D,
      clusters = lfdn,
      se_type = "stata")
       
       
       
    screenreg(mod_disgust_disease_gender)
       
    plot_summs(mod_disgust_disease_gender, 
                omit.coefs = c("maskmask","(Intercept)","positionright","trial"))
       
    ###Mask-------------------------------------
    mod_disgust_disease_mask<-
      lm_robust(choice_binary ~ mask*race*disgust_disease+gender+position+trial,
      data = D,
      clusters = lfdn,
      se_type = "stata")
       
       
       
    screenreg(mod_disgust_disease_mask)
       
    plot_summs(mod_disgust_disease_mask, 
                omit.coefs = c("(Intercept)","positionright","trial"))   
       
       
       
       
    