#Setup----------------------------
  library(tidyverse)
  library(haven)
  library(texreg)
  

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
    
  
  
    screenreg(list(mod_soc_dist_asian_index,mod_soc_dist_black_index,mod_soc_dist_muslim_index,mod_soc_dist_white_index))

  
  
  
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
  
  
  
  