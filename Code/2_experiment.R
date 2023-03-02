#Setup------------------
  library(tidyverse)
  library(haven)
  library(officer)
  library(huxtable)
  library(jtools)
  

  ##Merge Survey Waves
  D<-
    D_1 %>%
    dplyr::select(Befragten_ID,
                  mod_rac_w1,mod_rac_index,
                  starts_with("disgust_"),starts_with("consp_"),
                  white_resp,gender_resp,
                  lr,migrant)%>%
    right_join(D_3,by="Befragten_ID")


  
#Main Models----------------------
    mod_1<-
      D%>%
      filter(white_resp=="Yes")%>%
      filter(is.na(non_comp))%>%
      filter(is.na(no_experience))%>%
      filter(condition=="Control"|condition=="Disgust-Corona")%>%
      # filter(prime_length>10)%>%
      # filter(duration>0)%>%
      lm(mod_rac ~ condition+mod_rac_w1, data=.)

    summary(mod_1)
    plot_summs(mod_1)




    
    

  