qaly <- function(mean_age, 
                 utilities_post_recovery18_f = 0, 
                 utilities_post_recovery25_f = 0, 
                 utilities_post_recovery35_f = 0, 
                 utilities_post_recovery45_f = 0, 
                 utilities_post_recovery55_f = 0, 
                 utilities_post_recovery65_f = 0, 
                 utilities_post_recovery75_f = 0,
                 life_exp_f,
                 utilities_post_recovery18_m = 0, 
                 utilities_post_recovery25_m = 0, 
                 utilities_post_recovery35_m = 0, 
                 utilities_post_recovery45_m = 0, 
                 utilities_post_recovery55_m = 0, 
                 utilities_post_recovery65_m = 0, 
                 utilities_post_recovery75_m = 0,
                 life_exp_m, 
                 perc_females, 
                 discount_rate  = 0.03){
  if(mean_age < 18){stop('Warning: function is not suitable for mean ages of 17 and under')}
  t_f <- 0:ceiling(life_exp_f)
  t_m <- 0:ceiling(life_exp_m)
  
  dis_f <- 1 / (1 + discount_rate) ^ t_f # discounted life years
  dis_m <- 1 / (1 + discount_rate) ^ t_m
  
  dis_f[1] <- 0.5 # set first life year to 0.5 (first half year spent in hospital)
  dis_m[1] <- 0.5
  
  dis_f[length(t_f)] <- ifelse(life_exp_f == floor(life_exp_f), 1,
                               life_exp_f - floor(life_exp_f)) * dis_f[length(t_f)] # correct for the last year
  dis_m[length(t_m)] <- ifelse(life_exp_m == floor(life_exp_m), 1,
                               life_exp_m - floor(life_exp_m)) * dis_m[length(t_m)] # correct for the last year
  
  # create vector of utilities
  utilities_f <- c(rep(utilities_post_recovery18_f, 7),
                   rep(utilities_post_recovery25_f, 10), 
                   rep(utilities_post_recovery35_f, 10),
                   rep(utilities_post_recovery45_f, 10),
                   rep(utilities_post_recovery55_f, 10),
                   rep(utilities_post_recovery65_f, 10),
                   rep(utilities_post_recovery75_f, 40))
  
  utilities_m <- c(rep(utilities_post_recovery18_m, 7),
                   rep(utilities_post_recovery25_m, 10), 
                   rep(utilities_post_recovery35_m, 10),
                   rep(utilities_post_recovery45_m, 10),
                   rep(utilities_post_recovery55_m, 10),
                   rep(utilities_post_recovery65_m, 10),
                   rep(utilities_post_recovery75_m, 40))   
  
  # select utilities from mean age until death
  utilities_f <- utilities_f[(mean_age - 17):(ceiling(mean_age + life_exp_f) - 17)]
  utilities_m <- utilities_m[(mean_age - 17):(ceiling(mean_age + life_exp_m) - 17)]
  
  # calculate qalies by multiplying utilities with discounted life years for males and females
  qalies_post_recovery_p_patient_f <- sum(utilities_f * dis_f)
  qalies_post_recovery_p_patient_m <- sum(utilities_m * dis_m)  
  
  #qalies per patient using the percentage of females
  qalies_post_recovery_p_patient <- qalies_post_recovery_p_patient_f * perc_females + 
    qalies_post_recovery_p_patient_m * (1 - perc_females)
  
  return(qalies_post_recovery_p_patient)
}
