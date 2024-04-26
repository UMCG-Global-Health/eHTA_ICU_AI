library(dplyr)
library(dampack)
source('./funs/qaly.R')

#min, basecase and max for utilities hospitals (adjust utilities for other age groups if needed)
utilities_post_recovery55_f <- 0.917 #utility female 55-64 basecase
utilities_post_recovery55_m <- 0.927 #utility male 55-64 basecase
#max 
perc_females <- 0.3749 #percentage of females

disutility_mech <- 0.71 
disutility_non_mech <- 0.62
disutility_gen_ward <- 0.44
  
utility_mech <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_mech
utility_non_mech <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_non_mech
utility_gen_ward <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_gen_ward

#base case disutilities

disutility_mech <- 0.79
disutility_non_mech <- 0.69
disutility_gen_ward <- 0.49

#base case hospital substates utilities
utility_mech <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_mech
utility_non_mech <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_non_mech
utility_gen_ward <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_gen_ward

#minimum disutilities

disutility_mech <- 0.87
disutility_non_mech <- 0.76
disutility_gen_ward <- 0.54

#minimum hospital substates utilities
utility_mech <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_mech
utility_non_mech <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_non_mech
utility_gen_ward <- (perc_females*utilities_post_recovery55_f + (1-perc_females)* utilities_post_recovery55_m) - disutility_gen_ward


#calculate qalies base case for post recovery state

qalies_post_recovery_base <- qaly(mean_age = 63,
                             perc_females = 0.3749,
                             utilities_post_recovery55_f = 0.917, #utility female age 55-64
                             utilities_post_recovery65_f = 0.874, #utility female age 65-74
                             utilities_post_recovery75_f = 0.82, #utility female age 75+
                             utilities_post_recovery55_m = 0.927,#utility male age 55-64
                             utilities_post_recovery65_m = 0.915,#utility male age 65-74
                             utilities_post_recovery75_m = 0.88,#utility male age 75+  
                             life_exp_f = 23.09, 
                             life_exp_m = 19.83)

#calculate qalies minimum for post recovery state

qalies_post_recovery_min <- qaly(mean_age = 63,
                                  perc_females = 0.3749,
                                  utilities_post_recovery55_f = 0.90, #utility female age 55-64
                                  utilities_post_recovery65_f = 0.85, #utility female age 65-74
                                  utilities_post_recovery75_f = 0.79, #utility female age 75+
                                  utilities_post_recovery55_m = 0.91,#utility male age 55-64
                                  utilities_post_recovery65_m = 0.89,#utility male age 65-74
                                  utilities_post_recovery75_m = 0.85,#utility male age 75+  
                                  life_exp_f = 23.09, 
                                  life_exp_m = 19.83)

#calculate qalies maximum post recovery state

qalies_post_recovery_max <- qaly(mean_age = 63,
                                  perc_females = 0.3749,
                                  utilities_post_recovery55_f = 0.94, #utility female age 55-64
                                  utilities_post_recovery65_f = 0.90, #utility female age 65-74
                                  utilities_post_recovery75_f = 0.85, #utility female age 75+
                                  utilities_post_recovery55_m = 0.94,#utility male age 55-64
                                  utilities_post_recovery65_m = 0.94,#utility male age 65-74
                                  utilities_post_recovery75_m = 0.91,#utility male age 75+  
                                  life_exp_f = 23.09, 
                                  life_exp_m = 19.83)

set.seed(123)

#basecase parameters
params_basecase = list(mean_los_non_mech = 5.31, # mean los non mechanically ventilated ICU
mean_los_mech = 11.13, # mean los mechancially ventilaed ICU
mean_los_gen_ward = 4.15, # mean los general ward
in_hos_mort = 0.3336, # in hospital mortality
six_month_mort = 0.0621, # mortality during recovery state
time_to_mort = 15.63, # time to mortality during recovery state
prob_disabled = 0.2481, # probability of being disabled after discharge
los_mech_disabled = 6.06, # mean duration of mechanical ventilation for patient who were disabled during recovery state
los_mech_not_disabled = 4.18, # mean duration of mechanical ventilation for patients who were not to mildly disabled during recovery state
or_mech_vent_disabled = 1.04, #OR of being disabled after discharge for each hour increase in mechanical ventilation
utility_mech = 0.13, #utility for mechanically ventilated sub state
utility_non_mech = 0.23,#utility for not mechanically ventilated sub state
utility_gen_ward = 0.43,# utility for general ward sub state
utility_recovery_disabled = 0.5, # utility for recovery state in the disabled substate 
utility_recovery_not_disabled = 0.77, #utility for recovery state in the not disabled substate
qalies_post_recovery = round(qalies_post_recovery_base,1), # qalies post recovery
costs_gen_ward = 442.78, # costs per day in the general ward
costs_mech_vent = 2378.35 , # costs per mechanically ventilated day
costs_non_mech_vent =  991.44, # costs per not mechanically ventilated ICU day
costs_recovery_not_disabled = 1521.21 , #costs (per patient) for the substate not to mildly disabled in the recovery state
costs_recovery_disabled = 3*1521.21, # costs (per patient) for the substate disabled in the recovery state
intervention_effect_los= 4/24, # intervention effect on the duration of mechanical ventilation
intervention_effect_mort= 0.01) #intervention effect on the mortality

#function to simulate different groups
simulate_strategies <- function(l_params, 
                                wtp = 30000, 
                                ICU_occupancy = 0.044, 
                                costs_trt = 128, 
                                costs_trt_icu_oc = TRUE, 
                                n_patients = 1000, 
                                mean_age = 63){
  
  
  with(as.list(l_params), {
    # Strategy names
    v_names_strat <- c("control", "treatment")
    # Number of strategies
    n_strat <- length(v_names_strat)
    intervention <- FALSE

    # if costst_trt_icu_oc is true daily costs are calculated using mechanical ventilated occupancy
    if (costs_trt_icu_oc){
      costs_trt <- ((2050/ICU_occupancy))/365
    }
    
    # Create cost-effectiveness results data frame
    df_ce <- data.frame(Strategy = 'Increment',
                        Cost = numeric(1),
                        QALY = numeric(1),
                        ICER= numeric(1),
                        stringsAsFactors = FALSE)
    strategies_tot <- data.frame(costs_tot = as.numeric(n_strat), 
                                 qalies_tot = as.numeric(n_strat))
    
    #create a vector with time to death in recovery state
    time_to_death_disabled_recovery <-  rgamma(n_patients, shape = time_to_mort^2/4.44^2, scale = 4.44^2/time_to_mort )
    
    time_to_death_not_disabled_recovery <-   rgamma(n_patients, shape = time_to_mort^2/4.44^2, scale = 4.44^2/time_to_mort )
    
    #simulate different strategies
    for (i in 1:n_strat){

    strategy <- v_names_strat[i]
      
    #apply intervention effects for treatment group
     if (v_names_strat[i] == "treatment"){
        mean_los_mech <-  mean_los_mech - ifelse(intervention_effect_los<= mean_los_mech,intervention_effect_los, mean_los_mech) 
        in_hos_mort <- in_hos_mort - (ifelse(intervention_effect_mort <= in_hos_mort, intervention_effect_mort, in_hos_mort))
        intervention <- TRUE
      }
      
     #if death in recovery state is after recovery state set it at the end of the recovery state
      time_to_death_disabled_recovery <- ifelse(time_to_death_disabled_recovery > 180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, 
                                          180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, time_to_death_disabled_recovery)
      
      time_to_death_not_disabled_recovery <- ifelse(time_to_death_not_disabled_recovery> 180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, 
                                      180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, time_to_death_not_disabled_recovery)
      
      #calculate number of patients surviving the hospital state
      surv_hos <-n_patients - n_patients*in_hos_mort 
      #calculate qalies in hosital state
      qalies_hos <- n_patients*(mean_los_gen_ward*utility_gen_ward + mean_los_mech*utility_mech + mean_los_non_mech*utility_non_mech)/365
      
      
      #calculate the weighted mean mechanical ventilation duration 
      los_mech_disabled_not_disabled <- prob_disabled * los_mech_disabled + (1 - prob_disabled) * los_mech_not_disabled
      
      #calculate the probability of being disabled for different mean_los_mech and multiply by the number of patients surviving the hospital state.
      n_disabled <- (or_mech_vent_disabled^(mean_los_mech - los_mech_disabled_not_disabled) * prob_disabled) /
        (1 + or_mech_vent_disabled^(mean_los_mech - los_mech_disabled_not_disabled) * prob_disabled - prob_disabled) * surv_hos
      
      #number of not to mildly disabled patients
      n_not_disabled <- surv_hos - n_disabled
      
      #calculated number of deaths in each substates of the recovery state
      n_deaths_recovery_disabled <- six_month_mort * n_disabled
      n_deaths_recovery_not_disabled <- six_month_mort * n_not_disabled
      
      #select the time of death for the patients who die in the recovery state
      time_to_death_disabled_recovery_selection <- time_to_death_disabled_recovery[1:ceiling(n_deaths_recovery_disabled)]
      time_to_death_not_disabled_recovery_selection <- time_to_death_not_disabled_recovery[1:ceiling(n_deaths_recovery_not_disabled)]
      
      #calculate the qalies for the disabled and not disabled patients in the recovery state
      qalies_recovery_disabled <- (n_disabled - n_deaths_recovery_disabled) * utility_recovery_disabled * (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech) / 365 +
        sum(time_to_death_disabled_recovery_selection[1:floor(n_deaths_recovery_disabled)] * utility_recovery_disabled / 365) + 
        time_to_death_disabled_recovery_selection[ceiling(n_deaths_recovery_disabled)] * utility_recovery_disabled / 365 * (n_deaths_recovery_disabled - floor(n_deaths_recovery_disabled))
      
      qalies_recovery_not_disabled <- (n_not_disabled - n_deaths_recovery_not_disabled) * utility_recovery_not_disabled * (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech) / 365  +
        sum(time_to_death_not_disabled_recovery_selection[1:floor(n_deaths_recovery_not_disabled)] * utility_recovery_not_disabled / 365) +
        time_to_death_not_disabled_recovery_selection[ceiling(n_deaths_recovery_not_disabled)] * utility_recovery_not_disabled / 365 * (n_deaths_recovery_not_disabled - floor(n_deaths_recovery_not_disabled))
      
      #calculate total costs recovery states per substate
      costs_recovery_disabled_total <- (n_disabled - n_deaths_recovery_disabled) * costs_recovery_disabled + 
        sum(time_to_death_disabled_recovery_selection[1:floor(n_deaths_recovery_disabled)] * costs_recovery_disabled / (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech)) +
        time_to_death_disabled_recovery_selection[ceiling(n_deaths_recovery_disabled)] * costs_recovery_disabled / (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech) * (n_deaths_recovery_disabled - floor(n_deaths_recovery_disabled)) +
        ifelse(intervention, intervention_effect_los * n_disabled * costs_recovery_disabled / (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech), 0)
      
      costs_recovery_not_disabled_total <- (n_not_disabled - n_deaths_recovery_not_disabled) * costs_recovery_not_disabled + 
        sum(time_to_death_not_disabled_recovery_selection[1:floor(n_deaths_recovery_not_disabled)] * costs_recovery_not_disabled / (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech)) +
        time_to_death_not_disabled_recovery_selection[ceiling(n_deaths_recovery_not_disabled)] * costs_recovery_not_disabled / (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech) * (n_deaths_recovery_not_disabled - floor(n_deaths_recovery_not_disabled)) +
        ifelse(intervention, intervention_effect_los * n_not_disabled * costs_recovery_not_disabled / (180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech), 0)
      
      #calculate hospitalization costs
      costs_hos <- mean_los_mech * costs_mech_vent + 
        mean_los_non_mech * costs_non_mech_vent + 
        mean_los_gen_ward * costs_gen_ward
      
      #total costs
      costs_tot <- (costs_recovery_disabled_total + costs_recovery_not_disabled_total + 
                      costs_hos * n_patients  + 
                      ifelse(intervention, (costs_trt) * (mean_los_mech) * n_patients, 0)) / n_patients
      
      #total qalies
      
      qalies_tot <- (qalies_recovery_not_disabled + qalies_recovery_disabled + qalies_hos  + (qalies_post_recovery* 
                                                                                                (surv_hos - n_deaths_recovery_not_disabled - n_deaths_recovery_disabled)))/n_patients
      
      # making data frame
      
      strategies_tot[i, c("costs_tot", "qalies_tot")] <- c(costs_tot,
                                                           qalies_tot)
      
      
      if(i==2){
        df_ce[c("Cost", "QALY")] <- c(strategies_tot$costs_tot[2] - strategies_tot$costs_tot[1],
                                      strategies_tot$qalies_tot[2] - strategies_tot$qalies_tot[1])
        df_ce["NMB"] <- (strategies_tot$qalies_tot[2] - strategies_tot$qalies_tot[1])* wtp - 
          (strategies_tot$costs_tot[2] - strategies_tot$costs_tot[1])
        df_ce['ICER'] <- (strategies_tot$costs_tot[2] - strategies_tot$costs_tot[1])/(strategies_tot$qalies_tot[2] - strategies_tot$qalies_tot[1])
      }
      
    }
    return(df_ce)
  })
}


#perform owsa
x <- simulate_strategies(params_basecase)

pars <- c('mean_los_non_mech', 
          'mean_los_mech',
          'mean_los_gen_ward', 
          'in_hos_mort', 
          'six_month_mort', 
          'time_to_mort',
          'prob_disabled', 
          'los_mech_disabled', 
          'los_mech_not_disabled',
          'or_mech_vent_disabled', 
          'utility_non_mech',
          'utility_mech',
          'utility_gen_ward',
          'utility_recovery_disabled',
          'utility_recovery_not_disabled',
          'qalies_post_recovery',
          'costs_gen_ward', 
          'costs_mech_vent',
          'costs_non_mech_vent',
          'costs_recovery_not_disabled', 
          'costs_recovery_disabled', 
          'intervention_effect_los',
          'intervention_effect_mort')


min <- c(
  4.63, 
  9.64, 
  3.38, 
  0.3002, 
  0.0565, 
  14.07 ,
  0.1979, 
  5.45, 
  3.76,
  1.01, 
  0.14, 
  0.03,
  0.36, 
  0.44, 
  0.73, 
  round(qalies_post_recovery_min,1), 
  396.21, 
  2230.83  , 
  895.84 , 
  1369.08,
  3*1369.08,
  0.15, 
  0.009
)


max <- c(5.98, 
         12.62, 
         4.92, 
         0.3670, 
         0.0683, 
         17.19,
         0.3058, 
         6.67, 
         4.60,
         1.08, 
         0.28, 
         0.19, 
         0.46, 
         0.56, 
         0.81,
         round(qalies_post_recovery_max, 1), 
         489.35, 
         2525.88,
         1087.04, 
         1673.33, 
         3*1673.33, 
         (4/24 + 4/24/10),
         0.011) 

params_range <- data.frame(pars, min, max)

#running owsa
owsa_det <- run_owsa_det(params_range, params_basecase, nsamp =1000, FUN= simulate_strategies, outcomes = c( "ICER"), progress = TRUE)


owsa_det$outcome_val <- as.numeric(owsa_det$outcome_val)

#rename variables for plot
owsa_det <- owsa_det %>%  
  mutate(parameter= case_when(parameter == 'costs_gen_ward' ~ 'Daily costs general ward', 
                              parameter == 'costs_recovery_disabled' ~ 'Costs during recovery stage disabled patients',
                              parameter == 'costs_recovery_not_disabled' ~ 'Costs during recovery stage non-disabled patients', 
                              parameter == 'costs_mech_vent' ~ 'Daily costs of mechanical ventilation', 
                              parameter == 'costs_non_mech_vent' ~ 'Daily costs of ICU not mechanically ventilated', 
                              parameter == 'in_hos_mort' ~ 'In-hospital mortality', 
                              parameter == 'time_to_mort' ~ 'Time to mortality in recovery stage',
                              parameter == 'intervention_effect_los' ~ 'Intervention effect on duration of mechanical ventilation', 
                              parameter == 'intervention_effect_mort' ~'Intervention effect in-hospital mortality',
                              parameter == 'los_mech_disabled' ~ 'Duration of mechanical ventilation for post ICU disabled patients',
                              parameter == 'los_mech_not_disabled' ~'Duration of mechanical ventilation for post-ICU non-disabled patients', 
                              parameter == 'mean_los_gen_ward' ~ 'Mean length of stay general ward', 
                              parameter == 'mean_los_mech' ~ 'Mean duration of mechanical ventilation', 
                              parameter == 'mean_los_non_mech' ~ 'Mean duration ICU not mechanically ventilated', 
                              parameter == 'or_mech_vent_disabled' ~ 'Odds ratio of being disabled, with each day mechanical ventilation',
                              parameter == 'prob_disabled' ~ ' Probability of being disabled post-ICU', 
                              parameter == 'qalies_post_recovery' ~ 'Utilities during post-recovery stage', 
                              parameter == 'six_month_mort' ~ 'Mortality during recovery stage', 
                              parameter == 'utility_gen_ward' ~ 'Utility general ward', 
                              parameter == 'utility_recovery_disabled' ~ 'Utility during recovery stage disabled',
                              parameter == 'utility_recovery_not_disabled' ~ 'Utility during recovery stage non-disabled', 
                              parameter == 'utility_mech' ~ 'Utility mechanically ventilated patients', 
                              parameter == 'utility_non_mech' ~ 'Utility ICU not mechanically ventilated'))


#final plot
owsa_tornado(owsa_det, min_rel_diff = 0.01) + 
  ylab('Incremental cost-effectiveness ratio')+
  theme_light(base_size = 12)

