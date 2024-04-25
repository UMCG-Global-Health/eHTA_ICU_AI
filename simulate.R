simulate_strategies <- function(l_params, # list of parameters to be varied in sensitivity analysis
                                wtp = 30000, # willingness to pay threshold
                                ICU_occupancy = 0.044, # percentage of mech ventilated ICU beds occupied
                                costs_trt = 128, # costs per patient per mechanically ventilated ICU day
                                costs_trt_icu_oc = TRUE, # If true costs are calculated using ICU_occupancy
                                intervention_effect_los_scenario = 0, # intervention effect on duration of mechanical ventilation in scenario analysis
                                intervention_effect_mort_scenario = 0, # intervention effect on mortality in scenario analysis
                                explore_intervention = FALSE, # True if you want scenario analyses on intervention effects
                                ceac = FALSE, # output should be suitable for a cost effectiveness acceptibility curve
                                n_patients = 1000, # number of patients
                                mean_age = 63, #mean age of patient group 
                                perc_females = 0.3749, # percentage of females in the sample
                                sd_time_to_mort = 4.44, #standard deviation of mortalities in recovery state
                                life_exp_m = 19.83, #life expectancy males
                                life_exp_f = 23.09, # life expectancy females
                                #fixed values for utilities and costs, to keep same ratios in sensitivity analyses
                                utilities_post_recovery18_f_fixed = 0.974, #utility female 18-24
                                utilities_post_recovery25_f_fixed = 0.972, #utility female 25-34
                                utilities_post_recovery35_f_fixed = 0.962, #utility female 35-44
                                utilities_post_recovery45_f_fixed = 0.950, #utility female 45-54
                                utilities_post_recovery55_f_fixed = 0.917, #utility female age 55-64
                                utilities_post_recovery65_f_fixed = 0.874, #utility female age 65-74
                                utilities_post_recovery75_f_fixed = 0.82, #utility female age 75+
                                utilities_post_recovery18_m_fixed = 0.971, #utility male 18-24
                                utilities_post_recovery25_m_fixed = 0.973, #utility male 25-34
                                utilities_post_recovery35_m_fixed = 0.97, #utility male 35-44
                                utilities_post_recovery45_m_fixed = 0.940, #utility male 45-54
                                utilities_post_recovery55_m_fixed = 0.927,#utility male age 55-64
                                utilities_post_recovery65_m_fixed = 0.915,#utility male age 65-74
                                utilities_post_recovery75_m_fixed = 0.88,#utility male age 75+  
                                costs_mech_vent_fixed = 2378.35, #costs per mechanical ventilation day 
                                costs_non_mech_vent_fixed = 991.44, #cost per non mechanical ventilated ICU day
                                costs_gen_ward_fixed = 442.78, 
                                discount_rate = 0.03) {  # cost per day in the general ward
  with(as.list(l_params), {
    
    # Strategy names
    v_names_strat <- c("control", "treatment")
    # Number of strategies
    n_strat <- length(v_names_strat)
    intervention <- FALSE
    
    #calculate costs per mechanical ventilated bed day based on occupancy (only if costs_trt_icu_oc == TRUE)
    if (costs_trt_icu_oc) {
      costs_trt <- ((2050 / ICU_occupancy)) / 365
    }
    
    # Create cost-effectiveness results data frame
    df_ce <- data.frame(Strategy = 'Increment',
                        Cost = numeric(1),
                        QALY = numeric(1),
                        stringsAsFactors = FALSE)
    df_ceac <- data.frame(Strategy = c('control', 'treatment'),
                          Cost = numeric(2),
                          QALY = numeric(2),
                          stringsAsFactors = FALSE)
    strategies_tot <- data.frame(costs_tot = as.numeric(n_strat), 
                                 qalies_tot = as.numeric(n_strat))
    
    #ensure reproducibility by setting a seed
    set.seed(123)
    
    #Draw from a gamma distribution to obtain time to death in recovery state (seperately for non/mildly disabled and disabled)
    time_to_death_disabled_recovery <- rgamma(n_patients, shape = time_to_mort^2 / sd_time_to_mort^2, scale = sd_time_to_mort^2 / time_to_mort)
    time_to_death_not_disabled_recovery <- rgamma(n_patients, shape = time_to_mort^2 / sd_time_to_mort^2, scale = sd_time_to_mort^2 / time_to_mort)
    
    #simulated control group and treatment group
    for (i in 1:n_strat) {
      
      # Keeping same ratios among grouped variables 
      # keep similar ratios los_mech_disabled in sensitivity analysis 
      los_mech_disabled <- los_mech_not_disabled * 6.06 / 4.18
      
      #keep similar ratios utilities in sensitivity analysis
      utilities_post_recovery18_f <- utilities_post_recovery65_m * utilities_post_recovery18_f_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery18_m <- utilities_post_recovery65_m * utilities_post_recovery18_m_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery25_f <- utilities_post_recovery65_m * utilities_post_recovery25_f_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery25_m <- utilities_post_recovery65_m * utilities_post_recovery25_m_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery35_f <- utilities_post_recovery65_m * utilities_post_recovery35_f_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery35_m <- utilities_post_recovery65_m * utilities_post_recovery35_m_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery45_f <- utilities_post_recovery65_m * utilities_post_recovery45_f_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery45_m <- utilities_post_recovery65_m * utilities_post_recovery45_m_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery55_f <- utilities_post_recovery65_m * utilities_post_recovery55_f_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery55_m <- utilities_post_recovery65_m * utilities_post_recovery55_m_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery65_f <- utilities_post_recovery65_m * utilities_post_recovery65_f_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery75_m <- utilities_post_recovery65_m * utilities_post_recovery75_m_fixed / utilities_post_recovery65_m_fixed
      utilities_post_recovery75_f <- utilities_post_recovery65_m * utilities_post_recovery75_f_fixed / utilities_post_recovery65_m_fixed
      
      #keep similar ratios costs in sensitivity analysis
      costs_non_mech_vent <- costs_mech_vent * costs_non_mech_vent_fixed / costs_mech_vent_fixed
      costs_gen_ward <- costs_mech_vent * costs_gen_ward_fixed / costs_mech_vent_fixed
      
      #keep similar ratios disutility in sensitivity analysis (update ratio for other disutilities)
      disutility_non_mech <- disutility_mech * 0.69 / 0.79
      disutility_gen_ward <- disutility_mech * 0.49 / 0.79
      
      #keep similar ratios utility disabled/not disabled in recovery state for sensitivity analysis (update ratios for other utilities)
      utility_recovery_disabled <- utility_recovery_not_disabled * 0.5 / 0.77 
      
      #set costs for disabled in recovery state 3 times as for not disabled
      costs_recovery_disabled <- costs_recovery_not_disabled * 3
      
      #healthy utilities for hospital state,disutilities will be subtracted from these
      utility_hos_f <- case_when(mean_age >=18 & mean_age < 25 ~ utilities_post_recovery18_f, 
                                 mean_age >= 25 & mean_age < 35 ~ utilities_post_recovery25_f, 
                                 mean_age >=35 & mean_age < 45 ~ utilities_post_recovery35_f, 
                                 mean_age >= 45 & mean_age < 55 ~ utilities_post_recovery45_f, 
                                 mean_age >=55 & mean_age < 65 ~ utilities_post_recovery55_f, 
                                 mean_age >= 65 & mean_age < 75 ~ utilities_post_recovery65_f, 
                                 mean_age >= 75 ~ utilities_post_recovery75_f )
      utility_hos_m <- case_when(mean_age >=18 & mean_age < 25 ~ utilities_post_recovery18_m, 
                                 mean_age >= 25 & mean_age < 35 ~ utilities_post_recovery25_m, 
                                 mean_age >=35 & mean_age < 45 ~ utilities_post_recovery35_m, 
                                 mean_age >= 45 & mean_age < 55 ~ utilities_post_recovery45_m,
                                 mean_age >=55 & mean_age < 65 ~ utilities_post_recovery55_m, 
                                 mean_age >= 65 & mean_age < 75 ~ utilities_post_recovery65_m, 
                                 mean_age >= 75 ~ utilities_post_recovery75_m )
      
      #calculate utilities for different hospital substates
      utility_mech <- (perc_females * utility_hos_f + (1 - perc_females) * utility_hos_m) - disutility_mech
      utility_non_mech <- (perc_females * utility_hos_f + (1 - perc_females) * utility_hos_m) - disutility_non_mech
      utility_gen_ward <- (perc_females * utility_hos_f + (1 - perc_females) * utility_hos_m) - disutility_gen_ward
      
      #set strategy we are simulating
      strategy <- v_names_strat[i]
      
      #if statement for scenario analysis intervention effects
      if (explore_intervention == TRUE) {
        intervention_effect_los <- intervention_effect_los_scenario
        intervention_effect_mort <- intervention_effect_mort_scenario
      }
      
      #set treatment effects. In case of other treatments effect next to mechanical ventilation los and mortality add them here.
      if (v_names_strat[i] == "treatment") {
        mean_los_mech <-  mean_los_mech - ifelse(intervention_effect_los <= mean_los_mech, intervention_effect_los, mean_los_mech) 
        in_hos_mort <- in_hos_mort - (ifelse(intervention_effect_mort <= in_hos_mort, intervention_effect_mort, in_hos_mort))
        intervention <- TRUE
      }
      
      #if time to death in recovery state is later than 180 days after hospital admission set it to 180 days after hospital admission
      time_to_death_disabled_recovery <- ifelse(time_to_death_disabled_recovery > 180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, 
                                          180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, time_to_death_disabled_recovery)
      
      time_to_death_not_disabled_recovery <- ifelse(time_to_death_not_disabled_recovery > 180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, 
                                      180 - mean_los_gen_ward - mean_los_mech - mean_los_non_mech, time_to_death_not_disabled_recovery)
      
      #calculate number of patients surviving the hospital state
      surv_hos <- n_patients - n_patients * in_hos_mort 
      
      #calculate the qalies in the hospital state using substats los and utilities
      qalies_hos <- n_patients * (mean_los_gen_ward * utility_gen_ward + mean_los_mech * utility_mech + mean_los_non_mech * utility_non_mech) / 365
      
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
      
      #calculate the qalies in post recovery state
      qalies_post_recovery_p_patient <- qaly(mean_age, 
                                   utilities_post_recovery18_f, 
                                   utilities_post_recovery25_f, 
                                   utilities_post_recovery35_f, 
                                   utilities_post_recovery45_f, 
                                   utilities_post_recovery55_f, 
                                   utilities_post_recovery65_f, 
                                   utilities_post_recovery75_f, 
                                   life_exp_f,
                                   utilities_post_recovery18_m, 
                                   utilities_post_recovery25_m, 
                                   utilities_post_recovery35_m, 
                                   utilities_post_recovery45_m,
                                   utilities_post_recovery55_m, 
                                   utilities_post_recovery65_m, 
                                   utilities_post_recovery75_m, 
                                   life_exp_m, 
                                   perc_females, 
                                   discount_rate) 
      
      qalies_post_recovery = qalies_post_recovery_p_patient * (surv_hos - n_deaths_recovery_not_disabled - n_deaths_recovery_disabled)
      
      #calculate hospitalization costs
      costs_hos <- mean_los_mech * costs_mech_vent + 
        mean_los_non_mech * costs_non_mech_vent + 
        mean_los_gen_ward * costs_gen_ward
      
      #total costs
      costs_tot <- (costs_recovery_disabled_total + costs_recovery_not_disabled_total + 
                      costs_hos * n_patients  + 
                      ifelse(intervention, (costs_trt) * (mean_los_mech) * n_patients, 0)) / n_patients
      
      #total qalies
      qalies_tot <- (qalies_recovery_not_disabled + qalies_recovery_disabled + qalies_hos  + qalies_post_recovery) / n_patients
      
      strategies_tot[i, c("costs_tot", "qalies_tot")] <- c(costs_tot,
                                                           qalies_tot)
      
      df_ceac[i, c("Cost", "QALY")] <- c(costs_tot,
                                         qalies_tot)
      
      if (i == 2) {
        df_ce[c("Cost", "QALY")] <- c(strategies_tot$costs_tot[2] - strategies_tot$costs_tot[1],
                                      strategies_tot$qalies_tot[2] - strategies_tot$qalies_tot[1])
        df_ce["NMB"] <- (strategies_tot$qalies_tot[2] - strategies_tot$qalies_tot[1]) * wtp - 
          (strategies_tot$costs_tot[2] - strategies_tot$costs_tot[1])
      }
      
      if (ceac == TRUE & i == 2) {
        df_ce <- df_ceac
      }
    }
    
    return(df_ce)
  })
}
