
library(lme4)
library(lmerTest)

# Create a list of all DVs you want to loop over
DVs <- c("affect_balance", "lonely_r", "stressed_r", "accepted")

# Create a list of lagged DVs
DVs_lagged <- c("lagged_affective_balance.cz", "laggedLonely.cz", "laggedStress.cz", "lagged_accepted.cz")

# Create a list of all well-being traits you want to loop over
well_being_traits <- c("affect_balance_trait.z", "socialConnectedness.z", "cesd.z", "swls_sum.z", "uls_mean.z")

# Create empty lists to save model objects
models_use_list <- list()
models_dur_list <- list()

for(i in 1:length(DVs)) { # loop over DVs
  for(j in 1:length(well_being_traits)) { # loop over well-being traits
    
    # Estimate the following two models for every combination of the four DVs and the five well-being traits:
    
    # Use vs. No-Use
    model_use <- eval(parse(text = paste0(
      "lmer(", DVs[i], " ~ 1 + demog_sex_recoded + demog_age.z + surveys_per_id.z + overall_study_day + weekend + nAct.cz + lagDur.cz*", DVs_lagged[i],
      " + MediaUse.mean*", well_being_traits[j], " + MediaUse.scaled*", well_being_traits[j],
      " + (1 + MediaUse.scaled | id), na.action = na.exclude, data = main_effect_updated, REML = TRUE, control = lmerControl(optimizer = 'Nelder_Mead'))")))
    
    # Duration
    model_dur <- eval(parse(text = paste0(
      "lmer(", DVs[i], " ~ 1 + demog_sex_recoded + demog_age.z + surveys_per_id.z + overall_study_day + weekend + nAct.cz + lagDur.cz*", DVs_lagged[i],
      " + socialmedia.bp.z*", well_being_traits[j], " + socialmedia.cz*", well_being_traits[j],
      " + (1 + socialmedia.cz | id), na.action = na.exclude, data = main_effect_updated, REML = TRUE, control = lmerControl(optimizer = 'Nelder_Mead'))")))
    
    # Save model outputs in lists:
    models_use_list <- append(models_use_list, model_use) # list of 20 use vs. no-use models
    models_dur_list <- append(models_dur_list, model_dur) # list of 20 duration models
    # Of course, you can save the model outputs in different formats as you see fit
    
    print(paste(DVs[i], well_being_traits[j])) # prints where you are at
    
  }
}


models_use_list # look at 20 use vs. no-use models
models_dur_list # look at 20 duration models

