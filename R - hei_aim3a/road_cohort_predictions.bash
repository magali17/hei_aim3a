# to run this script in the terminal, enter: bash road_cohort_predictions.bash
# 6/6/24
########################################################
# PREDICT AT COHORT LOCATIONS
########################################################
## used 10-20 smp for each
#Rscript r3_road_predict.R route.rda 20240605 
#Rscript r3_road_predict.R random.rda 20240605
#Rscript r3_road_predict.R road_type.rda 20240605
#Rscript r3_road_predict.R sensible.rda 20240605
#Rscript r3_road_predict.R unsensible.rda 20240605

############ 
# non-clustered are last priority? model CV would be diff though

#Rscript r3_road_predict.R balanced.rda 20240605

## note: these models have _random_NA_ in the name
#Rscript r3_road_predict.R unbalanced.rda 20240605


########################################################
# MODEL VALIDATION
########################################################
#Rscript r2_road_model_eval.R balanced.rda


# TO DO
Rscript r2_road_model_eval.R route.rda
Rscript r2_road_model_eval.R random.rda
Rscript r2_road_model_eval.R road_type.rda

Rscript r2_road_model_eval.R sensible.rda
Rscript r2_road_model_eval.R unsensible.rda
Rscript r2_road_model_eval.R unbalanced.rda


echo "done running scripts"
