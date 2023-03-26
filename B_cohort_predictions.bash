# script generates cohort predictions & saves them in the correct format for KPRI

# doesn't work b/c file is too large?
# Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_all_selected_campaigns.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/psd_and_no2 rda

#############################################################################################
 
Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_total_conc.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_total_conc rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_10_100.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_10_100 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_11.5.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_11.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_15.4.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_15.4 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_20.5.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_20.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_27.4.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_27.4 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_36.5.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_36.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_48.7.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_48.7 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_64.9.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_64.9 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_86.6.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_86.6 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_115.5.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_115.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_154.0.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_154.0 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_205.4.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/ns_205.4 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_no2.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/no2 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_pnc_noscreen.rda data/dr0357_cohort_covar_20220404.csv Output/v3_20230321/"UK Predictions"/cohort/pnc_noscreen rda


#############################################################################################

# Rscript 6_clean_predictions.R

#############################################################################################

echo "DONE MAKING COHORT PREDICTIONS"
