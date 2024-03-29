# script generates cohort predictions & saves them in the correct format for KPRI
## bash B_cohort_predictions.bash

#############################################################################################
# NanoScan, including bins
Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_total_conc.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_total_conc rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_10_100.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_10_100 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_11.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_11.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_15.4.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_15.4 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_20.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_20.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_27.4.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_27.4 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_36.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_36.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_48.7.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_48.7 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_64.9.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_64.9 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_86.6.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_86.6 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_115.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_115.5 rda

Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_ns_154.0.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/ns_154.0 rda

#############################################################################################
# NO2
Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_no2.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/no2 rda
#############################################################################################
# ptrak
Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/site_data_for_pnc_noscreen.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/pnc_noscreen rda

#############################################################################################
# ONROAD - can I run this all at once?

# Rscript 5_prediction_program.R Output/v3_20230321/"Selected Campaigns"/onroad_modeling_data.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda Output/v3_20230321/"UK Predictions"/cohort/onroad_modeling_data rda



#############################################################################################

# Rscript 6_clean_predictions.R

#############################################################################################

echo "DONE MAKING COHORT PREDICTIONS"
