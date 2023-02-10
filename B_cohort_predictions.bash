# script generates cohort predictions & saves them in the correct format for KPRI

Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_all_selected_campaigns.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/psd_and_no2 rda

Rscript 6_clean_predictions.R






#######################################################
#################### OLD ##############################

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_no2.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/no2 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_total_conc.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_total_conc csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_10_100.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_10_100 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_11.5.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_11.5 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_15.4.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_15.4 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_20.5.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_20.5 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_27.4.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_27.4 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_36.5.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_36.5 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_48.7.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_48.7 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_64.9.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_64.9 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_86.6.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_86.6 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_115.5.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_115.5 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_154.0.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_154.0 csv

#Rscript 5_prediction_program.R Output/"Selected Campaigns"/site_data_for_ns_205.4.rda ../../dr0357/update_20220404/dr0357_cohort_covar_20220404.csv Output/"UK Predictions"/cohort/ns_205.4 csv



echo "DONE RUNNING ALL SCRIPTS"
