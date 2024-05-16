# to run this script in the terminal, enter: other_designs_cohort_predictions.bash

## run rscripts
Rscript other_designs_2predict.R other_stop_designs_data_fewhrs.rda cohort/other\ designs/fewhrs
Rscript other_designs_2predict.R other_stop_designs_data_balsea_1.rda cohort/other\ designs/balsea_1
Rscript other_designs_2predict.R other_stop_designs_data_balsea_2.rda cohort/other\ designs/balsea_2
Rscript other_designs_2predict.R other_stop_designs_data_balsea_3.rda cohort/other\ designs/balsea_3
Rscript other_designs_2predict.R other_stop_designs_data_balsea_4.rda cohort/other\ designs/balsea_4
Rscript other_designs_2predict.R other_stop_designs_data_sitetype_no2.rda cohort/other\ designs/sitetype_no2
Rscript other_designs_2predict.R other_stop_designs_data_sitetype_ns_total_conc.rda cohort/other\ designs/sitetype_ns_total_conc
Rscript other_designs_2predict.R other_stop_designs_data_sitetype_pnc_noscreen.rda cohort/other\ designs/sitetype_pnc_noscreen
Rscript other_designs_2predict.R other_stop_designs_data_sitetype_ns_10_100.rda cohort/other\ designs/sitetype_ns_10_100


## 5/16/24 - temporally adjusted designs
Rscript other_designs_2predict.R other_stop_designs_data_temp_adj.rda cohort/other\ designs/temp_adj

echo "done running scripts"
