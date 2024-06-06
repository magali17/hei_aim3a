# to run this script in the terminal, enter: bash road_cohort_predictions.bash

# run rscripts

## compute node 1
Rscript r3_road_predict.R route.rda 20240605


## compute node 8
Rscript r3_road_predict.R random.rda 20240605



########################################################
# TO DO

Rscript r3_road_predict.R road_type.rda 20240605
Rscript r3_road_predict.R sensible.rda 20240605
Rscript r3_road_predict.R unsensible.rda 20240605

# non-clustered are last priority? model CV would be diff though

Rscript r3_road_predict.R balanced.rda 20240605
Rscript r3_road_predict.R unbalanced.rda 20240605

 
echo "done running scripts"
