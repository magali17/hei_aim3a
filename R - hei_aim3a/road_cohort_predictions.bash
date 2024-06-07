# to run this script in the terminal, enter: bash road_cohort_predictions.bash
# 6/6/24
########################################################
# run rscripts

## compute node 1  #20 smp # 4 cores 
Rscript r3_road_predict.R route.rda 20240605 

## compute node 14 # 10 smp # 4 cores
Rscript r3_road_predict.R random.rda 20240605

## compute node 12 # 11 smp # 4 cores
Rscript r3_road_predict.R road_type.rda 20240605

## compute node 8 # 12 smp # 4 cores
Rscript r3_road_predict.R sensible.rda 20240605

## compute node 3 # 12 smp # 4 cores
Rscript r3_road_predict.R unsensible.rda 20240605


########################################################
# non-clustered are last priority? model CV would be diff though

## compute node 11 # 12 smp # 4 cores
Rscript r3_road_predict.R balanced.rda 20240605

## compute node 00 # 10 smp # 4 cores
Rscript r3_road_predict.R unbalanced.rda 20240605


########################################################



echo "done running scripts"
