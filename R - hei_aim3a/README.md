# HEI Studies - Impact of mobile monitoring design on exposure assessment and environmental epidemiology

This repository contains some of the analysis code for the HEI work. It includes much of the mobile monitoring work and some additional health analyses related to low-cost monitors, machine learning, costs, etc.

# Scripts

### Stationary roadside mobile monitoring work 
Scripts are numbered for the stationary data work.

Scripts that start with "other_designs_...R" include analyses that were added later, including spatially unbalanced designs, temporal adjustments

### On-road mobile monitoring work 
The on-road mobile monitoring work has scripts that start with "r_" like the model names. 


### Related work
The 7_clean_act_dt.R and 8_eip_models.R scripts run health models based on the low-cost monitor and machine learning work. 

These results are summarized in the 9_ml..Rmd, 9_lcm...Rmd, and 9_comparisons.Rmd scripts. 


## Other 
There are several .bash scripts that were used to run multiple scripts back-to-back for efficiency. 


## Location
This work was primarily run from plasmid, with longer running scripts (most of the on-road scripts) on the Brain Cluster, with results transfered back to plasmid.

Some of the result summary scripts (e.g., for manuscript, reports) were coded locally for efficiency and synced with plasmid. 

