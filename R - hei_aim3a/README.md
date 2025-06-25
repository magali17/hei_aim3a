# HEI Studies – Impact of Mobile Monitoring Design on Exposure Assessment and Environmental Epidemiology

This repository contains some of the analysis conducted under the Health Effects Institute (HEI) award to Dr. Lianne Sheppard:  
**"Optimizing Exposure Assessment for Inference About Air Pollution Effects with Application to the Aging Brain"** (RFA 19-1).

The work includes:

- Analyses of mobile monitoring designs  
- Summary of health analyses related to low-cost monitors  
- Summary of applications of machine learning  
- Summary of cost assessments  

---

## Scripts Overview

### Stationary Roadside Mobile Monitoring

Scripts for stationary data analysis are numbered sequentially. Examples include:

- `0_setup.R`  
- `1.1_temporal_adjustment.Rmd`  
- `1.2_site_type.Rmd`  

Additional analyses (e.g., spatially unbalanced designs, temporal adjustments) are included in scripts prefixed with `other_designs_...R`.

### On-Road Mobile Monitoring

Scripts beginning with `r_` summarize analyses using on-road mobile monitoring data.

### Related Health Analyses

- `7_clean_act_dt.R` and `8_eip_models.R`: Run health models based on low-cost monitor and machine learning data.  
- Summaries of these results are found in:
  - `9_ml_summary.Rmd`  
  - `9_lcm_summary.Rmd`  
  - `9_comparisons.Rmd`  

---

## Other Utilities

Several `.bash` scripts are included to automate the execution of multiple R scripts.





