# to run this script in the terminal, enter: bash <script_name>.bash

Rscript 7_clean_act_dt.R
Rscript 8_epi_models.R

## knit markdown with results 
Rscript -e 'rmarkdown::render("9_stationary_summary.Rmd", "html_document")'
  

echo "DONE RUNNING ALL SCRIPTS"
