# to run this script in the terminal, enter: bash run_scripts.bash

# script runs R scripts to: 1) estimate annual averages, 2) make UK-pls predictions, 3) evaluate models, 4) generate summary figures and tables

## run rscripts
Rscript 1_annual_avg.R
Rscript 2.0_uk_workspace.R
Rscript 2.1_uk_cv.R
Rscript 2.3_uk_combine_predictions.R
Rscript 3_model_eval.R
Rscript 4_select_models.R

## knit markdown with results 
#Rscript -e 'rmarkdown::render("4_results_summary.Rmd", "html_document")'

echo "done running scripts"
