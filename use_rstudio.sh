#!/usr/bin/bash

# Install sf and related R packages

# Install modules (adjust as needed for your module versions)
module load GCC/gcc-10.3.0 
module load GEOS/geos-3.8.0 
module load GDAL/gdal-3.0.2 
module load UDUNITS/udunits-2.2.26 

module load R/R-4.2.2
module load R/rstudio-1.2.1335

# next, enter "rstudio" into the terimal if you wish to open an interactive session