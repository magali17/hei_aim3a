#!/usr/bin/bash

# Install sf and related R packages

# Install modules (adjust as needed for your module versions)
module purge
module load GCC/gcc-10.3.0 
module load GEOS/geos-3.8.0 
module load GDAL/gdal-3.0.2 
module load R/R-4.2.2 
module load UDUNITS/udunits-2.2.26 

# Create a R script to install the R packages
OUTFILE='install_sf.R'
(
cat <<'EOF'
# Install sf and related R packages, if not already installed

# Force use of personal library and specify CRAN repo
lib_dir <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(lib_dir)) dir.create(lib_dir, recursive = TRUE)
.libPaths(lib_dir, include.site = FALSE)
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Define a function to conditionally install packages, if needed
pkg_load <- function(pkgs) {
    if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
    res <- sapply(pkgs, function(pkg) {
        if (!requireNamespace(pkg, quietly = TRUE)) pak::pkg_install(pkg)
        suppressPackageStartupMessages(require(pkg, character.only = TRUE))
    })
}

# Install other packages, if needed
pkg_load(c("units", "stringr", "rgdal", "raster", "sf", "gstat"))
EOF
) > $OUTFILE

# Run the R script created above
# - Note: A log of the R output will be stored in install_sf.Rout
R CMD BATCH --vanilla --no-save install_sf.R

module purge