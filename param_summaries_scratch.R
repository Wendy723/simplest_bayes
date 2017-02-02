library(tidyverse)
read_params <- function(run) {
    iparams <- read_nm(paste0(run, "/fort.50"))
    names(iparams) <- c("ITERATION", "ID", "CL", "V")
    return(iparams)
}
param_summary <- function(run) {
    read_params(run)%>%
    select(CL, V) %>% summary
}

param_summary("modeling/rundir_002/ind_01/run001_ODF_100_SDF_100_est_01")
param_summary("modeling/rundir_002/ind_01/run018_ODF_500_SDF_10000_est_01/")
param_summary("modeling/rundir_002/ind_01/run014_ODF_3_SDF_20_est_01/")
param_summary("modeling/rundir_002/ind_01/run021_ODF_5000_SDF_50000_est_01/")
param_summary("modeling/rundir_002/ind_01/run022_ODF_500_SDF_500_est_01/")
param_summary("modeling/rundir_002/ind_01/run020_ODF_500_SDF_500_est_01//")
param_summary("modeling/rundir_002/ind_01/run024_ODF_500_SDF_10000_est_02//")
param_summary("modeling/rundir_002/ind_01/run025_ODF_500_SDF_10000_est_01//")
param_summary("modeling/rundir_002/ind_01/run026_ODF_500_SDF_10000_est_01//")
param_summary("modeling/rundir_002/ind_01/run027_ODF_500_SDF_10000_est_01//")
param_summary("modeling/rundir_002/ind_01/run030_ODF_100_SDF_100_est_01/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_04/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_06/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_07/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_08/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_09/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_10/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_11/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_12/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_13/")
param_summary("modeling/rundir_002/ind_01/run031_ODF_100_SDF_100_est_14/")

read_nm("modeling/rundir_002/ind_01/run001_ODF_100_SDF_100_FOCE_est_01/sdtab001")
