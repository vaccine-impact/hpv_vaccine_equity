# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Convert HPV vaccination coverage in wuenic file to PRIME format
# ------------------------------------------------------------------------------
convert_wuenic_prime_coverage <- function (wuenic_file) {
  
  # Human papillomavirus (HPV) immunization coverage estimates (July 2022)
  # https://data.unicef.org/wp-content/uploads/2016/07/wuenic2021rev_hpv-estimates.xlsx
  wuenic_dt <- read_xlsx (path      = wuenic_file, 
                          col_names = TRUE)
  
  # change to data.table format
  setDT (wuenic_dt)
  
  # extract rows of interest
  dt <- wuenic_dt [`vaccine-code` == "15HPVC_F", c('iso-code', 'year', 'coverage')]
  
  
  
  # batch cohort in prime format
  batch_cohorts <- dt
  
  # return vaccine coverage estimates
  return (batch_cohorts)
  
} # end of function -- function_name
# ------------------------------------------------------------------------------

