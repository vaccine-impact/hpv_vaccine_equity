# main.R
# run it from the source directory

# To assess the inequities in cervical cancer burden across countries pre and 
# post-introduction of HPV vaccination from 2010 to 2020.

# load libraries
library (countrycode)         # country codes
library (data.table)          # data table
library (ggplot2)             # plot
library (tictoc)              # timer 
library (readxl)              # read excel files in R 
library (foreach)             # looping -- supports parallel execution
library (doParallel)          # foreach parallel adaptor for the 'parallel' Package
library (ggforce)
library (ggpubr)
library (rineq)               # health inequalities
library (prime)               # prime package

# remove all objects from workspace
rm (list = ls ())

# source functions
source ("functions.R")

# start time
print (Sys.time ())
tic ()

# initialise values
vaccine         <- "4vHPV"
vaccination_age <- 14
results_file    <- "../output/results_age14_4vHPV.csv" # can be removed later

# Convert HPV vaccination coverage in wuenic file to PRIME format
cohorts <- convert_wuenic_prime_coverage (wuenic_file     = "../data/wuenic2022rev_hpv-estimates.xls", 
                                          vaccination_age = vaccination_age)

# streamline batch cohorts table
batch_cohorts <- streamline_cohorts (cohorts, 
                                     vaccination_age = vaccination_age)

# register batch cohorts
RegisterBatchData (batch_cohorts, force = T)

cl <- makeCluster (detectCores())   # registering number of cores
registerDoParallel (cl)             # start of parallelisation


# ------------------------------------------------------------------------------
# uncomment for full run
# run batch file for vaccination of cohorts and estimate vaccination impact
# results_file <- estimate_vaccine_impact (vaccine         = vaccine,
#                                        vaccination_age = vaccination_age,
#                                        canc.inc        = "2020")
# ------------------------------------------------------------------------------


# Combine burden estimates
# Add columns for calendar year, cases, deaths, yld, yll, dalys
# Add columns for (cases, deaths, yld, yll, dalys) per 100,000
# Add column for number of vaccines administered
allburden <- combine_burden_estimate (vaccine,
                                      vaccination_age,
                                      results_file) 

# create table of country-specific cervical cancer burden
create_table_country_burden (allburden,
                             vaccine         = vaccine,
                             vaccination_age = vaccination_age)

# compute vaccine impact -- country level
vaccine_impact_tab <- compute_vaccine_impact_country (allburden,
                                                      vaccine         = vaccine,
                                                      vaccination_age = vaccination_age)

# sort by vaccine impact (by deaths averted per vaccinated girl)
setorder (vaccine_impact_tab, - deaths_averted_perVG) 
# setorder (vaccine_impact_tab, - cases_averted_perVG)
# setorder (vaccine_impact_tab, - dalys_averted_perVG)

# average vaccine coverage
vaccine_impact_coverage_tab <- vaccine_coverage_average (batch_cohorts, 
                                                         vaccine_impact_tab, 
                                                         plot_curve = T)

# estimate concentration index and plot concentration curve
con_index <- concentration_index_curve (vaccine_impact_coverage_tab, 
                                        plot_curve = T)
print (summary (con_index))

# -----------------------
# save table for 84 countries (analysed in this study)
countries_dt <- vaccine_impact_coverage_tab [, country_code, Country]

setcolorder (countries_dt, neworder = c("country_code"))

countries_dt <- countries_dt [order (country_code)]

fwrite (x    = countries_dt, 
        file = "../tables/Table_countries.csv")
# -----------------------


# # -----------------------
# # generate concentration indices by year
# dt_con_index <- data.table (year = (1995+ vaccination_age):(2006 + vaccination_age), 
#                             con_ind = numeric())
# 
# for (birth_year in 1995:2006) {
#   
#   # extract burden estimates for a birth cohort
#   cohort_burden <- allburden [birthcohort == birth_year]
#   
#   # allburden <- combine_burden_estimate (vaccine,
#   #                                       vaccination_age,
#   #                                       results_file) 
#   
#   # # create table of country-specific cervical cancer burden
#   # create_table_country_burden (allburden,
#   #                              vaccine         = vaccine,
#   #                              vaccination_age = vaccination_age)
#   
#   # compute vaccine impact -- country level
#   vaccine_impact_tab <- compute_vaccine_impact_country (allburden       = cohort_burden,
#                                                         vaccine         = vaccine,
#                                                         vaccination_age = vaccination_age)
#   
#   # sort by vaccine impact (by deaths averted per vaccinated girl)
#   # setorder (vaccine_impact_tab, - deaths_averted_perVG) 
#   setorder (vaccine_impact_tab, - cases_averted_perVG)
#   # setorder (vaccine_impact_tab, - dalys_averted_perVG)
#   
#   # average vaccine coverage
#   vaccine_impact_coverage_tab <- vaccine_coverage_average (batch_cohorts [year == birth_year + vaccination_age, ], 
#                                                            vaccine_impact_tab, 
#                                                            plot_curve = F)
#   
#   # estimate concentration index and plot concentration curve
#   con_index <- concentration_index_curve (vaccine_impact_coverage_tab [coverage > 0], 
#                                           plot_curve = F)
#   # print (summary (con_index))
#   print (con_index)
#   
#   # add concentration index of specific year
#   dt_con_index [year == (birth_year + vaccination_age), 
#                 c(con_ind := con_index$concentration_index, 
#                   con_ind_low := ]
#   
# }
# 
# # generate plot -- concentration indices by year
# plot_con_index_year (dt_con_index)
# 
# 
# # -----------------------




# end of parallelism
stopCluster (cl)                    

# stop time
print (Sys.time ())
toc ()

# -----------------------
# analysis outline
# - convert wuenic coverage to prime format
# - run batch cohorts
# - compare pre- and post-vaccine burden
# - generate equity impact
# -----------------------

# -----------------------
# check later
# - why all countries have to have the same start and end year of vaccination
# -----------------------






