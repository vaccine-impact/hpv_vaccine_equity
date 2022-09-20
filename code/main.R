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

# remove all objects from workspace
rm (list = ls ())

# source functions
source ("functions.R")

# start time
print (Sys.time ())
tic ()

# Convert HPV vaccination coverage in wuenic file to PRIME format
batch_cohorts <- convert_wuenic_prime_coverage (wuenic_file = "../data/wuenic2021rev_hpv-estimates.xlsx")




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
