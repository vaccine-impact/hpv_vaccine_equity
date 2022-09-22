# # iso3 <- unique(data.pop [, country_code])
# # iso3 <- as.data.table (iso3)
# # iso3 [, test := 1]
# # 
# # wuenic_countries <- unique (batch_cohorts [, country_code])
# # wuenic_countries <- as.data.table (wuenic_countries)
# # 
# # dt <- merge (wuenic_countries, iso3, by.x = "wuenic_countries", by.y = "iso3", all.x = T)
# 
# batch_cohorts <- batch_cohorts [country_code == "ARG" | country_code == "ARM"]
# 
# # create new/empty batch cohort table
# new_batch_cohorts <- batch_cohorts [0, ]
# 
# # PRIME currently doesn't have the functionality to handle different start and 
# # end years between different countries, that is all countries should have the 
# # same start year (for example, 2000)  and all countries should have the same 
# # end year (for example, 2030). 
# # Instead of adding the functionality, add additional rows to countries with 
# # zero coverage so that all countries should have the same start year and end years
# # (increases run time a bit and a relatively easier fix to get the runs going).
# 
# for (country in unique (batch_cohorts [, country_code]) ) {
#   
#   # extract coverage data for a single country
#   dt <- batch_cohorts [country_code == country]
#   
#   # create zero coverage data table for a single country
#   tdt <- data.table (country_code = country, 
#                      year         = 2010:2021, 
#                      age_first    = 15, 
#                      age_last     = 15, 
#                      coverage     = 0)
#   
#   
#   # extract rows/years for zero coverage for which no coverage exists in wuenic file
#   zero_dt <- tdt [!year %in% dt [, year]]
#   
#   # merge rows/years for zero coverage to the coverage data table for a single country
#   dt <- rbind (zero_dt, dt)
#   
#   # order/sort by year
#   dt <- dt [order (year)]
#   
#   # add coverage data for a single country to the new batch cohort table
#   new_batch_cohorts <- rbind (new_batch_cohorts, dt)
# }
# 
# # tdt <- data.table (country_code = unique (dt [, country_code]), 
# #                    year         = 2010:2021, 
# #                    age_first    = 15, 
# #                    age_last     = 15, 
# #                    coverage     = 0) 
# # 
# # (tdt [!year %in% dt [, year]])
# 
# 
# dt <- copy (vaccine_impact_tab)
# cdt <- copy (batch_cohorts)
# 
# cdt <- cdt [, lapply (.SD, mean, na.rm=TRUE), 
#             .SDcols = "coverage",
#             by = .(country_code) ]
# 
# new_dt <- cdt [dt, on = .(country_code = country)]

# cumulative sum
dt <- data.table (a = 1:100, 
                  b = rlnorm (100))
setorder (dt, b)

dt [, d := cumsum (b)]
dt [, e := 1:100]

plot (x=dt$e, y=dt$d)

# devtools::install_github("brechtdv/rineq")
library (rineq)
library (ineq)

example <- with (dt, ci (x = e, y = b))
plot (example)
summary(example)

vdt <- copy (vaccine_impact_coverage_tab)
vdt [, ranking := 1:length (vaccine_impact_coverage_tab$country_code)]

ex <- with (vdt, ci (x = ranking, y = coverage))
plot (ex)
summary (ex)
            
            # data(nigeria)
            # example <- with(nigeria, ci(x = wealth, y = zscore, wt = weight, type = "CIg"))
            # plot(example)
            # summary(example)

plot (ex$outcome)
plot (ex, type = "l")

plot_hci (ex)

temp <- cumsum (ex$outcome / sum(ex$outcome)) 

plot (temp)


plot_hci <-
  function(x, ...) {
    if (!any(class(x) == 'hci')) stop("Object is not of class hci")
    myOrder <- order(x$fractional_rank)
    xCoord <- x$fractional_rank[myOrder]
    y <- x$outcome[myOrder]
    cumdist <- cumsum(y) / sum(y)
    
    plot(c(1,1), xlim = c(0,1), ylim = c(0,1), type = "n", xlab = "fractional rank (countries with high to low vaccine impact)", ylab = "cumulative distribution (vaccine coverage)" , 
         main = "Concentration Curve", ...)
    polygon(xCoord, cumdist, col = "light gray", ...)
  }



