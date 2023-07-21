# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Convert HPV vaccination coverage in wuenic file to PRIME format
# ------------------------------------------------------------------------------
convert_wuenic_prime_coverage <- function (wuenic_file, 
                                           vaccination_age) {
  
  # Human papillomavirus (HPV) immunization coverage estimates (July 2023)
  # https://data.unicef.org/resources/dataset/immunization/
  wuenic_dt <- read_xls (path      = wuenic_file, 
                          col_names = TRUE)
  
  # change to data.table format
  setDT (wuenic_dt)
  
  # 15HPVC_F: HPV Vaccination coverage by age 15, last dose, females
  # extract rows of interest
  dt <- wuenic_dt [`vaccine_code` == "15HPVC_F", c('iso3c', 'year', 'value')]

  # set age of vaccination
  dt [, age_first := vaccination_age]
  dt [, age_last  := vaccination_age]

  # change colnames
  setnames (dt, "iso3c", "country_code")
  setnames (dt, "value", "coverage")
  
  # set column order
  setcolorder (dt, 
               neworder = c("country_code", "year", "age_first", "age_last", "coverage"))
  
  # cohorts in prime format
  cohorts <- dt
  
  # return vaccine coverage estimates
  return (cohorts)
  
} # end of function -- convert_wuenic_prime_coverage
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# streamline batch cohorts table
# ------------------------------------------------------------------------------
streamline_cohorts <- function (cohorts, 
                                vaccination_age) {
  
  # change coverage to proportion between 0 and 1
  cohorts [, coverage := coverage / 100]  
  
  # remove non-UNWPP countries (Andorra, Cook Islands, Palau, San Marino) -- demography data is missing
  # remove Seychelles (some data is missing -- check later)
  # remove Saint Vincent and the Grenadines (check later)
  cohorts <- cohorts [(country_code != "AND") & (country_code != "COK") & (country_code != "PLW") & (country_code != "SMR"), ]
  cohorts <- cohorts [(country_code != "SYC")]
  cohorts <- cohorts [(country_code != "VCT")]
  
  # DEBUG
  # cohorts <- cohorts [(country_code == "AND")]
  # cohorts <- cohorts [(country_code == "COK")]
  # cohorts <- cohorts [(country_code == "PLW")]
  # cohorts <- cohorts [(country_code == "SMR")]
  
  # cohorts <- cohorts [(country_code == "SYC")]
  # cohorts <- cohorts [(country_code == "VCT")]
  
  # ok: 
  # [1] "ARE" "ARG" "ARM" "AUS" "AUT" "BEL" "BGR"
  # [9] "BHS" "BLZ" "BOL" "BRA" "BRB" "BRN" "BTN" "BWA"
  # [17] "CAN" "CHE" "CHL" "COL" "CYP" "DEU" "DNK"
  # [25] "DOM" "ECU" "ESP" "EST" "ETH" "FIN" "FJI" "FRA"
  # [33] "FSM" "GBR" "GMB" "GTM" "GUY" "HND" "HUN" "IDN"
  # [41] "IRL" "ISL" "ISR" "ITA" "JAM" "JPN" "KEN" "KOR"
  # [49] "LAO" "LKA" "LTU" "LUX" "LVA" "MDA" "MEX" "MHL"
  # [57] "MKD" "MLT" "MRT" "MUS" "MYS" "NLD" "NOR" "NZL"
  # [65] "PAN" "PER" "PHL" "PRT" "PRY" "RWA" "SGP"
  # [73] "SLB" "SUR" "SVN" "SWE" "THA" "TKM"
  # [81] "TON" "TTO" "TZA" "UGA" "URY" "USA" "VCT" "ZAF"
  # [89] "ZMB" "ZWE"
  
  # [1] "AND" "ARE" "ARG" "ARM" "AUS" "AUT" "BEL" "BGR"
  # [9] "BHS" "BLZ" "BOL" "BRA" "BRB" "BRN" "BTN" "BWA"
  # [17] "CAN" "CHE" "CHL" "COK" "COL" "CYP" "DEU" "DNK"
  # [25] "DOM" "ECU" "ESP" "EST" "ETH" "FIN" "FJI" "FRA"
  # [33] "FSM" "GBR" "GMB" "GTM" "GUY" "HND" "HUN" "IDN"
  # [41] "IRL" "ISL" "ISR" "ITA" "JAM" "JPN" "KEN" "KOR"
  # [49] "LAO" "LKA" "LTU" "LUX" "LVA" "MDA" "MEX" "MHL"
  # [57] "MKD" "MLT" "MRT" "MUS" "MYS" "NLD" "NOR" "NZL"
  # [65] "PAN" "PER" "PHL" "PLW" "PRT" "PRY" "RWA" "SGP"
  # [73] "SLB" "SMR" "SUR" "SVN" "SWE" "SYC" "THA" "TKM"
  # [81] "TON" "TTO" "TZA" "UGA" "URY" "USA" "ZAF"
  # [89] "ZMB" "ZWE"
  
  
  # create new/empty batch cohort table
  batch_cohorts <- cohorts [0, ]
  
  # PRIME currently doesn't have the functionality to handle different start and 
  # end years between different countries, that is all countries should have the 
  # same start year (for example, 2000)  and all countries should have the same 
  # end year (for example, 2030). 
  # Instead of adding the functionality, add additional rows to countries with 
  # zero coverage so that all countries should have the same start year and end years
  # (increases run time a bit and a relatively easier fix to get the runs going).
  
  for (country in unique (cohorts [, country_code]) ) {
    
    # extract coverage data for a single country
    dt <- cohorts [country_code == country]
    
    # create zero coverage data table for a single country
    tdt <- data.table (country_code = country, 
                       year         = 2010:2022, 
                       age_first    = vaccination_age, 
                       age_last     = vaccination_age, 
                       coverage     = 0)
    
    
    # extract rows/years for zero coverage for which no coverage exists in wuenic file
    zero_dt <- tdt [!year %in% dt [, year]]
    
    # merge rows/years for zero coverage to the coverage data table for a single country
    dt <- rbind (zero_dt, dt)
    
    # order/sort by year
    dt <- dt [order (year)]
    
    # add coverage data for a single country to the batch cohort table
    batch_cohorts <- rbind (batch_cohorts, dt)
  }

  return (batch_cohorts)
  
} # end of function -- streamline_cohorts
# ------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# run batch file for vaccination of cohorts and estimate vaccination impact
# ------------------------------------------------------------------------------
estimate_vaccine_impact <- function (vaccine,
                                     vaccination_age,
                                     canc.inc) {
  
  # batch run
  results <- BatchRun(countries                       = -1,
                      coverage                        = -1,
                      agevac                          = -1,
                      agecohort                       = -1,
                      sens                            = -1,
                      year_born                       = -1,
                      year_vac                        = -1,
                      runs                            = 1,
                      vaccine_efficacy_beforesexdebut = 1,
                      vaccine_efficacy_aftersexdebut  = 0,
                      log                             = -1,
                      by_calendaryear                 = FALSE,
                      use_proportions                 = TRUE,
                      analyseCosts                    = FALSE,
                      psa                             = 0,
                      psa_vals                        = ".data.batch.psa",
                      unwpp_mortality                 = TRUE,
                      disability.weights              = "gbd_2017",
                      canc.inc                        = canc.inc,
                      vaccine                         = vaccine
  )
  
  # save full results
  results_file <- paste0 ("../output/results_age",
                          vaccination_age,
                          "_",
                          vaccine,
                          ".csv")
  fwrite (results, results_file)
  
  # return results file name
  return (results_file)
  
} # end of function -- estimate_vaccine_impact
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# Combine burden estimates from different simulation scenarios
# Add columns for calendar year, cases, deaths, yld, yll, dalys
# Add columns for (cases, deaths, yld, yll, dalys) per 100,000
# Add column for number of vaccines administered
# ------------------------------------------------------------------------------
combine_burden_estimate <- function (vaccine,
                                     vaccination_age,
                                     folder = "output/") {
  
  # burden estimates
  allburden <- NULL
  
  # read burden estimate 
  burdenfile <- results_file
  burden <- fread (burdenfile, header = "auto", stringsAsFactors = F)
  allburden <- burden

  # set to data table
  setDT (allburden)
  
  # Add columns for calendar year, cases, deaths, yld, yll, dalys
  allburden [, year   := birthcohort + age]
  allburden [, cases  := cohort_size * inc.cecx]
  allburden [, deaths := cohort_size * mort.cecx]
  allburden [, yld    := cohort_size * disability]
  allburden [, yll    := cohort_size * lifey]
  allburden [, dalys  := yll + yld]
  
  # Add columns for (cases, deaths, yld, yll, dalys) per 100,000
  allburden [, cases_p100  := cases  / cohort_size * 100000]
  allburden [, deaths_p100 := deaths / cohort_size * 100000]
  allburden [, yld_p100    := yld    / cohort_size * 100000]
  allburden [, yll_p100    := yll    / cohort_size * 100000]
  allburden [, dalys_p100  := dalys  / cohort_size * 100000]
  
  # NA values result due to division by 0 for UNWPP simulations,
  # since cohort size for ages 0 to 7 are 0
  allburden [is.na(allburden)] <- 0
  
  # Add column for number of vaccines administered
  # vaccined administered to girls at vaccination age
  allburden [(scenario=="post-vaccination" & age==vaccination_age),
             vaccines := cohort_size * vaccinated, with=T]
  
  # return comnbined burden estimates from all simulation scenarios
  return (allburden)
  
} # end of function -- combine_burden_estimate
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# create table of country-specific cervical cancer burden
# ------------------------------------------------------------------------------
create_table_country_burden <- function (allburden,
                                         vaccine,
                                         vaccination_age) {
  
  # extract burden for pre-vaccination and post-vaccination
  pre_vac  <- allburden [scenario == "pre-vaccination"]
  post_vac <- allburden [scenario == "post-vaccination"]
  
  # extract columns for country
  # cases, deaths, yld, yll, dalys
  pre_vac  <- pre_vac  [, list (country, cases, deaths, yld, yll, dalys)]
  post_vac <- post_vac [, list (country, cases, deaths, yld, yll, dalys)]
  
  # burden columns
  burden_columns <- c("cases", "deaths", "yld", "yll", "dalys")
  
  # summarise burden by country 
  #   dt[, lapply(.SD, sum, na.rm=TRUE), by=category, .SDcols=c("a", "c", "z") ]
  pre_vac <- pre_vac [, lapply (.SD, sum, na.rm=TRUE),
                      .SDcols = burden_columns,
                      by = .(country) ]
  
  post_vac <- post_vac [, lapply (.SD, sum, na.rm=TRUE),
                        .SDcols = burden_columns,
                        by = .(country) ]
  
  # sort by country
  pre_vac  <- pre_vac  [order (country)]
  post_vac <- post_vac [order (country)]
  
  # add pre-vaccination / post-vaccination to burden column names
  setnames (pre_vac,
            old = c("cases", "deaths", "yld", "yll", "dalys"),
            new = c("Cases (pre-vaccination)",
                    "Deaths (pre-vaccination)",
                    "YLDs (pre-vaccination)",
                    "YLLs (pre-vaccination)",
                    "DALYs (pre-vaccination)"))
  
  setnames (post_vac,
            old = c("cases", "deaths", "yld", "yll", "dalys"),
            new = c("Cases (post-vaccination)",
                    "Deaths (post-vaccination)",
                    "YLDs (post-vaccination)",
                    "YLLs (post-vaccination)",
                    "DALYs (post-vaccination)"))
  
  # combine tables -- pre-vaccination (and) post-vaccination
  burden <- pre_vac [post_vac, on = .(country = country)]
  
  # add country name from iso3 country code
  burden  [, Country := countrycode (burden  [, country], origin = "iso3c", destination = "country.name")]
  
  # set Country column as first column
  setcolorder (burden,  "Country")
  
  # save burden data table
  fwrite (burden,
          paste0 ("../tables/Table-Cervical_cancer_burden_age",
                  vaccination_age, "_", vaccine, ".csv"),
          col.names = T, row.names = F)
  
  return ()  # return null
  
} # end of function -- create_table_country_burden
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# compute vaccine impact and comparison metrics -- country level
# ------------------------------------------------------------------------------
compute_vaccine_impact_country <- function (allburden,
                                            vaccine,
                                            vaccination_age) {
  
  # set vaccine type
  vaccine_type <- switch (vaccine,
                          "4vHPV" = "bivalent/quadrivalent",
                          "9vHPV" = "nonavalent")
  
  # burden summary
  burden_summary <- allburden [ , .(total_cases  = sum (cases),
                                    total_deaths = sum (deaths),
                                    total_yld    = sum (yld),
                                    total_yll    = sum (yll),
                                    total_dalys  = sum (dalys)),
                                # total_cohort_size = sum (cohort_size)),
                                by=.(scenario, country)]
  
  # sort by country
  burden_summary  <- burden_summary  [order (country)]
  
  # set country column as first column
  setcolorder (burden_summary,  "country")
  
  # cohort size of girls at vaccination age
  burden_9 <- allburden [age == vaccination_age,
                         .(total_cohort_size_9 = sum (cohort_size),
                           total_vaccines      = sum (vaccines)),
                         by=.(scenario, country)]
  
  # combine burden summary tables
  burden_summary <-
    burden_summary [burden_9, on = .(scenario=scenario, country=country)]
  
  # sort by country
  burden_summary  <- burden_summary  [order (country)]
  
  # compute metrics per 100,000 girls at vaccination age
  burden_summary [, `:=` (cases_p100  = total_cases  / total_cohort_size_9 * 100000,
                          deaths_p100 = total_deaths / total_cohort_size_9 * 100000,
                          yld_p100    = total_yld    / total_cohort_size_9 * 100000,
                          yll_p100    = total_yll    / total_cohort_size_9 * 100000,
                          dalys_p100  = total_dalys  / total_cohort_size_9 * 100000)]
  
  # compute vaccine impact table
  burden_summary_prevac  <- burden_summary [scenario == "pre-vaccination"]
  burden_summary_postvac <- burden_summary [scenario == "post-vaccination"]
  
  vaccine_impact <- burden_summary_prevac [burden_summary_postvac,
                                           on = .(country=country)]
  
  # compute vaccine impact -- burden averted
  vaccine_impact [, `:=` (cases_averted  = total_cases  - i.total_cases,
                          deaths_averted = total_deaths - i.total_deaths,
                          yld_averted    = total_yld    - i.total_yld,
                          yll_averted    = total_yll    - i.total_yll,
                          dalys_averted  = total_dalys  - i.total_dalys)]
  
  
  vaccine_impact [, `:=` (cases_averted_perVG  = cases_averted  / i.total_vaccines * 1000,
                          deaths_averted_perVG = deaths_averted / i.total_vaccines * 1000,
                          yld_averted_perVG    = yld_averted    / i.total_vaccines * 1000,
                          yll_averted_perVG    = yll_averted    / i.total_vaccines * 1000,
                          dalys_averted_perVG  = dalys_averted  / i.total_vaccines * 1000)]
  
  # # ----------------------------------------------------------------------------
  # # plot vaccine impact
  # # plot lifetime health impact per 1000 vaccinated girls
  # # plot file -- cases, deaths, ylls, ylds & dalys for 5 scenarios in 177 countries (177 pages)
  # pdf (paste0 ("../figures/Figure-Country_vaccine_impact_age",
  #              vaccination_age, "_", vaccine, ".pdf"))
  # 
  # counter <- 0   # UNCOMMENT this line for final run
  # # counter <- 174   # COMMENT   this line for final run
  # 
  # # loop through each country
  # for (countries in unique (vaccine_impact$country)) {
  #   
  #   if (counter < 74) {  # plot subset of countries
  #     counter <- counter + 1
  #     
  #     tic ()
  #     print (countries)
  #     country_vaccine_impact <- vaccine_impact [country == countries]
  #     
  #     plotwhat <- c("cases_averted_perVG",
  #                   "deaths_averted_perVG",
  #                   "yld_averted_perVG",
  #                   "yll_averted_perVG",
  #                   "dalys_averted_perVG")
  #     
  #     y_axis <- c("Cases averted",
  #                 "Deaths averted",
  #                 "YLDs averted",
  #                 "YLLs averted",
  #                 "DALYs averted")
  #     
  #     plot_list = list ()
  #     
  #     plot_list <- lapply (1:length(plotwhat), function (i) {
  #       
  #       toplot = plotwhat[i]
  #       
  #       p <- ggplot (country_vaccine_impact,
  #                    aes (x = country, y = get(toplot), fill=toplot)) +
  #                    # aes (x = simulation, y = get(toplot), fill=toplot)) +
  #         geom_bar (stat="identity") +
  #         labs (
  #           x = NULL,
  #           y = y_axis[i]
  #         ) +
  #         theme_bw (base_size = 10) +
  #         theme(legend.position="none") +
  #         theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  #         scale_y_continuous (labels = scales::comma)
  #     })
  #     
  #     # arrange plot columns and rows
  #     q <- ggarrange(plotlist=plot_list, ncol = 2, nrow = 3)
  #     
  #     print (annotate_figure(q,
  #                            top = text_grob (paste0("Lifetime health impact per 1000 vaccinated girls - ",
  #                                                    countrycode (countries, 'iso3c', 'country.name'),
  #                                                    "\n (vaccination age = ", vaccination_age, " years / ", vaccine_type, " vaccine)"),
  #                                             color = "black", size = 12)))
  #     
  #   }
  #   toc ()
  #   
  # }  # end of for loop
  # 
  # dev.off ()
  # # ----------------------------------------------------------------------------
  
  
  # ----------------------------------------------------------------------------
  # plot file -- cases, deaths & dalys for updated scenario (s5) in 177 countries (1 page)
  pdf (paste0 ("../figures/Figure_vaccine_impact_age",
               vaccination_age, "_", vaccine, ".pdf"))
  
  
  # extract vaccine impact results 
  country_vaccine_impact <- vaccine_impact 
  
  # add country name from iso3 country code
  country_vaccine_impact [, Country := countrycode (country_vaccine_impact  [, country],
                                                    origin = "iso3c",
                                                    destination = "country.name")]
  
  plotwhat <- c("cases_averted_perVG",
                "deaths_averted_perVG",
                "yld_averted_perVG",
                "yll_averted_perVG",
                "dalys_averted_perVG")
  
  y_axis <- c("Cases averted",
              "Deaths averted",
              "YLDs averted",
              "YLLs averted",
              "DALYs averted")
  
  plot_list = list ()
  
  plot_list <- lapply (1:length(plotwhat), function (i) {
    
    # which burden to plot
    toplot = plotwhat[i]
    
    # p <- ggplot (country_vaccine_impact,
    fig <- ggplot (country_vaccine_impact,
                   aes (x = reorder (country, -get(toplot)), y = get(toplot), fill = get(toplot))) +
             geom_bar (stat="identity") +
             labs (
               x = NULL,
               y = y_axis[i],
               title = paste0 (y_axis[i],
                               " per 1000 vaccinated girls",
                               " (vaccination age = ", vaccination_age, " years / ", vaccine_type, " vaccine)")
             ) +
             theme_classic (base_size = 8) +
             theme(legend.position="none") +
             theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
             scale_y_continuous (labels = scales::comma) +
             scale_fill_gradientn(colours = rev(terrain.colors(10))) +
             # coord_flip() +
             theme (axis.text.x=element_text(size=rel(0.75))) + 
             theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    
    print (fig)
    
    # save plot as png file
    ggsave (filename = paste0 ("../figures/Figure_vaccine_impact_age",
                               vaccination_age, "_", vaccine, ".png"), 
            plot     = fig, 
            dpi      = 600)
    
           
  })
  
  dev.off ()
  
  

  
  # ----------------------------------------------------------------------------
  
  # save streamlined table of vaccine impact
  # country, (cases, deaths, ylds, ylls, dalys) averted per 1000 FVG
  vaccine_impact_table <- vaccine_impact [, c("country", 
                                              "cases_averted_perVG",
                                              "deaths_averted_perVG",
                                              "yld_averted_perVG",
                                              "yll_averted_perVG",
                                              "dalys_averted_perVG")]
  
  fwrite (vaccine_impact_table,
          paste0 ("../tables/Table-Vaccine_impact_iso3_age",
                  vaccination_age, "_", vaccine, ".csv"),
          col.names = T, row.names = F)
  
  
  
  # add country name from iso3 country code
  vaccine_impact_table [, Country := countrycode (vaccine_impact_table  [, country],
                                                  origin = "iso3c",
                                                  destination = "country.name")]
  
  # set Country column as first column
  setcolorder (vaccine_impact_table,  "Country")
  
  # sort by Country name
  vaccine_impact_table  <- vaccine_impact_table  [order (Country)]
  
  # vaccine impact table (shorter column names)
  vaccine_impact_tab <- copy (vaccine_impact_table)
  
  # update column names for burden averted
  setnames (vaccine_impact_table,
            old = c("country",
                    "cases_averted_perVG",
                    "deaths_averted_perVG",
                    "yld_averted_perVG",
                    "yll_averted_perVG",
                    "dalys_averted_perVG"),
            new = c("Country code (ISO 3)",
                    "Cases averted per 1000 vaccinated girls",
                    "Deaths averted per 1000 vaccinated girls",
                    "YLDs averted per 1000 vaccinated girls",
                    "YLLs averted per 1000 vaccinated girls",
                    "DALYs averted per 1000 vaccinated girls"))
  
  # save vaccine impact data table
  fwrite (vaccine_impact_table,
          paste0 ("../tables/Table-Vaccine_impact_age",
                  vaccination_age, "_", vaccine, ".csv"),
          col.names = T, row.names = F)
  
  
  # ----------------------------------------------------------------------------
  # vaccine impact data table of only updated simulation scenario (s5)
  vaccine_impact_table_s5 <- vaccine_impact_table
  
  # round off values (no decimal point)
  vaccine_impact_table_s5 <-
    vaccine_impact_table_s5 [, lapply(.SD, round, 0),
                             .SDcols = c(
                               "Cases averted per 1000 vaccinated girls",
                               "Deaths averted per 1000 vaccinated girls",
                               "YLDs averted per 1000 vaccinated girls",
                               "YLLs averted per 1000 vaccinated girls",
                               "DALYs averted per 1000 vaccinated girls"),
                             by = .(Country)]
  
  # save save vaccine impact data table of only updated simulation scenario
  fwrite (vaccine_impact_table_s5,
          paste0 ("../tables/Table-Vaccine_impact_country_s5_age",
                  vaccination_age, "_", vaccine, ".csv"),
          col.names = T, row.names = F)
  
  
  # return vaccine impact table (shorter column names)
  return (vaccine_impact_tab)
  
} # end of function -- compute_vaccine_impact_country
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# average vaccine coverage
# ------------------------------------------------------------------------------
vaccine_coverage_average <- function (batch_cohorts, 
                                      vaccine_impact_tab, 
                                      plot_curve) {
  
  # average vaccine coverage
  coverage_dt <- batch_cohorts [, lapply (.SD, mean, na.rm=TRUE), 
                        .SDcols = "coverage",
                        by = .(country_code) ]
  
  # average vaccine coverage ordered by vaccine impact
  vaccine_impact_coverage_tab <- coverage_dt [vaccine_impact_tab, on = .(country_code = country)]
  
  # plot -- average vaccine coverage ordered by vaccine impact
  if (plot_curve == T) {
    
    pdf (paste0 ("../figures/Figure_vaccine_coverage_age",
                 vaccination_age, "_", vaccine, ".pdf"))
    
      
    fig <- ggplot (vaccine_impact_coverage_tab, 
                   aes (x = factor (country_code, level = country_code), y = (coverage * 100), fill = coverage)) + 
      geom_bar (stat="identity") + 
      labs (
        x = NULL,
        y = "vaccine coverage (%)",
        title = "Average coverage of HPV vaccination during 2010-2022"
      ) +
      theme_classic (base_size = 8) +
      theme(legend.position="none") +
      theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous (labels = scales::comma) +
      scale_fill_gradientn(colours = rev(terrain.colors(10))) +
      theme (axis.text.x=element_text(size=rel(0.75))) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) 
    
    print (fig)
    
    dev.off ()
    
    # save plot as png file
    ggsave (filename = paste0 ("../figures/Figure_vaccine_coverage_age",
                               vaccination_age, "_", vaccine, ".png"), 
            plot     = fig, 
            dpi      = 600)
    
  }

  return (vaccine_impact_coverage_tab)
  
} # end of function -- vaccine_coverage_average
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# plot_hci -- plot concentration curve
# ------------------------------------------------------------------------------
plot_hci <- function(x, ...) {
  
  # figure for concentration curve
  png (paste0 ("../figures/Figure_concentration_curve_age",
               vaccination_age, "_", vaccine, ".png"), 
       width = 6000, height = 6000, units = "px", pointsize = 144)
  
  if (!any(class(x) == 'hci')) stop ("Object is not of class hci")
  myOrder <- order(x$fractional_rank)
  xCoord <- x$fractional_rank[myOrder]
  y <- x$outcome[myOrder]
  cumdist <- cumsum(y) / sum(y)
  
  plot(c(1,1), xlim = c(0,1), ylim = c(0,1), type = "n", 
       xlab = "fractional rank (countries with high to low vaccine impact)", 
       ylab = "cumulative distribution (vaccine coverage)" , 
       main = "Concentration Curve", ...)
  polygon(xCoord, cumdist, col = "light gray", ...)
  
  dev.off ()
  
  # # save plot as png file
  # ggsave (filename = paste0 ("../figures/Figure_concentration_curve_age",
  #                            vaccination_age, "_", vaccine, ".png"), 
  #         plot     = fig, 
  #         dpi      = 600)
  
  return ()
  
} # end of function -- plot_hci
# ------------------------------------------------------------------------------


# estimate concentration index and plot concentration curve
concentration_index_curve <- function (vaccine_impact_coverage_tab, plot_curve) {
  
  # make copy of vaccine impact and coverage table
  vdt <- copy (vaccine_impact_coverage_tab)
  
  # add ranking column (countries with high vaccine impact to low vaccine impact)
  vdt [, ranking := 1:length (vaccine_impact_coverage_tab$country_code)]
  
  # estimate concentration index
  con_index <- with (vdt, ci (x = ranking, y = coverage, type = "CIc"))
  
  # plot concentration curve
  if (plot_curve == TRUE) {
    
    plot_hci (con_index)
  }
  
  # return concentration index estimates (includes mean and 95% confidence interval)
  return (con_index)

} # end of function -- concentration_index_curve
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# generate plot -- concentration indices by year
plot_con_index_year <- function (dt_con_index) {
  
  fig <- ggplot (dt_con_index, 
                 aes (x = year, y = con_ind)) + 
    geom_point () + 
    labs (
      x = "year",
      y = "concentration index",
      title = "Concentration index for each year during 2010-2022"
    ) + 
    theme_classic () + 
    geom_hline (yintercept = 0, linetype = "dashed")
  
  pdf (paste0 ("../figures/Figure_con_index_year",
               vaccination_age, "_", vaccine, ".pdf"))
  
  print (fig)
  
  dev.off ()
  
  
} # end of function -- plot_con_index_year
# ------------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# attach World bank income levels to countries
world_bank_income_level <- function (countries_dt) {
  
  # download updated country and region information from World Bank API
  wb_dt <- wbcountries ()
  
  # append world bank income level to countries 
  contries_wb_dt <- merge (x    = countries_dt, 
                           y    = wb_dt, 
                           by.x = "country_code", 
                           by.y = "iso3c")
  
  
  return (contries_wb_dt)
  
}  # end of function -- world_bank_income_level
# ------------------------------------------------------------------------------


