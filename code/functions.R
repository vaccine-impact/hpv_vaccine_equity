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
    
    # generate plot
    fig <- ggplot (country_vaccine_impact,
                   aes (x = reorder (country, -get(toplot)), y = get(toplot), fill = get(toplot))) +
             geom_bar (stat="identity") +
             labs (
               x = NULL,
               y = paste0 (y_axis[i],
                           " per 1000 vaccinated girls"),
               title = paste0 (y_axis[i],
                               " per 1000 vaccinated girls",
                               " (vaccination age = ", vaccination_age, " years)"),
                               # " (vaccination age = ", vaccination_age, " years / ", vaccine_type, " vaccine)"),
               fill = ""
             ) +
             theme_classic (base_size = 8) +
             # theme(legend.position="none") +
             theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
             scale_y_continuous (labels = scales::comma) +
             scale_fill_gradientn(colours = rev(terrain.colors(10))) +
             # coord_flip() +
             theme (axis.text.x=element_text(size=rel(0.75))) +
             theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    
    print (fig)
    
    # save plot as png file (for deaths averted on y-axis)
    if (toplot == "deaths_averted_perVG") {
      ggsave (filename = paste0 ("../figures/Figure_vaccine_impact_age",
                                 vaccination_age, "_", vaccine, ".png"), 
              plot     = fig, 
              dpi      = 600)
    }
    
           
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
                   aes (x = factor (country_code, level = country_code), y = (coverage * 100), fill = coverage * 100)) + 
      geom_bar (stat="identity") + 
      labs (
        x = NULL,
        y = "vaccine coverage (%)",
        title = "Average coverage of HPV vaccination during 2010-2022",
        fill = ""
      ) +
      theme_classic (base_size = 8) +
      # theme(legend.position="none") +
      theme (panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_y_continuous (labels = scales::comma) +
      scale_fill_gradientn (colours = rev(terrain.colors(10))) +
      theme (axis.text.x=element_text(size=rel(0.75))) +
      theme (axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
    
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


# ------------------------------------------------------------------------------
# my_con_index_plot -- plot concentration curve (subplots by income level and region)
# ------------------------------------------------------------------------------
my_con_index_plot <- function (con_index_a, 
                               level_a, 
                               level_name) {
  
  myOrder <- order (con_index_a$fractional_rank)
  xCoord  <- con_index_a$fractional_rank [myOrder]
  y       <- con_index_a$outcome [myOrder]
  cumdist <- cumsum (y) / sum (y)
  
  plot (c(1,1), xlim = c(0,1), ylim = c(0,1), type = "n", 
        xlab = "fractional rank (countries with high to low vaccine impact)", 
        ylab = "cumulative distribution (vaccine coverage)" , 
        main = paste0 (level_a, level_name))
  
  polygon (xCoord, cumdist, col = "light gray")
  
} # end of function -- my_con_index_plot
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# estimate concentration index and plot concentration curve
# ------------------------------------------------------------------------------
concentration_index_curve <- function (vaccine_impact_coverage_tab, 
                                       countries_wb_who_dt) {
  
  # make copy of vaccine impact and coverage table
  vdt <- copy (vaccine_impact_coverage_tab)
  
  # add ranking column (countries with high vaccine impact to low vaccine impact)
  vdt [, ranking := 1:length (vaccine_impact_coverage_tab$country_code)]
  
  # estimate concentration index
  con_index_all <- with (vdt, ci (x = ranking, y = coverage, type = "CIc"))
  
  # print concentration index
  print ( c (nrow (vdt), "all 84 countries") )
  print (summary (con_index_all))
  
  # plot concentration curve
  plot_hci (con_index_all)
  
  # add income level and region to countries in vaccine impact coverage table
  vaccine_impact <- merge (x   = vdt, 
                           y    = countries_wb_who_dt, 
                           by.x = "country_code", 
                           by.y = "ISO3 country code")
  
  
  # ----------------------------------------------------------------------------
  # concentration curve by -- income level
  # ----------------------------------------------------------------------------

  # concentration curve subplots by income level
  subplots       <- length (unique (countries_wb_who_dt$`Income level`))
  subplot_number <- 0

  # estimate concentration indices by levels (income levels)
  for (level in unique (countries_wb_who_dt$`Income level`) ) {

    subplot_number <- subplot_number + 1

    # extract rows for countries at this level
    vaccine_impact_level <- vaccine_impact [`Income level` == level]

    # sort by vaccine impact (by deaths averted per vaccinated girl)
    setorder (vaccine_impact_level, - deaths_averted_perVG)
    # setorder (vaccine_impact_tab, - cases_averted_perVG)
    # setorder (vaccine_impact_tab, - dalys_averted_perVG)

    # add ranking column (countries with high vaccine impact to low vaccine impact)
    vaccine_impact_level [, ranking := 1:length (vaccine_impact_level$country_code)]

    # estimate concentration index
    con_index <<- with (vaccine_impact_level, ci (x = ranking, y = coverage, type = "CIc"))

    # print concentration index
    print ( c (nrow (vaccine_impact_level), level) )
    print (summary (con_index))
    
    # make sure that variables in the next steps are within scope
    level_toplevel <<- level
    
    assign (paste0 ("subplot_", subplot_number), as.ggplot (~my_con_index_plot (con_index_a = con_index,
                                                                                level_a     = level_toplevel,
                                                                                level_name  = " countries")))
    
  }

  con_curve_level <- plot_grid (subplot_1, subplot_2, subplot_3, subplot_4, ncol = 2)

  # save plot as png file
  ggsave (filename = paste0 ("../figures/Figure_concentration_curve_income_levels_age",
                             vaccination_age, "_", vaccine, ".png"),
          plot     = con_curve_level,
          dpi      = 600,
          bg       = 'white',
          width    = 11,
          height   = 11,
          units    = "in")
  # ----------------------------------------------------------------------------
  
  
  # # ----------------------------------------------------------------------------
  # concentration curve by -- WHO regions
  # ----------------------------------------------------------------------------

  # concentration curve subplots by income level
  subplots       <- length (unique (countries_wb_who_dt$`WHO region`))
  subplot_number <- 0

  # estimate concentration indices by level (regions)
  for (level in unique (countries_wb_who_dt$`WHO region`) ) {

    if (level != "Eastern Mediterranean") {

      subplot_number <- subplot_number + 1

      # extract rows for countries at this level
      vaccine_impact_level <- vaccine_impact [`WHO region` == level]

      # sort by vaccine impact (by deaths averted per vaccinated girl)
      setorder (vaccine_impact_level, - deaths_averted_perVG)
      # setorder (vaccine_impact_tab, - cases_averted_perVG)
      # setorder (vaccine_impact_tab, - dalys_averted_perVG)

      # add ranking column (countries with high vaccine impact to low vaccine impact)
      vaccine_impact_level [, ranking := 1:length (vaccine_impact_level$country_code)]

      # estimate concentration index
      con_index <<- with (vaccine_impact_level, ci (x = ranking, y = coverage, type = "CIc"))

      # print concentration index
      print ( c (nrow (vaccine_impact_level), level) )
      print (summary (con_index))
      
      # make sure that variables in the next steps are within scope
      level_toplevel <<- level

      assign (paste0 ("subplot_", subplot_number), as.ggplot (~my_con_index_plot (con_index_a = con_index,
                                                                                  level_a     = level_toplevel,
                                                                                  level_name = " Region")))
    }
  }

  con_curve_level <- plot_grid (subplot_1, subplot_2, subplot_3, subplot_4, subplot_5, ncol = 2)

  # save plot as png file
  ggsave (filename = paste0 ("../figures/Figure_concentration_curve_regions_age",
                             vaccination_age, "_", vaccine, ".png"),
          plot     = con_curve_level,
          dpi      = 600,
          bg       = 'white',
          width    = 11,
          height   = 16,
          units    = "in")
  # ----------------------------------------------------------------------------
  
  # return concentration index estimates (includes mean and 95% confidence interval)
  return (con_index_all)

} # end of function -- concentration_index_curve
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# generate plot -- concentration indices by year
# note: function not further used in this analysis
# ------------------------------------------------------------------------------
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
# -----------------------------------------------------------------------------
world_bank_income_level <- function (countries_dt) {
  
  # download updated country and region information from World Bank API
  wb_dt <- wb_countries ()
  
  # append world bank income level to countries 
  countries_wb_dt <- merge (x   = countries_dt, 
                           y    = wb_dt, 
                           by.x = "country_code", 
                           by.y = "iso3c")
  
  # extract requisite columns: iso3 code, country name, income level
  countries_wb_dt <- countries_wb_dt [, c("country_code", "Country", "income_level")]
  
  # read WHO region-country table
  who_dt <- fread (file = "../input/who_regions.csv")
  
  # append WHO region to countries 
  countries_wb_who_dt <- merge (x   = countries_wb_dt, 
                               y    = who_dt, 
                               by.x = "country_code", 
                               by.y = "Code")
  
  # extract requisite columns: iso3 code, country name, income level
  countries_wb_who_dt <- countries_wb_who_dt [, c("country_code", "Country", "income_level", "WHO region")]
  
  # renames column names
  setnames (countries_wb_who_dt, 
            old = c("country_code", "income_level"),
            new = c("ISO3 country code", "Income level") )
  
  return (countries_wb_who_dt)
  
}  # end of function -- world_bank_income_level
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# scatter plot of vaccine coverage versus impact
# ------------------------------------------------------------------------------
scatter_plot <- function (vaccine_impact_coverage_tab,
                          scatter_plot_file) {
  
  plotwhat <- c("cases_averted_perVG",
                "deaths_averted_perVG",
                "yld_averted_perVG",
                "yll_averted_perVG",
                "dalys_averted_perVG")
  
  y_axis <- c("cases averted",
              "deaths averted",
              "YLDs averted",
              "YLLs averted",
              "DALYs averted")
  
  plot_list = list ()
  
  pdf (paste0 (scatter_plot_file, ".pdf"))
  
  plot_list <- lapply (1:length(plotwhat), function (i) {
    
    # which burden to plot
    toplot = plotwhat[i]
    
    # generate plot
    fig <- ggplot (vaccine_impact_coverage_tab, 
                   aes (x = coverage*100, y = get(toplot))) + 
      geom_point (aes (color = "red")) +
      geom_text_repel (aes (label = country_code), 
                       size = 3, 
                       max.overlaps = 84) +
      labs (
        x = "HPV vaccine coverage (average percentage during 2010-2022)",
        y = paste0 ("Vaccine impact (", y_axis [i], " per 1000 vaccinated girls)"),
        title = "HPV vaccine coverage and impact on reducing cervical cancer burden"
      ) + 
      theme_classic () + 
      theme (legend.position = "none")
    
    print (fig)
    
    # save plot as png file (for deaths averted on y-axis)
    if (toplot == "deaths_averted_perVG") {
      ggsave (filename = paste0 (scatter_plot_file, ".png"), 
              plot     = fig, 
              dpi      = 600)
    }
    
  })
    
  dev.off ()
  
} # end of function -- scatter_plot
# ------------------------------------------------------------------------------

