

library (grid)
library (ggplotify)
library (cowplot)




x <- con_index

if (!any(class(x) == 'hci')) stop ("Object is not of class hci")
myOrder <- order(x$fractional_rank)
xCoord <- x$fractional_rank[myOrder]
y <- x$outcome[myOrder]
cumdist <- cumsum(y) / sum(y)

plot(c(1,1), xlim = c(0,1), ylim = c(0,1), type = "n", 
     xlab = "fractional rank (countries with high to low vaccine impact)", 
     ylab = "cumulative distribution (vaccine coverage)" , 
     main = "Concentration Curve")
polygon(xCoord, cumdist, col = "light gray")



my_con_index_plot <- function (con_index, 
                               level) {
  
  myOrder <- order (con_index$fractional_rank)
  xCoord  <- con_index$fractional_rank [myOrder]
  y       <- con_index$outcome [myOrder]
  cumdist <- cumsum (y) / sum (y)
  
  plot (c(1,1), xlim = c(0,1), ylim = c(0,1), type = "n", 
        xlab = "fractional rank (countries with high to low vaccine impact)", 
        ylab = "cumulative distribution (vaccine coverage)" , 
        main = paste0 (level, " countries"))
  
  polygon (xCoord, cumdist, col = "light gray")
  
}


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
  con_index <- with (vaccine_impact_level, ci (x = ranking, y = coverage, type = "CIc"))
  
  # print concentration index
  print ( c (nrow (vaccine_impact_level), level) )
  # print (summary (con_index))
  
  assign (paste0 ("subplot_", subplot_number), as.ggplot (~my_con_index_plot (con_index, level)))
}

con_curve_income_levels <- plot_grid (subplot_1, subplot_2, subplot_3, subplot_4, ncol = 2)


