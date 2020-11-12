density_plot <- function(data, var, catvar, yvar, logbool) {
  var <- as.name(var)
  #xvar <- as.name(xvar)
  yvar <- as.name(yvar)
  catvar <- as.name(catvar)
  
  plot1 <- ggplot(data)
  
  if(logbool == T) {
    plot1 + geom_density(aes(x=!!var, fill=!!catvar),alpha=.5) +
      theme_minimal() +
      scale_fill_brewer(type="qual", palette = 2) +
      labs(title = "Obervation Density") +
      scale_x_log10(labels = scales::comma)
  } else {
    plot1 + geom_density(aes(x=!!var, fill=!!catvar),alpha=.5) +
      theme_minimal() +
      scale_fill_brewer(type="qual", palette = 2) +
      labs(title = "Observation Density")  +
      scale_x_continuous(labels = scales::comma)
  }

  
  # quality_rating_plt <- plot1 + geom_density(aes(x=!!var, fill=!!catvar),alpha=.5) +
  #   theme_minimal() +
  #   scale_fill_brewer(type="qual", palette = 2)
  
}

count_plot <- function(data, var, catvar, yvar) {
  
  var <- as.name(var)
  #xvar <- as.name(xvar)
  yvar <- as.name(yvar)
  catvar <- as.name(catvar)
  
  msn_qual <- data %>%
    group_by(!!catvar,!!yvar) %>%
    summarise(number = n(), meanval = mean(!!var,na.rm = T))
  
  plot2 <- ggplot(msn_qual)
  plot2 + geom_point(aes(x = !!catvar, y = !!yvar, size = number,color = number)) +
    geom_text(aes(x = !!catvar, y = !!yvar, label = number)) +
    scale_color_distiller(type="seq", palette = 7, direction = 1) +
    scale_size_continuous(range = c(5,20)) +
    labs(title = "Number of Facilities") +
    theme_minimal() +
    theme(legend.position = "none")
}
 

mean_plot <- function(data, var, catvar, yvar) {
  
  var <- as.name(var)
  #xvar <- as.name(xvar)
  yvar <- as.name(yvar)
  catvar <- as.name(catvar)
  
  msn_qual <- data %>%
    group_by(!!catvar,!!yvar) %>%
    summarise(number = n(), meanval = mean(!!var,na.rm = T))
  
  plot2 <- ggplot(msn_qual)
  
  plot2 + geom_point(aes(x = !!catvar, y = !!yvar, size = meanval,color = meanval)) +
    geom_text(aes(x = !!catvar, y = !!yvar, label = round(meanval,1))) +
    scale_color_distiller(type="seq", palette = 5, direction = 1) +
    scale_size_continuous(range = c(5,20)) +
    theme_minimal() +
    labs(title = "Mean Values",subtitle = var) +
    theme(legend.position = "none")
}

hist_plot <- function(data, var, catvar, yvar) {
  
  var <- as.name(var)
  #xvar <- as.name(xvar)
  yvar <- as.name(yvar)
  catvar <- as.name(catvar)
  
plot1 <- ggplot(data)
  
plot1 + geom_histogram(aes(x=rpa_plcd_in_svc_dt, fill = !!catvar),alpha=.8) +
  facet_wrap(vars(!!yvar)) +
  theme_minimal() + 
  scale_fill_brewer(type="qual",palette = 2) +
  labs(title = "Construction Date Histograms by Category", subtitle = yvar, x = "Placed in Service Date")
}


