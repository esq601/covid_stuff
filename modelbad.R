library(tidyverse)
library(lubridate)
library(httr)
library(patchwork)
library(gganimate)
library(transformr)

load("covid.rda")


deaths <- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")))
#GET("https://raw.githubusercontent.com/")
colnames(deaths)



deaths %>%
  select(Province_State,Country_Region, `4/9/20`,`4/10/20`) %>%
  filter(Country_Region == "US") %>%
  #filter(Province_State == "Massachusetts") %>%
  summarise(today = sum(`4/10/20`)-sum(`4/9/20`))
colnames(deaths)

#loc_select <- "Florida"

deaths_long <- function(loc_select = "US") {
  deaths %>%
    filter(case_when(
      loc_select == "US" ~ Country_Region %in% loc_select,
      T ~ Province_State %in% c(loc_select)
    )) %>%
    select(-UID:-Population) %>%
    gather("date","deaths") %>%
    group_by(date) %>%
    summarise(deaths = sum(deaths,na.rm=T)) %>%
    mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
    arrange(date) %>%
    mutate(new_deaths = deaths-lag(deaths))
}

proj_prep_long <- function(loc_select = "United States of America") {
  
  #print(loc_select)
  if(loc_select == "United States of America") {
    loc_select <- "US"
    
    proj_2apr <- read_csv("misc/Hospitalization_all_locs2APR.csv", 
                            col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>%
      select(location,date,deaths_mean,deaths_lower,deaths_upper) %>%
      mutate(model = "02-Apr-20", color = "#3f007d") %>%
      filter(location %in% c(loc_select))
    
    proj_25mar <- read_csv("misc/ihme-covid19_all_locs25mar.csv", 
                          col_types = cols(date_reported = col_date(format = "%Y-%m-%d"))) %>%
      select(location=location_name,date=date_reported,deaths_mean,deaths_lower,deaths_upper) %>%
      mutate(model = "25-Mar-20", color = "black") %>%
      filter(location %in% c(loc_select))
    
    loc_select <- "United States of America"
  } else {
    proj_2apr <- read_csv("misc/Hospitalization_all_locs2APR.csv", 
                          col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>%
      select(location,date,deaths_mean,deaths_lower,deaths_upper) %>%
      mutate(model = "02-Apr-20", color = "#3f007d") %>%
      filter(location %in% c(loc_select))

    
    proj_25mar <- read_csv("misc/ihme-covid19_all_locs25mar.csv", 
                           col_types = cols(date_reported = col_date(format = "%Y-%m-%d"))) %>%
      select(location=location_name,date=date_reported,deaths_mean,deaths_lower,deaths_upper) %>%
      mutate(model = "25-Mar-20", color = "black") %>%
      filter(location %in% c(loc_select))
  }
  #print(loc_select)

  proj_7apr <- read_csv("misc/Hospitalization_all_locs IHME 7APR.csv", 
                          col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
    select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
    mutate(model = "05-Apr-20", color = "#6a51a3")%>%
    filter(location %in% c(loc_select))
  
  proj_8apr <- read_csv("misc/Hospitalization_all_locs_8apr.csv", 
                          col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
    select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
    mutate(model = "08-Apr-20", color = "#807dba")%>%
    filter(location %in% c(loc_select))
  
  proj_10apr <- read_csv("misc/Hospitalization_all_locs10apr.csv", 
                           col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
    select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
    mutate(model = "10-Apr-20", color = "#9e9ac8")%>%
    filter(location %in% c(loc_select))
  
  proj_13apr <- read_csv("misc/Hospitalization_all_locs13apr.csv", 
                           col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
    select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
    mutate(model = "13-Apr-20", color = "#bcbddc")%>%
    filter(location %in% c(loc_select))
  
  proj_17apr <- read_csv("misc/Hospitalization_all_locs_17apr.csv", 
                           col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
    select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
    mutate(model = "17-Apr-20", color = "#dadaeb")%>%
    filter(location %in% c(loc_select))
  
  
  bind_rows(proj_25mar,proj_2apr,proj_7apr,proj_8apr,proj_10apr,proj_13apr,proj_17apr) 
}
  
#proj_prep_long("Texas")
proj_prep <- function(loc_select = "United States of America") {
  proj_prep_long(loc_select) %>%
    group_by(model,date) %>%
    summarise(deaths_mean = sum(deaths_mean),deaths_lower = sum(deaths_lower),deaths_upper = sum(deaths_upper)) %>%
    mutate(deaths_total = cumsum(deaths_mean),deaths_total_upper = cumsum(deaths_upper),deaths_total_lower = cumsum(deaths_lower))
}



# proj_max <- proj_total_sum %>%
#   filter(date == max(date)) %>%
#   ungroup() %>%
#   mutate(model_date = as.Date(model,format="%d-%b-%y")) %>%
#   arrange(model)
# 
# 
# ggplot() + geom_path(data=deaths_long(),aes(x = as.Date(date), y=deaths)) +
#   geom_path(data = proj_max, aes(x=model_date, y = deaths_total), color = "darkred") +
#   scale_x_date(limits = c(as.Date("2020-03-15"),Sys.Date())) +
#   theme_minimal()
#   

#### Indiana stuff, not up to date ####
indiana_state <- read_csv("misc/indiana.csv", col_names = T, 
                          col_types = cols(date = col_date(format = "%m/%d/%Y"))) %>%
  filter(is.na(date)==F) %>%
  pivot_longer(-date,names_to = "as_of",values_to = "new_deaths") %>%
  mutate_at(vars(as_of),as.Date) %>%
  mutate(color = case_when(
    as_of == max(as_of) ~ "Indiana Govt",
    TRUE ~ "Indiana Govt (10-Apr)"
  ),
  size =case_when(
    as_of == max(as_of) ~ 1.5,
    TRUE ~ .75
  )) %>%
  group_by(as_of) %>%
  mutate(deaths = cumsum(new_deaths))

#str(indiana_state)
##indianamanual <- data.frame(date = ("4/12/20"),deaths= 343)

# 
# indiana_worldometer <- deaths %>%
#   filter(Province_State == "Indiana") %>%
#   select(-UID:-Population) %>%
#   gather("date","deaths") %>%
#   #bind_rows(indianamanual) %>%
#   group_by(date) %>%
#   summarise(deaths = sum(deaths)) %>%
#   mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
#   arrange(date) %>%
#   mutate(new_deaths = deaths-lag(deaths)) %>%
#   filter(date >= min(indiana_state$date)) %>%
#   mutate(color = "Johns Hopkins CSSE")
# 
# 
# 
# ind1 <- plot1 + geom_path(data = indiana_state, aes(x=date,y=new_deaths,color=color,size=size)) +
#   geom_path(data = indiana_worldometer, aes(x=date,y=new_deaths,color="Johns Hopkins CSSE"),size=1.5) +
#   ggthemes::theme_fivethirtyeight() +
#   scale_color_manual(name = "Source",limits = c("Indiana Govt (10-Apr)" ,"Indiana Govt" ,"Johns Hopkins CSSE"), values = c("grey60", "darkblue", 'darkred')) +
#   labs(title = "Daily Death Counts",subtitle="Indiana adjusts past data based on date of death while JHU CSSE\n adds the revised cumulative number to the previous total",
#        y="Daily Deaths Reported",x="Date") +
#   scale_size_identity()
# 
# #ind1
# 
# ind2 <- plot1 + geom_path(data = indiana_state, aes(x=date,y=deaths,color=color,size=size)) +
#   geom_path(data = indiana_worldometer, aes(x=date,y=deaths,color="Johns Hopkins CSSE"),size=1.5) +
#   ggthemes::theme_fivethirtyeight() +
#   scale_color_manual(name = "Source",limits = c("Indiana Govt (10-Apr)" ,"Indiana Govt" ,"Johns Hopkins CSSE"), values = c("grey60", "darkblue", 'darkred')) +
#   labs(title = "Cumulative Death Counts",subtitle="Both datasets maintain the same total deaths; however, each reporting method\n creates a different total death growth rates",
#        y="Cumulative Deaths",x="Date") +
#   scale_size_identity()
# 
# 
# 
# 
# colnames(indiana_updates)
# indiana_updates <- read_csv("misc/indiana.csv", col_names = T, 
#                           col_types = cols(date = col_date(format = "%m/%d/%Y"))) %>%
#   filter(is.na(date)==F) %>%
#   mutate(updated_deaths = `4/12/2020`-`4/9/2020`)
# 
# sum(indiana_updates$updated_deaths)
# 
# 
# jhu_update <- indiana_worldometer %>%
#   mutate(updated_deaths = case_when(
#     date <= as.Date("2020-04-10") ~ 0,
#     TRUE ~ new_deaths
#   ))
# 
# 
# ind3 <-  plot1 + geom_tile(data = indiana_updates, aes(x=date, y = "Indiana Govt", fill=updated_deaths,color=updated_deaths)) +
#   geom_tile(data = jhu_update,aes(x=date, y ="JHU Dataset", fill = updated_deaths,color=updated_deaths)) +
#   scale_fill_distiller(name = "Updated Deaths",type="seq", palette = 4,direction = 1,limits=c(1,max(jhu_update$updated_deaths)),na.value = "transparent") +
#   ggthemes::theme_fivethirtyeight() +
#   scale_color_gradient(low = "black", high = "black", na.value = "transparent",limits = c(1,max(jhu_update$updated_deaths)),guide = F) +
#   scale_x_date(limits = c(as.Date("2020-03-20"),as.Date("2020-04-13"))) +
#   labs(title = "Differences in Daily Deaths From 10-Apr to 12-Apr Data",
#        subtitle = "All new deaths in the JHU datasets were assigned to the dates added, 11-12 April. The Indiana government data made updates to past date totals with a general lag of 1-2 weeks.")
# 
# #cowplot::plot_grid(ind1,ind2,ind3,nrow=2,rel_widths = c(2,1),align="hv")
# 
# patchwork1 <- (ind1 + ind2) / ind3
# 
# patchwork1 + plot_annotation(
#   title = 'Data Source Comparison for Indiana COVID-19 Deaths',
#   subtitle = 'The different methods of compiling COVID-19 data can produce very different pictures of the actual situation.',
#   caption = "Data accessed 2020-04-12 -- Sources: https://coronavirus.in.gov/\nhttps://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/\ncsse_covid_19_time_series/time_series_covid19_deaths_US.csv" 
#   ) & ggthemes::theme_fivethirtyeight()
#save(file = "indiana_10apr",deaths,indiana_state,indiana_worldometer)



### New York ####
# 
# new_york_city_raw <- read_csv(content(GET("https://raw.githubusercontent.com/nychealth/coronavirus-data/master/case-hosp-death.csv"))) 
# 
# colnames(new_york_city_raw) <- c("date","cases","hosp","DEATH_COUNT")
# 
# new_york_city <- new_york_city_raw %>%
#   mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
#   mutate_if(is.character,as.numeric) %>%
#   mutate(new_deaths = ifelse(is.na(DEATH_COUNT), 0, DEATH_COUNT)) %>%
#   mutate(deaths = cumsum(new_deaths))
# 
# 
# 
# 
# new_york_jhu <- deaths %>%
#   filter(Province_State == "New York", Admin2 == "New York") %>%
#   select(-UID:-Population) %>%
#   gather("date","deaths") %>%
#   group_by(date) %>%
#   summarise(deaths = sum(deaths)) %>%
#   mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
#   arrange(date) %>%
#   mutate(new_deaths = deaths-lag(deaths)) %>%
#   filter(date >= min(indiana_state$date)) %>%
#   mutate(color = "Johns Hopkins CSSE")
# 
# sum(new_york_city$DEATH_COUNT,na.rm=T)
# sum(new_york_jhu$new_deaths)
# 
# plot1 + geom_path(data=new_york_city, aes(x=date, y = new_deaths),color="darkgreen") +
#   geom_path(data=new_york_jhu, aes(x=date, y = new_deaths),color="darkorange")
# 
# plot1 + geom_path(data=new_york_city, aes(x=date, y = deaths),color="darkgreen") +
#   geom_path(data=new_york_jhu, aes(x=date, y = deaths),color="darkorange")
# 
# 
state_pop <- deaths %>%
  group_by(Province_State) %>%
  #filter(Province_State %in% c(loc_place)) %>%
  summarise(population = sum(Population, na.rm=T))

### Deaths vs Projections ####

state_prep <- function(loc_place="United States of America", title,y_upper=NULL) {
  
  state_pop <- deaths %>%
    filter(Province_State %in% c(loc_place)) %>%
    summarise(population = sum(Population, na.rm=T))

  proj_total_tx <- proj_prep_long(loc_place)
  
  
  proj_total_sumtx <- proj_prep(loc_place)
  
  print(summary(proj_total_tx))
  
  if(loc_place != "United States of America") {
    texas_jhu <- deaths %>%
      filter(Province_State %in% c(loc_place)) %>%
      select(-UID:-Population) %>%
      gather("date","deaths") %>%
      group_by(date) %>%
      summarise(deaths = sum(deaths)) %>%
      mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
      arrange(date) %>%
      mutate(new_deaths = deaths-lag(deaths)) %>%
      filter(date >= min(indiana_state$date)) %>%
      mutate(color = "Johns Hopkins CSSE")
  } else {
    texas_jhu <- deaths %>%
      select(-UID:-Population) %>%
      gather("date","deaths") %>%
      group_by(date) %>%
      summarise(deaths = sum(deaths)) %>%
      mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
      arrange(date) %>%
      mutate(new_deaths = deaths-lag(deaths)) %>%
      filter(date >= min(indiana_state$date)) %>%
      mutate(color = "Johns Hopkins CSSE")
  }


print(c(unique(proj_total_tx$color)))
print(summary(texas_jhu))

  plot_daily <- ggplot() + geom_path(data=proj_total_sumtx, aes(x=date,y=deaths_mean,color=model),size=1.5) +
    #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_mean,ymin=deaths_lower,fill=model),alpha=.15) +
    #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_upper,ymin=deaths_mean,fill=model),alpha=.15) +
    geom_point(data=texas_jhu,aes(x = as.Date(date), y=new_deaths,color="Actual"),size=1.5) +
    geom_smooth(data=texas_jhu,aes(x = as.Date(date), y=new_deaths,color="LOESS Smoother"),se=F,size=1.5) +
    theme_minimal() +
    scale_fill_brewer(type = "qual", palette =2) +
    #scale_color_brewer(type = "qual", palette =4) +
    scale_color_manual(limits = c(unique(proj_total_tx$model),"Actual","LOESS Smoother"), values = c(unique(proj_total_tx$color),"#fdb863","#e08214")) +
    
    #scale_x_date(limits = c(as.Date("2020-03-25"),max(proj_total_sum$date)))
    scale_x_date(limits = c(as.Date("2020-03-13"),as.Date("2020-06-08"))) +
    labs(title = title, x = element_blank(), y= " Daily Deaths", color = "IHME Model Prediction", fill = "IHME 95% CI",
         subtitle = paste("Total Population: ",scales::comma(state_pop$population),sep="")) +
    theme(plot.title  = element_text(hjust=.5))
  
  if(is.null(y_upper) == F) {
    plot_daily + coord_cartesian(ylim = c(0,y_upper))
  } else {
    plot_daily
  }
}

state_prep(loc_place=c("New Jersey"),title="Texas Daily Deaths vs. Projections")


state_prep_cum <- function(loc_place="United States of America", title,y_upper,pop_val) {
  proj_total_tx <- proj_prep_long(loc_place)
  proj_total_sumtx <- proj_prep(loc_place)
  
  
  if(loc_place != "United States of America") {
    texas_jhu <- deaths %>%
      filter(Province_State %in% c(loc_place)) %>%
      select(-UID:-Population) %>%
      gather("date","deaths") %>%
      group_by(date) %>%
      summarise(deaths = sum(deaths)) %>%
      mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
      arrange(date) %>%
      mutate(new_deaths = deaths-lag(deaths)) %>%
      filter(date >= min(indiana_state$date)) %>%
      mutate(color = "Johns Hopkins CSSE")
  } else {
    texas_jhu <- deaths %>%
      select(-UID:-Population) %>%
      gather("date","deaths") %>%
      group_by(date) %>%
      summarise(deaths = sum(deaths)) %>%
      mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
      arrange(date) %>%
      mutate(new_deaths = deaths-lag(deaths)) %>%
      filter(date >= min(indiana_state$date)) %>%
      mutate(color = "Johns Hopkins CSSE")
  }
  

  #print(unique(proj_totaltx$location))
  

  ggplot() + geom_path(data=proj_total_sumtx, aes(x=date,y=deaths_total,color=model),size=1.5) +
    #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_mean,ymin=deaths_lower,fill=model),alpha=.15) +
    #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_upper,ymin=deaths_mean,fill=model),alpha=.15) +
    geom_path(data=texas_jhu,aes(x = as.Date(date), y=deaths,color="Actual"),size=2) +
    #geom_smooth(data=texas_jhu,aes(x = as.Date(date), y=deaths,color="LOESS Smoother"),se=F,size=1.5) +
    theme_minimal() +
    scale_fill_brewer(type = "qual", palette =2) +
    #scale_color_brewer(type = "qual", palette =4) +
    scale_color_manual(limits = c(unique(proj_total_tx$model),"Actual","LOESS Smoother"), values = c(unique(proj_total_tx$color),"#fdb863","#e08214")) +
    coord_cartesian(ylim = c(0,y_upper)) +
    #scale_x_date(limits = c(as.Date("2020-03-25"),max(proj_total_sum$date)))
    scale_x_date(limits = c(as.Date("2020-03-13"),as.Date("2020-06-08"))) +
    labs(x = element_blank(), y= " Cumulative Deaths", color = "IHME Model Prediction", fill = "IHME 95% CI")
}




governers <- read_csv(content(GET("https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/us-governors/data/us-governors.csv")))


governers[17,7] <- "democrat"
governers[19,7] <- "democrat"




repub <- governers %>%
  filter(party == "republican") %>%
  select(state_name) %>%
  left_join(state_pop, by = c("state_name" = "Province_State"))

dems <- governers %>%
  filter(party == "democrat") %>%
  select(state_name) %>%
  #filter(state_name != "New York") %>%
  left_join(state_pop, by = c("state_name" = "Province_State"))

sum(repub$population)
sum(dems$population)

rep_chrt <- state_prep(repub$state_name,title= "GOP States",y_upper=1500)
dem_chrt <- state_prep(dems$state_name, title = "Dem States (Minus NY)",y_upper=1500)

tn_chrt <- state_prep("Tennessee",y_upper=200,title="Tennessee", pop_val = filter(state_pop,Province_State=="Tennessee")$population)
ky_chrt <- state_prep("Kentucky",y_upper=200,title="Kentucky", pop_val = filter(state_pop,Province_State=="Kentucky")$population)

state_prep("New Jersey",y_upper=NA,title="New Jersey", pop_val = filter(state_pop,Province_State=="New Jersey")$population)


rep_chrt_cum <- state_prep_cum(repub$state_name,title= "GOP States",y_upper=50000,pop_val = sum(repub$population))
dem_chrt_cum <- state_prep_cum(dems$state_name,title= "Dem States (Minus NY)",y_upper=50000,pop_val = sum(dems$population))

bothchrt <- (rep_chrt + dem_chrt) / (rep_chrt_cum + dem_chrt_cum)
bothchrt + plot_layout(guides = 'collect') +
  plot_annotation(
  title = 'COVID-19 Projections and Deaths by State Governor\'s Party',
  caption = paste("Data Source: JHU Time Series and IHME Model datasets as of: ",Sys.Date(),sep=""),
  theme = theme(plot.title = element_text(size = 22,hjust=.5, face="bold"))
)

tn_chrt + ky_chrt + plot_layout(guides = 'collect')

fips_states <- read_csv("misc/uszips.csv") %>%
  mutate(area = population / density) %>%
  mutate(area = case_when(
    is.infinite(area) == T ~ 0,
    is.nan(area) == T ~ 0,
    T ~ area
  )) %>%
  group_by(county_fips) %>%
  summarise(population = sum(population),area=sum(area,na.rm=T)) 


#View(filter(fips_states,county_fips==36061))

ny_test <- deaths %>%
  filter(Province_State %in% c("Virginia","New York")) %>%
  #group_by(Province_State) %>%
  select(Province_State,FIPS,Combined_Key,Population,ends_with("20")) %>%
  pivot_longer(cols=ends_with("20"),"date",values_to = "deaths") %>%
  group_by(Province_State,FIPS,Combined_Key,Population,date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
  filter(date==max(date)) %>%
  left_join(fips_states, by = c("FIPS" = "county_fips")) %>%
  summarise(deaths= mean(deaths), population = sum(population, na.rm=T),area = sum(area, na.rm=T)) %>%
  mutate(density = population/area) %>%
  left_join(governers, by = c("Province_State" = "state_name"))


population_comp <- function(...) {
  df1 <- deaths %>%
    filter(Province_State %in% governers$state_name) %>%
    mutate(color = case_when(
      Province_State %in% c(...) ~ "darkblue",
      T ~ "grey80")
      )%>%
    #group_by(Province_State) %>%
    select(Province_State,FIPS,Combined_Key,Population,color,ends_with("20")) %>%
    pivot_longer(cols=ends_with("20"),"date",values_to = "deaths") %>%
    group_by(Province_State,FIPS,Combined_Key,Population,color,date) %>%
    summarise(deaths = sum(deaths)) %>%
    mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
    filter(date==max(date)) %>%
    left_join(fips_states, by = c("FIPS" = "county_fips")) %>%
    summarise(deaths= mean(deaths), population = sum(population, na.rm=T),area = sum(area, na.rm=T)) %>%
    mutate(density = population/area,death100k = (deaths/population)*1000000) %>%
    left_join(governers, by = c("Province_State" = "state_name")) %>%
    arrange(desc(color))%>%
    mutate(partycolor = case_when(
      party == "democrat" ~ "blue",
      party == "republican" ~ "red",
      T ~ "green"
    )) %>%
    filter(deaths >0 & density > 0)
  
  df1a <- df1 %>%
    filter(color == "darkblue")
  
  df1b <- df1 %>%
    filter(color == "grey80")
  

  model1 <- df1 %>% 
    group_by(Province_State) %>% 
    do(lm(death100k~density, data=.) %>% coef %>% as_tibble)
  
  model2 <- lm(death100k~density, data = df1)

  print(summary(model2))
  #print(summary(df1$density))
  
  
  print(
    df1 %>%
      group_by(party) %>%
      summarise(`Number of Counties` = n(),
                `Total Area` = sum(area, na.rm=T),
                `Mean Area` = mean(area, na.rm=T),
                `Median Area` = median(area, na.rm=T),
                `Total Population` = sum(population, na.rm=T),
                #`Mean Population/County`= mean(population, na.rm=T),
                `Mean Population Density/County`= `Total Population` / `Total Area`,
                `Median Population Density/County`= median(density, na.rm=T),
                `10% Quantile` = quantile(density, .1,na.rm=T),
                `90% Quantile` = quantile(density, .9,na.rm=T)))
  
  plotly::ggplotly(ggplot() +
    # geom_density2d(aes(x=`density`,y=`deaths`)) 
    # geom_density(aes(x=density,fill=party),color="transparent",alpha=.5) +
    # scale_x_log10() +
    # theme_minimal() +
    # scale_fill_manual(breaks = c("republican","democrat"),values=c("blue","red")) #+
    #geom_vline(aes(xintercept=mean(density),color=party))
    
    geom_point(data = df1b, aes(x=density, y = death100k, county = Combined_Key,color=partycolor),size=2) +
    #geom_smooth(data = df1b, method= "lm", aes(x=density, y = death100k,color=color), se=F) +
    geom_point(data = df1a, aes(x=density, y = death100k, county = Combined_Key,color=partycolor),size=2) +
    #geom_smooth(data = df1a, method= "lm", aes(x=density, y = death100k,group = Province_State,color=color), se=F) +
    theme_minimal() +
    scale_color_manual(labels =df1$party, values = df1$partycolor,limits = df1$partycolor) +
    coord_cartesian(ylim=c(0,1500)) +
    scale_x_log10() +
    #scale_y_log10() +
    
    labs(x = "Population Density (People/km^2)", y = "Deaths / 1M Residents", title = "Population Adjusted Deaths by Density",
         subtitle = "For Individual Counties", color = "State",caption = paste("Data Source: JHU Time Series as of: ",Sys.Date(),sep=""))
  )
}


population_comp(filter(governers,state_name!="New York")$state_name)


population_comp("New Jersey")

52.2/38.3

plot1 + geom_path(data=proj_total_sumtx, aes(x=date,y=deaths_total,color=model)) +
  #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_total,ymin=deaths_total_lower,fill=model),alpha=.2) +
  #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_total_upper,ymin=deaths_total,fill=model),alpha=.2) +
  geom_path(data=texas_jhu,aes(x = as.Date(date), y=deaths)) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette =6) +
  scale_color_brewer(type = "qual", palette =6) +
  scale_x_date(limits = c(Sys.Date()-lubridate::days(14),Sys.Date()+lubridate::days(50))) +
  #scale_x_date(limits = c(Sys.Date()-lubridate::days(3),Sys.Date()+lubridate::days(55))) +
  coord_cartesian(ylim = c(0,5000)) +
  #geom_label(data = subset(proj_total_sum,date==Sys.Date()), aes(x=Sys.Date(),y=deaths_total,color = model, label = round(deaths_total,0)),hjust=0) +
  geom_text(data = subset(deaths_long,date == max(deaths_long$date)),aes(x=max(date),y=deaths,label=round(deaths,0)),hjust=0)

austin_jhu <- deaths %>%
  filter(Province_State == "Texas", Admin2 == "Austin") %>%
  select(-UID:-Population) %>%
  gather("date","deaths") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths-lag(deaths)) %>%
  filter(date >= min(indiana_state$date)) %>%
  mutate(color = "Johns Hopkins CSSE")
  
  
  
  
### Sweden ####
proj_2aprsw <- read_csv("misc/Hospitalization_all_locs2APR.csv", 
                        col_types = cols(date = col_date(format = "%Y-%m-%d"))) %>%
  select(location,date,deaths_mean,deaths_lower,deaths_upper) %>%
  mutate(model = "02-Apr-20") %>%
  filter(location %in% "Sweden")


proj_7aprsw <- read_csv("misc/Hospitalization_all_locs IHME 7APR.csv", 
                        col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
  select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
  mutate(model = "05-Apr-20")%>%
  filter(location %in% "Sweden")

proj_8aprsw <- read_csv("misc/Hospitalization_all_locs_8apr.csv", 
                        col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
  select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
  mutate(model = "08-Apr-20")%>%
  filter(location %in% "Sweden")

proj_10aprsw <- read_csv("misc/Hospitalization_all_locs10apr.csv", 
                         col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
  select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
  mutate(model = "10-Apr-20")%>%
  filter(location %in% "Sweden")

proj_13aprsw <- read_csv("misc/Hospitalization_all_locs13apr.csv", 
                         col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
  select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
  mutate(model = "13-Apr-20")%>%
  filter(location %in% "Sweden")

proj_17aprsw <- read_csv("misc/Hospitalization_all_locs_17apr.csv", 
                         col_types = cols(date = col_date(format = "%Y-%m-%d")))%>%
  select(location=location_name,date,deaths_mean,deaths_lower,deaths_upper) %>%
  mutate(model = "17-Apr-20")%>%
  filter(location %in% "Sweden")

proj_totalsw <- bind_rows(proj_2aprsw,proj_7aprsw,proj_8aprsw,proj_10aprsw,proj_13aprsw,proj_17aprsw) 

deathssw <- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")))
head(deathssw)
sweden_jhu <- deathssw %>%
  filter(`Country/Region` == "Sweden") %>%
  select(-`Province/State`:-Long) %>%
  gather("date","deaths") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths-lag(deaths)) %>%
  filter(date >= min(indiana_state$date)) %>%
  mutate(color = "Johns Hopkins CSSE")


plot1 + geom_path(data=proj_totalsw, aes(x=date,y=deaths_mean,color=model),linetype="longdash",size=.65) +
  #geom_ribbon(data=proj_totalsw,aes(x=date,ymax=deaths_mean,ymin=deaths_lower,fill=model),alpha=.15) +
  #geom_ribbon(data=proj_totalsw,aes(x=date,ymax=deaths_upper,ymin=deaths_mean,fill=model),alpha=.15) +
  geom_path(data=sweden_jhu,aes(x = as.Date(date), y=new_deaths,color="Actual"),size=1) +
  geom_smooth(data=sweden_jhu,aes(x = as.Date(date), y=new_deaths,color="LOESS Smoother"),se=F,span=1.5,size=1) +
  theme_minimal() +
  scale_fill_brewer(type = "qual", palette =2) +
  scale_color_brewer(type = "qual", palette =2) +
  coord_cartesian(ylim = c(0,600)) +
  #scale_x_date(limits = c(as.Date("2020-03-25"),max(proj_total_sum$date)))
  scale_x_date(limits = c(as.Date("2020-04-03"),as.Date("2020-06-08"))) +
  labs(title = "Sweden Daily COVID-19 Deaths", x = "Date", y= "Deaths", color = "IHME Model Prediction", fill = "IHME 95% CI")




#### New pred vs actuals ####

state_prep_new <- function(loc_place="United States of America", title,model_filter = "25-Mar-20",y_upper=NULL) {
  
  state_pop <- deaths %>%
    filter(Province_State %in% c(loc_place)) %>%
    summarise(population = sum(Population, na.rm=T))
  
  proj_total_tx <- proj_prep_long(loc_place) %>%
    filter(model %in% model_filter)
  
  
  proj_total_sumtx <- proj_prep(loc_place) %>%
    filter(model %in% model_filter)
  
  #print(summary(proj_total_tx))
  
  if(loc_place != "United States of America") {
    texas_jhu <- deaths %>%
      filter(Province_State %in% c(loc_place)) %>%
      select(-UID:-Population) %>%
      gather("date","deaths") %>%
      group_by(date) %>%
      summarise(deaths = sum(deaths)) %>%
      mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
      arrange(date) %>%
      mutate(new_deaths = deaths-lag(deaths)) %>%
      filter(date >= min(indiana_state$date)) %>%
      mutate(color = "Johns Hopkins CSSE")
  } else {
    texas_jhu <- deaths %>%
      select(-UID:-Population) %>%
      gather("date","deaths") %>%
      group_by(date) %>%
      summarise(deaths = sum(deaths)) %>%
      mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
      arrange(date) %>%
      mutate(new_deaths = deaths-lag(deaths)) %>%
      filter(date >= min(indiana_state$date)) %>%
      mutate(color = "Johns Hopkins CSSE")
  }
  
  
  print(c(unique(proj_total_tx$color)))
  print(summary(texas_jhu))
  
  plot_daily <- ggplot() + geom_path(data=proj_total_sumtx, aes(x=date,y=deaths_mean,color=model),size=1.5) +
    #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_mean,ymin=deaths_lower,fill=model),alpha=.15) +
    #geom_ribbon(data=proj_total_sumtx,aes(x=date,ymax=deaths_upper,ymin=deaths_mean,fill=model),alpha=.15) +
    geom_point(data=texas_jhu,aes(x = as.Date(date), y=new_deaths,color="Actual"),size=1.5) +
    geom_smooth(data=texas_jhu,aes(x = as.Date(date), y=new_deaths,color="LOESS Smoother"),se=F,size=1.5) +
    theme_minimal() +
    scale_fill_brewer(type = "qual", palette =2) +
    #scale_color_brewer(type = "qual", palette =4) +
    scale_color_manual(limits = c(unique(proj_total_tx$model),"Actual","LOESS Smoother"), values = c(unique(proj_total_tx$color),"#fdb863","#e08214")) +
    
    #scale_x_date(limits = c(as.Date("2020-03-25"),max(proj_total_sum$date)))
    scale_x_date(limits = c(as.Date("2020-03-13"),as.Date("2020-06-08"))) +
    labs(title = title, x = element_blank(), y= " Daily Deaths", color = "IHME Model Prediction", fill = "IHME 95% CI",
         subtitle = paste("Total Population: ",scales::comma(state_pop$population),sep="")) +
    theme(plot.title  = element_text(hjust=.5))
  
  if(is.null(y_upper) == F) {
    plot_daily + coord_cartesian(ylim = c(0,y_upper))
  } else {
    plot_daily
  }
}

state_prep_new(dems$state_name,title="Texas Models")


repub_model <- proj_prep_long(loc_select = repub$state_name) %>%
  filter(model == "02-Apr-20") %>%
  group_by(date) %>%
  summarise(deaths_mean = sum(deaths_mean)) %>%
  ungroup() %>%
  mutate(deaths_cum = cumsum(deaths_mean),date_mod = date,datenum = as.numeric(date),party="gop") 



dem_model <- proj_prep_long(loc_select =subset(dems,state_name != "New York")$state_name) %>%
  filter(model == "02-Apr-20")%>%
  group_by(date) %>%
  summarise(deaths_mean = sum(deaths_mean)) %>%
  ungroup() %>%
  mutate(deaths_cum = cumsum(deaths_mean),date_mod = date,datenum = as.numeric(date),party="dem")

ny_model <- proj_prep_long(loc_select ="New York") %>%
  filter(model == "02-Apr-20")%>%
  group_by(date) %>%
  summarise(deaths_mean = sum(deaths_mean)) %>%
  ungroup() %>%
  mutate(deaths_cum = cumsum(deaths_mean),date_mod = date,datenum = as.numeric(date),party="ny")

full_model <- proj_prep_long(loc_select =governers$state_name) %>%
  filter(model == "02-Apr-20")%>%
  group_by(date) %>%
  summarise(deaths_mean = sum(deaths_mean)) %>%
  ungroup() %>%
  mutate(deaths_cum = cumsum(deaths_mean),date_mod = date,datenum = as.numeric(date),party="all")

repub_actual <- deaths %>%
  filter(Province_State %in% repub$state_name) %>%
  select(-UID:-Population) %>%
  gather("date","deaths") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths-lag(deaths),party="gop",datenum_act = as.numeric(date))

dem_actual <- deaths %>%
  filter(Province_State %in% subset(dems,state_name != "New York")$state_name) %>%
  select(-UID:-Population) %>%
  gather("date","deaths") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths-lag(deaths),party="dem",datenum_act = as.numeric(date))

ny_actual <- deaths %>%
  filter(Province_State %in% "New York") %>%
  select(-UID:-Population) %>%
  gather("date","deaths") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths-lag(deaths),party="ny",datenum_act = as.numeric(date))

full_actual <- deaths %>%
  filter(Province_State %in% governers$state_name) %>%
  select(-UID:-Population) %>%
  gather("date","deaths") %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y")) %>%
  arrange(date) %>%
  mutate(new_deaths = deaths-lag(deaths),party="all",datenum_act = as.numeric(date))

actual <- rbind(repub_actual,dem_actual,ny_actual)

#repub


df_hline <- actual %>%
  select(date,party,deaths) %>%
  pivot_wider(names_from ='party', values_from  = 'deaths') %>%
  mutate(datenum_act = as.numeric(date))

full_df_hline <- full_actual %>%
  select(date,party,deaths) %>%
  pivot_wider(names_from ='party', values_from  = 'deaths') %>%
  mutate(datenum_act = as.numeric(date))

popdf <- data.frame(party = c("gop","dem","ny"),pop = scales::comma(c(sum(repub$population),sum(subset(dems,state_name != "New York")$population),
                      sum(subset(dems,state_name == "New York")$population))))


ggplot() +
  geom_path(data=repub_model, aes(x=date, y = deaths_cum), color = "darkblue", size=2) + 
  geom_path(data=dem_model, aes(x=date, y = deaths_cum), color = "darkorange", size=2) +
  theme_minimal() +
  scale_x_date(limits = c(as.Date("2020-03-20"),as.Date("2020-07-04"))) +
  labs(title = "IHME Prediction (25-March) for Two Subsets of States", subtitle = "Population split 52/48 of total")

df_comp_gop <- actual %>%
left_join(repub_model, by = c("datenum_act"="datenum")) %>%
filter(party.x=="gop") %>%
mutate(party = party.x)
df_comp_dem <- actual %>%
left_join(dem_model, by = c("datenum_act"="datenum")) %>%
filter(party.x=="dem") %>%
mutate(party = party.x)

df_comp_ny <- actual %>%
  left_join(ny_model, by = c("datenum_act"="datenum")) %>%
  filter(party.x=="ny") %>%
  mutate(party = party.x)

df_comp_full <- full_actual %>%
  left_join(full_model, by = c("datenum_act"="datenum")) %>%
  filter(party.x=="all") %>%
  mutate(party = party.x)


aniplot <- ggplot(dat = data.frame(actual), aes(x=date, y = deaths, color=party,group=2)) +
  geom_line(size=2) +
  geom_line(data=data.frame(repub_model), aes(x=date_mod, y = deaths_cum,group=1, color = "gop"), size=.75,inherit.aes =F,linetype="dashed") +
  geom_line(data=data.frame(dem_model), aes(x=date_mod, y = deaths_cum,group="model", color = "dem"), size=.75,inherit.aes =F,linetype="dashed") +
  geom_line(data=data.frame(ny_model), aes(x=date_mod, y = deaths_cum,group="model", color = "ny"), size=.75,inherit.aes =F,linetype="dashed") +
  
  geom_hline(data= df_hline, aes(yintercept = dem, color = "dem",group=2),linetype='dotted') +
  geom_hline(data= df_hline, aes(yintercept = gop, color = "gop",group=2),linetype='dotted') +
  geom_hline(data= df_hline, aes(yintercept = ny, color = "ny",group=2),linetype='dotted') +
  
  geom_segment(data=df_comp_gop,(aes(x=date.x,xend=date.x, y = deaths, yend = deaths_cum,group=2,
                                     color=ifelse(deaths>deaths_cum,"red","green")))) +
  geom_segment(data=df_comp_dem,(aes(x=date.x,xend=date.x, y = deaths, yend = deaths_cum,group=2,
                                     color=ifelse(deaths>deaths_cum,"red","green"))),lineend = "round") +
  geom_segment(data=df_comp_ny,(aes(x=date.x,xend=date.x, y = deaths, yend = deaths_cum,group=2,
                                     color=ifelse(deaths>deaths_cum,"red","green"))),lineend = "round") +
  
  geom_text(data=popdf, aes(x = as.Date("2020-04-15"), y = 55000 , color=party,label=paste("Pop. ",pop,sep=""))) +
  
  scale_color_manual(breaks = c("dem","gop","ny","red","green"), values = c("blue","red","darkgreen","#f74d1e","#00bf00")) +
  scale_x_date(limits = c(as.Date("2020-03-20"),as.Date("2020-07-04"))) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill="#fffcf5",color="transparent")
  ) +
  annotate(x = as.Date(x="2020-06-15"),y=200,geom="text",label = "u/theesto", alpha=0.07)  +
  labs(title = " US State Political Affiliations and COVID-19 Deaths", y = "Cumulative Deaths",subtitle = "Compared to States' April 2nd IHME Model (Dashed Line)") +
  facet_wrap(~party, nrow=1, labeller = as_labeller(c(dem="Democrat Governors (-NY)",gop="Republican Governors",ny="New York State"))) +
  transition_reveal(datenum_act,range =c(18340,max(actual$datenum_act)))

#aniplot
#enter_appear() +
#labs(title = "Actual Cumulative Deaths", subtitle = "Population split 52/48 of total")
anim <- animate(aniplot,duration = 20,end_pause = 30,start_pause=2,fps=10,rewind=F,width = 600, height = 300)
anim
print(anim)
magick::image_write_gif(anim, path ="states_25jun.gif")

aniplot_full <- ggplot(dat = data.frame(full_actual), aes(x=date, y = deaths, color=party,group=2)) +
  geom_line(size=2) +
  geom_line(data=data.frame(full_model), aes(x=date_mod, y = deaths_cum,group=1, color = "all"), size=.75,inherit.aes =F,linetype="dashed") +

  
  geom_segment(data=df_comp_full,(aes(x=date.x,xend=date.x, y = deaths, yend = deaths_cum,group=2,
                                     color=ifelse(deaths>deaths_cum,"red","green")))) +

  scale_color_manual(breaks = c("all","red","green"), values = c("black","#f74d1e","#00bf00")) +
  scale_x_date(limits = c(as.Date("2020-03-20"),as.Date("2020-07-04"))) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    plot.background = element_rect(fill="#fffcf5",color="transparent")
  ) +
  annotate(x = as.Date(x="2020-06-15"),y=200,geom="text",label = "u/theesto", alpha=0.07)  +
  labs(title = " US State Political Affiliations and COVID-19 Deaths", y = "Cumulative Deaths",subtitle = "Compared to States' April 2nd IHME Model (Dashed Line)") +
  facet_wrap(~party, nrow=1, labeller = as_labeller(c(dem="Democrat Governors (-NY)",gop="Republican Governors",ny="New York State"))) +
  transition_reveal(datenum_act,range =c(18340,max(actual$datenum_act)))

aniplot_full

ggplot(dat = data.frame(test=1:10,y=1:10), aes(x=test,y=y)) +
  geom_point() +
  geom_line(data = data.frame(x2 = 1:10, y = 1:10),
            aes(x = x2, y = y, group = 1)) +
  transition_time(test)
animate(last_plot(), nframes = 50)

death1 <- deaths %>%
  filter(`5/9/20` == max(`5/9/20`))


density_df <- deaths %>%
  filter(Province_State %in% governers$state_name) %>%
  # mutate(color = case_when(
  #   Province_State %in% c(...) ~ "darkblue",
  #   T ~ "grey80")
  # )%>%
  #group_by(Province_State) %>%
  select(Province_State,FIPS,Combined_Key,Population,ends_with("20")) %>%
  pivot_longer(cols=ends_with("20"),"date",values_to = "deaths") %>%
  group_by(Province_State,FIPS,Combined_Key,Population,date) %>%
  summarise(deaths = sum(deaths)) %>%
  mutate(date = as.Date(date,format = "%m/%d/%y"))%>%
  filter(date==max(date)) %>%
  left_join(fips_states, by = c("FIPS" = "county_fips")) %>%
  summarise(deaths= mean(deaths), population = sum(population, na.rm=T),area = sum(area, na.rm=T)) %>%
  rowwise() %>%
  mutate(density = Population/area,death100k = (deaths/Population)*1000000) %>%
  left_join(governers, by = c("Province_State" = "state_name")) %>%
  #arrange(desc(color))%>%
  mutate(partycolor = case_when(
    party == "democrat" ~ "blue",
    party == "republican" ~ "red",
    T ~ "green"
  )) %>%
  filter(deaths >0 & population > 5)




density_df$bins <- cut(density_df$density,breaks = c(0,100,1000,100000))


density_df %>%
  group_by(party,bins) %>%
  summarise(n())

sum(density_df$deaths) / sum(density_df$Population) * 1000000

box1 <- ggplot(density_df, aes(x=bins, y = death100k)) +
  scale_color_brewer(name = "Governor's Party",type="qual", palette = 6, direction = -1) +
  scale_fill_brewer(name = "Governor's Party",type="qual", palette = 6, direction = -1) +
  scale_x_discrete(labels = c("0 to 100", "100 to 1,000", "1,000+"))+
  scale_y_continuous(limits = c(0,2000), oob = scales::squish) +
  labs(x="People per Square km",y = "Deaths / Million People", title = "County Population Density and Death Rates") + 
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

box1 + geom_boxplot(fill="black",alpha=.6)

box1 +geom_boxplot(aes(fill=party,color=party),alpha=.6)


### New bar char ####

state_act <- deaths %>%
  filter(Province_State %in% governers$state_name) %>%
  select(-UID:-Admin2,-Country_Region:-Population)%>%
  pivot_longer(cols = -Province_State,names_to = "date",values_to = "deaths") %>%
  group_by(Province_State) %>%
  filter(date == max(date)) %>%
  summarise(deaths=sum(deaths,na.rm = T),date=max(date)) %>%
  mutate(date = as.Date(date, format="%m/%d/%y"))

str(state_act)


state_pred <- proj_prep_long(loc_select =governers$state_name) %>%
  filter(model == "02-Apr-20") %>%
  group_by(location,date) %>%
  summarise(deaths_mean = sum(deaths_mean)) %>%
  ungroup() %>%
  group_by(location) %>%
  mutate(deaths_cum = cumsum(deaths_mean))

state_total <- state_act %>%
  left_join(state_pred, by = c("Province_State"="location","date")) %>%
  left_join(governers, by = c("Province_State" = "state_name")) %>%
  left_join(state_pop, by="Province_State") %>%
  mutate(error = (deaths_cum-deaths)/(deaths),abs_error = deaths_cum-deaths) %>%
  mutate(pred_mil = 1000000*(deaths_cum/population), act_mil = 1000000 *(deaths/population)) %>%
  mutate(error_mil = (pred_mil-act_mil)/act_mil, abs_error_mil = pred_mil - act_mil) %>%
  select(Province_State,population,party,pred_deaths=deaths_cum,act_deaths=deaths,error,abs_error,pred_mil,act_mil,error_mil,abs_error_mil)
  
sum(state_total$deaths_cum) -sum(state_total$deaths)


ggplot(state_total, aes(y=reorder(Province_State,abs_error),x=abs_error, fill=abs_error)) +
  geom_bar(stat="identity") +
  geom_text(aes(x=ifelse(abs_error<0,0,abs_error),color=party,label=Province_State),hjust=0,size=3) +
  scale_color_manual(breaks = c("democrat","republican"), values = c("blue","red")) + 
  theme_minimal() +
  labs(title="State Performance against Projected (Totals)",x = "Difference between Actual and Projected Deaths", caption = "As of: 11 MAY 20 Data: JHU timeseries; IHME 02 APR Model") +
  scale_x_continuous(limits = c(-12000,12000)) +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  scale_fill_gradient2(low="red",high="blue",mid="green",midpoint = 0)
