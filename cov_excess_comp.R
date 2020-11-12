library(httr)
library(readr)
library(tidyverse)
library(pins)
library(RCurl)
library(jsonlite)
library(sp)
library(maps)
library(maptools)
library(lubridate)
library(ggmap)
library(sp)
library(extrafont)
library(ggtext)
library(readxl)
library(purrr)

loadfonts(device = "win")

#set_config(config(ssl_verifypeer = 0L))

all_hist <- content(GET("https://data.cdc.gov/resource/muzy-jte6.csv?%24limit=100000"))



all_hist_far <- content(GET("https://data.cdc.gov/resource/3yf8-kanr.csv?%24limit=100000"))

curr_cov <- content(GET("https://data.cdc.gov/resource/r8kw-7aab.csv?%24limit=100000"))


all_age <- content(GET("https://data.cdc.gov/resource/y5bj-9g5w.csv?%24limit=1000000"))

deaths <- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")))

age_dist <- read_csv("~/agedist.csv")

df1 <- all_hist %>%
  mutate(jurisdiction_of_occurrence = case_when(
    jurisdiction_of_occurrence == "New York City" ~ "New York",
    T ~ jurisdiction_of_occurrence
  )) %>%
  filter(jurisdiction_of_occurrence != "United States") %>%
  mutate(month = mmwrweek) %>% #mont(as.Date(week_ending_date, format = "%m/%d/%Y"))) %>%
  mutate(all_cause2 = all_cause - ifelse(is.na(covid_19_u071_multiple_cause_of_death) ==T, 0,covid_19_u071_multiple_cause_of_death),unnatural = all_cause - natural_cause) %>%
  group_by(mmwryear,month,jurisdiction_of_occurrence) %>%
  summarise(num = sum(all_cause, na.rm=T)) %>%
  group_by(mmwryear,jurisdiction_of_occurrence) %>%
  mutate(cumnum = cumsum(num), mean_cum = cumnum / mean(cumnum) )

df2 <- all_hist_far %>%
  filter(jurisdiction_of_occurrence != "United States") %>%
  mutate(jurisdiction_of_occurrence = case_when(
    jurisdiction_of_occurrence == "New York City" ~ "New York",
    T ~ jurisdiction_of_occurrence
  )) %>%
  mutate(unnatural = allcause - naturalcause) %>%
  mutate(month =  mmwrweek) %>% #week(as.Date(weekendingdate, format = "%m/%d/%Y"))) %>%
  group_by(mmwryear,month,jurisdiction_of_occurrence) %>%
  summarise(num = sum(allcause, na.rm= T))%>%
  group_by(mmwryear,jurisdiction_of_occurrence) %>%
  mutate(cumnum = cumsum(num), mean_cum = cumnum / mean(cumnum))

state_pop_year <- read_excel("~/state_pop_year.xlsx",skip=3)

state_pop_year1 <- state_pop_year %>%
  filter(is.na(Census)==F) %>%
  mutate(state = str_remove(`...1`,"\\.")) %>%
  select(state, starts_with("20")) %>%
  pivot_longer(cols = starts_with("20"),names_to = "year",values_to = "population") %>%
  mutate(year = as.numeric(year))

df3 <- bind_rows(df1,df2) %>%
  left_join(state_pop_year1, by = c("jurisdiction_of_occurrence" = "state", "mmwryear" = "year")) %>%
  group_by(jurisdiction_of_occurrence) %>%
  arrange(mmwryear) %>%
  mutate(pop_base = case_when(
    mmwryear == 2014 ~ population,
    T ~ 0
  )) %>%
  mutate(pop_base = max(pop_base)) %>%
  rowwise() %>%
  mutate(pct_change = population / pop_base) %>%
  group_by(jurisdiction_of_occurrence) %>%
  arrange(mmwryear) %>%
  fill(pct_change) %>%
  fill(population) %>%
  mutate(num_adj = num / pct_change) %>%
  group_by(mmwryear,jurisdiction_of_occurrence) %>%
  mutate(cumnum_adj = cumsum(num_adj), mean_cum = cumnum_adj / mean(cumnum_adj))


df_pred <- df3 %>%
  filter(mmwryear > 2014) %>%
  select(year = mmwryear,state = jurisdiction_of_occurrence,num,month,population) %>%
  filter(year != 2020) %>%
  group_by(state,month) %>%
  nest()

pop_fun <- function(df) {
  lm(population ~ ., data = df)
}

mod_fun <- function(df) {
  lm(num ~ year, data = df)
}

a_fun <- function(mod,year) {
  predict(mod,year)
}

b_fun <- function(mod,year) {
  predict(mod,newdata = data.frame(year))
}
#str(state_pop_year1)
pop20 <- state_pop_year1 %>%
  group_by(state) %>%
  nest() %>%
  mutate(model = purrr::map(data,pop_fun)) %>%
  mutate(pred20 = purrr::map_dbl(model,a_fun,year = data.frame(year = 2020)))

pop20df <- pop20 %>%
  select(state,population = pred20) %>%
  mutate(year = 2020)

cases_mapped <- df_pred %>%
  mutate(model = map(data,mod_fun)) %>%
  left_join(pop20df, by = "state") %>%
  mutate(year = 2020) %>%
  rename(population = pred20) %>%
  nest(data_vars = c(year,population)) %>%
  mutate(pred_num = purrr::map_dbl(model,b_fun,year = data_vars)) 

cases_df <- cases_mapped %>%
  select(state,month,num = pred_num)  %>%
  mutate(year = 2020)#,cur_year = map_dbl(model,a_fun,year=2020))


df_joined <- df_pred %>%
  unnest(cols = data) %>%
  select(-population) %>%
  bind_rows(cases_df) %>%
  rename(num_pred = num)



df3a <- df_joined %>%
  filter(state %in% c("Texas","New York")) %>%
  mutate(color = case_when(
    year == 2020 ~ "darkred",
    T ~ "grey70"
  )) %>%
  mutate(size = case_when(
    year == 2020 ~ 1.25,
    T ~ .75
  )) %>%
  group_by(state) %>%
  arrange(month) 

ggplot(df3a, aes(x = month, y = num,group = year,color=color)) +
  geom_path(size = 1.25) +
  scale_color_identity() + 
  scale_size_identity() +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format()) +
  facet_wrap(~state,ncol=1,scales = "free_y") +
  labs(title = "Predicting 2020 Weekly deaths",
       subtitle = "Each week shows total deaths each year from 2015 through 2019.<br> 2020 Predicions in red",
       y = "All Deaths per Week",
       x = "Week of the Year") +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.spacing.x = unit(.5, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold")
  )




deaths <- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")))

deaths_pop <- deaths %>%
  #left_join(population, by = "UID") %>%
  select(state=Province_State, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "deaths", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(state,date) %>%
  summarise(deaths = sum(deaths, na.rm = T)) %>%
  mutate(week = week(date)) %>%
  group_by(state) %>%
  mutate(new_deaths = deaths -lag(deaths, default = 0)) %>%
  
  group_by(state,week) %>%
  summarise(deaths_wk = sum(new_deaths, na.rm = T))
#filter(pop > 0 ) %>%


df_20 <- df_joined %>%
  filter(year == 2020) %>%
  left_join(df3, by = c("state" = "jurisdiction_of_occurrence", "month" , "year" = "mmwryear")) %>%
  left_join(deaths_pop, by = c("state","month" = "week")) %>%
  select(state,year,week = month,num_pred, num_real = num,num_cov = deaths_wk) %>%
  mutate(diff = num_real - num_pred) %>%
  group_by(state) %>%
  arrange(week) %>%
  mutate(num_cov = replace_na(num_cov,0)) %>%
  mutate(cum_diff = cumsum(diff),cum_cov = cumsum(num_cov),cum_pred = cumsum(num_pred),cum_real =cumsum(num_real))


df_test <- df_20 %>%
  filter(state == "Arizona") 


df_past <- df_20 %>%
  filter(week <= 29) %>%
  filter(state %in% c("New York", "New Jersey", "Michigan","Texas","Florida", "Arizona","California","Illinois"))
#filter(state %in% c("Iowa","Missouri","Connecticut","Washington","Oregon","Tennessee","Kentucky","New Mexico"))
p1 <- ggplot(df_past, aes(x = week)) +
  theme_minimal() +
  facet_wrap(~state, scales = "free_y") +
  ggsci::scale_color_jama() +
  scale_y_continuous(labels = scales::comma_format()) +
  #guides(color = guide_legend(label = T)) +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = c(.85,.1),
    legend.spacing.x = unit(.5, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold")
  ) 



p1 +   geom_path(aes(y = diff, color = "Excess Deaths"),size = 1.25) +
  geom_path(aes(y = num_cov, color = "COVID Deaths"),size = 1.25) +
  labs(title = "COVID vs. Excess Deaths", color = "Type:",
       x = "Week of the Year", y = "Number of Deaths",subtitle = "Weekly")

p1 + geom_path(aes(y = cum_diff, color = "Excess Deaths"),size = 1.25) +
  geom_path(aes(y = cum_cov, color = "COVID Deaths"),size = 1.25) +
  labs(title = "COVID vs. Excess Deaths", color = "Type:",
       x = "Week of the Year", y = "Number of Deaths",subtitle = "Cumulative")

df_comp <- df_past %>%
  filter(week == 29) %>%
  mutate(cov_diff = cum_diff - cum_cov) %>%
  mutate(diff_pct = cov_diff / cum_pred) #

df_comp1 <- df_comp%>%
  select(state,cum_diff,cum_cov) %>%
  pivot_longer(cols = contains("cum"))

ggplot(df_comp1, aes( x = reorder(state,-value))) +
  geom_bar(aes(y = value,fill=name), stat = "identity",position = "dodge") +
  #geom_bar(aes(y = cum_cov,fill = "COVID Deaths"),stat = "identity",position = "dodge") +
  scale_y_continuous(labels = scales::comma_format()) +
  ggsci::scale_fill_jama(labels = c("COVID Deaths","Excess Deaths")) +
  theme_minimal() +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.spacing.x = unit(.5, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold"),
    axis.title.x = element_blank()
  ) +
  labs(title = "COVID vs. Excess Deaths", fill = "Type:",
       y = "Number of Deaths",subtitle = "Cumulative Through Week 29")


agedist <- read_csv("~/agedist.csv") %>%
  select(-`CENSUS2010POP`,-ESTIMATESBASE2010) %>%
  pivot_longer(contains("POPES"), "year", "people") %>%
  mutate(year = as.numeric(str_sub(year,start = -4, end = -1))) %>%
  filter(AGE < 200 & SEX == 0) %>%
  mutate(age_group = case_when(
    AGE < 25 ~ "Under 25 Years",
    AGE >= 25 & AGE < 45 ~ "25-44 years",
    AGE >= 45 & AGE < 65 ~ "45-64 years",
    AGE >= 65 & AGE < 75 ~ "65-74 years",
    AGE >= 75 & AGE < 85 ~ "75-84 years",
    AGE >= 85 ~ "85 years and older",
    T ~ "unk"
  )) 

agedist20 <- agedist %>%
  filter(year == 2019) %>%
  ungroup() %>%
  mutate(year = 2020) 

agedist1 <- bind_rows(agedist, agedist20)%>%
  group_by(year,age_group) %>%
  summarise(population_prop = sum(value)) %>%
  group_by(year) %>%
  mutate(proportion = population_prop / sum(population_prop))


ggplot(agedist1, aes(x = year, y = proportion)) +
  geom_path() + 
  facet_wrap(~age_group)

unique(all_age_unweighted$age_group)

statepop2 <- bind_rows(state_pop_year1, pop20df)



all_age_unweighted <- all_age %>%
  filter(type == "Unweighted") %>%
  left_join(statepop2, by = c("jurisdiction" = "state", "year")) %>%
  left_join(agedist1, by = c("year","age_group")) %>%
  left_join(pop20df, by = c("year", "jurisdiction" = "state")) %>%
  mutate(population.x = case_when(
    is.na(population.x) == T ~ population.y,
    T ~ population.x
  )) %>%
  rename(population = population.x) %>%
  rowwise() %>%
  mutate(pop_est = population * proportion) %>%
  mutate(death_percap = 100000 * number_of_deaths / pop_est) %>%
  mutate(thisyear = case_when(
    year == 2020 ~ "yes",
    T ~ "no"
  ))

young_df <- all_age_unweighted %>%
  filter(age_group == "25-44 years") %>%
  filter(jurisdiction %in% c("Texas", "New York","Florida","California")) %>%
  group_by(year) %>%
  arrange(week) %>%
  mutate(cumdeath = cumsum(death_percap))


ggplot(young_df, aes(x = week, y = cumdeath, group = year)) +
  geom_path(aes(color = thisyear)) +
  facet_wrap(~jurisdiction)



mod_fun <- function(df) {
  lm(number_of_deaths ~ ., data = df)
}

a_fun <- function(mod) {
  coefficients(mod)[[1]]
}

b_fun <- function(mod) {
  coefficients(mod)[[2]]
}

c_fun <- function(mod) {
  fitted(mod)
}

pred_fun <- function(mod) {
  predict(mod, data.frame(year = 2020), type = "response")
}

all_age <- all_age %>%
  mutate(jurisdiction = case_when(
    jurisdiction == "New York City" ~ "New York",
    T ~ jurisdiction
  )) %>%
  group_by(jurisdiction,year,week, age_group,type) %>%
  summarise(number_of_deaths = sum(number_of_deaths,na.rm=TRUE)) %>%
  ungroup()

all_age_mod <- all_age %>%
  filter(year != 2020) %>%
  filter(type == "Unweighted") %>%
  select(jurisdiction, age_group, week, number_of_deaths,year) %>%
  group_by(jurisdiction, age_group, week) %>%
  nest() %>%
  mutate(model = purrr::map(data,mod_fun))

all_age_mod <- all_age %>%  #collapse the age groups
  filter(year != 2020) %>%
  filter(type == "Unweighted") %>%
  select(jurisdiction, age_group, week, number_of_deaths,year) %>%
  mutate(age_group = case_when(
    age_group == "Under 25 years" ~ "0-44 Years",
    age_group == "25-44 years" ~ "0-44 Years",
    age_group == "45-64 years" ~ "45-74 Years",
    age_group == "65-74 years" ~ "45-74 Years",
    age_group == "75-84 years" ~ "75 years and older",
    age_group == "85 years and older" ~ "75 years and older",
    T ~ "NA"
  )) %>%
  group_by(jurisdiction, age_group, year,week) %>%
  summarise(number_of_deaths = sum(number_of_deaths,na.rm=TRUE)) %>%
  group_by(jurisdiction, age_group, week) %>%
  nest() %>%
  mutate(model = purrr::map(data,mod_fun))




#dplyr::anti_join(select(all_age_mod,jurisdiction, week, age_group),select(all_age_2020, jurisdiction, week,age_group))

all_age_2020 <- all_age %>%
  filter(year != 2020) %>%
  filter(type == "Unweighted") %>%
  select(jurisdiction,age_group, week) %>%
  mutate(year = 2020 ) %>%
  distinct() %>%
  left_join(all_age_mod, by = c("jurisdiction", "age_group", "week")) %>%
  mutate(pred = purrr::map_dbl(model,pred_fun), type = "pred")


all_age_2020 <- all_age %>%  ## collapsed age groups
  filter(year != 2020) %>%
  filter(type == "Unweighted") %>%
  select(jurisdiction,age_group, week) %>%
  mutate(year = 2020 ) %>%
  distinct() %>%
  mutate(age_group = case_when(
    age_group == "Under 25 years" ~ "0-44 Years",
    age_group == "25-44 years" ~ "0-44 Years",
    age_group == "45-64 years" ~ "45-74 Years",
    age_group == "65-74 years" ~ "45-74 Years",
    age_group == "75-84 years" ~ "75 years and older",
    age_group == "85 years and older" ~ "75 years and older",
    T ~ "NA"
  )) %>%
  left_join(all_age_mod, by = c("jurisdiction", "age_group", "week")) %>%
  mutate(pred = purrr::map_dbl(model,pred_fun), type = "pred")



all_age1 <- all_age %>%
  #filter(year != 2020) %>%
  filter(type == "Unweighted") %>%
  select(jurisdiction,age_group,week, number_of_deaths, year) %>%
  mutate(age_group = case_when(
    age_group == "Under 25 years" ~ "0-44 Years",
    age_group == "25-44 years" ~ "0-44 Years",
    age_group == "45-64 years" ~ "45-74 Years",
    age_group == "65-74 years" ~ "45-74 Years",
    age_group == "75-84 years" ~ "75 years and older",
    age_group == "85 years and older" ~ "75 years and older",
    T ~ "NA"
  )) %>%
  group_by(jurisdiction, age_group, year,week) %>%
  summarise(number_of_deaths = sum(number_of_deaths,na.rm=TRUE)) %>%
  mutate(type = "actual") %>%
  bind_rows(select(all_age_2020,jurisdiction, age_group, week, number_of_deaths = pred, year, type))

state_vec <- c("California","New York", "Texas","Florida","New Jersey","Pennsylvania","Arizona","Massachusetts","Michigan")
unique(all_age1$type)

all_age_nest <- all_age1 %>%
  group_by(jurisdiction,age_group,week) %>%
  filter(year == 2020) %>%
  filter(jurisdiction != "United States") %>%
  distinct() %>%
  pivot_wider(names_from = type, values_from = number_of_deaths) %>%
  group_by(age_group,jurisdiction) %>%
  filter(pred > 0 & actual > 0) %>%
  #rename(predicted = `Predicted (weighted)`, actual = `Unweighted`) %>%
  mutate(difference = actual - pred, rmse = sqrt((actual-pred)^2),iqr = (quantile(pred,.75,na.rm=T)-quantile(pred,.25,na.rm=T)),rmsen = rmse /iqr)

all_age_rmse <- all_age_nest %>%
  group_by(jurisdiction,age_group) %>%
  summarise(rmse = sqrt(mean(actual-pred,na.rm=T)^2),iqr = (quantile(pred,.75,na.rm=T)-quantile(pred,.25,na.rm=T)),rmsen = rmse /iqr,meanval = mean(pred,na.rm=T),rmsem = rmse/meanval)


fac_order <- all_age_nest %>%
  group_by(jurisdiction) %>%
  filter(difference == max(difference,na.rm=T)) %>%
  arrange(week)



ggplot(all_age_nest, aes(x = week, y = difference, color = age_group)) +
  geom_path() +
  facet_wrap(~jurisdiction)

age_dist1 <- age_dist %>%
  select(sex=SEX,age=AGE, population = POPESTIMATE2019) %>%
  filter(sex == 0) %>%
  filter(age < 200) %>%
  mutate(age_group = case_when(
    age < 45 ~ "0-44 Years",
    age >= 45 & age < 75 ~ "45-74 Years",
    age >= 75 ~ "75 years and older",
    T ~"NA"
  )) %>%
  group_by(age_group) %>%
  summarize(pop = sum(population)) %>%
  mutate(proportion = pop/sum(pop))

ggplot(subset(all_age_nest, week < 30 &age_group == "0-44 Years"), aes(x = week, y = difference,group = week)) +
  geom_hline(aes(yintercept =0))+
  ggrepel::geom_text_repel(data = subset(all_age_nest, week < 30 &age_group == "0-44 Years"&difference>=100),aes(label = jurisdiction)) +
  geom_boxplot(outlier.size = 1,outlier.color = "red") +
  # geom_label(data = age_dist1,aes(label = scales::percent(proportion,1), x = 5, y = 2500), inherit.aes = F,
  #            family = "Bahnschrift", face = "bold",size = 8) +
  # geom_text(data = age_dist1,aes(label = "Proportion of\n population:", x = 5, y = 2900), inherit.aes = F,
  #            family = "Bahnschrift", face = "bold",size = 4) +
  #facet_wrap(~age_group,nrow = 1)  +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    panel.background = element_rect(fill = "#fff7fb",color = "grey70"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold")
    ) +
  labs(
    title = "Excess Death Estimate by US State and Age Group",
    subtitle = "Compared to each state's predicted all cause deaths",
    y = "Difference between actual and predicted",
    x = "Week of 2020"
  )

all_age_nest1 <- all_age_nest %>%
  filter(difference > 0 )

ggplot(subset(all_age_nest1,week<30), aes(x = week, y = rmsen,group = week)) +
  geom_hline(aes(yintercept =0))+
  ggrepel::geom_text_repel(data = subset(all_age_nest1, week < 30 &rmsen>=7.5),aes(label = jurisdiction,color = jurisdiction)) +
  geom_boxplot(outlier.size = 1,outlier.color = "red") +
  #geom_point(size =1, color = "red", alpha = .25) +
  facet_wrap(~age_group,nrow = 1)  +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    panel.background = element_rect(fill = "#fff7fb",color = "grey70"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold")
  ) +
  labs(
    title = "Excess Death Estimate by US State and Age Group",
    subtitle = "Compared to each state's predicted all cause deaths",
    y = "Difference between actual and predicted",
    x = "Week of 2020"
  )

test1 <- all_age %>%
  filter(jurisdiction == "Idaho", week == 23)


deaths_pop <- deaths %>%
  #left_join(population, by = "UID") %>%
  select(UID,jurisdiction=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "deaths", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(jurisdiction, date) %>%
  summarise(pop=sum(Population, na.rm =T), deaths = sum(deaths, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  #group_by(State) %>%
  mutate(new_deaths = deaths - lag(deaths, default = 0)) %>%
  #mutate(deaths_mil = 1000000 * (deaths/pop),new_deaths_mil = 1000000 * (new_deaths/pop)) %>%
  arrange(jurisdiction, date) %>%
  #mutate(new_deaths_mil_roll = zoo::rollapply(new_deaths_mil,7,mean,align = "right",fill = 0)) %>%
  mutate(week = week(date)) %>%
  group_by(jurisdiction, week) %>%
  summarise(pop = max(pop), deaths = max(deaths), new_deaths = sum(new_deaths)) %>%
  filter(jurisdiction %in% state_vec)




ggplot(all_age_nest_total, aes(x = week, y = difference)) +
  #geom_path(data = deaths_pop, aes( x  = week, y = new_deaths), color = "darkred") +
  geom_path() +
  facet_wrap(~jurisdiction, scales = "free_y")



all_age_nest$jurisdiction <- as.factor(all_age_nest$jurisdiction)
levels(all_age_nest$jurisdiction) <- fac_order$jurisdiction


ggplot(all_age_nest, aes(x = week, y = difference, color = age_group)) +
  #geom_path(data = deaths_pop, aes( x  = week, y = new_deaths), color = "darkred") +
  geom_path() +
  facet_wrap(~jurisdiction, scales = "free_y")



str(all_age_nest_total)
all_age_nest_total <- all_age1 %>%
  left_join(select(all_age_2020,jurisdiction,age_group,week,pred), by = c("jurisdiction", "age_group", "week")) %>%
  group_by(jurisdiction,age_group,week) %>%
  filter(jurisdiction %in% state_vec) %>%
  distinct() %>%
  filter(type != "pred") %>%
  #pivot_wider(names_from = type, values_from = number_of_deaths) %>%
  #rename(predicted = `Predicted (weighted)`, actual = `Unweighted`) %>%
  mutate(difference = number_of_deaths - pred) %>%
  filter(age_group == "0-44 Years")

all_age_nest_total$jurisdiction <- as.factor(all_age_nest_total$jurisdiction)
levels(all_age_nest_total$jurisdiction) <- fac_order$jurisdiction

ggplot(all_age_nest_total, aes(x = week, y = difference, color = year,group = year)) +
  geom_path() +
  facet_wrap(~jurisdiction)
