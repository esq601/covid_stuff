library(tidyverse)
library(httr)
library(readxl)
library(extrafont)
library(ggtext)
library(geosphere)
library(ggmap)
library(statebins)

loadfonts(device = "win")

deaths <- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")))
cases<- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")))
cases_global <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

population <- deaths %>%
  select(UID,Population)

governers <- read_csv(content(GET("https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/us-governors/data/us-governors.csv")))


governers[17,7] <- "democrat"
governers[19,7] <- "democrat"


gov1 <- governers %>%
  select(State=state_name, party) %>%
  mutate(party = case_when(
    party == "republican" ~ "Republican",
    party == "democrat" ~ "Democrat"
  ))

day_val <- 14

cases_pop <- cases %>%
  left_join(population, by = "UID") %>%
  select(UID,State=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(State, date) %>%
  summarise(pop=sum(Population, na.rm =T), cases = sum(cases, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(State, date) %>%
  mutate(new_case_mil_roll = zoo::rollapply(new_case_mil,7,mean,align = "right",fill = 0)) %>%
  #filter(case_mil > 1) %>%
  #arrange(State, desc(date)) %>%
  group_by(State) %>%
  mutate(day = as.numeric(abs(date- (max(date)+1))),day_gp = ceiling(day / day_val), day_num = day %% day_val, country = "US",type="Cases") %>%
  mutate(day_num = case_when(
    day_num == 0 ~ day_val,
    T ~day_num
  )) %>%
  left_join(gov1, by = "State") %>%
  select(state = State, date,new_case_mil_roll,day_gp, day_num) 

cases_pop1 <- cases_pop %>%
  filter(day_gp != max(day_gp)) %>%
  group_by(state,day_gp) %>%
  nest()

mod_fun <- function(df) {
  lm(new_case_mil_roll ~ day_num, data = df)
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

cases_pop %>%
  filter(day_gp == 1) %>%
  filter(state == "Hawaii") %>%
  ggplot(aes(x = date, y= new_case_mil_roll)) + 
  geom_path() +
  geom_smooth(method = "lm")

cases_mapped <- cases_pop1 %>%
  mutate(model = map(data,mod_fun),intercept = map_dbl(model,a_fun),slope = map_dbl(model,b_fun),values = map(model,summary))


cases_mapped_rec <- cases_mapped %>%
  filter(day_gp ==1) %>%
  select(state, values,slope,intercept) %>%
  unnest(cols = values) %>%
  mutate(day = seq(1:day_val))

ggplot(cases_mapped_rec, aes( x= day, y = values, color = state)) +
  geom_path() + 
  #facet_wrap(~state,scales = "free_y") +
  theme(
    legend.position = "none"
  )


