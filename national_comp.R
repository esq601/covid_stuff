library(httr)
library(tidyverse)
library(readr)

cases_global<- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")))
deaths_global <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

intl_cases <- read_excel("COVID-19-geographic-disbtribution-worldwide-2020-06-27.xlsx", 
                         col_types = c("date", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "text", "text", "text", "numeric", 
                                       "text"))


population <- read_csv("~/intl_pop.csv")

continents <- read_csv("https://raw.githubusercontent.com/dbouquin/IS_608/master/NanosatDB_munging/Countries-Continents.csv")

unique(deaths_global$`Country/Region`)

pop <- population %>%
  select(country = `Country Name`,pop= `2019`) %>%
  mutate(country = case_when(
    country == "United States" ~ "US",
    str_detect(country, "Iran") == T ~ "Iran",
    str_detect(country, "Russia") == T ~ "Russia",
    str_detect(country, "Egypt") == T ~ "Egypt",
    T ~ country
  ))
str(deaths_pop)
deaths_pop <- deaths_global %>%
  #left_join(population, by = "UID") %>%
  select(country = `Country/Region`,State=`Province/State`, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "deaths", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(country, date) %>%
  summarise( deaths = sum(deaths, na.rm = T)) %>%
  #filter(country %in% c("Sweden","Spain","United Kingdom","Italy","Germany","Norway","Denmark","US")) %>%
  left_join(pop, by = "country") %>%
  #rowwise() %>%
  mutate(new_deaths = deaths - lag(deaths, default = 0),new_deaths_roll = zoo::rollapply(new_deaths,7,mean,align = "right",fill = 0)) %>%
  rowwise() %>%
  mutate(pop_new_deaths = 1000000* new_deaths_roll / pop, pop_deaths = deaths/ pop) %>%
  mutate(swe = case_when(
    country == "Sweden" ~ "swe",
    T ~ "else"
  ),type = "deaths")

cases_pop <- cases_global %>%
  #left_join(population, by = "UID") %>%
  select(country = `Country/Region`,State=`Province/State`, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "deaths", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(country, date) %>%
  summarise( deaths = sum(deaths, na.rm = T)) %>%
  #filter(country %in% c("Sweden","Spain","United Kingdom","Italy","Germany","Norway","Denmark","US")) %>%
  left_join(pop, by = "country") %>%
  #rowwise() %>%
  mutate(new_deaths = deaths - lag(deaths, default = 0),new_deaths_roll = zoo::rollapply(new_deaths,7,mean,align = "right",fill = 0)) %>%
  rowwise() %>%
  mutate(pop_new_deaths = 1000000* new_deaths_roll / pop, pop_deaths = deaths/ pop) %>%
  mutate(swe = case_when(
    country == "Sweden" ~ "swe",
    T ~ "else"
  ),type = "cases")


test <- deaths_pop %>%
  filter(is.na(pop) == T) %>%
  group_by(country) %>%
  filter(date == max(date, na.rm=T))

total_pop <- bind_rows(cases_pop,deaths_pop)

ggplot(total_pop, aes( x = date, y = pop_new_deaths ,color = swe,group = country,alpha =swe)) +
  geom_path(size = .5) +
  ggsci::scale_color_aaas() +
  scale_alpha_manual(breaks = c("swe","else"), values = c(1,.25)) +
  theme_minimal() +
  facet_wrap(~type,scales = "free_y")


deaths_pop1 <- deaths_pop %>%
  mutate(week = floor_date(date, "month")) %>%
  group_by(country, week) %>%
  summarise(new_deaths = sum(new_deaths), pop = max(pop)) %>%
  rowwise() %>%
  mutate(deaths_pop = new_deaths / pop) %>%
  group_by(week) %>%
  arrange(deaths_pop) %>%
  mutate(rank = rank(-deaths_pop, ties.method = "first")) %>%
  mutate(swe = case_when(
    country == "Sweden" ~ "swe",
    country == "US" ~ "us",
    country == "United Kingdom" ~ "uk",
    T ~ "else"
  )
  ) %>%
  arrange(week)

deaths_all_rank <- deaths_pop1 %>%
  filter(rank <= 10) %>%
  filter(week > as.Date("2020-03-01")) %>%
  left_join(continents, by = c("country" = "Country"))


ggplot(deaths_swe_rank, aes( x = week, y = rank, color = Continent)) +
  geom_label(aes(label = country)) +
  geom_density2d() +
  scale_y_reverse() +
  theme(
    legend.position = "none"
  )

deaths_pop2 <- deaths_pop1   %>%
  filter(week == as.Date("2020-04-01") | week == as.Date("2020-08-01")) %>%
  #filter(country %in% c("Sweden","United Kingdom","Italy","Germany","US","Mexico","Brazil","Canada","Columbia","Peru","Bolivia")) %>%
  left_join(continents, by = c("country" = "Country")) %>%
  mutate(Continent = case_when(
    country == "US" ~ "USA",
    T ~ Continent
  )) %>%
  group_by(country) %>%
  filter(min(rank) <= 20)
  #filter(Continent %in% c("Europe","South America","North America","Africa","Asia", "USA")) %>%
  #filter(pop > 20000000)
  

ggplot(deaths_pop2 , aes(x = week, y= rank, color = Continent,group = country)) +
  geom_path(size = 1.5) +
  ggrepel::geom_label_repel(aes(label = country)) +
  ggsci::scale_color_jama() +
  scale_x_discrete(expand = c(.2,.2), labels = c("April","August")) +
  #scale_alpha_manual(breaks = c("swe","else"), values = c(1,.25)) +
  theme_minimal()
