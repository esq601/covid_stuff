library(ggtext)
library(statebins)
library(hrbrthemes)
library(tidyverse)
library(httr)

deaths <- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")))
cases<- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")))

election_results <- read_csv("https://raw.githubusercontent.com/kshaffer/election2016/master/2016ElectionAnalysis.csv")

election_results1 <- election_results %>%
  mutate(result = case_when(
    trumpVotes > clintonVotes ~ "Trump",
    T ~ "Clinton"
  )) %>%
  select(state, result, marginOfVictoryPercent)

population <- deaths %>%
  select(UID,Population)

pop_state <- deaths %>%
  select(state = Province_State, Population) %>%
  group_by(state) %>%
  summarise(Population = sum(Population))

governers <- read_csv(content(GET("https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/us-governors/data/us-governors.csv")))


governers[17,7] <- "democrat"
governers[19,7] <- "democrat"


gov1 <- governers %>%
  select(State=state_name, party) %>%
  mutate(party = case_when(
    party == "republican" ~ "Republican",
    party == "democrat" ~ "Democrat"
  ))


deaths_pop <- deaths %>%
  #left_join(population, by = "UID") %>%
  select(UID,State=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "deaths", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(State, date) %>%
  summarise(pop=sum(Population, na.rm =T), deaths = sum(deaths, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  group_by(State) %>%
  filter(date == max(date)) %>%
  mutate(death_mil = 1000000 * (deaths/pop)) %>%
  left_join(gov1, by = "State") %>%
  left_join(election_results1, by=c("State" = "state")) %>%
  mutate(fill_bin = factor(case_when(
    death_mil <=150 ~ "< 150",
    death_mil >150 & death_mil <= 500 ~ "150-500",
    death_mil >500 & death_mil <= 1000 ~ "500-1000",
    T ~ "> 1000"
  ),levels = c("< 150","150-500","500-1000","> 1000"))
  )

ggplot(subset(deaths_pop,is.na(result) == F)) +
  geom_statebins(
    aes(state = State, fill = fill_bin,label  = party),border_col = "grey40",border_size = .4,lbl_size = 6,
    radius=grid::unit(8, "pt")
  ) +
  coord_equal() +
  #scale_fill_brewer(type = "div", palette = "RdYlGn", direction = -1) +
  #scale_fill_manual(values = c("#a6d96a","#ebeb2a","#fdae61","#d7191c")) +
  scale_fill_viridis_d(begin = .1,option = 'B', direction = -1) +
  theme_statebins() +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.spacing.x = unit(.5, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8)
  ) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  facet_wrap(~result,ncol=1) +
  labs(title = "COVID-19 Deaths by US State",subtitle = "Population adjusted. Split by 2016 election results.",caption = paste0("As of: ",format(Sys.Date(),"%d-%b-%y")), fill = "Deaths per Million Residents")

       