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
election_results <- read_csv("https://raw.githubusercontent.com/kshaffer/election2016/master/2016ElectionAnalysis.csv")

population <- deaths %>%
  select(UID,Population)

pop_state <- deaths %>%
  select(state = Province_State, Population) %>%
  group_by(state) %>%
  summarise(Population = sum(Population))

intl_cases <- read_excel("COVID-19-geographic-disbtribution-worldwide-2020-06-27.xlsx", 
              col_types = c("date", "numeric", "numeric", 
                            "numeric", "numeric", "numeric", 
                            "text", "text", "text", "numeric", 
                            "text"))


governers <- read_csv(content(GET("https://raw.githubusercontent.com/CivilServiceUSA/us-governors/master/us-governors/data/us-governors.csv")))


governers[17,7] <- "democrat"
governers[19,7] <- "democrat"


gov1 <- governers %>%
  select(State=state_name, party) %>%
  mutate(party = case_when(
    party == "republican" ~ "Republican",
    party == "democrat" ~ "Democrat"
  ))

repub <- governers %>%
  filter(party == "republican") %>%
  select(state_name) %>%
  left_join(state_pop, by = c("state_name" = "Province_State"))

dems <- governers %>%
  filter(party == "democrat") %>%
  select(state_name) %>%
  #filter(state_name != "New York") %>%
  left_join(state_pop, by = c("state_name" = "Province_State"))


state_date <- cases %>%
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
  filter(case_mil > 1) %>%
  select(State,date) %>%
  group_by(State) %>%
  mutate(day = row_number())



deaths_pop <- deaths %>%
  #left_join(population, by = "UID") %>%
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
  left_join(state_date, by = c("State","date")) %>%
  filter(is.na(day) == F) %>%
  group_by(State) %>%
  mutate(day = row_number(), country = "US",type = "Deaths") %>%
  left_join(gov1, by = "State") %>%
  #filter(State %in% c("Arizona", "New York", "New Jersey","Texas","Florida","California", "Michigan","Connecticut","Massachusetts","Georgia"))
filter(State %in% c("New York","Texas"))

deaths_us <- deaths %>%
  #left_join(population, by = "UID") %>%
  select(UID,State=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(date) %>%
  summarise(pop=sum(Population, na.rm =T), cases = sum(cases, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(date) %>%
  mutate(new_case_mil_roll = zoo::rollapply(new_case_mil,7,mean,align = "right",fill = 0)) %>%
  filter(case_mil > 1) %>%
  mutate(day = row_number(),State = "US",party = "US", country = "US")

deaths_party <- deaths %>%
  #left_join(population, by = "UID") %>%
  select(UID,State=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  left_join(gov1, by = "State") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(party,date) %>%
  summarise(pop=sum(Population, na.rm =T), cases = sum(cases, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(party,date) %>%
  mutate(new_case_mil_roll = zoo::rollapply(new_case_mil,7,mean,align = "right",fill = 0)) %>%
  filter(case_mil > 1) %>%
  mutate(day = row_number(),country = "US",State = party)



intl_deaths1 <- intl_cases %>%
  select(State = countriesAndTerritories, date= dateRep,new_cases = deaths, pop = popData2019) %>%
  filter(pop > 0 ) %>%
  group_by(State) %>%
  arrange(date) %>%
  mutate(cases = cumsum(new_cases)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(State, date) %>%
  mutate(new_case_mil_roll = zoo::rollapply(new_case_mil,7,mean,align = "right",fill = 0)) %>%
  filter(case_mil > 1) %>%
  group_by(State) %>%
  mutate(day = row_number()) %>%
  #filter(pop > min(cases_pop$pop) & pop < max(cases_pop$pop)) %>%
  filter(State %in% c("United_Kingdom", "Germany", "France","Italy","Canada","Australia","Sweden")) %>%
  mutate(party = State, country = "Not-US", type = "Deaths")


df_plot_deaths <- bind_rows(
  #select(deaths_party, day, new_case_mil_roll,State, party),
  #select(intl_deaths1, day, new_case_mil_roll,State, party),
  #select(deaths_us, day, new_case_mil_roll,State, party),
  select(deaths_pop, day, new_case_mil_roll,State, party,type)
)

#unique(intl_cases$countriesAndTerritories)
ggplot(df_plot_deaths, aes(x = day, y = new_case_mil_roll,group = State,color = State)) + 
  geom_path(size = 1.15) +
  ggrepel::geom_label_repel(data=filter(df_plot,new_case_mil_roll == max(new_case_mil_roll)), aes(label = State),size = 5,hjust=-.5) +
  #scale_color_brewer(type ="qual", palette = 7) +
  #geom_path(data = intl_cases1) +
  # scale_color_manual(breaks = c("Republican","Democrat","US","United_Kingdom", "Germany", "France","Italy","Canada","Australia"),
  #                    values = c("red","blue","darkgreen","#b3de69","#fdb462","#80b1d3","#fb8072","#bebada","#d9d9d9")) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fff7fb")
  ) +
  #expansion(add=20) +
  labs(title = "Daily New Confirmed COVID-19 Deaths per Million People", subtitle = "Shown as 7-day rolling average.",x= "Days since 1st case/million", y = "New deaths/million")




##### Cases #####


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
  filter(case_mil > 1) %>%
  group_by(State) %>%
  mutate(day = row_number(), country = "US",type="Cases") %>%
  left_join(gov1, by = "State") %>%
  #filter(State %in% c("Arizona", "New York","Texas","Florida","California","Michigan","Massachusetts","New Jersey","Connecticut","Georgia")) 
  filter(State %in% c("New York","Texas"))

cases_us <- cases %>%
  left_join(population, by = "UID") %>%
  select(UID,State=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(date) %>%
  summarise(pop=sum(Population, na.rm =T), cases = sum(cases, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(date) %>%
  mutate(new_case_mil_roll = zoo::rollapply(new_case_mil,7,mean,align = "right",fill = 0)) %>%
  filter(case_mil > 1) %>%
  mutate(day = row_number(),State = "US",party = "US", country = "US")

cases_party <- cases %>%
  left_join(population, by = "UID") %>%
  select(UID,State=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  left_join(gov1, by = "State") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(party,date) %>%
  summarise(pop=sum(Population, na.rm =T), cases = sum(cases, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(party,date) %>%
  mutate(new_case_mil_roll = zoo::rollapply(new_case_mil,7,mean,align = "right",fill = 0)) %>%
  filter(case_mil > 1) %>%
  mutate(day = row_number(),country = "US",State = party)



intl_cases1 <- intl_cases %>%
  select(State = countriesAndTerritories, date= dateRep,new_cases = cases, pop = popData2019) %>%
  filter(pop > 0 ) %>%
  group_by(State) %>%
  arrange(date) %>%
  mutate(cases = cumsum(new_cases)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(State, date) %>%
  mutate(new_case_mil_roll = zoo::rollapply(new_case_mil,7,mean,align = "right",fill = 0)) %>%
  filter(case_mil > 1) %>%
  group_by(State) %>%
  mutate(day = row_number()) %>%
  #filter(pop > min(cases_pop$pop) & pop < max(cases_pop$pop)) %>%
  #filter(State %in% c("United_Kingdom", "Germany", "France","Italy","Canada","Australia","Sweden")) %>%
  filter(State %in% "New_Zealand") %>%
  mutate(party = State, country = "Not-US", type = "Cases")


df_plot_cases <- bind_rows(
  #select(cases_party, day, new_case_mil_roll,State, party),
  #select(intl_cases1, day, new_case_mil_roll,State, party),
  #select(cases_us, day, new_case_mil_roll,State, party),
  select(cases_pop, day, new_case_mil_roll,State, party,type),
  select(deaths_pop, day, new_case_mil_roll,State,party,type)
)




peaklage <- df_plot_cases %>%
  group_by(type, State) %>%
  filter((new_case_mil_roll > 50 & type == "Cases")) %>%
  left_join(filter(df_plot_cases, type == "Deaths"), by = c("day","State")) %>%
  group_by(State) %>%
  mutate(day = row_number())

ggplot(peaklage, aes(x = day, y = new_case_mil_roll.y,color = State)) +
  geom_path(size =1.15) +
  theme_minimal() +
  ggrepel::geom_label_repel(data=filter(peaklage,new_case_mil_roll.y == max(new_case_mil_roll.y) & max(new_case_mil_roll.y)>10), aes(label = State),size = 3,hjust=-.5) +
  ggrepel::geom_label_repel(data=filter(peaklage,day == max(day) & max(new_case_mil_roll.y)<=10), aes(label = State),size = 3,hjust=-.5) +
  
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fff7fb"),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(title = "Lag Between Daily Rates of Case Increases and Death Rates", subtitle = paste0("Shown as 7-day rolling average. As of ",Sys.Date()),x= "Days Since 50 New Cases / Million", y = "New Deaths / Million")

  

ggplot(peaklage, aes(y = new_case_mil_roll, x = day_diff,color = State)) +
  geom_point(size = 4) +
  ggrepel::geom_label_repel(aes(label = State)) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(peaklage$new_case_mil_roll))) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fff7fb"),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  labs(title = "Lag Between Case and Death Peaks", subtitle = "Shown as 7-day rolling average.",x= "Days Since 50 New Cases / Million", y = "New Deaths / Million")


df_plot_cases <- df_plot_cases %>%
  mutate(early = case_when(
    State %in% c("New Jersey","New York","Massachusetts","Michigan","Connecticut") ~ "Early",
    T ~ "Late"
  ))

df_plot_deaths <- df_plot_deaths %>%
  mutate(early = case_when(
    State %in% c("New Jersey","New York","Massachusetts","Michigan","Connecticut") ~ "Early",
    T ~ "Late"
  ))

#unique(intl_cases$countriesAndTerritories)
ggplot(df_plot_cases, aes(x = day, ymax = new_case_mil_roll,y = new_case_mil_roll,group = State,fill = early)) + 
  geom_ribbon(size = .25,aes(ymin = 0,color = early), alpha = .35,color = "grey50") +
  ggrepel::geom_label_repel(data=filter(df_plot_cases,new_case_mil_roll == max(new_case_mil_roll)), aes(label = State),size = 5,vjust=-.95,hjust = .5,family = "Bahnschrift",alpha = .75,nudge_y = 100) +
  ggrepel::geom_label_repel(data=filter(df_plot_deaths,new_case_mil_roll == max(new_case_mil_roll)), aes(label = State),size =5,vjust=-.95,hjust = .5,family = "Bahnschrift",alpha = .75, nudge_y = 10) +
  theme_minimal() +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "none",
    legend.spacing.x = unit(.95, 'cm'),
    legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8),
    strip.text = element_text(size = 18, face = "bold"),
    axis.title.y = element_blank()
  ) +
  facet_wrap(~type, ncol = 1, scales = "free_y") +
  ggsci::scale_fill_aaas() +
  ggsci::scale_color_aaas() +
  
  #scale_color_manual(breaks = c("Early","Late"), values = c("blue","red")) +
  #expansion(add=20) +
  labs(title = "Daily COVID-19 Cases and Deaths per Capita", subtitle = "Shown as 7-day rolling average per million population.",x= "Days since 1 case / million in state", y = "New case/deaths / million")



election_results1 <- election_results %>%
  mutate(result = case_when(
    trumpVotes > clintonVotes ~ "Trump",
    T ~ "Clinton"
  )) %>%
  select(state, result, marginOfVictoryPercent)

death_election <- deaths %>%
  left_join(election_results1, by=c("Province_State" = "state")) %>%
  select(UID,State=Province_State,Population, contains("/"),result) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(result,date) %>%
  summarise(pop=sum(Population, na.rm =T), deaths = sum(cases, na.rm = T),Population = sum(Population)) %>%
  mutate(new_deaths = deaths - lag(deaths),new_deaths_capita = new_deaths/Population) %>%
  mutate(new_deaths_roll = zoo::rollapply(new_deaths,7,mean,align = "right",fill = 0)) %>%
  mutate(new_deaths_roll_capita = 1000000*zoo::rollapply(new_deaths_capita,7,mean,align = "right",fill = 0)) %>%
  filter(is.na(result) == F) %>%
  filter(is.infinite(new_deaths_roll) == FALSE & is.na(new_deaths_roll) == F)

ggplot(death_election, aes( x = date, y = new_deaths_roll, color = result, group = result)) +
  geom_path(size = 1.15) +
  ggrepel::geom_label_repel(data=filter(death_election,new_deaths_roll == max(new_deaths_roll)), aes(label = result),size = 6,hjust=-.5,vjust =-1) +
  theme_minimal() +
  scale_color_manual(breaks = c("Clinton","Trump"), values = c("blue","red")) +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fff7fb"),
    axis.title.x = element_blank()
  ) +
  #scale_y_continuous(labels = scales::comma_format()) +
  scale_x_date(limits = c(as.Date("2020-03-01"),max(death_election$date))) +
  labs(title = "Deaths from COVID-19 by State Results of 2016 Election", subtitle = "Aggregated as 7 day rolling average",
       y = "Deaths")


excess_deaths_new <- read_csv("https://data.cdc.gov/resource/muzy-jte6.csv?$limit=1000000")
excess_deaths_old <- read_csv("https://data.cdc.gov/resource/3yf8-kanr.csv?$limit=1000000")

us_new <- excess_deaths_new %>%
  filter(jurisdiction_of_occurrence != "New York City") %>%
  group_by(mmwryear,mmwrweek) %>%
  summarise(all_cause = sum(all_cause, na.rm=T)) %>%
  mutate(jurisdiction_of_occurrence = "United States")
excess_deaths <- bind_rows(
  select(excess_deaths_new,jurisdiction_of_occurrence,all_cause,mmwryear,mmwrweek),
  select(excess_deaths_old,jurisdiction_of_occurrence,all_cause=allcause,mmwryear,mmwrweek),
  us_new
)


colnames(excess_deaths)
excess_deaths1 <- excess_deaths %>%
  filter(mmwrweek <= 22) %>%
  left_join(election_results1, by=c("jurisdiction_of_occurrence" = "state")) %>%
  group_by(jurisdiction_of_occurrence,mmwryear,result) %>%
  summarise(deaths = sum(all_cause, na.rm = T)) %>%
  filter(is.na(result) == FALSE) %>%
  arrange(mmwryear) %>%
  group_by(jurisdiction_of_occurrence) %>%
  mutate(diff = deaths - lag(deaths)) %>%
  filter(is.na(diff) == F) %>%
  left_join(pop_state , by = c("jurisdiction_of_occurrence"="state")) %>%
  mutate(diff_capita =1000000* diff/Population)


ggplot(excess_deaths1, aes(x = reorder(jurisdiction_of_occurrence,diff_capita), y = diff_capita, fill = result)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(breaks = c("Clinton","Trump"), values = c("blue","red")) +
  geom_text(aes(label = jurisdiction_of_occurrence), angle = 0,hjust = 0) +
  coord_flip()



excess_deaths2 <- excess_deaths %>%
  filter(jurisdiction_of_occurrence != "United States" & jurisdiction_of_occurrence != "New York City") %>%
  filter(mmwrweek <= 22) %>%
  filter(mmwryear %in% c(2019,2020)) %>%
  group_by(jurisdiction_of_occurrence,mmwryear) %>%
  summarise(deaths = sum(all_cause, na.rm = T)) %>%
  arrange(mmwryear) %>%
  group_by(jurisdiction_of_occurrence) %>%
  mutate(diff = deaths - lag(deaths)) %>%
  filter(is.na(diff) == F) %>%
  left_join(pop_state , by = c("jurisdiction_of_occurrence"="state")) %>%
  mutate(diff_capita =1000000* diff/Population) %>%
  left_join(election_results1, by=c("jurisdiction_of_occurrence" = "state")) %>%
  filter(is.na(result) == FALSE) %>%
  mutate(result_inv = case_when(
    result == "Clinton" ~ -marginOfVictoryPercent,
    T ~ marginOfVictoryPercent
  ))    %>%
  left_join(gov1, by = c("jurisdiction_of_occurrence"="State"))


model1 <- lm(excess_deaths2$diff_capita ~ excess_deaths2$result_inv)

summary(model1)

ggplot(excess_deaths2, aes(x = result_inv, y = diff_capita,color = result)) +
  geom_point() +
  scale_color_manual(breaks = c("Clinton","Trump"), values = c("blue","red")) +
  geom_smooth(method = "lm", aes(x = result_inv, y = diff_capita),inherit.aes = FALSE,color = "grey40",alpha = .25) +
  #scale_x_continuous(limits = c(-1,1)) +
  ggrepel::geom_text_repel(aes(label = jurisdiction_of_occurrence),size = 3) +
  scale_x_continuous(labels = scales::percent_format()) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#fff7fb"),
    panel.grid = element_line(color = "grey85"),
    panel.grid.minor = element_blank()
    ) +
  annotate("text", x=-.65, y = 300, label = "R^2: 0.28", size = 6) +
  labs(title = "Votes and Excess Deaths Through May from 2019 to 2020",subtitle = "Population adjusted and arranged according to 2016 election results",
       x = "Trump Margin of Victory Percent", y = "Change per Million of State Population")
  

'%!in%' <- function(x,y)!('%in%'(x,y))
  
# excess_deaths3 <- excess_deaths %>%
#   filter(jurisdiction_of_occurrence != "New York City") %>%
#   left_join(election_results1, by=c("jurisdiction_of_occurrence" = "state")) %>%
#   left_join(pop_state , by = c("jurisdiction_of_occurrence"="state")) %>%
#   group_by(result,jurisdiction_of_occurrence,mmwryear,mmwrweek) %>%
#   summarise(total = sum(all_cause),Population = sum(Population)) %>%
#   mutate(Population = case_when(
#     jurisdiction_of_occurrence == "United States" ~ 328200000,
#     T ~ Population
#   )) %>%
#   ungroup() %>%
#   mutate(mmwryear = as.character(mmwryear)) %>%
#   mutate(total_adj = 1000000 * total/Population)

repub <- election_results1 %>%
  filter(result == "Trump")

dems <- election_results1 %>%
  filter(result == "Clinton")

excess_deaths3 <- bind_rows(excess_deaths %>%
                              filter(jurisdiction_of_occurrence != "New York City" & jurisdiction_of_occurrence != "United States") %>%
                              left_join(election_results1, by=c("jurisdiction_of_occurrence" = "state")) %>%
                              left_join(pop_state , by = c("jurisdiction_of_occurrence"="state")) %>%
                              filter(jurisdiction_of_occurrence %in% repub$state) %>%
                              group_by(mmwryear,mmwrweek) %>%
                              summarise(total = sum(all_cause),Population = sum(Population,na.rm = T)) %>%
                              ungroup() %>%
                              mutate(mmwryear = as.character(mmwryear)) %>%
                              mutate(total_adj = 1000000 * total/Population,party = "Trump States"),
                            excess_deaths %>%
                              filter(jurisdiction_of_occurrence != "New York City" & jurisdiction_of_occurrence != "United States") %>%
                              left_join(election_results1, by=c("jurisdiction_of_occurrence" = "state")) %>%
                              left_join(pop_state , by = c("jurisdiction_of_occurrence"="state")) %>%
                              filter(jurisdiction_of_occurrence %in% dems$state) %>%
                              group_by(mmwryear,mmwrweek) %>%
                              summarise(total = sum(all_cause),Population = sum(Population,na.rm = T)) %>%
                              ungroup() %>%
                              mutate(mmwryear = as.character(mmwryear)) %>%
                              mutate(total_adj = 1000000 * total/Population, party = "Clinton States")
)
                            
#state <- "United States"

library(extrafont)
loadfonts(device = "win")

ggplot(excess_deaths3 , aes(x = mmwrweek, y = total, group = mmwryear, color = mmwryear)) +
  geom_path(size = 1.25) +
  #scale_y_continuous(limits = c(0,max(excess_deaths3$total_adj,na.rm=TRUE))) +
  labs(title = "Excess Deaths", x = "Week of the Year", y = "Weekly Deaths",
       subtitle = "Split by 2016 Presidential Election Results") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_brewer(type = "qual",palette = 6) +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(
    plot.background = element_rect(fill = "#fff7fb"),
    legend.position = "bottom",
    legend.title = element_blank(),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 26),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  ) +
 # scale_x_continuous(limits = c(0,22)) +
  facet_wrap(~party)


ed4 <- excess_deaths %>%
  filter(jurisdiction_of_occurrence != "New York City" & jurisdiction_of_occurrence != "United States") %>%
  left_join(election_results1, by=c("jurisdiction_of_occurrence" = "state")) %>%
  left_join(pop_state , by = c("jurisdiction_of_occurrence"="state")) %>%
  #filter(jurisdiction_of_occurrence %in% dems$state) %>%
  group_by(mmwryear,mmwrweek) %>%
  summarise(total = sum(all_cause),Population = sum(Population,na.rm = T)) %>%
  ungroup() %>%
  mutate(mmwryear = as.character(mmwryear)) %>%
  mutate(total_adj = 1000000 * total/Population)

ggplot(ed4 , aes(x = mmwrweek, y = total, group = mmwryear, color = mmwryear)) +
  geom_path(size = 1.25) +
  #scale_y_continuous(limits = c(0,max(excess_deaths3$total_adj,na.rm=TRUE))) +
  labs(title = "Excess Deaths", x = "Week of the Year", y = "Weekly Deaths",
       subtitle = "Number of Deaths Each Week per Year") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_color_brewer(type = "qual",palette = 6) +
  guides(color=guide_legend(nrow=1,byrow=TRUE)) +
  theme(
    plot.background = element_rect(fill = "#fff7fb"),
    legend.position = "bottom",
    legend.title = element_blank(),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 26),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey80")
  )


#### Nusing homes #####

deaths <- read_csv(content(GET("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")))
states_df <- data.frame(cbind(state.abb,state.name))


deaths_state <- deaths %>%
  #left_join(population, by = "UID") %>%
  select(UID,state=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  group_by(state) %>%
  filter(date == max(date)) %>%
  summarise(pop = sum(Population), deaths = sum(cases)) %>%
  left_join(states_df, by = c("state" = "state.name"))

#### Nursing Home Stuff ####


nursing_home <- read_csv("https://data.cms.gov/resource/s2uc-8wxp.csv?$limit=1000000")


data_provided <- nursing_home %>%
  group_by(provider_state,submitted_data) %>%
  summarise(num = n()) %>%
  mutate(pct = num/sum(num)) %>%
  filter(submitted_data == "Y")

nursing_home_recent <- nursing_home %>%
  group_by(federal_provider_number) %>%
  filter(week_ending == max(week_ending))

nh_deaths <- nursing_home_recent %>%
  group_by(provider_state) %>%
  summarise(all_deaths = sum(residents_total_all_deaths,na.rm=T),
            covid_deaths = sum(residents_total_covid_19, na.rm = T)) %>%
  left_join(deaths_state, by = c("provider_state" = "state.abb")) %>%
  mutate(pct_nh = covid_deaths/deaths)

nh_residents <- nursing_home_recent %>%
  select(provider_state,provider_city,number_of_all_beds,
         total_number_of_occupied,residents_total_all_deaths,
         residents_total_covid_19,total_resident_covid_19_deaths_per_1_000_residents) %>%
  filter(residents_total_covid_19 >= .15*number_of_all_beds)

homes_num <- nursing_home_recent %>%
  group_by(provider_state) %>%
  summarise(homes = n())

nh_residents_sum <- nh_residents %>%
  group_by(provider_state) %>%
  summarise(num = n()) %>%
  left_join(homes_num, by = "provider_state") %>%
  left_join(states_df, by = c("provider_state" = "state.abb")) %>%
  mutate(pct_homes = num/homes) %>% 
  mutate(state.name = as.character(state.name)) %>%
  mutate(state.name = case_when(
    provider_state == "DC" ~ "Wash. DC",
    T ~ state.name
  ))

dftest <- nursing_home_recent %>%
  filter(provider_state == "TX") %>%
  select(provider_name,provider_city,residents_total_confirmed,residents_total_covid_19,number_of_all_beds)


sum(nh_residents_sum$pct_homes)

ggplot(nh_residents_sum) +
  treemapify::geom_treemap(aes(area=num, fill = num)) +
  treemapify::geom_treemap_text(aes(area = num, label = paste(state.name,num,sep="\n")),
                                color = "black",family = "Bahnschrift",place = "middle") +
  #scale_fill_viridis_c(option = "B") +
  scale_fill_distiller(palette = 8,direction = 1) +
  theme(
    plot.background = element_rect(fill = "#fff7fb"),
    legend.title = element_blank(),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 26),
    legend.position = "none"
  ) +
  labs(title = "State Nursing Home Disaster",subtitle = "Number of state's nursing homes where COVID deaths were greater\n than 15% of each home's capacity.")


ggplot(nh_residents_sum) +
  geom_bar(aes(x = pct_homes, y = reorder(provider_state,pct_homes), fill = pct_homes), stat = "identity") +
  geom_text(aes( x= pct_homes,y=reorder(provider_state,pct_homes), 
                 label = provider_state),hjust =0, nudge_x = .001,family = "Bahnschrift") +
  #scale_fill_viridis_c(option = "B") +
  scale_fill_distiller(palette = 8,direction = 1) +
  theme_minimal() +
  scale_x_continuous(labels = scales::percent_format()) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    plot.background = element_rect(fill = "#fff7fb"),
    legend.title = element_blank(),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 26),
    legend.position = "none"
  ) +
  labs(title = "State Nursing Home Disaster",subtitle = "Percent of state's nursing homes where COVID deaths were greater\n than 15% of each home's capacity.")


str_split(str_remove_all(str_remove(head(nursing_home_recent$geolocation),"POINT "),"\\(|\\)")," ")[1]
library(sp)
nylat <- 40.7128
nylong <- -74.0060

nursing_home_dist <- nursing_home_recent %>%
  ungroup() %>%
  rowwise() %>%
  mutate(coord = str_remove_all(str_remove(head(geolocation),"POINT "),"\\(|\\)")) %>%
  select(provider_name,provider_city,provider_state,number_of_all_beds,
         total_number_of_occupied,residents_total_all_deaths,
         residents_total_covid_19,total_resident_covid_19_deaths_per_1_000_residents,
         geolocation, coord) %>%
  separate(coord, into = c("lon","lat"), sep = "[:space:]") %>%
  filter(is.na(lon) == F) %>%
  mutate_at(vars("lon","lat"),as.numeric)

  #mutate(dist = distm(c(lon, lat), c(nylong, nylat), fun = distHaversine))

sp::coordinates(nursing_home_dist) <- cbind(nursing_home_dist$lon , nursing_home_dist$lat)
proj4string(nursing_home_dist) = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")

dist <- data.frame(distance_km = distm(nursing_home_dist, c(nylong, nylat), fun = distHaversine)/1000)


nh_dist <- bind_cols(
  nursing_home_dist@data,
  #data.frame(longitude = coordinates(nursing_home_dist)[,1], latitude = coordinates(nursing_home_dist)[,2]),
  dist
)
nh_dist1 <- nh_dist %>%
  mutate(covid_beds = residents_total_covid_19/number_of_all_beds) %>%
  arrange(desc(covid_beds)) %>%
  mutate(cum_deaths = cumsum(residents_total_covid_19)) %>%
  mutate(state_expand = case_when(
    distance_km < 100 & provider_state == "NY" ~ "NYC",
    T ~ provider_state
  ))

nh_dist2 <- top_frac(nh_dist1,.005,covid_beds)

nh_dist2 %>%
  group_by(provider_state) %>%
  summarise(num=n()) %>%
  arrange(desc(num))

#ggmap::register_google("AIzaSyDANC9iK4X1vslXncTXfppRtaSs4EWDYRA")

#map1 <- get_map(location = "USA",zoom = 4) 
#?get_map
ggmap(map1) +
  geom_point(data = nh_dist2, aes( x = lon, y = lat, color = dist_bin))

election_results1 <- election_results %>%
  mutate(result = case_when(
    trumpVotes > clintonVotes ~ "Trump",
    T ~ "Clinton"
  )) %>%
  select(state, result, marginOfVictoryPercent)

nh_dist3 <- nh_dist1 %>%
  group_by(state_expand) %>%
  summarise(occupied = sum(total_number_of_occupied,na.rm=T),covid_deaths = sum(residents_total_covid_19,na.rm=T)) %>%
  mutate(pct = covid_deaths/occupied) %>%
  left_join(election_results1, by=c("state_expand" = "state"))
  


ggplot(nh_dist3) +
  geom_statebins(
    aes(state = state_expand, fill = pct),border_col = "grey40",border_size = .4,lbl_size = 5,
    radius=grid::unit(8, "pt")
  ) +
  coord_equal() +
  #scale_fill_brewer(type = "div", palette = "RdYlGn", direction = -1) +
  #scale_fill_manual(values = c("#a6d96a","#ebeb2a","#fdae61","#d7191c")) +
  scale_fill_viridis_c(begin = 0,option = 'B', direction = 1) +
  theme_statebins() +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "bottom",
    # legend.spacing.x = unit(1, 'cm'),
    # legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8)
  ) +
  #guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank()) +
  labs(title = "COVID-19 Deaths by US State",subtitle = "Population adjusted. Split by 2016 election results.",caption = paste0("As of: ",format(Sys.Date(),"%d-%b-%y")), fill = "Deaths per Million Residents")



sum(filter(nh_dist,distance_km < 400)$residents_total_covid_19,na.rm=T)


death_curve <- deaths %>%
  #left_join(population, by = "UID") %>%
  select(UID,State=Province_State,Population, contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(State, date) %>%
  summarise(pop=sum(Population, na.rm =T), cases = sum(cases, na.rm = T)) %>%
  filter(pop > 0 ) %>%
  mutate(new_deaths = cases -lag(cases, default = 0)) %>%
  mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_deaths/pop)) %>%
  arrange(State, date) %>%
  mutate(new_deaths_roll = zoo::rollapply(new_deaths,7,mean,align = "right",fill = 0)) %>%
  group_by(State) %>%
  mutate(death_norm = new_deaths_roll/max(new_deaths_roll)) %>%
  select(State,date,new_deaths,new_deaths_roll,death_norm)


case_curve <- cases %>%
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
  mutate(new_cases_roll = zoo::rollapply(new_cases,7,mean,align = "right",fill = 0)) %>%
  group_by(State) %>%
  mutate(case_norm = new_cases_roll/max(new_cases_roll)) %>%
  mutate(day = case_when(
    case_norm == max(case_norm) ~ as.numeric(date),
    T ~ as.numeric(NA)
  )) %>%
  mutate(date_num = as.numeric(date)) %>%
  mutate(val = date_num - max(day,na.rm=T)) %>%
  #filter(State %in% c("New York","Texas","Florida","Arizona")) %>%
  left_join(death_curve, by =c("State","date"))


case_filt <- case_curve %>%
  filter(date == max(date)) %>%
  filter(val >=14 & is.infinite(val)==F) %>%
  select(State) %>%
  distinct()

case_curve1 <- case_curve %>%
  select(State,date,val,new_cases_roll,new_deaths_roll) %>%
  pivot_longer(cols = c(new_cases_roll,new_deaths_roll), names_to = "type",values_to = "num") %>%
  filter(State %in% case_filt$State)



case_curve1$type <- as.factor(case_curve1$type)

levels(case_curve1$type) <- c("Daily Cases", "Daily Deaths")







ggplot(case_curve1, aes(x=val, y=num,group=State,color=State)) +
  geom_path(size = .75) +
  scale_x_continuous(limits = c(-100,100)) +
  #scale_color_brewer(type = "qual", palette = 6) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma_format()) +
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "bottom",
    # legend.spacing.x = unit(1, 'cm'),
    # legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8)
  ) + 
  labs(
    title = "COVID Outbreaks Adjusted from Peak Day",
    subtitle = "Seven day rolling average of daily new cases and deaths",
    y = "Daily New Cases/Deaths",
    x = "Days Since Peak in New Cases"
  ) +
  facet_wrap(~type, ncol=1,scales = "free_y")



ggplot(case_curve, aes(x=val, y=case_norm,group=State,color=State)) +
  geom_path(size = 1.25) +
  scale_x_continuous(limits = c(-100,100)) +
  scale_color_brewer(type = "qual", palette = 2)


ggplot(case_curve, aes(x = val, y = new_cases_roll))
  



#### global cases ####

cases_global1 <- cases_global %>%
  #left_join(population, by = "UID") %>%
  select(country = `Country/Region`,state=`Province/State`,contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(country, date) %>%
  summarise(cases = sum(cases, na.rm = T)) %>%
  #filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  #mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(country, date) %>%
  mutate(new_case_roll = zoo::rollapply(new_cases,7,mean,align = "right",fill = 0)) %>%
  #filter(case_mil > 1) %>%
  #select(country,date) %>%
  group_by(country) %>%
  mutate(day = row_number())

cases_us <- cases %>%
  #left_join(population, by = "UID") %>%
  select(state=`Province_State`,contains("/")) %>%
  pivot_longer(cols = contains("/"), values_to = "cases", names_to  = "date") %>%
  mutate(date = as.Date(date, "%m/%d/%y")) %>%
  group_by(date) %>%
  summarise(cases = sum(cases, na.rm = T)) %>%
  #filter(pop > 0 ) %>%
  mutate(new_cases = cases -lag(cases, default = 0)) %>%
  #mutate(case_mil = 1000000 * (cases/pop),new_case_mil = 1000000 * (new_cases/pop)) %>%
  arrange(date) %>%
  mutate(new_case_roll = zoo::rollapply(new_cases,7,mean,align = "right",fill = 0)) %>%
  #filter(case_mil > 1) %>%
  #select(country,date) %>%
  #group_by(state) %>%
  mutate(day = row_number()) 




testing <- read_csv(content(GET("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/testing/covid-testing-all-observations.csv")))
           
testing1 <- testing %>%
  group_by(`ISO code`) %>%
  filter(`ISO code` == "USA" & Entity == "United States - tests performed (CDC) (incl. non-PCR)")

testing2 <- testing %>%
  group_by(`ISO code`) %>%
  filter(`ISO code` == "JPN" & Entity == "Japan - people tested (incl. non-PCR)")


cases_testing <- cases_us %>%
  left_join(testing1 ,by = c("date" = "Date")) %>%
  rowwise() %>%
  mutate(case_avg = new_case_roll / `7-day smoothed daily change per thousand`)
  
japan <- cases_global1 %>%
  filter(country == "Japan") %>%
  left_join(testing2 ,by = c("date" = "Date")) %>%
  rowwise() %>%
  mutate(case_avg = new_case_roll / `7-day smoothed daily change per thousand`)


ggplot(japan, aes( x = date, y = case_avg)) +
  geom_line(color = "darkred", size = .75) +
  geom_line(data = cases_testing, color = "darkblue",size = .75) +
  theme_minimal() + 
  theme(
    plot.subtitle = element_markdown( face = "plain", hjust = 0.5, size = 16),
    legend.position = "bottom",
    # legend.spacing.x = unit(1, 'cm'),
    # legend.background = element_rect(fill = "#e8e1e5"),
    plot.background = element_rect(fill = "#fff7fb"),
    text  = element_text(family = "Bahnschrift", size = 16),
    plot.title = element_text(size = 24,hjust = .5),
    plot.title.position = "plot",
    plot.caption = element_text(color = "grey50",size = 8)
  ) +
  labs(
    title = "New COVID-19 Cases",
    x = "Date",
    y = "New Cases"
  )
