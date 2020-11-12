library(rlang, lib.loc = "/data/library/R/3.5.1/lib64/R/library")
library(httr)
library(readr)
library(tidyverse)
library(pins)

azure_headers <- function(board, verb, path, file) {
  date <- format(Sys.time(), "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT")
  azure_version <- "2015-04-05"
  
  # allow full urls to allow arbitrary file downloads
  path <- gsub(paste0(board$url, "/"), "", path, fixed = TRUE)
  
  content_length <- ""
  content_type <- ""
  
  if (!is.null(file)) {
    content_length <- as.integer(file.info(file)$size)
    content_type <- mime::guess_type(file)
  }
  
  content <- paste(
    verb,
    "\n",
    content_length,
    "",
    content_type,
    "\n\n\n\n\n",
    paste("x-ms-blob-type", "BlockBlob", sep = ":"),
    paste("x-ms-date", date, sep = ":"),
    paste("x-ms-version", azure_version, sep = ":"),
    paste0("/", board$account, "/", board$container, "/", path),
    sep = "\n")
  
  signature <- openssl::sha256(charToRaw(content), key = base64enc::base64decode(board$key)) %>%
    base64enc::base64encode()
  
  headers <- httr::add_headers(
    `x-ms-date` = date,
    `x-ms-version` = azure_version,
    `x-ms-blob-type` = "BlockBlob",
    Authorization = paste0("SharedKey ", board$account, ":", signature)
  )
  
  headers
}

board_register_datatxt(name = "facilities_board",
                       url = "https://aztxupadhocdata01.blob.core.usgovcloudapi.net/pinboard",
                       cache = "/data/rsworkspace/shinyapps/afc_pxc/pins",
                       headers = azure_headers,
                       needs_index = FALSE,
                       container = "pinboard",
                       account = "aztxupadhocdata01",
                       key = "DUm3z0Qe1eSFxOZ/4OAwSYGMhcItuyzl6WNsiaxJ9alc9GW9NTiPcVmYAp+EV5W3+Pq6ClJpucBPqhYtfvD75w==",
                       connect = FALSE,
                       borwse_url = "https://portal.azure.us")


load("covid.rda")
load("vantage_installations.rda")
load("token.rds")

#date_pull <- format(Sys.time()- lubridate::hours(5),format = "%m-%d-%Y")
date_pull <- "04-23-2020"
cases <- read_csv(content(GET(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",date_pull,".csv")))) %>%
  mutate_if(is.numeric,as.character) %>%
  mutate(Combined_Key = paste(Admin2,Province_State,Country_Region,sep=","))



cases <- cases %>%
  mutate(Last_Update = as.POSIXct(date_pull, format = "%m-%d-%Y"))
#afc_locs$Combined_Key <-  paste(afc_locs$Admin2,afc_locs$Province_State,afc_locs$Country_Region,sep=",")
#full_cases$Combined_Key <- paste(full_cases$Admin2,full_cases$Province_State,full_cases$Country_Region,sep=",")
# cases_22mar <- read_csv(content(GET(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-22-2020.csv")))) %>%
#  mutate_all(as.character)
# cases_22mar$Last_Update <- as.POSIXct(cases_22mar$Last_Update, format = "%m/%d/%y %H:%M")
# 
# #colnames(cases_temp) <- colnames_cases
# 
# 
# full_cases <- cases
# 
# 
#   for(j in 23:23) {
# #    tryCatch({
#       print(paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/03-",j,"-2020.csv"))
#       
#       cases_temp <- mutate_all(cases_temp, as.character)
#       full_cases <- rbind(full_cases,cases_temp)
# #    }, error=function(e){})
# 
#   }
# 
#token <- "Bearer xxxxx"
#save(file = "token.rds",token)
colnames(full_cases)

full_cases <- full_cases %>%
  ungroup() #%>%
  #filter(lubridate::date(Last_Update) < lubridate::date(as.POSIXct(date_pull, format = "%m-%d-%Y")))

full_cases <- bind_rows(full_cases,cases) %>%
  mutate(day = lubridate::date(Last_Update)) %>%
  group_by(FIPS,day) %>%
  filter(Last_Update == max(Last_Update)) %>%
  distinct() %>%
  ungroup() 

full_cases <- full_cases %>%
  select(-day)

#%>%
  #filter(Last_Update < "2020-4-01")

avg_natl <- full_cases %>%
  #mutate(Last_Update = lubridate::floor_date(Last_Update,"day")) %>%
  filter(Last_Update >= "2020-3-22", Country_Region =="US") %>%
  group_by(Last_Update) %>%
  mutate_at(vars(Confirmed),as.numeric) %>%
  summarise(cases = sum(Confirmed, na.rm=T)) %>%
  ungroup() %>%
  mutate(pct = (cases-lag(cases))/lag(cases))

#summary(full_cases)
installations <- data4 %>%
  ungroup() %>%
  select(instl_cd,instl_name) %>%
  distinct()


vantage_pull <- function(dataset) {
  
  cataloghost <- "https://vantage.army.mil/foundry-data-proxy/api/dataproxy/query"
  
  van_dataset <- dataset
  
  args <-  paste0('{"query":"SELECT * FROM `master`.`',van_dataset,
                       '` "}')
  
  qry <- POST(cataloghost,
                   add_headers(Authorization = token,"Content-Type"="application/json"),
                   body = args,verbose())
  
  content <- content(qry)
  
  schema <- data.frame(Reduce(rbind, content[[1]]$fieldSchemaList))
  
  df <- data.frame(Reduce(rbind, content[[2]])) %>%
    mutate_if(is.list, as.character)
  
  colnames(df) <- schema$name
  
  df
  
}





instaldata <- "ri.foundry.main.dataset.646dd517-d27c-4e6e-9d17-365512646b20"

inst_df <- vantage_pull(instaldata)





#inst_df$instl_cd <- as.character(inst_df$instl_cd)

afc_inst_other <- read_csv("Army Futures Command Unit Location Listing Test.csv") %>%
  mutate(instl = case_when(
    Post == "Off Post" ~ City,
    is.na(Post) == T ~ City,
    TRUE ~ Post
  )) %>%
  select(instl_cd = NULL, instl_name.x = instl,instl_lat_crd = Latitude, instl_lng_crd = Longitude, on_inst_people_all_ranks = NULL) %>%
  distinct() %>%
  #mutate(instl_source = "afc_list") %>%
  bind_rows(data.frame(read_csv("locations_ccdc.csv")))


#afc_inst_other <- data.frame(afc_inst_other)
#str(afc_inst_other)
#inst_df <- data.frame(inst_df)

afc_inst <- installations %>%
  left_join(inst_df, by = "instl_cd") %>%
  select(instl_cd,instl_name.x,state,instl_lat_crd,instl_lng_crd,on_inst_people_all_ranks) %>%
  mutate_at(vars(instl_lat_crd,instl_lng_crd,on_inst_people_all_ranks),as.numeric) %>%
  #mutate(instl_source = "vantage_list") %>%
  bind_rows(afc_inst_other) %>%
  distinct() %>%
  mutate(instl_name.x = stringr::str_to_upper(instl_name.x)) %>%
  group_by(instl_name.x) %>%
  summarise(instl_lat_crd= mean(instl_lat_crd, na.rm = T),instl_lng_crd= mean(instl_lng_crd, na.rm = T),
            on_inst_people_all_ranks= mean(on_inst_people_all_ranks, na.rm = T)) %>%
  filter(is.na(instl_lat_crd) == F) %>%
  mutate(cases = 0,deaths=0,active=0,case_last = 0, deaths_last=0)
#i <- 27
#j <- 28


full_cases_mod <- full_cases %>%
  group_by(Combined_Key) %>%
  arrange(Last_Update) %>%
  mutate_at(vars(Confirmed, Deaths,Active, Lat, Long_),as.numeric) %>%
  mutate(case_last = lag(Confirmed),death_last = lag(Deaths))


full_cases_latest <- full_cases_mod %>%
  filter(Last_Update== max(Last_Update)) %>%
  mutate(afc_impact = NA) %>%
  filter(is.na(Lat) == F)

#j <- 320
loc_reference <- data.frame(installation = NA, Combined_Key = NA)
bind_rows(loc_reference ,c(installation = 1,Combined_Key = 2))
for(i in 1:nrow(afc_inst)) {
  print(i)
  
  for(j in 1:nrow(full_cases_latest)) {
    #print(j)
    if(0.621371 *sp::spDists(x = matrix(c(afc_inst[[i,3]],full_cases_latest[[j,7]], afc_inst[[i,2]],full_cases_latest[[j,6]]),nrow=2),longlat=T,segments=T) < 50) {
      afc_inst[i,5] <- afc_inst[i,5] + full_cases_latest[j,8]
      afc_inst[i,6] <- afc_inst[i,6] + full_cases_latest[j,9]
      afc_inst[i,7] <- afc_inst[i,7] + full_cases_latest[j,11]
      afc_inst[i,8] <- afc_inst[i,8] + if(is.na(full_cases_latest[j,13]) == T) {0} else {full_cases_latest[j,13]}
      afc_inst[i,9] <- afc_inst[i,9] + if(is.na(full_cases_latest[j,14]) == T) {0} else {full_cases_latest[j,14]}
      full_cases_latest[j,15] <- T
      
      loc_reference <- bind_rows(loc_reference,c(installation = afc_inst[i,1],Combined_Key = full_cases_latest[j,12]))
      
    }
  }
  
}

loc_reference <- loc_reference[,3:4]
loc_reference <- loc_reference[-1,]

colnames(loc_reference) <- c("installation","Combined_Key")
# afc_locs <- cases %>%
#   filter(afc_impact == T)


afc_inst <- afc_inst %>%
  mutate(scaled_cases = scales::rescale(log(cases,5),to=c(3,20))) %>%
  mutate(case_pct = (cases-case_last)/case_last, death_pct = (deaths - deaths_last)/deaths_last) %>%
  mutate(content = paste(sep = "<br/>",
                         paste0("<b>Intallation: </b>",instl_name.x),
                         paste0("<b>Inst. Employees: </b>",on_inst_people_all_ranks),
                         paste0("<b>Local Cases: </b>",cases),
                         paste0("<b>Local Deaths: </b>",deaths),
                         paste0("<b>Local Active: </b>",active))
  )


### personnel stuff ####
pers_mil <- "ri.foundry.main.dataset.4fd82ba1-9fb6-4cde-ab51-fe446f7ffd36"
pers_mil_df <- vantage_pull(pers_mil)

pers_loc <- pers_mil_df %>%
  mutate(geohash = paste0(round(as.numeric(latitude),0),round(as.numeric(longitude),0))) %>%
  group_by(geohash) %>%
  summarise(num = n())

pers_inst <- afc_inst %>%
  mutate(geohash = paste0(round(as.numeric(instl_lat_crd),0),round(as.numeric(instl_lng_crd),0))) %>%
  left_join(pers_loc, by= "geohash")


afc_table <- pers_inst %>%
  select(Installation = instl_name.x, `Assigned Military PAX` = num, `Local Cases` = cases, `Local Deaths` = deaths, `Case Change` = case_pct)

### time series ####

afc_locs_ts <- full_cases %>%
  left_join(afc_locs[,12:13], by="Combined_Key") %>%
  filter(afc_impact == T) %>%
  mutate(date = lubridate::floor_date(Last_Update, "day")) %>%
  group_by(date) %>%
  summarise(cases = sum(as.numeric(Confirmed), na.rm=T),deaths = sum(as.numeric(Deaths), na.rm=T)) %>%
  mutate(new_deaths=deaths-lag(deaths),new_cases = cases-lag(cases))

full_cases_ts <- full_cases %>%
  mutate(date = lubridate::floor_date(Last_Update, "day")) %>%
  filter(`Country_Region` == "US") %>%
  group_by(date) %>%
  summarise(cases = sum(as.numeric(Confirmed), na.rm=T),deaths = sum(as.numeric(Deaths), na.rm=T)) %>%
  filter(date >= min(afc_locs_ts$date)) %>%
  mutate(new_deaths = deaths-lag(deaths),new_cases = cases-lag(cases))


full_cases_latest_afc <- full_cases_latest %>%
  filter(afc_impact==T) %>%
  select(Combined_Key)

'%!in%' <- function(x,y)!('%in%'(x,y))

ccdc_locs <- stringr::str_to_upper(read_csv("locations_ccdc.csv")$instl_name.x)

full_cases_afc <- full_cases %>%
  filter(Combined_Key %in% full_cases_latest_afc$Combined_Key) %>%
  left_join(loc_reference, by = "Combined_Key") %>%
  filter(stringr::str_to_upper(installation) %!in% ccdc_locs) %>%
  mutate(installation = stringr::str_wrap(stringr::str_to_title(installation),16)) %>%
  arrange(Last_Update) %>%
  mutate(day = lubridate::date(Last_Update)) %>%
  group_by(Combined_Key,day) %>%
  filter(Last_Update == max(Last_Update)) %>%
  distinct() %>%
  ungroup() %>%
  select(-day) %>%
  group_by(installation,Last_Update) %>%
  mutate_at(vars(Confirmed,Deaths,Active), as.numeric) %>%
  summarise(Confirmed = sum(Confirmed, na.rm=T), Deaths = sum(Deaths,na.rm = T),Active = sum(Active, na.rm=T)) %>%
  mutate(pct_change = (Confirmed - lag(Confirmed)) / lag(Confirmed)) %>%
  left_join(avg_natl, by = "Last_Update") %>%
  #ungroup() %>%
  mutate(color = case_when(
    lead(pct_change) > lead(pct) ~ "darkred",
    T ~ "darkgreen"
  )) 

plot1 <- ggplot()
plot1 + geom_bar(data=afc_locs_ts,aes(x = date, y=new_deaths),stat="identity", fill="darkred",alpha=.75,width = 10000) +
  theme_minimal() +
  labs(title = "Daily Deaths",x=element_blank(),y=element_blank())

# plot1 <- ggplot(afc_locs_ts)
# plot1 + #geom_ribbon(aes(x=date, ymax = cases,ymin=new_deaths,fill="Cases"), alpha=.5) +
#   geom_ribbon(aes(x=date, ymax = deaths,ymin=0,fill="Deaths"), alpha=.5) +
#   #geom_path(data = full_cases_ts, aes(x=date, y =cases), color = "darkblue") +
#   geom_path(data = full_cases_ts, aes(x=date, y =new_deaths), color = "darkred") +
#   #scale_y_log10(labels=scales::comma_format()) +
#   scale_fill_manual(name="Designation",
#                       values=c(Cases="darkblue", Deaths="darkred")) +
#   scale_y_continuous(labels=scales::comma_format()) +
#   theme_minimal()
# 
# plot2 <- ggplot(full_cases_afc)
# plot2 + geom_path(aes(x=Last_Update, y = pct_change, color = color, group = installation),size=1.25) +
#   facet_wrap(~installation) +
#   theme_minimal() +
#   geom_line(data=avg_natl,aes(x=Last_Update,y=pct), color="grey5", alpha = .8,linetype="longdash") +
#   scale_color_identity() +
#   theme(strip.text = element_text(size = 8)) +
#   scale_y_continuous(labels = scales::percent(c(0,.5,1)), breaks = c(0,.5,1)) +
#   scale_x_datetime(breaks = c(min(full_cases_afc$Last_Update),max(full_cases_afc$Last_Update)), labels =c("",format(max(full_cases_afc$Last_Update),format = "%d-%b-%y"))) +
#   labs(x= "", y = "Percent Change (with National Average)", title ="Day-to-Day Case Change Percentage",subtitle = "Differences in local case development rates by installation (risk).",
#        caption = "Red lines indicate rate above the Natl. Avg., green lines below the Natl. Avg.") +
#   coord_cartesian(ylim =c(0,1))


save(file = "covid.rda", afc_inst, afc_table, afc_locs, full_cases, afc_locs_ts,full_cases_ts, full_cases_afc,avg_natl) %>%
  pin(board = "facilities_board", name="covid")
