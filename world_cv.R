## Covid-2019 interactive mapping tool: script to reformat JHU data from scratch
## This App adapted from R-App of: https://github.com/eparker12/nCoV_tracker which create by:
## Edward Parker and Quentic Leclerc, London School of Hygiene & Tropical Medicine, March 2019
## Data extracted from Johns Hopkins data obtained from following Github repository
# https://github.com/CSSEGISandData/COVID-19

# Thank you to Edward Parker and Quentic Leclerc.

#########################################################################
# Function to update jhu input data according to mapping base format
update_jhu <- function(input_df){
  names(input_df)[1:2] = c("Province", "Country")
  input_df$Country[input_df$Province=="Hong Kong"] = "Hong Kong"
  input_df$Country[input_df$Province=="Macau"] = "Macao"
  input_df$Country[input_df$Country=="Taiwan*"] = "Taiwan"
  input_df$Country[input_df$Country=="Korea, South"] = "Republic of Korea"
  input_df$Country[input_df$Country=="Congo (Brazzaville)" | input_df$Country=="Republic of the Congo"] = "Congo"
  input_df$Country[input_df$Country=="Congo (Kinshasa)"] = "Democratic Republic of the Congo"
  input_df$Country[input_df$Country=="Cote d'Ivoire"] = "CotedIvoire"
  input_df$Country[input_df$Country=="Gambia, The"] = "TheGambia"
  input_df$Country[input_df$Country=="Bahamas, The"] = "TheBahamas"
  input_df$Country[input_df$Country=="Cabo Verde"] = "CapeVerde"
  input_df$Country[input_df$Country=="Timor-Leste"] = "TimorLeste"
  input_df$Country[input_df$Country=="Guinea-Bissau"] = "GuineaBissau"
  input_df$Country = input_df$Country %>% str_replace_all(., " ", "")
  
  #Group by country and sum cumulative for each patient
  input_df <- input_df%>%
    select(-c(Province, Lat, Long))%>%
    group_by(Country)%>%
    summarise_each(funs(sum))%>%
    #transpose table
    pivot_longer(-1) %>%
    pivot_wider(names_from = 1, values_from = value)%>%
    mutate(
      date = as_date(name, tz = NULL, format = "%m/%d/%y"),
    )%>%
    relocate(date, .before = name)%>%
    select(-name)
  
  input_df
}

## function for transpose table 
flip <- function(tbl){
  tbl <- tbl%>%
    pivot_longer(-1) %>%
    pivot_wider(names_from = 1, values_from = value)
  
  names(tbl)[1] <- "Country"
  tbl
}

## merge 3 cases (cases, deaths, recover)
merge <- function(cases, deaths, rec){
  jhu_merge <- cases%>%
    left_join(deaths, by='Country')%>%
    left_join(rec, by = "Country")
  
  jhu_merge
}

# Re-pattern table
data_collect <- function(merge_tbl, case_tbl, collated_data){
  ##For Test##
  # merge_tbl <- jhu_Daily
  # case_tbl <- jhu_cases_daily
  # collated_data <- world_daily %>%
  #   select(-c(country, last_update))
  # i <- 2
  
  x <- ncol(case_tbl)-1
  names(collated_data) = c("Country", "date", "update", "cases", "new_cases", "deaths", "new_deaths", "recovered", "new_recovered")
  
  for (i in seq(1:x)){
    #create placeholder df
    new_data <- merge_tbl%>%
      select(c(1, i+1, i+x+1, i+2*x+1)) #select column match
    
    #fetch date
    date <- as.Date(names(new_data)[4], "%Y-%m-%d")
    names(new_data) <- c('Country', 'Dcase', 'Ddeath', 'Drec')
    
    new_data <- new_data%>%
      filter(Dcase > 0 | Ddeath > 0 )%>%# cut zero cases
      #filter((.[[2]] > 0) | (.[[3]] > 0) )%>%# cut zero cases
      mutate(
        date = date,
        update = max(collated_data$update)+1,
        cases = Dcase,
        new_cases = 0,
        deaths = Ddeath,
        new_deaths = 0,
        recovered = Drec,
        new_recovered =0
      )%>%
      select(Country, date:new_recovered)
    # select(-names(.)[2:4]) #cut temporary
    
    # New country report (Country which not ever report)
    new_country <- new_data%>%
      filter(!Country %in% collated_data$Country)%>%
      mutate(
        new_cases = cases,
        new_deaths = deaths,
        new_recovered = recovered
      )
    
    #N ew data of ever recorded country
    old_country <- new_data%>%
      filter(Country %in% collated_data$Country)
    
    # Pull last report of ever recorded country
    Umax = max(collated_data$update)
    last_report <- collated_data%>%
      filter(Country %in% old_country$Country)%>%
      arrange(Country, cases)%>% #sort
      filter(update == Umax)
    
    # Calculate new_... for ever recorded country && finally merge all
    collated_data <- old_country%>%
      mutate(
        new_cases = cases - last_report$cases,
        new_deaths = deaths - last_report$deaths,
        new_recovered = recovered - last_report$recovered) %>%
      bind_rows(new_country) %>%
      bind_rows(collated_data) %>%
      arrange(date, Country)
  }
  collated_data
}

## Clean negative value
neg_clean <- function(tbl){
  tbl <- tbl%>%
    mutate(
      new_cases = replace(new_cases, new_cases < 0, 0),
      new_deaths = replace(new_deaths, new_deaths < 0, 0),
      new_recovered = replace(new_recovered, new_recovered < 0, 0))
  tbl
}

final <- function(tbl){
  # set new names
  names(tbl) <- c("jhu_ID", "date", "update", "cases", "new_cases",
                  "deaths", "new_deaths", "recovered", "new_recovered")
  tbl <- tbl%>%
    arrange(jhu_ID)%>%
    left_join(countries[, c("jhu_ID", "country")], by = "jhu_ID")%>% #join with country
    arrange(date, -cases, country)%>%
    mutate(
      last_update = NA)#added column
  
  tbl$last_update[nrow(tbl)] = paste(format(as.POSIXlt(Sys.time(), "GMT"), "%d %B %Y"))#update time stamp
  tbl
}

week_cal <- function(x){
  dweek = 1+floor(x/7)
  ifelse(dweek > 4, 4, dweek)
}

#########################################################################
# Load latest Covid-2019 data: confirmed cases from Johns Hopkins
if(!exists('jhu_cases')){
  jhu_cases <-   readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") 
}

if(!exists('world_daily')){
  world_daily <- readr::read_csv("data/world_Daily.csv")
}

# Time machine for ack calculation
# world_daily <- world_daily %>%
#   filter(date < ymd('2022-03-16'))

LAST_REPORT <- ymd(max(world_daily$date))
new_report <- which(names(jhu_cases %>% #list of index of new report date [771, 772, 773,..]
                            as_tibble() %>%
                            select(-c(1:4))) %>% #delete first 4 index
                      mdy() > LAST_REPORT) %>%
  +4L #add 4 index back

if((length(new_report) == 0)){
  paste0('Data is up to date.No action Talk only!!!')
}else{
  paste0('There are new cases')
  ##Group data to daily, weekly and monthly case
  jhu_cases_daily <- jhu_cases%>%
    as_tibble() %>%
    select(c(1:4, new_report))%>% # select only new report indicated
    update_jhu()
  
  # Load latest Covid-2019 data: deaths cases
  jhu_deaths <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") 
  jhu_deaths_daily <- jhu_deaths%>%
    as_tibble() %>%
    select(c(1:4, new_report)) %>% # select only new report (diff from archived)
    update_jhu()
  
  # Load latest Covid-2019 data: recovered
  jhu_rec <- readr::read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",)
  jhu_rec_daily <- jhu_rec%>%
    as_tibble() %>%
    select(c(1:4, new_report)) %>% # select only new report (diff from archived)
    update_jhu()
  
  # Transpose all table
  jhu_cases_daily <- flip(jhu_cases_daily)
  jhu_deaths_daily <- flip(jhu_deaths_daily)
  jhu_rec_daily <- flip(jhu_rec_daily)
  
  ##Country name in JHU report must be match country name in map file.
  # Process for clean
  # Load country data
  countries <- readr::read_csv("data/countries_codes_and_coordinates.csv")
  
  jhu_Daily <- merge(jhu_cases_daily, jhu_deaths_daily, jhu_rec_daily )%>%
    filter(Country %in% countries$jhu_ID)
  
  #Re-pattern of table
  jhu_Daily <- data_collect(jhu_Daily, jhu_cases_daily, world_daily %>% select(-c(country, last_update)))
  
  # Clean negative value : new_cases < 0 , new_deaths < 0
  jhu_Daily <- neg_clean(jhu_Daily)
  jhu_Daily <- final(jhu_Daily)
  
  #--------------------------------------------------------------------
  # For weekly cases
  jhu_Weekly <- jhu_Daily %>%
    mutate(
      year = year(date),
      month = month(date),
      day = mday(date),
      week = week_cal(day),# calculated week of month label(1, 2, 3, 4)
      week_day = wday(date, label = TRUE)# added week day label(Sun, Mon, Tue,...)
    )%>%
    group_by(jhu_ID, year, month, week) %>%
    # Calculate true accumulate 'new' cases 
    mutate(
      new_cases = sum(new_cases),
      new_deaths = sum(new_deaths),
      new_recovered = sum(new_recovered),
    ) %>%
    filter(day == max(day)) %>%
    # filter(jhu_ID == 'US') %>%
    ungroup() %>%
    select(-c(year, month, day, week, week_day))
  
  jhu_Monthly <- jhu_Daily %>%
    mutate(
      year = year(date),
      month = month(date),
      day = mday(date)) %>%
    group_by(jhu_ID, year, month) %>%
    # Calculate true accumulate 'new' cases 
    mutate(
      new_cases = sum(new_cases),
      new_deaths = sum(new_deaths),
      new_recovered = sum(new_recovered),
    ) %>%
    filter(day == max(day)) %>% #we get day of month end and a present day
    filter(day %in% c(28, 29, 30, 31)) %>% #cut non end of month day
    ungroup() %>%
    select(-c(year, month, day))
  
  #--------------------------------------------------------------------
  # save file
  write.csv(jhu_Daily, "data/world_daily.csv", row.names = FALSE)
  write.csv(jhu_Weekly, "data/world_weekly.csv", row.names = FALSE)
  write.csv(jhu_Monthly, "data/world_monthly.csv", row.names = FALSE)
  
  # Clear all 
  rm(list = ls())
}



