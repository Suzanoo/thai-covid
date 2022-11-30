#------------------------------------------------------------------------
## Function for pre-processing
# add counter(column = update) to data
counter <- function(tbl){
  df <- unique(tbl$date)%>% #fetch date of new report
    as_tibble()%>% #cast them to table
    bind_cols(c(1:length((unique(tbl$date)))))%>% 
    set_names(c("date", "update"))%>%
    right_join(tbl, by = "date")
  df
}

# Calculate accumulate for new_case and new_death for weekly, monthly
accum_cal <- function(tbl, collated_data){
  
  collated <- collated_data
  x <- max(unique(tbl$update)) #how many update count
  
  for (i in seq(1:x)){
    last <- collated %>%
      filter(date == max(date))
    
    present <- tbl %>%
      filter(update == i)%>%
      mutate(
        new_case = abs(last$total_case - total_case),
        new_death = abs(last$total_death - total_death)) 
    
    collated <- collated %>%
      bind_rows(present)
  }
  collated
}

week_cal <- function(x){
  dweek = 1+floor(x/7)
  ifelse(dweek > 4, 4, dweek)
}

# --------------------------------
# call again after remove in world.R
province_daily0 <- read_csv('data/province_daily.csv')

# Read data from source
province_daily2 <- jsonlite::fromJSON("https://covid19.ddc.moph.go.th/api/Cases/timeline-cases-by-provinces")

THAI_LAST_REPORT <- ymd(max(province_daily0$date))

# thai_new_report <- province_daily2%>%
#   as_tibble() %>%
#   filter(update_date > THAI_LAST_REPORT)%>%
#   select(update_date)%>%
#   unique()%>%
#   pull() #Cast tibble to vector

# -----------------------------
province <- read_csv('data/province.csv')

# # get population
pop <- readr::read_csv("data/thai_population_2020.csv") %>%
  na.omit()%>%
  mutate(ADM1_PCODE= str_to_upper(ADM1_Pcode))%>%
  select(ADM1_PCODE, Both_TOTAL)

## download dataset and add province code into data
province_daily2 <- province_daily2 %>%
  as_tibble() %>%
  mutate(date = as_date(update_date),
         ADM1_TH = province) %>%
  relocate(date, .before = update_date) %>%
  relocate(ADM1_TH, .after = date) %>%
  ## I found data has problem (There are duplicate row since 28-12-2021) --> I fixed it below)
  group_by(date, ADM1_TH) %>% 
  filter(total_case == max(total_case)) %>% 
  filter(!duplicated(ADM1_TH)) %>% 
  ungroup() %>%
  select(-c("update_date", "province", "new_case_excludeabroad", "total_case_excludeabroad"))

# create province daily cases
province_daily <- province_daily2 %>%
  left_join(province, by = "ADM1_TH")%>%
  left_join(pop, by = "ADM1_PCODE")%>%
  na.omit()%>%
  mutate(Population = Both_TOTAL)%>%
  select(-Both_TOTAL)%>%
  filter(date > THAI_LAST_REPORT) %>%
  counter() %>%
  accum_cal(province_daily0) %>% 
  select(-c(year, weelnum))

province_weekly <- province_daily %>%
  mutate(
    year = year(date),
    month = month(date),
    day = mday(date),
    week = week_cal(day),# calculated week of month label(1, 2, 3, 4)
    week_day = wday(date, label = TRUE)# added week day label(Sun, Mon, Tue,...)
  )%>%
  group_by(ADM1_TH, year, month, week) %>%
  # Calculate true accumulate 'new' cases 
  mutate(
    new_case = sum(new_case),
    new_death = sum(new_death),
  ) %>%
  filter(day == max(day)) %>%
  ungroup() %>%
  select(-c(year, month, day, week, week_day, weeknum))

province_monthly <- province_daily %>%
  mutate(
    year = year(date),
    month = month(date),
    day = mday(date)) %>%
  group_by(ADM1_TH, year, month) %>%
  #Calculate true accumulate 'new' cases 
  mutate(
    new_case = sum(new_case),
    new_death = sum(new_death),
  ) %>%
  filter(day == max(day)) %>% #we get day of month end and a present day
  filter(day %in% c(28, 29, 30, 31)) %>% #cut present day
  ungroup() %>%
  select(-c(year, month, day, weeknum))

# create whole country
thai_daily <- province_daily%>%
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>%
  ungroup()%>%
  select(-c(update, year, weeknum))

thai_weekly <- province_weekly %>%
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>%
  ungroup()%>%
  select(-c(update, year, weeknum))

thai_monthly <- province_monthly %>%
  group_by(date) %>%
  summarise(across(where(is.numeric), sum)) %>%
  ungroup()%>%
  select(-c(update, year, weeknum))

write.csv(province_daily, "data/province_daily.csv", row.names = FALSE)
write.csv(province_weekly, "data/province_weekly.csv", row.names = FALSE)
write.csv(province_monthly, "data/province_monthly.csv", row.names = FALSE)
write.csv(thai_daily, "data/thai_daily.csv", row.names = FALSE)
write.csv(thai_weekly, "data/thai_weekly.csv", row.names = FALSE)
write.csv(thai_monthly, "data/thai_monthly.csv", row.names = FALSE)

rm(list = ls())

