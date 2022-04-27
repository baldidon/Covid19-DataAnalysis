library(ISOweek)
library(dplyr)
library(countrycode)

# Import datasets
deaths <- read.csv("datasets/deaths_daily.csv")
#hospitalizations<- read.csv("datasets/hospitalizationICU_daily.csv")
tests <- read.csv("datasets/tests_weekly.csv")
social_restrictions <- read.csv("datasets/socialRestrictions_weekly.csv")
vaccines <- read.csv("datasets/vaccines_weekly.csv")

# VACCINES DATASET

# filtering on TargetGroup feature. Select ALL value
vaccines <- vaccines[vaccines$TargetGroup == "ALL",]
vaccines$Doses  <-  vaccines$FirstDose + vaccines$SecondDose + vaccines$DoseAdditional1 + vaccines$UnknownDose 
column_2_save <- c("YearWeekISO","ReportingCountry","Doses")
vaccines <- vaccines[column_2_save]

# grouping weekly jabbed doses
vaccines <- vaccines %>%
  group_by(YearWeekISO,ReportingCountry) %>%
  summarise(Doses = sum(Doses))


#renaming YwarWeekISO and ReportingCountry columns
vaccines <- vaccines %>%
  rename(year_week = YearWeekISO, country_code =  ReportingCountry)




# SOCIAL RESTRICTIONS DATASETS

social_restrictions$Country <-countrycode(social_restrictions$Country, "country.name", "iso2c")

# check nan values in date_end. Will be changed in current Week
dim(social_restrictions[social_restrictions$date_end == "NA",])

social_restrictions$date_start <- date2ISOweek(social_restrictions$date_start)
social_restrictions$date_start <- substr(social_restrictions$date_start, start=1,stop=8)

is.na(social_restrictions)

# handle date_end NA values
Sys.Date()
social_restrictions$date_end[is.na(social_restrictions$date_end)] <- format(Sys.Date())
social_restrictions$date_end <- date2ISOweek(social_restrictions$date_end)
social_restrictions$date_end <- substr(social_restrictions$date_end, start=1,stop=8)

#renaming Country column
social_restrictions <- social_restrictions %>%
  rename(country_code = Country)




# DEATHS DATASET

# handle week-ISO date format
deaths$dateRep <- as.Date(deaths$dateRep, format = "%d/%m/%Y")
deaths$year_week <- date2ISOweek(deaths$dateRep)
deaths$year_week<- substr(deaths$year_week, start=1,stop=8)

# removing unused features
column_2_save <- c("cases","deaths","geoId", "year_week")
deaths <- deaths[column_2_save]

# group by deaths and cases by geoID and year_week
deaths <- deaths %>%
  group_by(geoId,year_week) %>%
  summarise(cases = sum(cases), deaths = sum(deaths))

#renaming geoid column
deaths <- deaths %>%
  rename(country_code = geoId)


# TESTS DATASET

#filtering on "national" on tests$level
tests <- tests[tests$level == "national",]

column_2_save <- c("country_code","year_week","new_cases","positivity_rate")
tests <- tests[column_2_save]



# # HOSPITALIZATION DATASETS
# 
# unique(hospitalizations$indicator)
# table(hospitalizations$indicator)
# 
# column_2_save <- c("country","indicator","year_week","value")
# hospitalizations <- hospitalizations[column_2_save]
# 
# # handle hospitalizations indicator
# unique(hospitalizations$indicator)
# table(hospitalizations$indicator)
# 
# # filter only Daily indicators
# hospitalizations <- dplyr::filter(hospitalizations, grepl("Weekly new hos",indicator))
# unique(hospitalizations$indicator)
# #table(hospitalizations$indicator)
# 
# 
# # grouping "value"by country and year_week
# #hospitalizations <- hospitalizations %>%
# #  group_by(country,year_week,indicator) %>%
# #  summarise(value = sum(value))
# 
# 
# #unique(hospitalizations$indicator)
# #table(hospitalizations$indicator)




# MERGING DATASETS
dataset <- merge(tests,deaths, by=c("year_week","country_code"))
dataset <- merge(dataset,vaccines, by=c("year_week","country_code"), all.x=TRUE )

