# Import datasets
deaths <- read.csv("datasets/deaths_daily.csv")
tests <- read.csv("datasets/hospitalizationICU_daily.csv")
hospitalizations <- read.csv("datasets/socialRestrictions_weekly.csv")
social_restrictions <- read.csv("datasets/socialRestrictions_weekly.csv")
vaccines <- read.csv("datasets/vaccines_weekly.csv")

# VACCINES DATASET

# filtering on TargetGroup feature. Select ALL value
vaccines <- vaccines[vaccines$TargetGroup == "ALL",]
vaccines$Doses  <-  vaccines$FirstDose + vaccines$SecondDose + vaccines$DoseAdditional1 + vaccines$UnknownDose 
column_2_save <- c("YearWeekISO","ReportingCountry","Vaccine","Doses")
vaccines <- vaccines[column_2_save] 


# SOCIAL RESTRICTIONS

library(countrycode)
social_restrictions$Country <-countrycode(social_restrictions$Country, "country.name", "iso2c")

# check nan values in date_end. Will be changed in current Week
dim(social_restrictions[social_restrictions$date_end == "NA",])

library(ISOweek)
social_restrictions$date_start <- date2ISOweek(social_restrictions$date_start)
social_restrictions$date_start <- substr(social_restrictions$date_start, start=1,stop=8)


# handle date end NA
Sys.Date()
social_restrictions$date_end[social_restrictions$date_end == "NA"] <- Sys.Date()
