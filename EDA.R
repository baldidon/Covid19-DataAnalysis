### EDA
library(dplyr)
library(ggpubr)
library(plotly)
library(ggplot2)
library(hrbrthemes)

dataset = read.csv("datasets/dataset.csv")
attach(dataset)


# show preliminary informations about dataset
columns_name = colnames(dataset)
# columns_name

head(dataset)

# TODO: spostare.

# FILL NA VALUES WITH ZEROS
dataset$doses[is.na(dataset$doses)] <- 0



# we have 2 sources for number of cases (from 2 different datasets)
# cases_from_tests and cases_from_deaths

# presence of NA values
sum(is.na(cases_from_deaths))  # 9 NA values!
cases_from_deaths[is.na(cases_from_deaths)] <- 0
sum(is.na(cases_from_deaths))  # 0 NA values!
sum(is.na(cases_from_tests)) # 0 NA values!

# sample mean values

mean_from_deaths = mean(cases_from_deaths) 
mean_from_tests = mean(cases_from_tests)

std_from_deaths = sd(cases_from_deaths)
std_from_tests = sd(cases_from_tests)

# we can use kolmogorov smirnov (2 sample) to support the current null_hypotesis:
# samples provided by same distribution
ks.test(cases_from_deaths,cases_from_tests)
# the resulting p-value (0.91) confirm our hypotesis and so we can choose the feature_vector to drop freely

#plot empyrical cdf with ggpubr
plot(ecdf(cases_from_deaths), col="blue") 
lines(ecdf(cases_from_tests), col="green") #TODO DA MIGLIORARE ASSOLUTAMENTE


# remove cases from deaths feature
dataset = select(dataset, -"cases_from_deaths")
dataset <- dataset %>%
  rename(cases = cases_from_tests)


# filtering based on country_code = IT and SE
dataset_it = dataset[country_code == "IT",]
dataset_se = dataset[country_code == "SE",]

length(unique(dataset_it$year_week))

dataset_final = dataset_it %>%
  inner_join(dataset_se, by = c("year_week" = "year_week"), suffix= c("_it","_se"))

detach(dataset)
attach(dataset_final)

## EXPLORE cases features. IT vs SE



#plot
#Usual area chart
p <- dataset_final %>%
  ggplot(aes(seq(1,length(cases_it)))) +
  geom_line(aes(y=100*cases_it/positivity_rate_it),color="#C71EA8") +
  geom_line(aes(y=cases_it),color="#0EC71A") +
  geom_line(aes(y=cumsum(doses_it)),color="#001AFF") +
  ylab("") +
  theme_ipsum()

p <- ggplotly(p)
p
# Turn it interactive with ggplotly
