### EDA
library(dplyr)
library(ggpubr)
library(plotly)
library(ggplot2)
library(hrbrthemes)
library(ISOweek)


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

# casting ISOweek format into date due to that ggplot work with date format
weeks_axis = paste(year_week, "1", sep="-")
weeks_axis <- ISOweek::ISOweek2date(weeks_axis)

# IT tests, cases and doses plot
p <- dataset_final %>%
  ggplot(aes(weeks_axis)) +
  geom_line(aes(y=100*cases_it/positivity_rate_it, colour="Tests")) +
  geom_line(aes(y=cases_it, colour="Cases")) +
  geom_line(aes(y=cumsum(doses_it/100), colour="Doses")) +
  scale_colour_manual("", 
                      breaks = c("Tests", "Cases", "Doses"),
                      values = c("#C71EA8", "#0EC71A", "#001AFF")) +
  xlab("Num. Week") +
  ylab("") +
  theme_ipsum()
# Turn it interactive with ggplotly
p <- ggplotly(p)
p

# TODO: le costanti usate per scalare i dati sono provviorie 

# IT deaths, positivity rate and doses plot
p2 <- dataset_final %>%
  ggplot(aes(weeks_axis))+
  geom_line(aes(y=deaths_it, colour="Deaths")) +
  geom_line(aes(y=positivity_rate_it*100, colour="Positivity Rate")) +
  geom_line(aes(y=cumsum(doses_it/100000), colour="Doses")) +
  scale_colour_manual("", 
                      breaks = c("Deaths", "Positivity Rate", "Doses"),
                      values = c("#C71EA8", "#0EC71A", "#001AFF")) +
  xlab("Num. Week") +
  ylab("") +
  theme_ipsum()

p2 <- ggplotly(p2)
p2
# Ipotesi da discutere: sul grafico p2 si può notare che l'estate scende sempre 
# il positivity rate ma nell'inverno successivo all'ascesa delle somministrazioni 
# dei vaccini nonostante i contagi siano comunque saliti (-> vaccino non rende immuni) 
# le morti sono circa la meta (-> vaccino previene la morte)


## Normality test 

# Graphic test using histograms and method of moments
par(mfrow=c(2,2))
hist(cases_it, prob=T)
curve(dnorm(x,mean(cases_it),sd(cases_it)), add=T)
hist(cases_se, prob=T)
curve(dnorm(x,mean(cases_se),sd(cases_se)), add=T)
hist(deaths_it, prob=T)
curve(dnorm(x,mean(deaths_it),sd(deaths_it)), add=T)
hist(deaths_se, prob=T)
curve(dnorm(x,mean(deaths_se),sd(deaths_se)), add=T)

# Shapiro-wilk tests
shapiro.test(cases_it)
shapiro.test(cases_se)
shapiro.test(deaths_it)
shapiro.test(deaths_se)






