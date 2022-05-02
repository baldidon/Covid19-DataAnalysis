### EDA
library(dplyr)
library(ggpubr)
library(plotly)
library(ggplot2)
library(hrbrthemes)
library(ggthemes)
library(ISOweek)


dataset <- read.csv("datasets/dataset.csv")
attach(dataset)



# show preliminary informations about dataset
columns_name = colnames(dataset)
columns_name

head(dataset)

# TODO: spostare.

# FILL NA VALUES WITH ZEROS
dataset$doses[is.na(dataset$doses)] <- 0


# we have 2 sources for number of cases (from 2 different datasets)
# cases_from_tests and cases_from_deaths
# which source?

# presence of NA values
sum(is.na(cases_from_deaths))  # 9 NA values!
cases_from_deaths[is.na(cases_from_deaths)] <- 0
sum(is.na(cases_from_deaths))  # 0 NA values!
sum(is.na(cases_from_tests)) # 0 NA values!

# sample mean values

mean_from_deaths <- mean(cases_from_deaths)
mean_from_tests <- mean(cases_from_tests)

std_from_deaths <- sd(cases_from_deaths)
std_from_tests <- sd(cases_from_tests)

# we can use kolmogorov smirnov (2 sample) to support the current null_hypotesis:
# samples provided by same distribution
ks.test(cases_from_deaths,cases_from_tests)
# the resulting p-value (0.91) confirm our hypotesis and so we can choose the feature_vector to drop freely

#plot empyrical cdf with ggpubr
# plot(ecdf(cases_from_deaths), col="blue") 
# lines(ecdf(cases_from_tests), col="green") #TODO DA MIGLIORARE ASSOLUTAMENTE


# remove cases from deaths feature
dataset <- select(dataset, -"cases_from_deaths")
dataset <- dataset %>%
  rename(cases = cases_from_tests)


# FEAT.ENG. : split active_restrictions in HARD and SOFT restrictions;
hard_restrictions_set <- c("StayHomeOrder",
                        "RegionalStayHomeOrder",
                        "ClosDaycare",
                        "ClosPrim",
                        "BanOnAllEvents",
                        "ClosPubAny",
                        "ClosureOfPublicTransport",
                        "NonEssentialShops",
                        "MaskMandatoryAllSpaces")

# regex checks if resctriction contain comma or is EOL
dataset$hard_restrictions <- grepl(paste(hard_restrictions_set , collapse = "(,|$)|"),
                                  dataset$active_restrictions
                                  )

# check frequencies of hard and soft restriction
# HARD: TRUE
table(dataset$hard_restrictions)

# insert new Feature, a SOFT RESTRICTION'S COUNTER
dataset$restrictions_count <- sapply(dataset$active_restrictions, function(x) length(unlist(strsplit(as.character(x), split=","))))

# filtering based on country_code = IT and SE
dataset_it <- dataset[country_code == "IT", ]
dataset_se <- dataset[country_code == "SE", ]

length(unique(dataset_it$year_week))

dataset_final = dataset_it %>%
  inner_join(dataset_se, by = c("year_week" = "year_week"), suffix= c("_it","_se"))

detach(dataset)
attach(dataset_final)


## EXPLORE cases features. IT vs SE

# casting ISOweek format into date due to that ggplot work with date format
weeks_axis <- paste(year_week, "1", sep = "-")
weeks_axis <- ISOweek::ISOweek2date(weeks_axis)

# IT tests, cases and doses plot
# TODO: to implement log-scale!
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
  ggtitle("IT tests, cases and doses") +
  theme_excel_new()
# Turn it interactive with ggplotly
p


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
  ggtitle("IT deaths, positivity rate and doses. Weekly report") +
  theme_excel_new()

p2
# Ipotesi da discutere: sul grafico p2 si pu� notare che l'estate scende sempre 
# il positivity rate ma nell'inverno successivo all'ascesa delle somministrazioni 
# dei vaccini nonostante i contagi siano comunque saliti (-> vaccino non rende immuni) 
# le morti sono circa la meta (-> vaccino previene la morte)


## Normality test
# Graphic test using histograms and method of moments
# par(mfrow = c(2,2))
# hist(cases_it, prob=T)
# curve(dnorm(x,mean(cases_it),sd(cases_it)), add=T)
# hist(cases_se, prob=T)
# curve(dnorm(x,mean(cases_se),sd(cases_se)), add=T)
# hist(deaths_it, prob=T)
# curve(dnorm(x,mean(deaths_it),sd(deaths_it)), add=T)
# hist(deaths_se, prob=T)
# curve(dnorm(x,mean(deaths_se),sd(deaths_se)), add=T)


ph1 <- dataset_final %>%
  ggplot( aes(x=cases_it)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(cases_it,mean(cases_it),sd(cases_it))), colour = "blue", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("Histogram of cases_it") +
  theme_excel_new()

ph2 <- dataset_final %>%
  ggplot( aes(x=cases_se)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(cases_se,mean(cases_se),sd(cases_se))), colour = "red", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("Histogram of cases_se") +
  theme_excel_new()


ph3 <- dataset_final %>%
  ggplot( aes(x=deaths_it)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(deaths_it,mean(deaths_it),sd(deaths_it))), colour = "blue", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("Histogram of deaths_it") +
  theme_excel_new()

ph4 <- dataset_final %>%
  ggplot( aes(x=deaths_se)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(deaths_se,mean(deaths_se),sd(deaths_se))), colour = "red", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("Histogram of deaths_se") +
  theme_excel_new()

ggarrange(ph1,ph2,ph3,ph4,ncol=2, nrow=2, common.legend = FALSE )



# Shapiro-wilk tests
shapiro.test(cases_it)
shapiro.test(cases_se)
shapiro.test(deaths_it)<
shapiro.test(deaths_se)

# obviously no normality!


# plot cases_it vs cases_se
p3 <- dataset_final %>%
  ggplot(aes(weeks_axis)) +
  geom_line(aes(y = 20*log10(cases_it), colour = "cases_it"), lwd=1) +
  geom_line(aes(y = 20*log10(cases_se), colour = "cases_se"), lwd=1) +
  geom_line(aes(y = positivity_rate_it, colour = "positivity_rate_it"), lwd=1) +
  geom_line(aes(y = positivity_rate_se, colour = "positivity_rate_se"), lwd=1) +
  scale_colour_manual("",
                      breaks = c("cases_it", "cases_se", "positivity_rate_it", "positivity_rate_se"),
                      values = c("#C71EA8", "#0EC71A", "#001AFF", "red")) +
  xlab("Num. Week") +
  ylab("") +
  ggtitle("ITvsSE cases and positivity rate. Weekly report") +
  theme_excel_new()

p3


#
#
# social restrictions analysis (pre-vaccines)
#
#
data_first_wave <- dataset_final[year_week < "2020-W36", ]

infection_fatality_rate_it <- 100*(data_first_wave$deaths_it/data_first_wave$cases_it)  
infection_fatality_rate_se <- 100*( data_first_wave$deaths_se/data_first_wave$cases_se)  

# geom_line(aes(y = infection_fatality_rate_it, colour = "infection_fatality_rate_it")) +

#  geom_line(aes(y = infection_fatality_rate_se, colour = "infection_fatality_rate_se")) +


p4 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (cases_it), colour = "Italy"), lwd=1) +
  geom_line(aes(y = (cases_se), colour = "Sweden"), lwd=1) +
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  geom_density()+
  xlab("Num. Week") +
  ylab("Num Cases") +
  ggtitle("ITvsSE cases. Weekly report")+
  theme_excel_new()


p5 <- ggplot(data=data_first_wave, aes(weeks_axis[1:nrow(data_first_wave)]))+
  geom_bar(aes(y=restrictions_count_it),stat="identity",position="identity",alpha=0.7,fill="red")+
  
  geom_bar(aes(y=restrictions_count_se),stat="identity",position="identity",fill="blue")+
  xlab("Num. Week") +
  ylab("Num restrictions") +
  ggtitle("ITvsSE restrictions. Weekly report")+
  theme_excel_new()

ggarrange(p4, p5,ncol=1, nrow=2, common.legend = TRUE)

# TODO: SUBPLOTTARE altre metriche

# DA RIMUOVERE: Possiamo notare come, in una fase embrionale della pandemia si evince
# come l'andamento settimanale dei casi, in Italia, sia drasticamente calato



p6 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (deaths_it), colour = "Italy"), lwd=1) +
  geom_line(aes(y = (deaths_se), colour = "Sweden"), lwd=1) +
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  geom_density()+
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("ITvsSE deaths. Weekly report")+
  theme_excel_new()


ggarrange(p4, p6,ncol=1, nrow=2, common.legend = TRUE)



p7 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (deaths_it), colour = "Italy"), lwd=1) +
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  geom_density()+
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("IT FATALITY RATE. Weekly report")+
  theme_excel_new()


p8 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (deaths_se), colour = "Sweden"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  geom_density()+
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("SE FATALITY RATE. Weekly report")+
  theme_excel_new()

ggarrange(p7, p8,ncol=2, nrow=1, common.legend = TRUE)


# hospitalizations:
# anche se dati non confrontabili, mostriamo andamento "nuove ospedalizzazioni"
# per 100k (in svezia si intende nuove ammissioni in TERAPIA INTENSIVA)

p9 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (600*hospitalizations_it), colour = "Italy"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  geom_density()+
  xlab("Week") +
  ylab("Admission") +
  ggtitle("new IT hospitalizations Weekly report")+
  theme_excel_new()

p10 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (100*hospitalizations_se), colour = "Sweden"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  geom_density()+
  xlab("Week") +
  ylab("admission") +
  ggtitle("new SE ICU Weekly report")+
  theme_excel_new()

ggarrange(p9, p10,ncol=2, nrow=1, common.legend = TRUE)


p11 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (cases_it), colour = "Italy"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("new IT cases Weekly report")+
  theme_excel_new()


p12 <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (cases_se), colour = "Sweden"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("new SE cases Weekly report")+
  theme_excel_new()


ggarrange(ggarrange(p11, p12,ncol=2, common.legend = TRUE),
          ggarrange(p9, p10,ncol=2, common.legend = TRUE),
          nrow=2)

# scrivere commenti su differenza di metrica


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################


# ANALYSIS (with vaccines)
# filter data
data_with_vaccines <- dataset_final[year_week >= "2021-W01",]
weeks <- nrow(data_with_vaccines)
first_week <- length(weeks_axis) - weeks 


p_vaccines_it <- data_with_vaccines %>%
  ggplot( aes(weeks_axis[-seq(1:first_week)])) +
  geom_line(aes(y = (doses_it), colour = "Italy"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("new IT Vaccines doses injected. Weekly report")+
  theme_excel_new()

p_vaccines_se <- data_with_vaccines %>%
  ggplot( aes(weeks_axis[-seq(1:first_week)])) +
  geom_line(aes(y = (doses_se), colour = "Sweden"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("new SE Vaccines doses injected. Weekly report")+
  theme_excel_new()


ggarrange(p_vaccines_it,p_vaccines_se,ncol=2, common.legend = TRUE)



