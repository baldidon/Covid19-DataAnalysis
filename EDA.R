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

## TODO: cambiare nomi grafici

# casting ISOweek format into date due to that ggplot work with date format
weeks_axis <- paste(year_week, "1", sep = "-")
weeks_axis <- ISOweek::ISOweek2date(weeks_axis)

# IT tests, cases and doses plot
# TODO: to implement log-scale!
# p_it <- dataset_final %>%
#   ggplot(aes(weeks_axis)) +
#   geom_line(aes(y=100*cases_it/positivity_rate_it, colour="Tests"), lwd=1) +
#   geom_line(aes(y=cases_it, colour="Cases"), lwd=1) +
#   geom_line(aes(y=cumsum(doses_it), colour="Doses"), lwd=1) +
#   scale_colour_manual("", 
#                       breaks = c("Tests", "Cases", "Doses"),
#                       values = c("#C71EA8", "#0EC71A", "#001AFF")) +
#   xlab("Week") +
#   ylab("") +
#   # ggtitle("IT tests, cases and doses") +
#   theme_bw()
# p_it #+ theme(legend.position = "bottom")


# IT deaths, positivity rate and doses plot
# p2 <- dataset_final %>%
#   ggplot(aes(weeks_axis))+
#   geom_line(aes(y=deaths_it, colour="Deaths")) +
#   geom_line(aes(y=positivity_rate_it*100, colour="Positivity Rate")) +
#   geom_line(aes(y=cumsum(doses_it/100000), colour="Doses")) +
#   scale_colour_manual("", 
#                       breaks = c("Deaths", "Positivity Rate", "Doses"),
#                       values = c("#C71EA8", "#0EC71A", "#001AFF")) +
#   xlab("Num. Week") +
#   ylab("") +
#   ggtitle("IT deaths, positivity rate and doses. Weekly report") +
#   theme_excel_new()
# p2


## Normality test
# Graphic test using histograms and method of moments
ph1 <- dataset_final %>%
  ggplot( aes(x=cases_it)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(cases_it,mean(cases_it),sd(cases_it))), colour = "red", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("IT Cases") +
  theme_excel_new()

ph2 <- dataset_final %>%
  ggplot( aes(x=cases_se)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(cases_se,mean(cases_se),sd(cases_se))), colour = "blue", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("SE Cases") +
  theme_excel_new()

ph3 <- dataset_final %>%
  ggplot( aes(x=deaths_it)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(deaths_it,mean(deaths_it),sd(deaths_it))), colour = "red", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("IT Deaths") +
  theme_excel_new()

ph4 <- dataset_final %>%
  ggplot( aes(x=deaths_se)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(deaths_se,mean(deaths_se),sd(deaths_se))), colour = "blue", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("SE Deaths") +
  theme_excel_new()

ggarrange(ph1,ph2,ph3,ph4,ncol=2, nrow=2, common.legend = FALSE )



# Shapiro-wilk tests
shapiro.test(cases_it)
shapiro.test(cases_se)
shapiro.test(deaths_it)
shapiro.test(deaths_se)

# obviously no normality!


# plot cases_it vs cases_se
p_cases <- dataset_final %>%
  ggplot(aes(weeks_axis)) +
  geom_line(aes(y = 20*log10(cases_it), colour = "IT Cases"), lwd=1.2) +
  geom_line(aes(y = 20*log10(cases_se), colour = "SE Cases"), lwd=1.2) +
  geom_line(aes(y = positivity_rate_it, colour = "IT Positivity Rate"), lwd=1.2) +
  geom_line(aes(y = positivity_rate_se, colour = "SE Positivity Rate"), lwd=1.2) +
  scale_colour_manual("",
                      breaks = c("IT Cases", "SE Cases", "IT Positivity Rate", "SE Positivity Rate"),
                      values = c("red", "blue", "orange", "purple")) +
  scale_y_continuous(
    name = "Cases",
    sec.axis = sec_axis(~.,"Positivity Rate")
  ) +
  xlab("Num. Week") +
  ylab("") +
  # ggtitle("ITvsSE cases and positivity rate. Weekly report") +
  theme_excel_new() +
  theme(legend.position = "bottom", axis.title = element_text(size=11))
p_cases

detach(dataset_final)

################################################################################
################################################################################
## social restrictions analysis (pre-vaccines)
################################################################################
################################################################################


data_first_wave <- dataset_final[dataset_final$year_week < "2020-W36", ]
attach(data_first_wave)

infection_fatality_rate_it <- 100*(deaths_it/cases_it)  
infection_fatality_rate_se <- 100*(deaths_se/cases_se)  

# geom_line(aes(y = infection_fatality_rate_it, colour = "infection_fatality_rate_it")) +

#  geom_line(aes(y = infection_fatality_rate_se, colour = "infection_fatality_rate_se")) +


p_cases_compare <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (cases_it), colour = "Italy"), lwd=1.2) +
  geom_line(aes(y = (cases_se), colour = "Sweden"), lwd=1.2) +
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Weeks") +
  ylab("Cases") +
  #ggtitle("ITvsSE cases. Weekly report")+
  theme_excel_new() +
  theme(legend.position = "bottom", axis.title = element_text(size=11))


p_restrictions <- ggplot(data=data_first_wave, aes(weeks_axis[1:nrow(data_first_wave)]))+
  geom_bar(aes(y=restrictions_count_it),stat="identity",position="identity",alpha=0.7,fill="red")+
  geom_bar(aes(y=restrictions_count_se),stat="identity",position="identity",fill="blue")+
  xlab("Week") +
  ylab("Num. restrictions") +
  #ggtitle("ITvsSE restrictions. Weekly report")+
  theme_excel_new() +
  theme(axis.title = element_text(size=11))

ggarrange(p_cases_compare, p_restrictions,ncol=1, nrow=2)

# TODO: subplot other metrics

# DA RIMUOVERE: Possiamo notare come, in una fase embrionale della pandemia si evince
# come l'andamento settimanale dei casi, in Italia, sia drasticamente calato


p_deaths <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (deaths_it), colour = "Italy"), lwd=1.2) +
  geom_line(aes(y = (deaths_se), colour = "Sweden"), lwd=1.2) +
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Deaths") +
  #ggtitle("ITvsSE deaths. Weekly report")+
  theme_excel_new() +
  theme(legend.position = "bottom", axis.title = element_text(size=11))

ggarrange(p_cases_compare, p_deaths,ncol=1, nrow=2, common.legend = TRUE, legend = "bottom")



# p7 <- data_first_wave %>%
#   ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
#   geom_line(aes(y = (infection_fatality_rate_it), colour = "Italy"), lwd=1) +
#   scale_colour_manual("",
#                       breaks = c("Italy", "Sweden"),
#                       values = c("red", "blue")) +
#   geom_density()+
#   xlab("Week") +
#   ylab("Deaths") +
#   ggtitle("IT FATALITY RATE. Weekly report")+
#   theme_excel_new()
# 
# 
# p8 <- data_first_wave %>%
#   ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
#   geom_line(aes(y = (infection_fatality_rate_se), colour = "Sweden"), lwd=1)+
#   scale_colour_manual("",
#                       breaks = c("Italy", "Sweden"),
#                       values = c("red", "blue")) +
#   geom_density()+
#   xlab("Week") +
#   ylab("Deaths") +
#   ggtitle("SE FATALITY RATE. Weekly report")+
#   theme_excel_new()
# 
# ggarrange(p7, p8,ncol=1, nrow=2, common.legend = TRUE)


# hospitalizations:
# anche se dati non confrontabili, mostriamo andamento "nuove ospedalizzazioni"
# per 100k (in svezia si intende nuove ammissioni in TERAPIA INTENSIVA)
p_hospit_it <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (600*hospitalizations_it), colour = "Italy"), lwd=1.2)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("New hospitalization") +
  ggtitle("New IT hospitalizations")+
  theme_excel_new() +
  theme(legend.position = "none", axis.title = element_text(size=11))

p_hospit_it


p_icu_se <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (100*hospitalizations_se), colour = "Sweden"), lwd=1.2)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("New ICU") +
  ggtitle("New SE ICU admissions")+
  theme_excel_new() +
  theme(legend.position = "none", axis.title = element_text(size=11))

p_icu_se


p_cases_it <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (cases_it), colour = "Italy"), lwd=1.2)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("IT Cases")+
  theme_excel_new() +
  theme(legend.position = "none", axis.title = element_text(size=11))


p_cases_se <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (cases_se), colour = "Sweden"), lwd=1.2)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("SE Cases")+
  theme_excel_new()+
  theme(legend.position = "none", axis.title = element_text(size=11))


ggarrange(ggarrange(p_cases_it, p_cases_se,ncol=2),
          ggarrange(p_hospit_it, p_icu_se,ncol=2),
          nrow=2)

# scrivere commenti su differenza di metrica

detach(data_first_wave)


################################################################################
################################################################################
## ANALYSIS (with vaccines)
################################################################################
################################################################################

# filter data
data_with_vaccines <- dataset_final[dataset_final$year_week >= "2020-W40",]
attach(data_with_vaccines)

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
  theme_excel_new() +
  theme(legend.position = "none", axis.title = element_text(size=11))

p_vaccines_it

p_vaccines_se <- data_with_vaccines %>%
  ggplot( aes(weeks_axis[-seq(1:first_week)])) +
  geom_line(aes(y = (doses_se), colour = "Sweden"), lwd=1)+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("new SE Vaccines doses injected. Weekly report")+
  theme_excel_new() +
  theme(legend.position = "none", axis.title = element_text(size=11))

p_vaccines_se


## Deaths comparison after vaccines
## TODO: mettere le giuste etichette negli assi
infection_fatality_rate_it <- 1000*(deaths_it/cases_it)  
infection_fatality_rate_se <- 1000*(deaths_se/cases_se)  

population_it = 60*10^6
population_se = 10*10^6

# Vaccination rate considering that 3 doses are somministrated to the same person (as worst case)
vaccination_rate_it = 100 * cumsum(doses_it) / (3 * population_it) 
vaccination_rate_se = 100 * cumsum(doses_se) / (3 * population_se)



# Deaths plot
p_deaths_it <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (deaths_it), colour = "IT Deaths"), lwd=1)+
  geom_line(aes(y = (60*vaccination_rate_it), colour = "IT % vaccinated population"), lwd=1)+
  scale_y_continuous(
    name = "Deaths",
    sec.axis = sec_axis(~./60,"% of vaccinated population ")
    
  ) +
  scale_colour_manual("",
                      breaks = c("IT Deaths", "IT % vaccinated population"),
                      values = c("red", "green")) +
  xlab("Week") +
  ylab("Doses") +
  #ggtitle("IT Deaths trend compared to vaccines somministration")
  theme_excel_new() +
  theme(legend.position="bottom", axis.title = element_text(size=11))


p_deaths_se <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (deaths_se), colour = "SE Deaths"), lwd=1)+
  geom_line(aes(y = (10*vaccination_rate_se), colour = "SE % vaccinated population"), lwd=1)+
  scale_y_continuous(
    name = "Deaths",
    sec.axis = sec_axis(~./10, "% of vaccinated population ")
    
  ) +
  scale_colour_manual("",
                      breaks = c("SE Deaths", "SE % vaccinated population"),
                      values = c("blue", "green")) +
  xlab("Week") +
  ylab("Doses") +
  #ggtitle("SE Deaths trend compared to vaccines somministration")
  theme_excel_new() +
  theme(legend.position="bottom", axis.title = element_text(size=11))

ggarrange(p_deaths_it,p_deaths_se,nrow=2)



# Fatality plot
p_fatality_it <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (infection_fatality_rate_it), colour = "IT Fatality Rate"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_it), colour = "IT % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    name = "% of vaccinated population ",
    sec.axis = sec_axis(~./10,"Fatality Rate")

  ) +
  scale_colour_manual("",
                      breaks = c("IT Fatality Rate", "IT % vaccinated population"),
                      values = c("red", "green")) +
  xlab("Week") +
  #ggtitle("IT Deaths trend compared to vaccines somministration") +
  theme_excel_new() +
  theme(legend.position="bottom", axis.title = element_text(size=11))

p_fatality_se <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (infection_fatality_rate_se), colour = "SE Fatality Rate"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_se), colour = "SE % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    name = "% of vaccinated population ",
    sec.axis = sec_axis(~./10,"Fatality Rate")
    
  ) +
  scale_colour_manual("",
                      breaks = c("SE Fatality Rate", "SE % vaccinated population"),
                      values = c("blue", "green")) +
  xlab("Week") +
 # ggtitle("SE Deaths trend compared to vaccines somministration")+
  theme_excel_new() +
  theme(legend.position="bottom", axis.title = element_text(size=11))

ggarrange(p_fatality_it,p_fatality_se,nrow=2)



## Hospitalization comparison after vaccines

# Those coeff. allow us to get absolute value of hospitalizations (sample are on 100000 population)
coef_population_it = 600
coef_population_se = 100

hosp_rate_it = coef_population_it * hospitalizations_it / cases_it
hosp_rate_se = coef_population_se * hospitalizations_se / cases_se

p_hosp_it <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (1000 * hosp_rate_it), colour = "IT Hospitalization Rate"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_it), colour = "IT % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    name = "% of vaccinated population ",
    sec.axis = sec_axis(~./1000,"Hospitalization rate")
    
  ) +
  scale_colour_manual("",
                      breaks = c("IT Hospitalization Rate", "IT % vaccinated population"),
                      values = c("red", "green")) +
  xlab("Week") +
  ylab("Doses") +
  #ggtitle("IT Hospitalizations trend compared to vaccines somministration")+
  theme_excel_new() +
  theme(legend.position="bottom", axis.title = element_text(size=11))

p_hosp_it

p_hosp_se <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (5000*hosp_rate_se), colour = "SE ICU rate"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_se), colour = "SE % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    name = "% of vaccinated population ",
    sec.axis = sec_axis(~./5000,"Hospitalization rate  (ICU)")
    
  ) +
  scale_colour_manual("",
                      breaks = c("SE ICU rate", "SE % vaccinated population"),
                      values = c("blue", "green")) +
  xlab("Week") +
  ylab("Doses") +
  #ggtitle("SE ICU trend compared to vaccines somministration")+
  theme_excel_new() +
  theme(legend.position="bottom", axis.title = element_text(size=11))
  
p_hosp_se
