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
 plot(ecdf(cases_from_deaths), col="blue") 
 lines(ecdf(cases_from_tests), col="green") #TODO DA MIGLIORARE ASSOLUTAMENTE


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
 dataset$hard_restrictions <- grepl(paste(hard_restrictions_set , collapse = "(,|$)|"), dataset$active_restrictions)

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


mean(cases_it)
sd(cases_it)
mean(deaths_it)
sd(deaths_it)
mean(hospitalizations_it)
sd(hospitalizations_it)
mean(positivity_rate_it)
sd(positivity_rate_it)


mean(cases_se)
sd(cases_se)
mean(deaths_se)
sd(deaths_se)
mean(hospitalizations_se)
sd(hospitalizations_se)
mean(positivity_rate_se)
sd(positivity_rate_se)



## EXPLORE cases features. IT vs SE

# casting ISOweek format into date due to that ggplot work with date format
weeks_axis <- paste(year_week, "1", sep = "-")
weeks_axis <- ISOweek::ISOweek2date(weeks_axis)

#IT tests, cases and doses plot
p_it <- dataset_final %>%
  ggplot(aes(weeks_axis)) +
  geom_line(aes(y=100*cases_it/positivity_rate_it, colour="Tests"), lwd=1) +
  geom_line(aes(y=cases_it, colour="Cases"), lwd=1) +
  geom_line(aes(y=cumsum(doses_it), colour="Doses"), lwd=1) +
  scale_colour_manual("",
                      breaks = c("Tests", "Cases", "Doses"),
                      values = c("#C71EA8", "#0EC71A", "#001AFF")) +
  xlab("Week") +
  ylab("") +
  ggtitle("IT tests, cases and doses") +
  theme_bw()
p_it #+ theme(legend.position = "bottom")


# IT deaths, positivity rate and doses plot
p_general <- dataset_final %>%
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
p_general


## Normality test
# Graphic test using histograms and method of moments
ph1 <- dataset_final %>%
  ggplot( aes(x=cases_it)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(cases_it,mean(cases_it),sd(cases_it))), colour = "red", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("Cases") +
  theme_excel_new() +
  theme(axis.text = element_text(size = 11, color="black"), title = element_text(color="black"))


ph2 <- dataset_final %>%
  ggplot( aes(x=cases_se)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(cases_se,mean(cases_se),sd(cases_se))), colour = "blue", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Cases") +
  ggtitle("Cases") +
  theme_excel_new() +
  theme(axis.text = element_text(size = 11, color="black"), title = element_text(color="black"))
  

ph3 <- dataset_final %>%
  ggplot( aes(x=deaths_it)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(deaths_it,mean(deaths_it),sd(deaths_it))), colour = "red", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("Deaths") +
  theme_excel_new()+
  theme(axis.text = element_text(size = 11, color="black"), title = element_text(color="black"))

ph4 <- dataset_final %>%
  ggplot( aes(x=deaths_se)) +
  geom_histogram(aes(y=..density..) ,lwd=0.3, colour="black", fill="#e6e6e6") +
  geom_line(aes(y = dnorm(deaths_se,mean(deaths_se),sd(deaths_se))), colour = "blue", lwd=1, lty="longdash") +
  xlab("Week") +
  ylab("Deaths") +
  ggtitle("Deaths") +
  theme_excel_new()+
  theme(axis.text = element_text(size = 11, color="black"), title = element_text(color="black"))

#ggarrange(ph1,ph2,ph3,ph4,ncol=2, nrow=2, common.legend = FALSE ) # 4x4
ggarrange(ph1,ph3,ncol=2, nrow=1, common.legend = FALSE ) #it
ggarrange(ph2,ph4,ncol=2, nrow=1, common.legend = FALSE ) #se


# Shapiro-wilk tests
shapiro.test(cases_it)
shapiro.test(deaths_it)
shapiro.test(positivity_rate_it)
shapiro.test(hospitalizations_it)

shapiro.test(cases_se)
shapiro.test(deaths_se)
shapiro.test(positivity_rate_se)
shapiro.test(hospitalizations_se)

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

p_cases_compare <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (cases_it), colour = "Italy"), lwd=1.2) +
  geom_line(aes(y = (cases_se), colour = "Sweden"), lwd=1.2) +
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Weeks") +
  ylab("Cases") +
  ggtitle("ITvsSE cases. Weekly report")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        axis.text.x = element_text(angle = 30, vjust = 0.5),
        title = element_text(color="black"),
        legend.text = element_text(size = 16, color="black"))


p_restrictions <- ggplot(data=data_first_wave, aes(weeks_axis[1:nrow(data_first_wave)]))+
  geom_bar(aes(y=restrictions_count_it),stat="identity",position="identity",alpha=0.7,fill="red")+
  geom_bar(aes(y=restrictions_count_se),stat="identity",position="identity",fill="blue")+
  xlab("Weeks") +
  ylab("Num. restrictions") +
  ggtitle("ITvsSE restrictions. Weekly report")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"))

p_cases_compare
ggarrange(p_cases_compare, p_restrictions,ncol=2, nrow=1, common.legend = TRUE, legend = "bottom") # 800x300



p_deaths <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (deaths_it), colour = "Italy"), lwd=1.2) +
  geom_line(aes(y = (deaths_se), colour = "Sweden"), lwd=1.2) +
  scale_y_continuous(breaks=seq(0,5000,by=1000)) +
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Weeks") +
  ylab("Deaths") +
  #ggtitle("ITvsSE deaths. Weekly report")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
      axis.text = element_text(size = 15, color="black"),
      title = element_text(color="black"),
      legend.position = "bottom",
      legend.text = element_text(size = 16, color="black"))

p_deaths

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
p_hospit_it <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (600*hospitalizations_it), colour = "Italy"), lwd=1.2)+
  scale_y_continuous(expand = c(0, 0), limits = c(0, 20000))+
  scale_colour_manual("",
                      breaks = c("Italy", "Sweden"),
                      values = c("red", "blue")) +
  xlab("Week") +
  ylab("New hospitalization") +
  ggtitle("New IT hospitalizations")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "none",
        axis.text.x = element_text(angle=30, vjust=0.5))

p_hospit_it


p_icu_se <- data_first_wave %>%
  ggplot( aes(weeks_axis[1:nrow(data_first_wave)])) +
  geom_line(aes(y = (100*hospitalizations_se), colour = "Sweden"), lwd=1.2)+
  #scale_y_continuous(breaks=seq(0,300,by=50))+
  scale_y_continuous(expand = c(0, 0), breaks  = c(0, 75, 150, 225, 300), limits = c(0, 300))+
  scale_colour_manual("",
                      breaks = c("Sweden"),
                      values = c("blue")) +
  xlab("Week") +
  #ylim(0,300) +
  ylab("New ICU") +
  ggtitle("New SE ICU admissions")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "none",
        axis.text.x = element_text(angle=30, vjust = 0.5))

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


ggarrange(p_hospit_it, p_icu_se,ncol=2,nrow=1)


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

# p_vaccines_it <- data_with_vaccines %>%
#   ggplot( aes(weeks_axis[-seq(1:first_week)])) +
#   geom_line(aes(y = (doses_it), colour = "Italy"), lwd=1)+
#   scale_colour_manual("",
#                       breaks = c("Italy", "Sweden"),
#                       values = c("red", "blue")) +
#   xlab("Week") +
#   ylab("Doses") +
#   ggtitle("new IT Vaccines doses injected. Weekly report")+
#   theme_excel_new() +
#   theme(legend.position = "none", axis.title = element_text(size=11))
# 
# p_vaccines_it
# 
# p_vaccines_se <- data_with_vaccines %>%
#   ggplot( aes(weeks_axis[-seq(1:first_week)])) +
#   geom_line(aes(y = (doses_se), colour = "Sweden"), lwd=1)+
#   scale_colour_manual("",
#                       breaks = c("Italy", "Sweden"),
#                       values = c("red", "blue")) +
#   xlab("Week") +
#   ylab("Doses") +
#   ggtitle("new SE Vaccines doses injected. Weekly report")+
#   theme_excel_new() +
#   theme(legend.position = "none", axis.title = element_text(size=11))
# 
# p_vaccines_se


## Deaths comparison after vaccines
infection_fatality_rate_it <- 100*(deaths_it/cases_it)  
infection_fatality_rate_se <- 100*(deaths_se/cases_se)  

population_it = 60*10^6
population_se = 10*10^6

# Vaccination rate considering that 3 doses are somministrated to the same person (as worst case)
vaccination_rate_it = 100 * cumsum(doses_it) / (3 * population_it) 
vaccination_rate_se = 100 * cumsum(doses_se) / (3 * population_se)


# Cases plot
max(cases_it)
scaler_it = 12000
p_cases_it <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (cases_it), colour = "IT Cases"), lwd=1.2)+
  geom_line(aes(y = (scaler_it*vaccination_rate_it), colour = "IT % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0,max(cases_it)),
    breaks = seq(0, 100*scaler_it, by=20*scaler_it),
    name = "Cases",
    sec.axis = sec_axis(~./scaler_it,"% of vaccinated population", breaks = seq(0, 100, by=20)),
    labels = function(x) format(x, scientific = TRUE)
  ) +
  scale_colour_manual("",
                      breaks = c("IT Cases", "IT % vaccinated population"),
                      values = c("red", "green")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("IT Deaths trend compared to vaccines somministration")
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom", 
        legend.text = element_text(size = 16, color="black"))
#axis.text.x = element_text(angle=30, vjust = 0.5))

scaler_se = 2500
p_cases_se <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (cases_se), colour = "SE Cases"), lwd=1.2)+
  geom_line(aes(y = (scaler_se*vaccination_rate_se), colour = "SE % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0,max(cases_se)),
    breaks = seq(0, 100*scaler_se, by=20*scaler_se),
    name = "Cases",
    sec.axis = sec_axis(~./scaler_se, "% of vaccinated population ", breaks = seq(0, 100, by=20)),
    labels = function(x) format(x, scientific = TRUE)
  ) +
  scale_colour_manual("",
                      breaks = c("SE Cases", "SE % vaccinated population"),
                      values = c("blue", "green")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("SE Deaths trend compared to vaccines somministration")
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom",
        legend.text = element_text(size = 16, color="black"))
#axis.text.x = element_text(angle=30, vjust = 0.5))

ggarrange(p_cases_it,p_cases_se,nrow=2)

# Deaths plot
p_deaths_it <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (deaths_it), colour = "IT Deaths"), lwd=1.2)+
  geom_line(aes(y = (60*vaccination_rate_it), colour = "IT % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0, 6000),
    breaks = c(0, 1200, 2400, 3600, 4800,6000),
    name = "Deaths",
    sec.axis = sec_axis(~./60,"% of vaccinated population", breaks = c(0,20,40,60,80,100))
  ) +
  scale_colour_manual("",
                      breaks = c("IT Deaths", "IT % vaccinated population"),
                      values = c("red", "green")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("IT Deaths trend compared to vaccines somministration")
  theme_excel_new() +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size = 11, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom", 
        legend.text = element_text(size = 11, color="black"))
        #axis.text.x = element_text(angle=30, vjust = 0.5))


p_deaths_se <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (deaths_se), colour = "SE Deaths"), lwd=1.2)+
  geom_line(aes(y = (10*vaccination_rate_se), colour = "SE % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0,1000),
    breaks = c(0, 200, 400, 600, 800, 1000),
    name = "Deaths",
    sec.axis = sec_axis(~./10, "% of vaccinated population ", breaks = c(0,20,40,60,80,100))
  ) +
  scale_colour_manual("",
                      breaks = c("SE Deaths", "SE % vaccinated population"),
                      values = c("blue", "green")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("SE Deaths trend compared to vaccines somministration")
  theme_excel_new() +
  theme(axis.title = element_text(size=12),
        axis.text = element_text(size = 11, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom",
        legend.text = element_text(size = 11, color="black"))
        #axis.text.x = element_text(angle=30, vjust = 0.5))

ggarrange(p_deaths_it,p_deaths_se,nrow=2)



# Fatality plot
p_fatality_it <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (infection_fatality_rate_it), colour = "IT Fatality Rate"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_it/10), colour = "IT % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0,10),
    name = "Fatality Rate (%)",
    sec.axis = sec_axis(~.*10," % of vaccinated population ")

  ) +
  scale_colour_manual("",
                      breaks = c("IT Fatality Rate", "IT % vaccinated population"),
                      values = c("red", "green")) +
  xlab("Week") +
  ggtitle("IT Deaths trend compared to vaccines somministration") +
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom",
        legend.text = element_text(size = 16, color="black"))


p_fatality_se <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (infection_fatality_rate_se), colour = "SE Fatality Rate"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_se/10), colour = "SE % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0,10),
    name = "Fatality Rate (%)",
    sec.axis = sec_axis(~.*10," % of vaccinated population ")
    
  ) +
  scale_colour_manual("",
                      breaks = c("SE Fatality Rate", "SE % vaccinated population"),
                      values = c("blue", "green")) +
  xlab("Week") +
  ggtitle("SE Deaths trend compared to vaccines somministration")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom",
        legend.text = element_text(size = 16, color="black"))

ggarrange(p_fatality_it,p_fatality_se,nrow=2)
p_fatality_se


## Hospitalization comparison after vaccines

# Those coeff. allow us to get absolute value of hospitalizations (sample are on 100000 population)
coef_population_it = 600
coef_population_se = 100

hosp_rate_it = coef_population_it * hospitalizations_it / cases_it
hosp_rate_se = coef_population_se * hospitalizations_se / cases_se

scaler_it = 400
p_hosp_it <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (hosp_rate_it), colour = "IT Hospitalization Rate"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_it/scaler_it), colour = "IT % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0, 0.25),
    name = "Hospitalization rate",
    sec.axis = sec_axis(~.*scaler_it, "% of vaccinated population ")
    
  ) +
  scale_colour_manual("",
                      breaks = c("IT Hospitalization Rate", "IT % vaccinated population"),
                      values = c("red", "green")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("IT Hospitalizations trend compared to vaccines somministration")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom",
        legend.text = element_text(size = 16, color="black"))

p_hosp_it

scaler_se = 5000
p_hosp_se <- data_with_vaccines %>%
  ggplot( aes((weeks_axis[-seq(1:first_week)]))) +
  geom_line(aes(y = (hosp_rate_se), colour = "SE Hospitalization Rate (ICU)"), lwd=1.2)+
  geom_line(aes(y = (vaccination_rate_se/scaler_se), colour = "SE % vaccinated population"), lwd=1.2)+
  scale_y_continuous(
    limits = c(0, 0.02),
    name = "Hospitalization rate (ICU)",
    sec.axis = sec_axis(~.*scaler_se, "% of vaccinated population ")
    
  ) +
  scale_colour_manual("",
                      breaks = c("SE Hospitalization Rate (ICU)", "SE % vaccinated population"),
                      values = c("blue", "green")) +
  xlab("Week") +
  ylab("Doses") +
  ggtitle("SE ICU trend compared to vaccines somministration")+
  theme_excel_new() +
  theme(axis.title = element_text(size=16),
        axis.text = element_text(size = 15, color="black"),
        title = element_text(color="black"),
        legend.position = "bottom",
        legend.text = element_text(size = 16, color="black"))
  
p_hosp_se
