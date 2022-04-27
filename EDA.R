### EDA
library(dplyr)

dataset = read.csv("datasets/dataset.csv")
# attach(dataset)


# show preliminary informations about dataset
columns_name = colnames(dataset)
# columns_name

head(dataset)

# we have 2 sources for number of cases (from 2 different datasets)
# cases_from_tests and cases_from_deaths

# we can use kolmogorov smirnov (2 sample) to support the current null_hypotesis:
# samples provided by same distribution
ks.test(cases_from_deaths,cases_from_tests)

#plot empyrical cdf



# the resulting p-value (0.91) confirm our hypotesis and so we can choose the feature_vector to drop freely 
dataset = select(dataset, -"cases_from_deaths")

# filtering based on country_code = IT and SE
dataset_it = dataset[country_code == "IT",]
dataset_se = dataset[country_code == "SE",]

length(unique(dataset_it$year_week))

dataset_final = dataset_it %>%
  inner_join(dataset_se, by = c("year_week" = "year_week"), suffix= c("_it","_se"))
