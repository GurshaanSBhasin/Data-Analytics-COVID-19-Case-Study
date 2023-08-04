print(getwd())
data <- read.csv("final_dataset.csv")
data
#data cleaning
data1 <- na.omit(data)
data1 <- data1 %>% filter(ISO_YEAR ==2021)
data1
summary(data1)
install.packages("mice")
library("mice")
md.pattern(data.miss)
mice_imp=mice(data,m=5,maxit = 22)
mice_imp
print(data)
data
install.packages("DT")
library(DT)
dt <- datatable(data1)
dt
data_1<- data[, -c(1:7)]
detect_outliers <- function(column) {
  z_scores <- scale(column)
  outliers <- which(abs(z_scores) > 7)
  return(outliers)
}
outliers <- sapply(data_1, detect_outliers)
outliers
unique_values <- unique(data[,c("ISO_START_DATE")])
unique_values
#stat
summary(data1)
corr_d <- cor.test(data1$DAILY_CASES, data1$DAILY_CASES_DEATHS)
corr_d
regression12=lm(data1$DAILY_CASES~data1$DAILY_CASES_DEATHS)
regression12
#goodnes of fit using R ^2
summary(regression12)$r.squared
plot(DAILY_CASES ~ DAILY_CASES_DEATHS, data = data1)
title("Scatter Plot of DAILY_CASES vs DAILY_CASES_DEATHS")
xlab("DAILY_CASES_DEATHS")
ylab("DAILY_CASES")
data$COUNTRY_NAME
abline(regression12)

name <- "Mexico"
name1 <- "Australia"
#influcence of other countries infection over other countries
data3 <- data1[data1$COUNTRY_NAME %in% name,]#selecting india and china from the dataset
data3
data4 <- data1[data1$COUNTRY_NAME %in% name1,]
data4
#stat
corr_d3 <- cor.test(data3$DAILY_CASES, data4$DAILY_CASES)
corr_d3
regression31=lm(data3$DAILY_CASES~data4$DAILY_CASES)
regression31
summary(regression31)$r.squared
plot(data3$DAILY_CASES ~ data4$DAILY_CASES)
title("Scatter Plot of DAILY_CASES vs DAILY_CASES_DEATHS (China AND India)")
xlab("DAILY_CASES_DEATHS")
ylab("DAILY_CASES")
abline(regression31)
#correlation between tested and daily cases 
corrtd <- cor.test(data1$PERSONS_TESTED,data1$DAILY_CASES)
corrtd
regtd <- lm(data1$DAILY_CASES~data1$PERSONS_TESTED)
regtd
summary(regtd)$r.squared
plot(data1$PERSONS_TESTED,data1$DAILY_CASES)
abline(regtd)
#finding the correlation between the confirmed cases and the confirmed deaths
corrcd <- cor.test(data1$DETAILED_CASES_CONFIRMED,data1$DETAILED_CASES_DEATHS)
corrcd
regcd <- lm(data1$DETAILED_CASES_CONFIRMED ~ data1$DETAILED_CASES_DEATHS)
regcd
summary(regcd)$r.squared
plot(data1$DETAILED_CASES_CONFIRMED,data1$DETAILED_CASES_DEATHS)
abline(regcd)
write.csv(data1,"C:/Users/DELL/Documents/college work/data_Filter.csv", row.names = FALSE)

