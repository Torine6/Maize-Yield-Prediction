library(tidyverse)
library(ggpubr)
library(dplyr)

library(readxl)
data2018 <- read_excel("2018.xlsx")
View(data2018)

cor(data2018$NDVI,data2018$Yield)
lm(data2018$Yield~data2018$NDVI)
summary(lm(data2018$Yield~data2018$NDVI))
plot(data2018$NDVI,data2018$Yield,
     main='Regression for NDVI and Yield for 2018',
     xlab='NDVI',ylab='Yield')
# plot a regression line
abline(lm(data2018$Yield~data2018$NDVI,data=data2018),col='magenta',lwd = 2)



data2019 <- read_excel("2019.xlsx")
View(data2019)
summary(lm(data2019$Yield~data2019$NDVI))
plot(data2019$NDVI,data2019$Yield,
     main='Regression for NDVI and Yield for 2019',
     xlab='NDVI',ylab='Yield')
# plot a regression line
abline(lm(data2019$Yield~data2019$NDVI,data=data2019),col='gold',lwd = 2)


install.packages("xtable")
library(xtable)
lm_model <- lm(data2019$Yield~data2019$NDVI,data=data2019)
summary_table <- summary(lm_model)
latex_table <- xtable(summary(lm(data2019$Yield~data2019$NDVI)))
print(latex_table, include.rownames = FALSE)

install.packages("stargazer")
library(stargazer)
stargazer(data2019)



data2020 <- read_excel("2020.xlsx")
View(data2020)
summary(lm(data2020$Yield~data2020$NDVI))
plot(data2020$NDVI,data2020$Yield,
     main='Regression for NDVI and Yield for 2020',
     xlab='NDVI',ylab='Yield')
# plot a regression line
abline(lm(data2020$Yield~data2020$NDVI,data=data2019),col='cyan',lwd = 2)




data2021 <- read_excel("2021.xlsx")
View(data2021)
summary(lm(data2021$Yield~data2021$NDVI))
plot(data2021$NDVI,data2021$Yield,
     main='Regression for NDVI and Yield for 2021',
     xlab='NDVI',ylab='Yield')
# plot a regression line
abline(lm(data2021$Yield~data2021$NDVI,data=data2019),col='salmon',lwd = 2)



data2022 <- read_excel("2022.xlsx")
View(data2022)
summary(lm(data2022$Yield~data2022$NDVI))
plot(data2022$NDVI,data2022$Yield,
     main='Regression for NDVI and Yield for 2022',
     xlab='NDVI',ylab='Yield')
# plot a regression line
abline(lm(data2022$Yield~data2022$NDVI,data=data2019),col='midnightblue',lwd = 2)



