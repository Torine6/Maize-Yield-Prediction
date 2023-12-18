rm(list=ls())


library(tidyverse)
library(ggplot2)



library(readxl)

yield<- read_excel(
  "C:/Users/user/Desktop/STA420 - PROJECT/DATA/MAIZE DATA.xlsx")
summary(yield)
View(yield)

ndvi<- read_excel(
  "C:/Users/user/Desktop/STA420 - PROJECT/DATA/NDVI.xlsx")
summary(ndvi)
View(ndvi)



#YIELD
#transpose yield
Yield_d <- t(yield)
Yield_d <- data.frame(Yield_d)
View(Yield_d)

#making first row column names
colnames(Yield_d) <- Yield_d[1, ] 
Yield_d <- Yield_d[-1,]
str(Yield_d)

#Changing data types to numeric 
Yield_d$Bungoma = as.numeric(Yield_d$Bungoma)
Yield_d$Nakuru = as.numeric(Yield_d$Nakuru)
Yield_d$`Trans Nzoia` = as.numeric(Yield_d$`Trans Nzoia`)
Yield_d$Nandi = as.numeric(Yield_d$Nandi)
Yield_d$`Uasin Gishu` = as.numeric(Yield_d$`Uasin Gishu`)
#Changing rownames
rownames(Yield_d) <- c("2018","2019","2020","2021","2022")
str(Yield_d)
View(Yield_d)



#NDVI
#transpose ndvi
NDVI <- t(ndvi)
NDVI <- data.frame(NDVI)

#making first row column names
colnames(NDVI) <- NDVI[1, ] 
NDVI <- NDVI[-1,]
str(NDVI)

#Changing data types to numeric
NDVI$Bungoma = as.numeric(NDVI$Bungoma)
NDVI$Nakuru = as.numeric(NDVI$Nakuru)
NDVI$`Trans-Nzoia` = as.numeric(NDVI$`Trans-Nzoia`)
NDVI$Nandi = as.numeric(NDVI$Nandi)
NDVI$`Uasin- Gishu` = as.numeric(NDVI$`Uasin- Gishu`)
str(NDVI)

#Changing rownames
rownames(NDVI) <- c("2018","2019","2020","2021","2022")
View(NDVI)
str(NDVI)


#SUMMARY
summary(Yield_d)
summary(NDVI)





library(ggplot2)             # Load the 'ggplot2' package

cor(Yield_d$`Trans Nzoia`, NDVI$`Trans-Nzoia`, method = 
      c("pearson", "kendall", "spearman"))

model1 = lm(Yield_d$`Trans Nzoia`~NDVI$`Trans-Nzoia`)
model1
plot(model1)

yield_ndvi_tr <- data.frame(Yield_d$`Trans Nzoia`,NDVI$`Trans-Nzoia`)
yield_ndvi_tr
rownames(yield_ndvi_tr) <- c("2018","2019","2020","2021","2022")
View(yield_ndvi_tr)



library(ggpubr)



plot1 <- ggscatter(yield_ndvi_tr, x ="NDVI..Trans.Nzoia.",
                   y = "Yield_d..Trans.Nzoia."
                   , 
                   add = "reg.line", conf.int = TRUE, 
                   cor.coef = TRUE, cor.method = "pearson",
                   xlab = "NDVI",ylab="Yield")
plot1
















#MODEL - SLRM
  #Putting together yield and ndvi values for each county separate.
yield_ndvi_bu<-data.frame(Yield_d$Bungoma,NDVI$Bungoma)
rownames(yield_ndvi_bu) <- c("2018","2019","2020","2021","2022")
View(yield_ndvi_bu)

yield_ndvi_nak <-data.frame(Yield_d$Nakuru,NDVI$Nakuru)
rownames(yield_ndvi_nak) <- c("2018","2019","2020","2021","2022")
View(yield_ndvi_nak)

yield_ndvi_TN<-data.frame(Yield_d$`Trans Nzoia`,NDVI$`Trans-Nzoia`)
rownames(yield_ndvi_TN) <- c("2018","2019","2020","2021","2022")
View(yield_ndvi_TN)

yield_ndvi_nan<-data.frame(Yield_d$Nandi,NDVI$Nandi)
rownames(yield_ndvi_nan) <- c("2018","2019","2020","2021","2022")
View(yield_ndvi_nan)

yield_ndvi_UG<-data.frame(Yield_d$`Uasin Gishu`,NDVI$`Uasin- Gishu`)
rownames(yield_ndvi_UG) <- c("2018","2019","2020","2021","2022")
View(yield_ndvi_UG)

#Modelling data, Fit a linear regression model using the lm() function.
model1 = lm(Yield_d$Bungoma~NDVI$Bungoma)
model1
plot(model1)
return(plot(model1))

#Create a scatter plot of the data points and add the regression line.
# Create a scatter plot of the data points
scatter_plot <- ggplot(data = NULL, 
                       aes(x = NDVI$Bungoma, y = Yield_d$Bungoma)) +
  geom_point(color = "blue") +
  xlab("NDVI") +
  ylab("Yield")


# Add the regression line
scatter_plot_with_regression <- scatter_plot +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Simple Linear Regression")

# Display the plot
scatter_plot_with_regression

#Generate predictions using the model.
predicted_values <- predict(lm_model)


