#clean workspace
rm(list = ls())

#read data and check data
library(readxl)

setwd("define working directory")

gdp <- read_excel("data_real_gdp.xlsx", 
                       sheet = 2, skip = 3)
gdp
str(gdp)

#create years as numerical data type (years were imported as data type character with did not work with regression)
#natural logarithm of value
gdp$year_n <- as.numeric(gdp$year)
gdp$value_ln <- log(gdp$value)
str(gdp)


#define vectors for labels and colors
countries <- unique(gdp$country)
countries

cols <- c("blue", "red", "green", "orange", "black", "purple")


#HOMEWORK plot with quadratic regression using forward loop
par(mfrow = c(2, 3))
for (i in 1:length(countries)) {
  plot(gdp$year[gdp$country == countries[i]],
       gdp$value_ln[gdp$country == countries[i]],
       main = countries[i],
       xlab = "Year",
       ylab = "ln GDP",
       type="l")
  fit2 <- lm(value_ln[gdp$country == countries[i]] ~ poly(gdp$year_n[gdp$country == countries[i]],2),gdp)
  lines(gdp$year[gdp$country == countries[i]], fit2$fitted.values,type = "l",col = cols[i])
  legend("bottomright",
         legend = paste0("Formula"),
         col = cols[i], 
         lty = 1, 
         lwd = 1, 
         bty = "n", 
         cex = 0.8)
  }

