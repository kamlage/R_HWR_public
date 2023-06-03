#Homework 30.05.2023
#clear objects 
rm(list = ls())

#set your own working directory
setwd('C:/Users/admin/Desktop/Beate/R/20230530')
getwd()

#packages and libraries
library("scales")  
library(ggplot2)
#install.packages("gridExtra")  
library("gridExtra")
#install.packages("cowplot")
library(cowplot)

#load data from lesson 7 - 30.05.2023
df_final_modified <- readRDS("df_final_modified.rds")
str(df_final_modified)
df_final_modified

#define countries and color vectors
countries <- unique(df_final_modified$Country)
countries
length(countries)

cols <- c("Germany" = "blue", 
          "Spain" = "red", 
          "France" = "green", 
          "United States" = "orange", 
          "Italy" = "black", 
          "Netherlands" = "purple")


cols2 <- c( "green", 
           "black",
           "purple",
           "red",
           "orange",
           "blue"
)

#Calculation of linear regression and extracting slope (regression coefficient) and
# rsquare (determination coefficient that gives the fraction of variation explained by the model)
#prepare slope and r2 for plotting (absolute value of slope and percentage of r2)

#prepare an empty data frame. The for loop will save the result for each iteration in this dataframe
#the print function is to see and check the data. 
#when saving the data to the data frame, absolute value of slope as well as percentage of R-squared are calculated
slope_r2 <- data.frame(Country=NA, abs_Slope=NA, Perc_RSquared=NA)
for (i in 1:length(countries)) {
  reg <- (lm(df_final_modified$dunemp[df_final_modified$Country == countries[i]] ~ df_final_modified$rgdp_gr[df_final_modified$Country == countries[i]]))
  print(c(summary(reg)$coefficients[2], summary(reg)$r.squared))
  slope_r2[i,] <- c(countries[i], abs(summary(reg)$coefficients[2]),(summary(reg)$r.squared)*100)
}

slope_r2
str(slope_r2)

##prepare data for plotting, convert data type character to numerical
slope_r2$abs_Slope <- as.numeric(as.character(slope_r2$abs_Slope))
slope_r2$Perc_RSquared <- as.numeric(as.character(slope_r2$Perc_RSquared))
str(slope_r2)
#I had problems to assign the colors after resorting the column plot, therefore I combined the colors with the statistics data
slope_r2_c <- cbind(slope_r2,cols2)
slope_r2_c
str(slope_r2_c)
######
#plot
p1 <- ggplot(slope_r2_c) +
  geom_col(mapping = aes(x=reorder(Country, -abs_Slope), y=abs_Slope), fill = cols2) + 
  theme_bw() +
  theme(legend.position = "none") + 
  labs(color = NULL,
     y = "Slope coefficient,\npercent (absolute value)",
     x = "") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))

p1

p2 <- ggplot(slope_r2_c) +
  geom_col(mapping = aes(x=reorder(Country, -Perc_RSquared), y=Perc_RSquared), fill = cols2) + 
  theme_bw() +
  theme(legend.position = "none") + 
  labs(color = NULL,
       y = "R−Squared,\npercent",
       x = "") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))+
  scale_y_continuous(labels = label_number(accuracy = 1))

p2

p1_2_legend <- ggplot(slope_r2_c, aes(x = abs_Slope, y = Perc_RSquared, group = Country, color = Country)) +
  geom_point(size= 5, shape = 15) +
  scale_color_manual(values = cols) +
  theme_bw() +
  theme(legend.position = "bottom")+
  labs(color = NULL,
       y = "",
       x = "")
p1_2_legend
shared_legend <- get_legend(p1_2_legend)


grid.arrange(arrangeGrob(p1, p2, ncol = 2),
             shared_legend, nrow = 2, heights = c(10, 1))
