#load library of package wooldridge, 
#if not installed, install first by clicking "install" in the message on top of the panel
library(wooldridge)

# get an overview of object "wage1" (type, column names, data format, number of rows and columns)
str(wage1)

#create three vectors in order to assign color to group. Vector "gender_marital" is the requested order. Vector "gender_marital2" is my preferred order.
cols2 <- c("blue","blue", "red","red")
gender_marital <- c("Married\nmen", "Single\nmen", "Single\nwomen","Married\nwomen")
gender_marital2 <- c("Married men", "Single men", "Married women","Single women")


#plots by gender and marital status

#bar plot, HOMEWORK, order of bars as requested
barplot(c(mean(wage1$wage[wage1$female == 0 & wage1$married == 1]),
          mean(wage1$wage[wage1$female == 0 & wage1$married == 0]),
          mean(wage1$wage[wage1$female == 1 & wage1$married == 0]),
          mean(wage1$wage[wage1$female == 1 & wage1$married == 1])),
        ylab = "Dollar per Hour",
        ylim = c(0, 10),
        main = "Mean hourly wage by gender and marital status",
        names = gender_marital,
        col = cols2)

#bar plot, prefered order of bars, without new line and optimized y-scale
barplot(c(mean(wage1$wage[wage1$female == 0 & wage1$married == 1]),
          mean(wage1$wage[wage1$female == 0 & wage1$married == 0]),
          mean(wage1$wage[wage1$female == 1 & wage1$married == 1]),
          mean(wage1$wage[wage1$female == 1 & wage1$married == 0])),
        ylab = "Dollar per Hour",
        ylim = c(0, 9),
        main = "Mean hourly wage by gender and marital status",
        names = gender_marital2,
        col = cols2)

#boxplot, preferred order of boxes
boxplot(wage1$wage[wage1$female == 0 & wage1$married == 1],
        wage1$wage[wage1$female == 0 & wage1$married == 0],
        wage1$wage[wage1$female == 1 & wage1$married == 1],
        wage1$wage[wage1$female == 1 & wage1$married == 0],
        ylab = "Dollar per Hour",
        xlab = "Gender and marital status",
        main = "Wage rate per hour by gender and marital status",
        names = gender_marital2,
        col = cols2)
