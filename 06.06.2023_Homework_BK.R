#clear
rm(list = ls())

# Some change

#load libraries
library(gapminder)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)

##extract data from gapminder, prepare and check data
#minimum
min <- gapminder %>%
  group_by(year) %>%
  summarise(lifeExp = min(lifeExp)) %>% 
  rename(Minimum = lifeExp)

#colnames(min) <- c("year", "Minimum")
min
str(min)

#median
median <- data.frame(gapminder %>%
                        group_by(year) %>%
                        summarise(lifeExp = median(lifeExp)))

colnames(median) <- c("year", "Median")
median
str(median)

#maximum
max <- data.frame(gapminder %>%
                        group_by(year) %>%
                        summarise(lifeExp = max(lifeExp)))

colnames(max) <- c("year", "Maximum")
max
str(max)

#China
China <- data.frame(gapminder %>%
                        filter(country == "China") %>%
                        group_by(year) %>%
                        summarise(lifeExp = (lifeExp)))

colnames(China) <- c("year", "China")
China
str(China)

##combine single datasets
#cbind does work here, since $years is identical in all datasets. Otherwise join by (merge)
# data_plot <- cbind(min,median$Median,max$Maximum,China$China)
# 
# colnames(data_plot) <- c("year", "Minimum", "Median", "Maximum", "China")
# data_plot
# str(data_plot)

# Using dplyr inner_join instead of cbind to join data frames 
# inner_join retains only rows with matches in both data frames 
# Other types of joins could work as well in this case
data_plot <- min %>% 
  inner_join(median, by = "year") %>% 
  inner_join(max, by = "year") %>% 
  inner_join(China, by = "year")

#####Plot

colors <- c("Minimum"= "red", "Median" = "orange", "Maximum" = "blue", "China"="black")

p1 <- ggplot(data_plot, aes(x=year)) +
  geom_line(aes(y=Minimum, color = "Minimum"), linetype="dashed", linewidth=1) +
  geom_line(aes(y=Median, color = "Median"), linetype="dashed", linewidth=1) +
  geom_line(aes(y=Maximum, color = "Maximum"), linetype="dashed", linewidth=1) +
  geom_line(aes(y=China, color = "China"), linetype="solid", linewidth=1) +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  scale_colour_manual(values=colors) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "bottom", 
        strip.background = element_blank()) +
  labs(color = NULL,
       y = "Life expectancy at birth",
       caption = "Source: Gapminder")

p1

#add data for China
# p2 <- p1 + geom_line(aes(y=China), color = "black", linetype="solid", linewidth=1) 
# 
# p2
  
###prepare legend

  
# plot_just_4_legend <- ggplot(data_plot, aes(x=year)) +
#   geom_line(aes(y=Minimum, color = "Minimum"), size =1.5) +
#   geom_line(aes(y=Median, color = "Median"), size = 1.5) +
#   geom_line(aes(y=Maximum, color = "Maximum"), size = 1.5) +
#   geom_line(aes(y=China, color = "China"), size = 1.5) +
#   labs(x = "Year",
#        y = "lifeExp",
#        color = NULL) +
#   scale_color_manual(values = colors) +
#   theme_bw() +
#   theme(legend.position = "bottom", strip.background = element_blank())
#  
# # extract legend 
# legend <- get_legend(plot_just_4_legend)   
# 
# # final combined plot with shared legend
# 
# grid.arrange(arrangeGrob(p2, ncol = 1),
#              legend, nrow = 2, heights = c(10, 1))



###########################
##Option 2, long data, was not successful

min2 <- data.frame(c("all","Minimum",gapminder %>%
                       group_by(year) %>%
                       summarise(lifeExp = min(lifeExp))))
colnames(min2) <- c("country", "parameter", "year", "lifeExp")
min2
str(min2)

median2 <- data.frame(c("all", "Median",gapminder %>%
                          group_by(year) %>%
                          summarise(lifeExp = median(lifeExp))))
colnames(median2) <- c("country", "parameter", "year", "lifeExp")
median2
str(median2)

max2 <- data.frame(c("all","Maximum",gapminder %>%
                       group_by(year) %>%
                       summarise(lifeExp = max(lifeExp))))
colnames(max2) <- c("country", "parameter", "year", "lifeExp")
max2
str(max2)

China2 <- data.frame(c("China","China",gapminder %>%
                         filter(country == "China") %>%
                         group_by(year) %>%
                         summarise(lifeExp = (lifeExp))))
colnames(China2) <- c("country", "parameter", "year", "lifeExp")
China2
str(China2)

data_2plot <- rbind(min2,median2,max2,China2)
str(data_2plot)

######
colors <- c("Minimum"= "red", "Median" = "orange", "Maximum" = "blue", "China"="black")
#####
p2 <- ggplot(subset(data_2plot, country == "all"), aes(x=year, y=lifeExp, group=parameter, color=parameter)) +
  geom_line(linetype=2) +
  theme_bw() +
  theme(legend.position = "bottom", strip.background = element_blank()) +
  scale_color_manual(values = colors)+
  labs(color = NULL,
       y = "Life expectancy at birth",
       x = "",
       caption = "Source: Gapminder")

p2
####at this point I did not succeed to add the line for China


