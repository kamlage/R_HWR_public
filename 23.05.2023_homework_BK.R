#clear
rm(list = ls())

#load libraries
library(dplyr)
library(ggplot2)
library(rdbnomics)


#prepare filtering during data import: create vector of country codes of countries to be imported
country_code <- c("D_W", "DEU", "FRA", "ITA", "NLD", "ESP", "USA")

#copy country code into full string that identifies the country in the data source
countries <- paste0("AMECO/ZUTN/", country_code, ".1.0.0.0.ZUTN")

#original period is renamed as year during import
#data format of year is imported as numeric. If this is not done, year is a character
df <- rdb(ids = countries) %>% 
  select(Country, value, original_period) %>% 
  rename(Year = original_period) %>% 
  filter(Year <= 2022) %>% 
  mutate(Year = as.numeric(Year))

#check data
df
str(df)

#until 1991 data from Germany are coded as West Germany, from 1991 as Germany. Corresponding rows are coded as NA
#in order to create a single line for Germany&West Germany, those have to be combined by the following operations
#Alternatively, NAs could be deleted
df_west_germany <- df %>% 
  filter(Country == "West Germany" & Year < 1991)

df_germany <- df %>% 
  filter(Country == "Germany" & Year >= 1991)

#due to empty space in West Germany, `` have to be used instead of ""
df_germany_together <- df_west_germany %>% 
  bind_rows(df_germany) %>% 
  mutate(Country = recode(Country, `West Germany` = "Germany"))

#negative filter is coded by explanation mark
df_final <- df %>% 
  filter(!(Country %in% c("West Germany", "Germany"))) %>% 
  bind_rows(df_germany_together)

#check final data
df_final
str(df_final)

#define color of plot
cols <- c("Germany" = "blue", 
          "Spain" = "red", 
          "France" = "green", 
          "United States" = "orange", 
          "Italy" = "black", 
          "Netherlands" = "purple")
#plot
p2 <- ggplot(df_final, aes(x = Year, 
                           y = value, 
                           group = Country, 
                           color = Country)) +
  scale_color_manual(values = cols) +
  geom_line() +
  facet_wrap(~ Country, scales="free") +
  theme_bw() +
  labs(y = "Percent",x="Year") +
  theme(axis.text.x = element_text(angle = 45,hjust = 1),strip.background = element_blank()) +
  scale_x_continuous(breaks = seq(min(df_final$Year), max(df_final$Year), 10))

p2

saveRDS(df_final, file = my_directory/df_final.rds)
