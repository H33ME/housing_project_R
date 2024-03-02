# loading the required packages and data ----------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
housing_data <- read_xlsx('File for Midterm.xlsx') %>%
  janitor::clean_names() %>%
  # drop na
  drop_na() %>%
  # character to factors
  dplyr::mutate_if(is.character, as.factor)

# summary of the data -----------------------------------------------------

summary(housing_data)

# ·Is it more expensive or less expensive to live in FL or NY? ------------

housing_data %>%
  ggplot(aes(x = location, y = price, fill = location)) +
  geom_col() +
  labs(title = 'A barplot displaying the relationship between the 2 different locations in terms of price',
       x = "Location",
       y = "House prices",
       fill = 'Location')

# # Is the crime rate higher in FL or NY (Note a low score in crim --------

housing_data %>%
  mutate(crime_rating = factor(crime_rating)) %>%
  ggplot(aes(x = crime_rating, fill = location)) +
  geom_bar(position = 'dodge') +
  labs(title = 'A Grouped Bar Plot displaying the relationship of different crime ratings between the locations',
       x = 'Rate of Crimes',
       y = 'Counts',
       fill = 'Location')


# Is the crime rate higher in lower or higher house price areas? ----------

housing_data %>%
  ggplot(aes(
    x = price,
    y = crime_rating,
    color = location,
    size = crime_rating
  )) +
  geom_point() +
  labs(
    title = 'A Scatterplot displaying the relationship between the crime rate and house prices in the 2 different locations',
    x = 'Price',
    y = 'crime rating',
    fill = 'Location',
    size = 'Rates of Crime'
  )

# On average what location will she be able to pay off her house  --------
# new data after considering the additional information

cleaner_housing_data <- housing_data %>%
  filter(price <= 220000) %>% 
  mutate(
    able_to_pay_off_house = case_when(
     price <= 220000 & price > 175000 ~ "pay in NY",
     price <= 175000 ~ "Pay in FL"
    )
    )
# Pie chart to show on average which house she will be able to pay off house
cleaner_housing_data %>% 
  ggplot(aes(x = '', y = price, fill = able_to_pay_off_house))+
  geom_bar(stat='identity', width=1)+
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_manual(values = c("#66c2a5", "#fc8d62"))+
  labs(
    title = 'A Piechart showing the proportion in which suzie can pay off house in either regions',
    y = 'House price',
    fill = 'Able to pay off house'
  )
