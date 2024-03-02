# loading the required packages and data ----------------------------------

library(tidyverse)
library(readxl)
library(dplyr)
library(treemapify)
house_and_crime_data <- read_xlsx('File for Midterm.xlsx') %>%
  janitor::clean_names() %>%
  # drop na
  drop_na() %>%
  # character to factors
  dplyr::mutate_if(is.character, as.factor)

# structure of the data ---------------------------------------------------

str(house_and_crime_data)

# Is it more expensive or less expensive to live in FL or NY? -------------

house_and_crime_data %>%
  group_by(location) %>%
  summarize(total = sum(price)) %>%
  mutate(percentage = round(total / sum(total) * 100)) %>%
  ggplot(aes(x = "", y = total, fill = location)) +
  geom_bar(stat = "identity", width = 1) +
  geom_text(aes(label = paste0(percentage, "%")),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(title = 'A piechart displaying which region is more or less expensive ',
       y = 'price', fill = 'location')

# Is the crime rate higher in FL or NY (Note a low score in crime  --------

house_and_crime_data %>%
  group_by(location) %>%
  ggplot(aes(area = crime_rating, fill = location, label = crime_rating)) +
  geom_treemap() +
  geom_treemap_text() +
  labs(title = 'A treemap displaying the relationship between different levels of crime rates on different locations',
       area = 'crime ratings',
       fill = 'location')

# Is the crime rate higher in lower or higher house price areas? ----------

house_and_crime_data %>%
  ggplot(aes(x = price,
             y = crime_rating,
             color = location)) +
  geom_point(size = 2) +
  geom_smooth(method = lm, se = FALSE) +
  scale_color_viridis_d(option = "plasma", end = .7)+
  labs(title = 'A Scatterplot displaying the relationship between the crime rate and house prices in the 2 different locations',
       x = 'Price',
       y = 'crime rating',
       fill = 'Location')

# On average what location will she be able to pay off her house f --------
# new data with the required information for this part
cleaner_house_and_crime_data <- house_and_crime_data %>%
  group_by(location) %>%
  filter(
    (location == 'NY' & price < 220000) |  # Filter for NY group
      (location == 'FL' & price < 175000)    # Filter for FL group
  ) %>%
  ungroup()  %>%
  mutate(able_to_pay_off_house = case_when(
    price <= 220000 & price > 175000 ~
    "Able to Pay in NY",
    price <= 175000 ~ "Able to Pay in FL",
  ))
# creating a barplot for this part
cleaner_house_and_crime_data %>% 
  count(able_to_pay_off_house = factor(able_to_pay_off_house), location = factor(location)) %>% 
  mutate(pct = prop.table(n)) %>% 
  ggplot(aes(x = location, y = pct, fill = able_to_pay_off_house, label = scales::percent(pct))) + 
  geom_col(position = 'dodge') + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+
  labs(
    title = 'A Bar Plot displaying which location Suzie will be able to pay off her house',
    fill = 'Able to pay off house',
    x = 'Location',
    y = 'percentage'
  )
