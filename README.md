# Vphilip2025
library(readr)
covid <- read.csv("COVID-19_Testing_Over_Time.csv")
head(covid)
nrow(covid)
n_obs <- nrow(covid)
n_obs #The number of Observations

n_var <- ncol(covid)
n_var # The number of variables
summary(covid)
library(dplyr)      # for data‐wrangling verbs
library(lubridate)  # for mdy() and year()
library(tidyverse)

head(covid)

covid2 <- covid %>%
  # 1) parse DATE and extract year
  mutate(
    DATE = mdy(DATE),      # instead of mdy(covid$DATE)
    Year = year(DATE)      # instead of year(covid$DATE)
  ) %>%
  # 2) group by your new Year column
  group_by(Year) %>%
  # 3) summarise however you like
  summarise(
    days        = n(),
    total_tests = sum(tests, na.rm = TRUE),
    pos_tests   = sum(pos,   na.rm = TRUE),
    neg_tests   = sum(neg,   na.rm = TRUE),
    neg_tests   = sum(neg,   na.rm = TRUE),
    indet_tests = sum(indet, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Year)

head(covid2)

library(ggplot2)

ggplot(covid2, aes(x = Year, y = total_tests))+
  geom_col(fill = "blue")+
  labs(title = "Total Covid Test by year", x = "Years", y = "Total tests")+
  theme_minimal()+
  scale_x_continuous(
    breaks = 2020:2025,  # Show all years
    labels = 2020:2025   # Label as integers
  )

ggplot(covid2, aes(x = Year, y = pos_tests))+
  geom_col(fill = "red")+
  labs(title = "Positive Covid Test by year", x = "Years", y = "Positive tests")+
  theme_minimal()+
  scale_x_continuous(
    breaks = 2020:2025,  # Show all years
    labels = 2020:2025   # Label as integers
  )

ggplot(covid2, aes(x = Year, y = neg_tests))+
  geom_col(fill = "orange")+
  labs(title = "Negative Covid Test by year", x = "Years", y = "Negative tests")+
  theme_minimal()+
  scale_x_continuous(
    breaks = 2020:2025,  # Show all years
    labels = 2020:2025   # Label as integers
  )

ggplot(covid2, aes(x = Year, y = indet_tests))+
  geom_col(fill = "black")+
  labs(title = "Indeterminate Covid Test by year", x = "Years", y = "Indeterminate tests")+
  theme_minimal()+
  scale_x_continuous(
    breaks = 2020:2025,  # Show all years
    labels = 2020:2025   # Label as integers
  )
library(readr)
vaccine <- read.csv("ARCHIVED__COVID-19_Vaccinations_Given_to_SF_Residents_Over_Time_v1.csv")

vaccine_df2 <- vaccine %>%
  # 1) parse DATE and extract year
  mutate(
    DATE = mdy(date_administered),      # instead of mdy(covid$DATE)
    Year = year(DATE)      # instead of year(covid$DATE)
  ) %>%
  # 2) group by your new Year column
  group_by(Year) %>%
  # 3) summarise however you like
  summarise(
    days        = n_distinct(DATE),
    total_vaccines = sum(vaccines, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Year)


vaccine_df2

combined_covid <- left_join(covid2, vaccine_df2, by = "Year")

combined_covid

plot_data <- combined_covid %>%
  select(Year, pos_tests, neg_tests, total_vaccines) %>%
  pivot_longer(cols = -Year, names_to = "Metric", values_to = "Count")


ggplot(plot_data, aes(x = Year, y = Count, fill = Metric ))+
   geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "COVID-19 Test Outcomes and Vaccines by Year",
    x = "Year",
    y = "Count",
    fill = "Metric"
  ) +
  theme_minimal() +
  scale_x_continuous(
    breaks = 2020:2023,  
    labels = 2020:2023  
  )
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )

