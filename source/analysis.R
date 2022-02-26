# Han Rui Liu
# 2/25/2022
# A3 Incarceration 

# load packages 
library(dplyr)
library(maps)
library(tidyverse)
library(ggplot2)

# load data
df_incarceration <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

## Introduction + Summary information

  # What is the amount of Asian American and Pacific Islanders that were incarcerated in California in 2018?
  ca_aapi_incar <- df_incarceration %>% 
    filter(state == "CA") %>%
    filter(year == max(year)) %>% 
    select(county_name, aapi_jail_pop)
  aapi_incar_2018 <- sum(ca_aapi_incar$aapi_jail_pop, na.rm = TRUE)

  # How many Native Americans were incarcerated in California in the year 2018?
  ca_native_incar <- df_incarceration %>% 
    filter(state == "CA") %>% 
    filter(year == max(year)) %>% 
    select(county_name, native_jail_pop)  
  native_incar_2018 <- sum(ca_native_incar$native_jail_pop, na.rm = TRUE)

  # The amount of Latinx incarcerated in California in the year 2018 is?
  ca_latinx_incar <- df_incarceration %>% 
    filter(state == "CA") %>%
    filter(year == max(year)) %>% 
    select(county_name, latinx_jail_pop)
  latinx_incar_2018 <- sum(ca_latinx_incar$latinx_jail_pop, na.rm = TRUE)

  # How do the incarcerations of different races compare in California?
  compare_races <- df_incarceration %>% 
  filter(state == "CA") %>% 
    group_by(year) %>%   
    summarize(white_incar = sum(white_jail_pop, na.rm = TRUE), native_incar = sum(native_jail_pop, na.rm = TRUE), 
              aapi_incar = sum(aapi_jail_pop, na.rm = TRUE), latinx_incar = sum(latinx_jail_pop, na.rm = TRUE), 
              black_incar = sum(black_jail_pop, na.rm = TRUE))

  # What is the top county of Asian American and Pacific Islander incarcerations in California in the year 2018?
  top_county_aapi <- df_incarceration %>% 
    filter(state == "CA") %>% 
    filter(year == max(year)) %>% 
    top_n(1, wt = aapi_jail_pop) %>% 
    select(year, county_name, aapi_jail_pop)

  # What is the top county of Native incarcerations in California in the year 2018?
  top_county_native <- df_incarceration %>% 
    filter(state == "CA") %>% 
    filter(year == max(year)) %>% 
    top_n(1, wt = native_jail_pop) %>% 
    select(year, county_name, native_jail_pop)

  # What is the top county of Latinx incarcerations in California in the year 2018?
  top_county_latinx <- df_incarceration %>% 
    filter(state == "CA") %>% 
    filter(year == max(year)) %>% 
    top_n(1, wt = latinx_jail_pop) %>% 
    select(year, county_name, latinx_jail_pop)

## Trends over time chart
incar_races <- gather(compare_races, key=Race, value=incarcerations, -year)
compare_races_chart <- ggplot(incar_races) +
  geom_line(aes(x=year, y=incarcerations, color=Race)) +
  labs(title="Total Incarcerations Over Time", 
       x="Year", 
       y="Latinx, White, AAPI, Black, Native Incarcerated") 
compare_races_chart
## Variable comparison chart

scatter_data <- df_incarceration %>% 
  filter(state == "CA") %>% 
  group_by(year) %>%
  summarize(aapi_incar = sum(aapi_jail_pop, na.rm = TRUE), latinx_incar = sum(latinx_jail_pop, na.rm = TRUE))
compare_inc_chart <- ggplot(scatter_data, mapping = aes(x = aapi_incar, y = latinx_incar)) +
  geom_point() +
  labs(x = "Asian American/Pacific Islander Incarcerated", 
       y = "Latinx Incarcerated", 
       title = "AAPI Incarceration and Latinx Incarceration")
compare_inc_chart

## Map

# Blank theme
blank_theme <- theme_bw() +
  theme(
    axis.line = element_blank(), # remove axis lines
    axis.text = element_blank(), # remove axis labels
    axis.ticks = element_blank(), # remove axis ticks
    axis.title = element_blank(), # remove axis titles
    plot.background = element_blank(), # remove gray background
    panel.grid.major = element_blank(), # remove major grid lines
    panel.grid.minor = element_blank(), # remove minor grid lines
    panel.border = element_blank() # remove border around plot
  )

