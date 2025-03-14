---
title: "Impact of Severe Weather Events in the U.S."
author: "Jean Obuya"
date: "2025-03-14"
output:
  html_document:
    keep_md: true
    code_folding: hide
---

## Synopsis
Severe weather events cause significant damage to both public health and the economy. This report analyzes the U.S. National Oceanic and Atmospheric Administration (NOAA) Storm Database to identify the most harmful events in terms of fatalities, injuries, and economic consequences. The data spans from 1950 to November 2011, with more complete records in recent years. The goal is to provide insights to municipal and government managers for resource prioritization.

## Data Processing


### Load Libraries

``` r
library(dplyr)
library(ggplot2)
library(readr)
```

### Load Data

``` r
storm_data <- read.csv(bzfile("repdata_data_StormData.csv.bz2"))
```

### Data Cleaning and Transformation

``` r
storm_data <- storm_data %>% 
  select(EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP) %>%
  mutate(PROPDMGEXP = toupper(PROPDMGEXP),
         CROPDMGEXP = toupper(CROPDMGEXP))

# Function to convert damage exponents
dmg_exp <- function(exp) {
  ifelse(exp == "K", 1e3,
  ifelse(exp == "M", 1e6,
  ifelse(exp == "B", 1e9, 1)))
}

# Apply transformations
storm_data <- storm_data %>%
  mutate(PROPDMG = PROPDMG * dmg_exp(PROPDMGEXP),
         CROPDMG = CROPDMG * dmg_exp(CROPDMGEXP))
```

## Results

### Events Most Harmful to Population Health

``` r
health_impact <- storm_data %>% 
  group_by(EVTYPE) %>% 
  summarise(Fatalities = sum(FATALITIES, na.rm = TRUE),
            Injuries = sum(INJURIES, na.rm = TRUE)) %>%
  arrange(desc(Fatalities + Injuries)) %>%
  top_n(10, wt = Fatalities + Injuries)

# Plot
health_plot <- ggplot(health_impact, aes(x = reorder(EVTYPE, -(Fatalities + Injuries)), y = Fatalities + Injuries, fill = Fatalities)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Events Impacting Population Health",
       x = "Event Type", y = "Total Fatalities & Injuries")
health_plot
```

![](weather_data_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Events with Greatest Economic Consequences

``` r
economic_impact <- storm_data %>% 
  group_by(EVTYPE) %>% 
  summarise(TotalDamage = sum(PROPDMG + CROPDMG, na.rm = TRUE)) %>%
  arrange(desc(TotalDamage)) %>%
  top_n(10, wt = TotalDamage)

# Plot
econ_plot <- ggplot(economic_impact, aes(x = reorder(EVTYPE, -TotalDamage), y = TotalDamage)) +
  geom_bar(stat = "identity", fill = "darkred") +
  coord_flip() +
  labs(title = "Top 10 Events Causing Economic Damage",
       x = "Event Type", y = "Total Damage (USD)")
econ_plot
```

![](weather_data_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

## Conclusion
Tornadoes cause the highest number of fatalities and injuries, making them the most dangerous for population health. In terms of economic damage, hurricanes and floods cause the greatest financial losses. These insights can help authorities allocate resources effectively to mitigate storm-related risks.
