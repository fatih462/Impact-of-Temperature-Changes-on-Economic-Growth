# Visualization of temperature change over the last decades in the 60 largest countries

library(rvest)
library(dplyr)
library(haven)
library(ggplot2)

# Scraping the Wikipedia page for country area data
url <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area"
page <- read_html(url)
tables <- page %>% html_table(fill = TRUE)
area_table <- tables[[2]]

# Clean area_table
area_table <- area_table[, c(2, 3)]
area_table <- area_table %>%
    rename(country = `Country / dependency`, total_area = `Totalin km2 (mi2)`)
area_table$total_area <- gsub(" \\(.*\\)", "", area_table$total_area)
area_table$total_area <- as.numeric(gsub("\\,", "", area_table$total_area))


# Load climate panel data
climate_panel <- read_dta("climate_panel.dta")

# Join area data to climate panel dataset
climate_panel <- climate_panel %>%
    left_join(area_table, by = "country")

# Filter data to only work with the 60 largest countries
climate_panel <- climate_panel %>%
    filter(!is.na(total_area))
sixty <- sort(unique(climate_panel$total_area), decreasing = TRUE)[60]
climate_panel <- climate_panel %>%
    filter(total_area >= sixty)

# Plot the temperature change
ggplot(climate_panel, aes(x = year, y = wtem, color = country_code)) +
    geom_line(alpha = 0.6) +
    labs(title = "Temperature Change Over Time for 60 Largest Countries", x = "Years (1950-2005)", y = "Temperature (°C)") +
    theme_minimal() +
    theme(axis.line = element_line(color = "black", size = 0.39),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
    ) +
    guides(color = "none") +
    facet_wrap(~ country_code, scales = "free_y")