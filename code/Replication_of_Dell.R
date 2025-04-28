library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(fixest)
library(sf)
library(stringr)


##### Replicating the main results #####

cl = read_dta("dataset_by_Dell.dta")


# Sort data
cl <- cl %>% arrange(fips60_06, year)

cl <- cl %>% filter(year >= 1960 & year <= 2003)

# Calculate GDP growth
cl$log_capita <- log(cl$gdpLCU)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(growth = (log_capita - lag(log_capita)) * 100) %>%
    ungroup()

# Keep countries with at least 20 non-missing growth observations
cl$tempnonmis <- ifelse(!is.na(cl$growth), 1, 0)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(tempsumnonmis = sum(tempnonmis)) %>%
    ungroup()
cl <- cl %>%
    filter(tempsumnonmis >= 20) %>%
    select(-tempnonmis)


# Assign regions based on predefined dummies
cl$region <- NA
regions <- c("_MENA", "_SSAF", "_LAC", "_WEOFF", "_EECA", "_SEAS")
for (region in regions) {
    cl$region[cl[[region]] == 1] <- region
}

# Create region-year interaction
cl$regionyear <- paste0(cl$region, cl$year)


# Creating poor_dummy and poor_temp interaction
cl <- cl %>%
    group_by(country_code) %>%
    mutate(first_PPP = log(first(na.omit(rgdpl)))) %>%
    ungroup()

median_m <- median(unique(cl$first_PPP), na.rm = T)

cl$poor_dummy <- ifelse(cl$first_PPP < median_m, 1, 0)

cl$poor_temp <- cl$wtem * cl$poor_dummy

# Creating year_poor FE
cl$year_poor <- ifelse(cl$poor_dummy != 0, paste0(cl$year, "_1"), "0")

# Run Regression
feols(growth ~ wtem + poor_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl)





##### Replication with new economic data #####

cl <- readRDS("cl.rds")  # Dataset with new economic data


# Sort data
cl <- cl %>% arrange(fips60_06, year)

cl <- cl %>% filter(year >= 1960 & year <= 2003)

# Calculate GDP growth
cl$log_capita <- log(cl$gdp_capita)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(growth = (log_capita - lag(log_capita)) * 100) %>%
    ungroup()

# Keep countries with at least 20 non-missing growth observations
cl$tempnonmis <- ifelse(!is.na(cl$growth), 1, 0)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(tempsumnonmis = sum(tempnonmis)) %>%
    ungroup()
cl <- cl %>%
    filter(tempsumnonmis >= 20) %>%
    select(-tempnonmis)


# Assign regions based on predefined dummies
cl$region <- NA
regions <- c("_MENA", "_SSAF", "_LAC", "_WEOFF", "_EECA", "_SEAS")
for (region in regions) {
    cl$region[cl[[region]] == 1] <- region
}

# Create region-year interaction
cl$regionyear <- paste0(cl$region, cl$year)


# Creating poor_dummy and poor_temp interaction
cl <- cl %>%
    group_by(country_code) %>%
    mutate(first_PPP = log(first(na.omit(gdp_ppp)))) %>%
    ungroup()

median_m <- median(unique(cl$first_PPP), na.rm = T)

cl$poor_dummy <- ifelse(cl$first_PPP < median_m, 1, 0)

cl$poor_temp <- cl$wtem * cl$poor_dummy

# Creating year_poor FE
cl$year_poor <- ifelse(cl$poor_dummy != 0, paste0(cl$year, "_1"), "0")

# Run Regression
feols(growth ~ wtem + poor_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl)
