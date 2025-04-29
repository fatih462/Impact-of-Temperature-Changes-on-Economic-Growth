library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(fixest)
library(sf)
library(stringr)


cl <- readRDS("cl.rds")


# Sort data
cl <- cl %>% arrange(fips60_06, year)

cl <- cl %>% filter(year >= 1960 & year <= 2014)

# Calculate GDP growth
cl$log_capita <- log(cl$gdp_capita)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(growth = (log_capita - lag(log_capita)) * 100) %>%
    ungroup()


# Keep countries with at least 25 non-missing growth observations
cl$tempnonmis <- ifelse(!is.na(cl$growth), 1, 0)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(tempsumnonmis = sum(tempnonmis)) %>%
    ungroup()
cl <- cl %>%
    filter(tempsumnonmis >= 25) %>%
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
    mutate(first_PPP = log(gdp_ppp[year == 1990][1])) %>%
    ungroup()

median_m <- median(unique(cl$first_PPP), na.rm = T)

cl$poor_dummy <- ifelse(cl$first_PPP < median_m, 1, 0)

cl$poor_temp <- cl$w_temp * cl$poor_dummy

# Creating year_poor FE
cl$year_poor <- ifelse(cl$poor_dummy != 0, paste0(cl$year, "_1"), "0")

# Run Regression
feols(growth ~ w_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (1)


feols(growth ~ w_temp + poor_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (2)






### Include lags

cl <- cl %>%
    arrange(fips60_06, year) %>%
    group_by(fips60_06) %>%
    mutate(
        wtemp_lag1 = lag(w_temp, 1),
        wtemp_lag2 = lag(w_temp, 2),
        wtemp_lag3 = lag(w_temp, 3),
        wtemp_lag4 = lag(w_temp, 4),
        wtemp_lag5 = lag(w_temp, 5),
        poor_temp_lag1 = lag(poor_temp, 1),
        poor_temp_lag2 = lag(poor_temp, 2),
        poor_temp_lag3 = lag(poor_temp, 3),
        poor_temp_lag4 = lag(poor_temp, 4),
        poor_temp_lag5 = lag(poor_temp, 5)
    ) %>%
    ungroup()


# 1 lag
feols(growth ~ w_temp + wtemp_lag1 + poor_temp + poor_temp_lag1 | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (3)


# 3 lags
feols(growth ~ w_temp + wtemp_lag1 + wtemp_lag2 + wtemp_lag3 + poor_temp + poor_temp_lag1 + poor_temp_lag2 + poor_temp_lag3 | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (4)


# 5 lags
feols(growth ~ w_temp + wtemp_lag1 + wtemp_lag2 + wtemp_lag3 + wtemp_lag4 + wtemp_lag5 + poor_temp + poor_temp_lag1 + poor_temp_lag2 + poor_temp_lag3 + poor_temp_lag4 + poor_temp_lag5 | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (5)

