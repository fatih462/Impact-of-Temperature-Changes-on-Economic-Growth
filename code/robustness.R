library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(fixest)
library(sf)
library(stringr)

### Quadratic effect of oemperature ###
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

# Quadratic
feols(growth ~ w_temp + I(w_temp^2) + poor_temp + I(poor_temp^2) | fips60_06 + regionyear + year_poor,
                    cluster = ~parent + regionyear, data = cl)
# --> no significant effect




### No filter for countries with < 25 growth data  ###
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


feols(growth ~ w_temp + poor_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (2)




### Poor defintion in year 2010 ###
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
    mutate(first_PPP = log(gdp_ppp[year == 2010][1])) %>%
    ungroup()

median_m <- median(unique(cl$first_PPP), na.rm = T)

cl$poor_dummy <- ifelse(cl$first_PPP < median_m, 1, 0)

cl$poor_temp <- cl$w_temp * cl$poor_dummy

# Creating year_poor FE
cl$year_poor <- ifelse(cl$poor_dummy != 0, paste0(cl$year, "_1"), "0")


feols(growth ~ w_temp + poor_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (2)



### Balanced sample 1990 - 2014
cl <- readRDS("cl.rds")

# Sort data
cl <- cl %>% arrange(fips60_06, year)

cl <- cl %>% filter(year >= 1990 & year <= 2014)

# Calculate GDP growth
cl$log_capita <- log(cl$gdp_capita)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(growth = (log_capita - lag(log_capita)) * 100) %>%
    ungroup()



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


model <- feols(growth ~ w_temp + poor_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl) # (3)

