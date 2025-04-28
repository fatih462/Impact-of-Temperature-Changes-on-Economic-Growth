library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(fixest)
library(sf)
library(stringr)

cl = read_dta("climate_panel.dta") # Dataset published by Dell et at. 2012



# Deleting unnecessary columns
cl <- cl[ , !(names(cl) %in% c("fips", "wtem50", "wpre", "wpre50", "ki", "gdpSHAREAG", "gdpWDIGDPAGR", "gdpWDIGDPIND"))]



# Deleting "Serbia and Montenegro"
cl <- cl[cl$country_code != "YUG", ]

# Adding Serbia and Montenegro seperately
new_rows <- data.frame(X1 = 1950:2006, X2 = "RI", X3 = NA, X4 = "Serbia", X5 = "SRB", X6 = NA, X7 = NA, X8 = NA,
                       X9 = NA, X10 = NA, X11 = NA, X12 = NA, X13 = NA, X14 = NA, X15 = "YGL")
colnames(new_rows) <- colnames(cl)
cl <- rbind(cl, new_rows)
new_rows <- data.frame(X1 = 1950:2006, X2 = "MJ", X3 = NA, X4 = "Montenegro", X5 = "MNE", X6 = NA, X7 = NA, X8 = NA,
                       X9 = NA, X10 = NA, X11 = NA, X12 = NA, X13 = NA, X14 = NA, X15 = "YGL")
colnames(new_rows) <- colnames(cl)
cl <- rbind(cl, new_rows)




# Adding the years 2007 to 2014 to the dataset
cl_2006 <- cl[cl$year == 2006, ]
new_years <- 2007:2014

cl_new <- cl_2006[rep(1:nrow(cl_2006), each = length(new_years)), ] # Duplicate the 2006 data for each new year
cl_new$year <- rep(new_years, times = nrow(cl_2006))

cl <- bind_rows(cl, cl_new) %>%
    arrange(fips60_06, year)


# Loading gdp per capita (constant LCU)
df_excel <- read_excel("capita_LCU.xlsx") %>%
    rename_with(~ str_remove(., " \\[YR\\d{4}\\]")) # Clean excel file

df_long <- df_excel %>%
    pivot_longer(
        cols = `1960`:`2014`,
        names_to = "year",
        values_to = "gdp_capita"
    ) %>%
    mutate(
        year = as.integer(year),
    )
df_final <- cl %>%
    left_join(df_long, by = c("country_code" = "Country Code", "year" = "year"))
df_final$gdp_capita[df_final$gdp_capita == ".."] <- NA

cl$gdp_capita <- as.numeric(df_final$gdp_capita)



# Loading gdp per capita PPP
df_excel <- read_excel("capita_PPP.xlsx") %>%
    rename_with(~ str_remove(., " \\[YR\\d{4}\\]")) # Clean excel file

df_long <- df_excel %>%
    pivot_longer(
        cols = `1960`:`2014`,
        names_to = "year",
        values_to = "gdp_ppp"
    ) %>%
    mutate(
        year = as.integer(year),
    )


df_final <- cl %>%
    left_join(df_long, by = c("country_code" = "Country Code", "year" = "year"))
df_final$gdp_ppp[df_final$gdp_ppp == ".."] <- NA

cl$gdp_ppp <- as.numeric(df_final$gdp_ppp)



# Loading temperature data from geodatabase
gdb_path = "temp_tables.gdb"

# Load layer
for (year in 1960:2014) {
    layer_name <- paste0("ZonalStats_annual_mean_filled_", year)

    if (layer_name %in% st_layers(gdb_path)$name) {
        data <- st_read(gdb_path, layer = layer_name)
        data$year <- year
        assign(paste0("data_", year), data)
    }
}

# Country code corrections
for (year in 1960:2014) {
    data_name <- paste0("data_", year)

    if (exists(data_name)) {
        df <- get(data_name)
        df$country_code <- data_1990$Iso3v10

        df$country_code[df$country_code == "COD"] <- "ZAR"
        df$country_code[df$country_code == "ROU"] <- "ROM"

        assign(data_name, df)

    }
}

# Join
for (year in 1960:2014) {
    data_name <- paste0("data_", year)

    if (exists(data_name)) {
        data_year <- get(data_name)
        cl <- cl %>%
            left_join(data_year %>% select(country_code, year, w_temp),
                      by = c("country_code" = "country_code", "year" = "year"))
    }
}

cl <- cl %>%
    mutate(WMMT = coalesce(!!!select(., starts_with("w_temp")))) %>%
    select(-starts_with("w_temp"))



saveRDS(cl, file = "cl.rds")


