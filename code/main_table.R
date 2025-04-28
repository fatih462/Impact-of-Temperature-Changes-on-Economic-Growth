library(haven)
library(readxl)
library(dplyr)
library(tidyr)
library(fixest)
library(sf)

cl = read_dta("climate_panel.dta")

##### Deleting "Serbia and Montenegro" #####
cl <- cl[cl$country_code != "YUG", ]






##### Adding the years 2007 to 2014 to the dataset #####
cl_2006 <- cl[cl$year == 2006, ]
new_years <- 2007:2014

cl_new <- cl_2006[rep(1:nrow(cl_2006), each = length(new_years)), ] # Duplicate the 2006 data for each new year
cl_new$year <- rep(new_years, times = nrow(cl_2006))

cl <- bind_rows(cl, cl_new) %>%
    arrange(fips60_06, year)


##### Loading gdp per capita (constant LCU) #####
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



setdiff(cl$country_code, df_excel$`Country Code`)

setdiff(df_excel$`Country Code`, cl$country_code)


##### Loading gdp per capita PPP #####
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








new <- cl[cl$year == 2000, ]

gdb_path = "/home/hr/research/AEJ/temp_tables.gdb"

# Auflisten aller Layer (Tabellen und Feature Classes)
st_layers(gdb_path)

# Eine bestimmte Tabelle oder Feature Class einlesen
#data <- st_read(gdb_path, layer = "ZonalStats_annual_mean_filled_1960")

# Anzeigen der Daten
#head(data)


for (year in 1960:2005) {
    layer_name <- paste0("ZonalStats_annual_mean_filled_", year)

    if (layer_name %in% st_layers(gdb_path)$name) {  # Prüfen, ob die Schicht existiert
        data <- st_read(gdb_path, layer = layer_name)
        data$year <- year
        assign(paste0("data_", year), data)
    } else {
        message("Layer ", layer_name, " nicht gefunden und wird übersprungen.")
    }
}

for (year in 1960:2005) {
    data_name <- paste0("data_", year)  # Erzeugt den Namen des Datensatzes als String

    if (exists(data_name)) {  # Prüft, ob der Datensatz existiert
        df <- get(data_name)  # Holt das tatsächliche Datenobjekt
        df$country_code <- data_1990$Iso3v10  # Neue Spalte hinzufügen
        assign(data_name, df)  # Aktualisiert das Datenobjekt mit der neuen Spalte
    }
}

for (year in 1960:2005) {
    data_name <- paste0("data_", year)
    if (exists(data_name)) {
        df <- get(data_name)
        df$country_code[df$country_code == "COD"] <- "ZAR"
        df$country_code[df$country_code == "ROU"] <- "ROM"
        assign(data_name, df)
  }
}

#for (year in 1960:1994) {
 #   data_name <- paste0("data_", year)
  #  if (exists(data_name)) {
   #     df <- get(data_name)
    #    df$Countryeng[df$Countryeng == "United States of America"] <- "United States"
     #   df$Countryeng[df$Countryeng == "Bosnia-Herzegovina"] <- "Bosnia and Herzegovina"
      #  df$Countryeng[df$Countryeng == "Syrian Arab Republic"] <- "Syria"
#        df$Countryeng[df$Countryeng == "Morocco (includes Western Sahara)"] <- "Morocco"
 #       assign(data_name, df)
  #  }
#}

for (year in 1960:2005) {
    data_name <- paste0("data_", year)  # Variablenname für das Jahr

    if (exists(data_name)) {  # Prüfen, ob die Variable existiert
        data_year <- get(data_name)  # Datensatz abrufen

        # Join mit cl, nur wo Jahr & Land übereinstimmen
        cl <- cl %>%
            left_join(data_year %>% select(country_code, year, WMMT),
                      by = c("country_code" = "country_code", "year" = "year"))
    }
}

cl <- cl %>%
    mutate(WMMT = coalesce(!!!select(., starts_with("WMMT")))) %>%
    select(-starts_with("WMMT."))




# Regression

cl$value <- df_final$value

#cor(cl$gdpLCU, cl$value, use = "complete.obs", method = "pearson")


test <- cl[cl$country_code == "DEU", ]

plot(test$year, test$value, type = "l")
plot(test$year, test$gdpLCU, type = "l")

cor(test$gdpLCU, test$value, use = "complete.obs", method = "pearson")


unique_countries <- unique(na.omit(cl$country_code))

for (country in unique_countries) {
    test <- cl[cl$country_code == country, ]

    # Nur vollständige Zeilen behalten (ohne NA in den relevanten Spalten)
    #test <- test[complete.cases(test$growth, test$value), ]

    if (nrow(test) > 1) {  # Mindestens zwei Werte für die Korrelation nötig
        #cor_value <- cor(test$growth, test$value, method = "pearson")
        land <- unique(test$country)

        #if (!is.na(cor_value) && !is.nan(cor_value) && cor_value < 0.9) {
            #print(paste("Country:", land, "- Code:", country, "- Correlation:", cor_value))
        #}
        print(mean(abs(test$gdpLCU-test$rgdpl)/test$rgdpl,na.rm=T))
        print(paste("Country:", land, "- Code:", country, "- Correlation:", cor_value))
    }
}



# Daten sortieren: Erst nach fips60_06 (Länder), dann nach year (Jahre)
cl <- cl %>% arrange(fips60_06, year)


# keep if year <= 2003
cl <- cl %>% filter(year >= 1960 & year <= 2003)

cl$lngdpwdi <- log(cl$gdp_capita)
# GDP-Wachstum berechnen
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(growth = (lngdpwdi - lag(lngdpwdi)) * 100) %>%
    ungroup()


# Länder mit growth Daten weniger als 20 werden rausgefiltert
cl$tempnonmis <- ifelse(!is.na(cl$growth), 1, 0)
cl <- cl %>% # Schritt 2: Summe der gültigen Werte pro Gruppe berechnen
    group_by(fips60_06) %>%
    mutate(tempsumnonmis = sum(tempnonmis)) %>%
    ungroup()
cl <- cl %>% filter(tempsumnonmis >= 25)

# Optional: Temporäre Variablen entfernen
cl <- cl %>% select(-tempnonmis)


# Regionen zuweisen
cl$region <- NA  # Leere Variable (NA für fehlende Werte)
regions <- c("_MENA", "_SSAF", "_LAC", "_WEOFF", "_EECA", "_SEAS")
for (region in regions) {
    cl$region[cl[[region]] == 1] <- region
}


# Variable regionyear erstellen
cl$regionyear <- paste0(cl$region, cl$year)




##### In diesem Abschnitt will ich alles rund um poor_dummy selber machen

cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(init_gdp_ppp = log(first(rgdpl[!is.na(gdp_ppp)]))) %>%
    ungroup()


# Median bestimmen
median_init <- median(unique(cl$lnrgdpl_t0), na.rm = TRUE)


cl$poor_dummy <- ifelse(cl$lnrgdpl_t0 < median_init, 1, 0)

cl$poor_temp <- cl$wtem * cl$poor_dummy



cl$year_poor <- ifelse(cl$poor_dummy != 0, paste0(cl$year, "_1"), "0")


feols(growth ~ wtem + poor_temp | fips60_06 + regionyear + year_poor, cluster = ~parent + regionyear, data = cl)





summary(cl$growth)

vektor1 <- unique(cl$country_code)
vektor2 <- data_1989$country_code

setdiff(vektor1, vektor2)

setdiff(vektor2, vektor1)

vektor2

union(setdiff(vektor1, vektor2), setdiff(vektor2, vektor1))


unique(cl$country_code)


