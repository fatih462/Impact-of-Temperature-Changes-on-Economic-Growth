library(ggplot2)
library(ggrepel)
library(dplyr)
library(tidyr)
library(np)
library(tibble)


cl <- readRDS("cl.rds")

# Sort data
cl <- cl %>% arrange(fips60_06, year)

# Calculate GDP growth and temperature deviation
cl$log_capita <- log(cl$gdp_capita)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(growth = (log_capita - lag(log_capita)) * 100,
           temp_anomaly = w_temp - mean(w_temp, na.rm = TRUE)) %>%
    ungroup()


cl <- cl %>%
    group_by(country_code) %>%
    mutate(first_PPP = log(gdp_ppp[year == 1990][1])) %>%
    ungroup()

median_init <- median(unique(cl$first_PPP), na.rm = T)

cl$poor_dummy <- ifelse(cl$first_PPP < median_init, 1, 0) # Classify country as poor (1) / rich (0)

cl <- cl %>%
    filter(!is.na(poor_dummy))



### Figure 1

# Filter for the year 1990
data_1990 <- cl %>%
    filter(year == 1990)

# Threshold (median of log GDP PPP)
threshold_income <- median(log(data_1990$gdp_ppp), na.rm = TRUE)

ggplot(data_1990, aes(x = w_temp, y = log(gdp_ppp))) +
    geom_point(aes(shape = factor(poor_dummy)), size = 1.7, stroke = 0.6, color = "#4d4d4d") +
    geom_text(aes(label = country_code), size = 2, hjust = -0.2, vjust = -0.2) +
    scale_shape_manual(
        values = c("0" = 0, "1" = 1),
        labels = c("0" = "Rich", "1" = "Poor"),
        name = "Poverty Status"
    ) +
    geom_hline(yintercept = threshold_income, linetype = "dashed", color = "gray50", linewidth = 0.5) +
    labs(
        title = "Figure 1: Temperature and Income Levels in 1990",
        x = "Average Annual Temperature (°C)",
        y = "Log per Capita GDP (PPP)"
    ) +
    theme_minimal(base_size = 13) +
    theme(
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0),
        legend.position = c(0.2, 0.2),
        legend.title = element_blank(),  # Removes legend title
        legend.background = element_rect(fill = "white", color = "black", size = 0.25),
        legend.text = element_text(margin = margin(r = 0, l = 0), size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)
    ) +
    geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "darkblue", linewidth = 0.25)


model_90 <- lm(log(gdp_ppp) ~ w_temp, data = data_1990)
summary(model_90)









### Figure 2

# Keep countries with at least 20 non-missing growth observations
cl$tempnonmis <- ifelse(!is.na(cl$growth), 1, 0)
cl <- cl %>%
    group_by(fips60_06) %>%
    mutate(tempsumnonmis = sum(tempnonmis)) %>%
    ungroup()
cl <- cl %>%
    filter(tempsumnonmis >= 25) %>%
    select(-tempnonmis)


cl_filtered <- cl %>% filter(year >= 1983, year <= 1992, !is.na(growth), !is.na(temp_anomaly))
cl_filtered <- cl_filtered[!(cl_filtered$country_code == "IRQ" & cl_filtered$year == 1991), ] # An outlier that is excluded for better visual presentation

ggplot(cl_filtered, aes(x = temp_anomaly, y = growth, color = factor(poor_dummy))) +
    geom_point(alpha = 0.6, size = 0.8) +
    geom_smooth(method = "loess", span = 1.5, se = FALSE, linewidth = 0.3) +
    scale_color_manual(values = c("0" = "darkblue", "1" = "darkorange"), labels = c("Rich", "Poor")) +
    labs(
        title = "Figure 2: GDP Growth and Temperature Anomalies",
        x = "Temperature Deviation from Country Mean (°C)",
        y = "GDP per Capita Growth",
        color = "Income Group"
    ) +
    theme_minimal() +
    theme(
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0),
        legend.position = c(0.2, 0.2),
        legend.title = element_blank(),
        legend.background = element_rect(fill = "white", color = "black", size = 0.25),
        legend.text = element_text(margin = margin(r = 0, l = 0), size = 13),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)
    )





# SE temperature
sd(cl_filtered$w_temp[cl_filtered$poor_dummy == 1], na.rm = TRUE)
sd(cl_filtered$w_temp[cl_filtered$poor_dummy == 0], na.rm = TRUE)

sd(cl$w_temp[cl$poor_dummy == 1], na.rm = TRUE)
sd(cl$w_temp[cl$poor_dummy == 0], na.rm = TRUE)


# SE growth
sd(cl_filtered$growth[cl_filtered$poor_dummy == 1], na.rm = TRUE)
sd(cl_filtered$growth[cl_filtered$poor_dummy == 0], na.rm = TRUE)

sd(cl$growth[cl$poor_dummy == 1], na.rm = TRUE)
sd(cl$growth[cl$poor_dummy == 0], na.rm = TRUE)



