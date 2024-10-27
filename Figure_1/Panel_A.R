# Changes and Variability in Temperature - recreating figure 1-Panel A from the paper in R

library(ggplot2)
library(tidyr)
library(haven) # For reading .dta files
library(dplyr)

climate_panel <- read_dta("climate_panel.dta")

# Filter countries with less than 20 GDP values
climate_panel <- climate_panel %>%
    group_by(country_code) %>%
    filter(sum(!is.na(gdpLCU)) >= 20) %>%
    ungroup()

panel_1950 <- climate_panel %>%
    filter(between(year, 1950, 1959))
panel_1996 <- climate_panel %>%
    filter(between(year, 1996, 2005))

countries <- sort(unique(panel_1950$country_code))
#use panel_1950 instead of climate_panel as there are countries with no temperature record between 1950 and 59

# Initialize data frame to hold summary statistics for each country
panel_summary <- data.frame(
    country = countries,
    mean_temp_1950 = 0,
    mean_temp_1996 = 0,
    gdp_2000 = 0,
    log_gdp = 0,
    max_temp = 0,
    min_temp = 0
)

# create subset of climate panel for year 2000 to extract GDP data
year_2000 <- climate_panel %>%
    filter(year == 2000)

# Loop through each country to calculate min, max and mean temperatures and GDP
for (i in 1:nrow(panel_summary)) {
    code <- countries[i]
    panel_summary$gdp_2000[i] <- subset(year_2000, country_code == code)$rgdpl #the paper says data from WDI was used. It looks like this is not the case (?)
    panel_summary$mean_temp_1950[i] <- mean(subset(panel_1950, country_code == code)$wtem, na.rm = TRUE)
    panel_summary$mean_temp_1996[i] <- mean(subset(panel_1996, country_code == code)$wtem, na.rm = TRUE)
    panel_summary$max_temp[i] <- max(max(subset(panel_1950, country_code == code)$wtem, na.rm = TRUE), max(subset(panel_1996, country_code == code)$wtem, na.rm = TRUE))
    panel_summary$min_temp[i] <- min(min(subset(panel_1950, country_code == code)$wtem, na.rm = TRUE), min(subset(panel_1996, country_code == code)$wtem, na.rm = TRUE))
}

# Compute the log of GDP
panel_summary$log_gdp <- log(panel_summary$gdp_2000)

# Transform panel summary to long format for plotting
panel_long <- panel_summary %>%
    pivot_longer(cols = c(mean_temp_1950, mean_temp_1996), names_to = "group", values_to = "mean_temp")

# Create plot
ggplot(panel_long, aes(x = log_gdp, y = mean_temp, color = group, shape = group)) +
    scale_x_continuous(limits = c(NA, 10.9)) + ylim(-10, 30) +
    coord_fixed(ratio = 0.09) +
    geom_segment(aes(x = log_gdp, xend = log_gdp, y = min_temp, yend = max_temp), color = "lightgray") +
    geom_point(size = 1.3) +
    geom_text(aes(label = ifelse(group == "mean_temp_1950", country, "")), 
              hjust = -0.2, vjust = -0.3, size = 1.8, color = "black") +
    labs(x = "Log per capita GDP in 2000", y = "Degrees", title = "Panel A. Temperature") +
    scale_shape_manual(values = c("mean_temp_1950" = 1, "mean_temp_1996" = 3), 
                       labels = c("mean_temp_1950" = "Mean 1950-1959", "mean_temp_1996" = "Mean 1996-2005"),
                       guide = guide_legend(nrow = 1)) +
    scale_color_manual(values = c("mean_temp_1950" = "darkblue", "mean_temp_1996" = "red"), 
                       labels = c("mean_temp_1950" = "Mean 1950-1959", "mean_temp_1996" = "Mean 1996-2005"),
                       guide = guide_legend(nrow = 1)) +
    theme(
        panel.background = element_rect(fill = "white"), 
        axis.line = element_line(color = "black", size = 0.39),
        legend.position = c(0.7, 0.1),  # bottom right
        legend.title = element_blank(),  # Removes legend title
        legend.background = element_rect(fill = "white", color = "black", size = 0.3),
        legend.text = element_text(margin = margin(r = 0, l = 0))
    )

# The paper says: "On average, a simple cross-section regression in the year 2000
# shows that a 1°C increase in average temperature predicts a fall in per capita income
# by 0.085 log points (i.e., about 8 percent)"
# Here, we check this relationship

avg_temp <- panel_summary %>%
    group_by(country) %>%
    summarize(mean_temp = rowMeans(cbind(mean_temp_1950, mean_temp_1996), na.rm = TRUE))

year_2000 <- year_2000 %>%
    left_join(avg_temp, by = c("country_code" = "country"))

year_2000$log_gdp <- log(year_2000$rgdpl)

model <- lm(log_gdp ~ mean_temp, data = year_2000)

summary(model)