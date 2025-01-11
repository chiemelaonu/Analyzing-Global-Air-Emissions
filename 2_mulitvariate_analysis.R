library(tidyverse) 
library(scales)

air_emissions <- read_csv("data/air_emissions_clean.csv")


# Economic Activity Plots ----
# distinct number of economic activities
air_emissions |>
  distinct(`Economic activity`) |>
  count() |>
  knitr::kable()

# Ten Most Frequently Recorded Economic Activities"
air_emissions |>
  count(`Economic activity`, sort = TRUE) |>
  slice_head(n = 10) |>
  knitr::kable()

# Ten Least Frequently Recorded Economic Activities"
air_emissions |>
  count(`Economic activity`, sort = FALSE) |>
  slice_head(n = 10) |>
  knitr::kable()


# Top Ten Frequently Recorded Economic Activities with Region Makeup"
air_emissions |>
  count(`Economic activity`, sort = TRUE) |>
  slice_head(n = 10) |>
  left_join(
    air_emissions |>
      count(`Economic activity`, Region) |>
      group_by(`Economic activity`) |>
      summarize(Regions = paste(paste0(Region, " (", n, ")"), collapse = ", ")),
    by = "Economic activity"
  ) |>
  knitr::kable()




# PLOT OF EMISSIONS ----
## BY QUARTER ----
# Ensure that TIME_PERIOD is a factor 
air_emissions$TIME_PERIOD <- as.factor(air_emissions$TIME_PERIOD)
air_emissions$EMISSIONS <- as.numeric(air_emissions$EMISSIONS)
# Summarize emissions by Quarter
quarterly_emissions <- air_emissions |>
  filter(grepl('-Q', TIME_PERIOD)) |>
  group_by(TIME_PERIOD) |>
  summarize(Total_Emissions = sum(EMISSIONS, na.rm = TRUE), .groups = "drop")


ggplot(quarterly_emissions, aes(x = TIME_PERIOD, y = Total_Emissions, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Change in Total Emissions by Quarter",
    x = "Quarter",
    y = "Total Emissions"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
  scale_y_continuous(labels = scales::comma) 


## BY YEAR ----
air_emissions$TIME_PERIOD <- as.factor(air_emissions$TIME_PERIOD)
air_emissions$EMISSIONS <- as.numeric(air_emissions$EMISSIONS)
# Summarize emissions by year
yearly_emissions <- air_emissions |>
  filter(!grepl('-Q', TIME_PERIOD)) |>
  group_by(TIME_PERIOD) |>
  summarize(Total_Emissions = sum(EMISSIONS, na.rm = TRUE), .groups = "drop")

# Plot emissions by year
ggplot(yearly_emissions, aes(x = TIME_PERIOD, y = Total_Emissions, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Change in Total Emissions by Year",
    x = "Quarter",
    y = "Total Emissions"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
  scale_y_continuous(labels = scales::comma) 



## BY COUNTRY ----
air_emissions$EMISSIONS <- as.numeric(air_emissions$EMISSIONS)

# Filter for United States
us_emissions <- air_emissions %>%
  filter(`Reference area` == "United States") %>%
  group_by(TIME_PERIOD) %>%
  summarize(Total_Emissions = sum(EMISSIONS, na.rm = TRUE), .groups = "drop")

# Plot emissions for the United States by year
ggplot(us_emissions, aes(x = TIME_PERIOD, y = Total_Emissions, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Total Emissions per Year for United States",
    x = "Year",
    y = "Total Emissions"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas for large numbers
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

air_emissions$EMISSIONS <- as.numeric(air_emissions$EMISSIONS)
# Filter for japan
japan_emissions <- air_emissions %>%
  filter(`Reference area` == "Japan") %>%
  group_by(TIME_PERIOD) %>%
  summarize(Total_Emissions = sum(EMISSIONS, na.rm = TRUE), .groups = "drop")

# Plot emissions for Japan by year
ggplot(japan_emissions, aes(x = TIME_PERIOD, y = Total_Emissions, group = 1)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Total Emissions per Year for Japan",
    x = "Year",
    y = "Total Emissions"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas for large numbers
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## BY REGION ----
air_emissions <- air_emissions |>
  unnest(cols = EMISSIONS)
air_emissions$TIME_PERIOD <- as.factor(air_emissions$TIME_PERIOD)
air_emissions$EMISSIONS <- as.numeric(air_emissions$EMISSIONS)
# Filter for North America
europe_emissions <- air_emissions |>
  filter(Region == "North America") |>
  group_by(TIME_PERIOD) %>%
  summarize(Total_Emissions = sum(EMISSIONS, na.rm = TRUE), .groups = "drop")

# Plot emissions for North America by year
ggplot(europe_emissions, aes(x = TIME_PERIOD, y = Total_Emissions, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Total Emissions per Year for North America",
    x = "Year",
    y = "Total Emissions",
    caption = "Data from Organization for Economic Co-operation and Development (OECD)"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas for large numbers
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Filter for Asia
asia_emissions <- air_emissions |>
  filter(Region == "Asia") |>
  group_by(TIME_PERIOD) |>
  summarize(Total_Emissions = sum(EMISSIONS, na.rm = TRUE), .groups = "drop")

# Plot emissions for Asia by year
ggplot(asia_emissions, aes(x = TIME_PERIOD, y = Total_Emissions, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Total Emissions per Year for Asia",
    x = "Year",
    y = "Total Emissions",
    caption = "Data from Organization for Economic Co-operation and Development (OECD)"
  ) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas for large numbers
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# zooming in below 5 billion tons to see the decrease better
ggplot(asia_emissions, aes(x = TIME_PERIOD, y = Total_Emissions, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkblue") +
  labs(
    title = "Total Emissions per Year for Asia",
    x = "Year",
    y = "Total Emissions",
    caption = "Data from Organization for Economic Co-operation and Development (OECD)"
  ) + 
  coord_cartesian(ylim = c(0, 5000000000)) +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas for large numbers
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
