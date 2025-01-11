library(tidyverse) 
library(scales)
air_emissions <- read_csv("data/air_emissions_clean.csv")


# finding na values ----
sum(is.na(air_emissions))

air_emissions |>
  count(is.na(`Observation status`))


# start of eda ----

air_emissions |>
  distinct(`Reference area`)

air_emissions |>
  distinct()

## plotting distribution of the reference areas/countries in the data ----
air_emissions |>
  ggplot(aes(`Reference area`)) +
  geom_bar(fill = "cadetblue") +
  coord_flip() +
  labs(
    y = "Number of Observations",
    x = "Reference Area")

## bar graph that shows the number of observations within each year in the dataset ----
air_emissions |>
  filter(TIME_PERIOD == c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024)) |>
  ggplot(aes(TIME_PERIOD)) +
  geom_bar(fill = "black") 




# function to find counts of unit measure and pollutants in data
emissions_func <- function(data, var) {
  data |>
    ggplot(aes({{ var }})) +
    geom_bar() +
    labs(
      title = "Pollutant Distribution",
      x = "Pollutant",
      y = "Count"
    )
}
emissions_func(air_emissions, POLLUTANT)
emissions_func(air_emissions, UNIT_MEASURE)

# highest yearly emissions ----
top5_yearly <- air_emissions |>
  filter(!grepl('-Q', TIME_PERIOD)) |>
  arrange(desc(EMISSIONS)) |>
  slice_head(n = 5) |>
  knitr::kable()
ggsave("plots/top5_yearly.png",
       plot = top5_yearly,
       width = 10,
       height = 8,
       units = "in")

# highest quarterly emissions ----
top5_quarterly <- air_emissions |>
  filter(grepl('-Q', TIME_PERIOD)) |>
  arrange(desc(EMISSIONS)) |>
  slice_head(n = 5) |>
  knitr::kable()

