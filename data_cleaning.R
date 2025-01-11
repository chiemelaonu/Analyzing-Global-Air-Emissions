# loading in packages and data ----

library(tidyverse) 
library(scales)
air_emissions_quarterly <- read_csv("data/OECD.SDD.NAD.SEEA,DSD_AEA@DF_AEA,1.1+.Q..T_CO2E+T...GHG+CO2+CO....csv")
air_emissions_data <- read_csv("data/2.csv")

# cleaning and rearranging data ----

# removing the duplicate columns 
air_emissions_data <- air_emissions_data |>
  select(-c(`Time period`, `Observation value`, OBS_STATUS,
            UNIT_MULT, `Unit multiplier`, Decimals, DECIMALS,
            REF_AREA, `Frequency of observation`, Measure,
            `Unit of measure`, Adjustment, ACTIVITY, Pollutants, `Activity scope`,
            Methodology, Source, STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION, ADJUSTMENT, METHODOLOGY))

# pivoting to make a clear column that hold the emissions values
air_emissions_yearly <- air_emissions_data |>
  pivot_wider(
    names_from = MEASURE,
    values_from = OBS_VALUE
  ) |> # moving the more important columns to the front so data is easier to read
  relocate(`Reference area`, EMISSIONS, `Observation status`, UNIT_MEASURE, POLLUTANT, TIME_PERIOD)

# completing the same process for the quarterly data 
air_emissions_quarterly <- air_emissions_quarterly |>
  select(-c(`Time period`, `Observation value`, OBS_STATUS,
            UNIT_MULT, `Unit multiplier`, Decimals, DECIMALS,
            REF_AREA, `Frequency of observation`, Measure,
            `Unit of measure`, Adjustment, ACTIVITY, Pollutants, `Activity scope`,
            Methodology, Source, STRUCTURE, STRUCTURE_ID, STRUCTURE_NAME, ACTION, ADJUSTMENT, METHODOLOGY))

air_emissions_quarterly <- air_emissions_quarterly |>
  pivot_wider(
    names_from = MEASURE,
    values_from = OBS_VALUE
  ) |>
  relocate(`Reference area`, EMISSIONS, `Observation status`, UNIT_MEASURE, POLLUTANT, TIME_PERIOD)


# combining the data by rows so that the quarterly and yearly missions are included in the same column
air_emissions <- rbind(air_emissions_yearly, air_emissions_quarterly)

air_emissions <- air_emissions |>
  mutate(
    Region = case_when(
      `Reference area` %in% c("United States", "Canada", "Mexico") ~ "North America",
      `Reference area` %in% c("Colombia", "Costa Rica") ~ "South America",
      `Reference area` %in% c("Lithuania", "Austria", "Germany", "United Kingdom", "Switzerland", "Sweden", "Spain", "Slovenia", "European Union (27 countries from 01/02/2020)", "Slovak Republic", "Serbia", "Romania", "Portugal", "Poland", "Norway", "New Zealand", "Netherlands", "Malta", "Luxembourg", "Latvia", "Italy", "Ireland", "Iceland", "Hungary", "Greece", "Germany", "France", "Finland", "Estonia", "Denmark", "Czechia", "Croatia", "Bulgaria", "Belgium", "Austria", "Australia", "Ukraine", "Russia") ~ "Europe",
      `Reference area` %in% c("Türkiye", "Korea", "Kazakhstan", "Japan", "Indonesia", "Cyprus") ~ "Asia"
    )
  ) # adding a regions column

air_emissions_clean <- air_emissions |>
  filter(`Reference area` != "OECD") # removing oecd observations because the countries are already represented in the data


air_emissions <- air_emissions %>%
  mutate(EMISSIONS = as.character(EMISSIONS))

write_csv(air_emissions, "data/air_emissions_clean.csv")

# making the codebook
emissions_codebook <- tibble(
`Reference Area` = "The country in which the observation was recorded",
EMISSIONS = "The air emissions value",
`Observation Status` = "How the observation was made. Either 'Estimated', 'Provisional Value', or 'Time series break'. A provisional value is one that was estimated with the expectation that the value would change. A time series break is when a pattern in the data aburptly changes in comparison to previous or following years.",
UNIT_MEASURE = "The units the emissions are in. Either Tonnes (T) or Tonnes of CO2-equivalent (T-CO2E)",
POLLUTANT = "The type of emissions. Carbon Monoxide (CO), Carbon Dioxide (CO2) and Greenhouse Gasses (GHG) are recorded.",
TIME_PERIOD = "The year quarter in which the observations are from. The observations that end in '-Q...' are the quarterly observations.",
FREQ = "How often the observations were recorded. Will either be Annual (A) or Quarterly (Q).",
`Economic Activity` = "The activity responsible for the emissions",
`Activity scope` = " Essentially a continuation of the Economic Activity variable; it gives more context surrounding the cause of the emissions source: Reported by the national statistical authority (REPORTED)")
write_csv(emissions_codebook, "data/emissions_codebook.csv")
