library(rvest)
library(tidyverse)
library(xml2)
raw_data <-
  read_html(
    "https://en.wikipedia.org/wiki/List_of_prime_ministers_of_Australia"
  )
write_html(raw_data, "pms.html")

raw_data <- read_html("pms.html")

parse_data_selector_gadget <-
  raw_data |>
  html_element(".wikitable.plainrowheaders")|>
  html_table()

parsed_data <- 
  parse_data_selector_gadget|>
  clean_names()|>
  rename(raw_text = name_birth_death_constituency)|>
  select(raw_text)|>
  filter(raw_text != "Name(Birth–Death)Constituency")|>
  distinct()

cleaned_data <- parsed_data |>
  mutate(
    name = str_extract(raw_text, "^[^\\(]+"),
    birth_year = str_extract(raw_text, "\\d{4}"),
    death_year = ifelse(str_detect(raw_text, "–\\d{4}"), str_extract(raw_text, "(?<=–)\\d{4}"), NA),
    birth_year = as.numeric(birth_year), # Convert to numeric for calculation
    death_year = as.numeric(death_year), # Convert to numeric, NA if still alive
    age_at_death = ifelse(is.na(death_year), NA, death_year - birth_year) # Calculate age only if death_year is not NA
  ) |>
  select(name, birth_year, death_year, age_at_death)
write_csv(cleaned_data, "/Users/alexanderguarasci/TUT5a/cleaned_data.csv")
