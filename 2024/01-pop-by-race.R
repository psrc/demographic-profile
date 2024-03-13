# TITLE: 2024 Demographic Profile - Table 01 - Population by Race
# GEOGRAPHIES: County & PSRC Region
# DATA SOURCE: ACS Data
# AUTHOR: Eric Clute


# Assumptions
library(psrccensus)
library(tidycensus)
library(tidyverse)

year <- (2022)
acs_type <- "acs1"

raw_export <- c("raw", year, acs_type)
formatted_export <- c("formatted", year, acs_type)
export_path <- "Y:/Demog Profile/2024/Data/01-pop-by-race"

table_order <- c("race",
                 "estimate_Region","moe_Region",
                 "estimate_King", "moe_King",
                 "estimate_Kitsap", "moe_Kitsap", 
                 "estimate_Pierce", "moe_Pierce", 
                 "estimate_Snohomish", "moe_Snohomish",
                 "rr_Region", "rr_King","rr_Kitsap","rr_Pierce", "rr_Snohomish")
re_order <- c("American Indian and Alaska Native", "Asian", "Black", "Hispanic or Latinx",
              "Native Hawaiian and Other Pacific Islander","White", "Some Other Race", "Two or more races", "Total")

# Pull Data
B03002_raw <- get_acs_recs(geography = 'county',
                           table.names = 'B03002',
                           years = year,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = acs_type)

# Export
file_name_raw <- paste(raw_export, collapse = "_")
file_name_raw <- paste(file_name_raw, ".csv", sep = "")
write.csv(B03002_raw, file = file.path(export_path, file_name_raw), row.names = FALSE)

# Summarize by race/ethnicity
B03002 <- B03002_raw %>%
  filter(!grepl("Two races including Some other race", label),
         !grepl("Two races excluding Some other race, and three or more races", label),
         !grepl("!!Hispanic or Latino:!!", label)) %>%
  mutate(race = case_when(grepl("^.*!White alone", label) ~ "White",
                          grepl("^.*!Black or African American alone", label) ~ "Black",
                          grepl("^.*!American Indian and Alaska Native alone", label) ~ "American Indian and Alaska Native",
                          grepl("^.*!Asian alone", label) ~ "Asian",
                          grepl("^.*!Native Hawaiian and Other Pacific Islander alone", label) ~ "Native Hawaiian and Other Pacific Islander",
                          grepl("^.*!Some other race alone", label) ~ "Some Other Race",
                          grepl("^.*!Two or more races:", label) ~ "Two or more races",
                          grepl("Estimate!!Total:!!Hispanic or Latino:", label) ~ "Hispanic or Latinx",
                          grepl("B03002_001", variable) ~ "Total",
                          !is.na(label) ~ "")) %>%           
  filter(!race == "")

B03002$name <- gsub(" County", "", B03002$name)

# Group by County & R/E, order
table01 <- B03002 %>%
  group_by(name,race) %>%
  reframe(estimate = estimate,
          moe = moe,
          rr = reliability) %>%
  pivot_wider(names_from = name, values_from = estimate|moe|rr) %>%
  select(all_of(table_order)) %>%
  slice(match(re_order, race))

# Calculate percentages
# Reference table
table01_prct <- table01 %>%
  subset(race == "Total") %>%
  select(starts_with("estimate"))

# Crunch Percentages for all R/E categories. Regionwide and by county
table01$prct_region = table01$estimate_Region/table01_prct$estimate_Region
table01$prct_king = table01$estimate_King/table01_prct$estimate_King
table01$prct_kitsap = table01$estimate_Kitsap/table01_prct$estimate_Kitsap
table01$prct_pierce = table01$estimate_Pierce/table01_prct$estimate_Pierce
table01$prct_snohomish = table01$estimate_Snohomish/table01_prct$estimate_Snohomish

# Calculate percentage POC by region/county


# Export
file_name_formatted <- paste(formatted_export, collapse = "_")
file_name_formatted <- paste(file_name_formatted, ".csv", sep = "")
write.csv(table01, file = file.path(export_path, file_name_formatted), row.names = FALSE)