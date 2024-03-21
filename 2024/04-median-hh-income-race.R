# TITLE: 2024 Demographic Profile - Table 04 - Median Income by Race
# GEOGRAPHIES: County & PSRC Region
# DATA SOURCE: ACS Data
# AUTHOR: Eric Clute

# Assumptions
library(psrccensus)
library(tidycensus)
library(tidyverse)
library(dplyr)

year <- (2022)
acs_type <- "1"

raw_export <- c("raw", year, "acs_pums", acs_type)
formatted_export <- c("formatted", year, "acs_pums", acs_type)
export_path <- "Y:/Demog Profile/2024/Data/04-median-hh-income-race"

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
pums_raw <- get_psrc_pums(acs_type,year,"h",c("PRACE","HINCP"))

# Summarize by race/ethnicity - county
pums <- pums_raw %>%
  mutate(race = case_when(grepl("White alone", PRACE) ~ "White",
                          grepl("Black or African American alone", PRACE) ~ "Black",
                          grepl("American Indian or Alaskan Native Alone", PRACE) ~ "American Indian and Alaska Native",
                          grepl("Asian alone", PRACE) ~ "Asian",
                          grepl("Native Hawaiian and Other Pacific Islander alone", PRACE) ~ "Native Hawaiian and Other Pacific Islander",
                          grepl("Some Other Race alone", PRACE) ~ "Some Other Race",
                          grepl("Two or More Races", PRACE) ~ "Two or more races",
                          grepl("Hispanic or Latino", PRACE) ~ "Hispanic or Latinx",
                          !is.na(PRACE) ~ ""),
         poc = case_when(grepl("Black", race) ~ "poc",
                         grepl("American Indian and Alaska Native", race) ~ "poc",
                         grepl("Asian", race) ~ "poc",
                         grepl("Native Hawaiian and Other Pacific Islander", race) ~ "poc",
                         grepl("Some Other Race", race) ~ "poc",
                         grepl("Two or more races", race) ~ "poc",
                         grepl("Hispanic or Latinx", race) ~ "poc",
                         !is.na(race) ~ "")) %>%            
  filter(!race == "")

incbyre <- psrc_pums_median(pums, "HINCP", group_vars = c("COUNTY", "race"),rr=TRUE)
names(incbyre)[names(incbyre) == 'COUNTY'] <- 'name'
incbyre <- incbyre %>% filter(!name == "Region")

# Summarize by race/ethnicity - region & join to county data
incbyre_region <- psrc_pums_median(pums, "HINCP", group_vars = c("race"),rr=TRUE)
names(incbyre_region)[names(incbyre_region) == 'COUNTY'] <- 'name'

incbyre <- rbind(incbyre, incbyre_region)
rm(incbyre_region)

# Group by County & R/E, order 
table04 <- incbyre %>%
  group_by(name,race) %>%
  reframe(estimate = HINCP_median,
          moe = HINCP_median_moe,
          rr = reliability) %>%
  pivot_wider(names_from = name, values_from = estimate|moe|rr) %>%
  select(all_of(table_order)) %>%
  slice(match(re_order, race))

# Calculate percentages
# Reference table
table04_prct <- table04 %>%
  subset(race == "Total") %>%
  select(starts_with("estimate"))

# Crunch Percentages for all R/E categories. Regionwide and by county
table04$prct_Region = table04$estimate_Region/table04_prct$estimate_Region
table04$prct_King = table04$estimate_King/table04_prct$estimate_King
table04$prct_Kitsap = table04$estimate_Kitsap/table04_prct$estimate_Kitsap
table04$prct_Pierce = table04$estimate_Pierce/table04_prct$estimate_Pierce
table04$prct_Snohomish = table04$estimate_Snohomish/table04_prct$estimate_Snohomish

# Calculate percentage POC by region/county, crunch new MOE
poc_raw <- pums %>% filter(!poc == "")

incpoc <- psrc_pums_median(poc_raw, "HINCP", group_vars = c("COUNTY"),rr=TRUE)
names(incpoc)[names(incpoc) == 'COUNTY'] <- 'name'

poc_pivot <- incpoc %>%
  group_by(name) %>%
  reframe(estimate = HINCP_median,
          moe = HINCP_median_moe,
          rr = reliability) %>%
  pivot_wider(names_from = name, values_from = estimate|moe|rr)

# Clean columns
poc_pivot$race = "poc"
poc_pivot <- poc_pivot %>% select(race, everything())

# Crunch Percentages for POC. Regionwide and by county
poc_pivot$prct_Region = poc_pivot$estimate_Region/table04_prct$estimate_Region
poc_pivot$prct_King = poc_pivot$estimate_King/table04_prct$estimate_King
poc_pivot$prct_Kitsap = poc_pivot$estimate_Kitsap/table04_prct$estimate_Kitsap
poc_pivot$prct_Pierce = poc_pivot$estimate_Pierce/table04_prct$estimate_Pierce
poc_pivot$prct_Snohomish = poc_pivot$estimate_Snohomish/table04_prct$estimate_Snohomish

# Join to table04
table04 <- rbind(poc_pivot, table04)
table04 <- table04[c(which(table04$race != "poc"), which(table04$race == "poc")), ]

# Rounded estimates
columns_to_round <- grep("^estimate_", names(table04), value = TRUE) # Columns to be rounded
table04[columns_to_round] <- lapply(table04[columns_to_round], function(x) round(x, -2)) # Round to nearest 100

columns_to_round <- grep("^moe_", names(table04), value = TRUE) # Columns to be rounded
table04[columns_to_round] <- lapply(table04[columns_to_round], function(x) round(x, 0)) # Round to nearest 1

columns_to_truncate <- grep("^prct_", names(table04), value = TRUE) # Columns to be truncated
table04[columns_to_truncate] <- lapply(table04[columns_to_truncate], function(x) trunc(x * 1000) / 1000) # Truncate to 3 decimals

# Export
file_name_formatted <- paste(formatted_export, collapse = "_")
file_name_formatted <- paste(file_name_formatted, ".csv", sep = "")
write.csv(table04, file = file.path(export_path, file_name_formatted), row.names = FALSE)