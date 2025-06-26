# TITLE: 2024 Demographic Profile - Table 01 - Population by Race/Ethnicity Using Census Categories
# GEOGRAPHIES: County & PSRC Region
# DATA SOURCE: ACS Data
# AUTHOR: Eric Clute

# Assumptions
library(psrccensus)
library(tidycensus)
library(tidyverse)
library(dplyr)

year <- (2023)
acs_type <- "acs1"

raw_export <- c("raw_census_categories", year, acs_type)
formatted_export <- c("table01_census_race_categories", year, acs_type)
export_path <- "Y:/Demog Profile/2025/Data/DP-Tables-ACS-2023"

table_order <- c("race",
                 "estimate_Region","moe_Region",
                 "estimate_King", "moe_King",
                 "estimate_Kitsap", "moe_Kitsap", 
                 "estimate_Pierce", "moe_Pierce", 
                 "estimate_Snohomish", "moe_Snohomish",
                 "rr_Region", "rr_King","rr_Kitsap","rr_Pierce", "rr_Snohomish")
re_order <- c("Total","Total Not Hispanic or Latinx","American Indian and Alaska Native NH", "Asian NH", "Black NH",
              "Native Hawaiian and Pacific Islander NH","White NH", "Some Other Race NH", "Two or more races NH", 
              
              "Total Hispanic or Latinx","American Indian and Alaska Native Hispanic", "Asian Hispanic", "Black Hispanic",
              "Native Hawaiian and Pacific Islander Hispanic","White Hispanic", "Some Other Race Hispanic", "Two or more races Hispanic")

# Pull Data
B03002_raw <- get_acs_recs(geography = 'county',
                           table.names = 'B03002',
                           years = year,
                           counties = c("King", "Kitsap", "Pierce", "Snohomish"),
                           acs.type = acs_type)

# Export
# file_name_raw <- paste(raw_export, collapse = "_")
# file_name_raw <- paste(file_name_raw, ".csv", sep = "")
# write.csv(B03002_raw, file = file.path(export_path, file_name_raw), row.names = FALSE)

# Summarize by race/ethnicity
B03002 <- B03002_raw %>%
  filter(!grepl("Two races including Some other race", label),
         !grepl("Two races excluding Some other race, and three or more races", label)) %>%
  mutate(race = case_when(grepl("^.*Not Hispanic or Latino:!!White alone", label) ~ "White NH",
                          grepl("^.*!Hispanic or Latino:!!White alone", label) ~ "White Hispanic",
                          
                          grepl("^.*Not Hispanic or Latino:!!Black or African American alone", label) ~ "Black NH",
                          grepl("^.*!Hispanic or Latino:!!Black or African American alone", label) ~ "Black Hispanic",
                          
                          grepl("^.*Not Hispanic or Latino:!!American Indian and Alaska Native alone", label) ~ "American Indian and Alaska Native NH",
                          grepl("^.*!Hispanic or Latino:!!American Indian and Alaska Native alone", label) ~ "American Indian and Alaska Native Hispanic",
                          
                          grepl("^.*Not Hispanic or Latino:!!Asian alone", label) ~ "Asian NH",
                          grepl("^.*!Hispanic or Latino:!!Asian alone", label) ~ "Asian Hispanic",
                          
                          grepl("^.*Not Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone", label) ~ "Native Hawaiian and Pacific Islander NH",
                          grepl("^.*!Hispanic or Latino:!!Native Hawaiian and Other Pacific Islander alone", label) ~ "Native Hawaiian and Pacific Islander Hispanic",
                          
                          grepl("^.*Not Hispanic or Latino:!!Some other race alone", label) ~ "Some Other Race NH",
                          grepl("^.*!Hispanic or Latino:!!Some other race alone", label) ~ "Some Other Race Hispanic",
                          
                          grepl("^.*Not Hispanic or Latino:!!Two or more races:", label) ~ "Two or more races NH",
                          grepl("^.*!Hispanic or Latino:!!Two or more races:", label) ~ "Two or more races Hispanic",
                          
                          grepl("Estimate!!Total:!!Not Hispanic or Latino:", label) ~ "Total Not Hispanic or Latinx",
                          grepl("Estimate!!Total:!!Hispanic or Latino:", label) ~ "Total Hispanic or Latinx",
                          grepl("B03002_001", variable) ~ "Total",
                          !is.na(label) ~ ""),
         poc = case_when(grepl("^.*Black", race) ~ "poc",
                         grepl("^.*American Indian and Alaska Native", race) ~ "poc",
                         grepl("^.*Asian", race) ~ "poc",
                         grepl("^.*Native Hawaiian and Pacific Islander", race) ~ "poc",
                         grepl("^.*White Hispanic", race) ~ "poc",
                         grepl("^.*Some Other Race", race) ~ "poc",
                         grepl("^.*Two or more races", race) ~ "poc",
                         grepl("Total$", race) ~ "Total",
                         !is.na(race) ~ "")) %>%            
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
table01$prct_Region = table01$estimate_Region/table01_prct$estimate_Region
table01$prct_King = table01$estimate_King/table01_prct$estimate_King
table01$prct_Kitsap = table01$estimate_Kitsap/table01_prct$estimate_Kitsap
table01$prct_Pierce = table01$estimate_Pierce/table01_prct$estimate_Pierce
table01$prct_Snohomish = table01$estimate_Snohomish/table01_prct$estimate_Snohomish

# Calculate percentage POC by region/county, crunch new MOE
poc <- B03002 %>%
  group_by(name, poc) %>%
  summarise(estimate=sum(estimate),
                   moe=moe_sum(estimate=estimate, moe=moe, na.rm = TRUE)) %>%
  filter(!poc == "")

poc <- poc %>%
  pivot_wider(names_from = poc, values_from = c(estimate,moe)) #%>%

poc <- poc %>%
  mutate(prct=((estimate_poc/estimate_Total))) #%>%

poc <- poc %>%
  rowwise() #%>%

poc <- poc %>%
  mutate(moe_new_total=moe_sum(estimate = estimate_poc,
                               moe = moe_poc)) %>%
  mutate(moe_perc_poc=moe_prop(num=estimate_poc,
                               denom=estimate_Total, moe_num=moe_poc, moe_denom=moe_new_total))

poc <- reliability_calcs(poc, estimate='prct',
                                  moe='moe_perc_poc')
head(poc %>% select(name, moe_perc_poc, reliability))

# Pivot
poc_pivot <- poc %>%
  select(estimate_poc, prct, moe_poc, reliability)
poc_pivot <- pivot_wider(poc_pivot, names_from = name, values_from = c(estimate_poc, prct, moe_poc, reliability))

# Clean columns
poc_pivot$race = "poc"
poc_pivot <- poc_pivot %>% select(race, everything())
names(poc_pivot) = gsub(pattern = "_poc", replacement = "", x = names(poc_pivot))
names(poc_pivot) = gsub(pattern = "reliability", replacement = "rr", x = names(poc_pivot))

# Join to Table01
table01 <- rbind(poc_pivot, table01)
table01 <- table01[c(which(table01$race != "poc"), which(table01$race == "poc")), ]
table01$race <- gsub("poc", "People of Color", table01$race)

# Rounded estimates
columns_to_round <- grep("^estimate_", names(table01), value = TRUE) # Columns to be rounded
table01[columns_to_round] <- lapply(table01[columns_to_round], function(x) round(x, -2)) # Round to nearest 100

columns_to_round <- grep("^moe_", names(table01), value = TRUE) # Columns to be rounded
table01[columns_to_round] <- lapply(table01[columns_to_round], function(x) round(x, 0)) # Round to nearest 1

columns_to_truncate <- grep("^prct_", names(table01), value = TRUE) # Columns to be truncated
table01[columns_to_truncate] <- lapply(table01[columns_to_truncate], function(x) trunc(x * 1000) / 1000) # Truncate to 3 decimals

# Export
file_name_formatted <- paste(formatted_export, collapse = "_")
file_name_formatted <- paste(file_name_formatted, ".csv", sep = "")
write.csv(table01, file = file.path(export_path, file_name_formatted), row.names = FALSE)
