# TITLE: 2024 Demographic Profile - Table 01 Supplemental - Population by Race
# GEOGRAPHIES: County & PSRC Region
# DATA SOURCE: 2000, 2010, 2020 Decennial Data
# AUTHOR: Eric Clute

# Assumptions
library(psrccensus)
library(tidycensus)
library(tidyverse)
library(dplyr)

integers_to_round_for_final_output <- c("King", "Kitsap", "Pierce", "Snohomish", "Region")
table_order <- c("race", "Region", "King", "Kitsap", "Pierce", "Snohomish")
re_order <- c("American Indian and Alaska Native", "Asian", "Black", "Hispanic or Latinx",
              "Native Hawaiian and Other Pacific Islander","White", "Some Other Race", "Two or more races", "Total")
export_path <- "Y:/Demog Profile/2024/Data/01-pop-by-race"

# 2020 Decennial Data ------------------------------------

year <- (2020)
data_type <- 'P9'

raw_export <- c("raw", year, data_type)
formatted_export <- c("formatted", year, data_type)

# Pull Data
P9_2020_raw <- get_decennial_recs(geography = 'county',
                             sumfile = 'dhc',
                             table_codes = data_type,
                             years = year)

# Summarize by race/ethnicity
P9_2020 <- P9_2020_raw %>%
  filter(!grepl("Two races including Some other race", label),
         !grepl("Two races excluding Some other race, and three or more races", label),
         !grepl("!!Hispanic or Latino:!!", label)) %>%
  mutate(race = case_when(grepl("^.*!White alone", label) ~ "White",
                          grepl("^.*!Black or African American alone", label) ~ "Black",
                          grepl("^.*!American Indian and Alaska Native alone", label) ~ "American Indian and Alaska Native",
                          grepl("^.*!Asian alone", label) ~ "Asian",
                          grepl("^.*!Native Hawaiian and Other Pacific Islander alone", label) ~ "Native Hawaiian and Other Pacific Islander",
                          grepl("^.*!!Some Other Race alone", label) ~ "Some Other Race",
                          grepl("^.*!!Population of two or more races:$", label) ~ "Two or more races",
                          grepl("!!Total:!!Hispanic or Latino", label) ~ "Hispanic or Latinx",
                          grepl("P9_001N", variable) ~ "Total",
                          !is.na(label) ~ ""),
         poc = case_when(grepl("Black", race) ~ "poc",
                         grepl("American Indian and Alaska Native", race) ~ "poc",
                         grepl("Asian", race) ~ "poc",
                         grepl("Native Hawaiian and Other Pacific Islander", race) ~ "poc",
                         grepl("Some Other Race", race) ~ "poc",
                         grepl("Two or more races", race) ~ "poc",
                         grepl("Hispanic or Latinx", race) ~ "poc",
                         grepl("Total", race) ~ "Total",
                         !is.na(race) ~ "")) %>%            
  filter(!race == "") %>%
  rename(name = NAME)

P9_2020$name <- gsub(" County, Washington", "", P9_2020$name)

# Export raw data
file_name_raw <- paste(raw_export, collapse = "_")
file_name_raw <- paste(file_name_raw, ".csv", sep = "")
write.csv(P9_2020, file = file.path(export_path, file_name_raw), row.names = FALSE)

# Group by County & R/E, order
table01 <- P9_2020 %>%
  group_by(name,race) %>%
  reframe(value = value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(all_of(table_order)) %>%
  slice(match(re_order, race))

# Calculate percentages
# Reference table
table01_prct <- table01 %>%
  subset(race == "Total") %>%
  select(!starts_with("prct"))

# Crunch Percentages for all R/E categories. Regionwide and by county
table01$prct_Region = table01$Region/table01_prct$Region
table01$prct_King = table01$King/table01_prct$King
table01$prct_Kitsap = table01$Kitsap/table01_prct$Kitsap
table01$prct_Pierce = table01$Pierce/table01_prct$Pierce
table01$prct_Snohomish = table01$Snohomish/table01_prct$Snohomish

# Calculate percentage POC by region/county
poc <- P9_2020 %>% 
  group_by(name, poc) %>%
  summarise(value=sum(value)) %>%
  filter(!poc == "")

poc <- poc %>%   
  pivot_wider(names_from = poc, values_from = value) %>%
  mutate(prct=(poc/Total))

# Pivot
poc <- poc %>%
  select(-c(Total))
poc_pivot <- pivot_wider(poc, names_from = name, values_from = c(poc, prct))

# Clean columns
poc_pivot$race = "poc"
poc_pivot <- poc_pivot %>% select(race, everything())
names(poc_pivot) = gsub(pattern = "poc_", replacement = "", x = names(poc_pivot))

# Join to Table01
table01 <- rbind(poc_pivot, table01)
table01 <- table01[c(which(table01$race != "poc"), which(table01$race == "poc")), ]
table01$race <- gsub("poc", "People of Color", table01$race)
table01$year <- year

# Rounded estimates
columns_to_round <- grep(paste(integers_to_round_for_final_output, collapse = "|"), names(table01), value = TRUE)
table01[columns_to_round] <- lapply(table01[columns_to_round], function(x) round(x, -2)) # Rounded to nearest 100

columns_to_truncate <- grep("^prct_", names(table01), value = TRUE) # Columns to be truncated
table01[columns_to_truncate] <- lapply(table01[columns_to_truncate], function(x) trunc(x * 1000) / 1000) # Truncate to 3 decimals

# Export
file_name_formatted <- paste(formatted_export, collapse = "_")
file_name_formatted <- paste(file_name_formatted, ".csv", sep = "")
write.csv(table01, file = file.path(export_path, file_name_formatted), row.names = FALSE)

# 2010 Decennial Data -------------------------------------

year <- (2010)
data_type <- 'P'

raw_export <- c("raw", year, data_type)
formatted_export <- c("formatted", year, data_type)

# Pull Data
p_2010_raw <- get_decennial_recs(geography = 'county',
                                 sumfile = 'sf1',
                                 table_codes = data_type,
                                 years = year)

# Summarize by race/ethnicity
p_2010 <- p_2010_raw %>%
  filter(concept == "HISPANIC OR LATINO ORIGIN BY RACE",
         !grepl("Two races including Some other race", label),
         !grepl("Two races excluding Some other race, and three or more races", label),
         !grepl("^.*!!Hispanic or Latino!!", label)) %>%
  mutate(race = case_when(grepl("^.*!White alone", label) ~ "White",
                          grepl("^.*!Black or African American alone", label) ~ "Black",
                          grepl("^.*!American Indian and Alaska Native alone", label) ~ "American Indian and Alaska Native",
                          grepl("^.*!Asian alone", label) ~ "Asian",
                          grepl("^.*!Native Hawaiian and Other Pacific Islander alone", label) ~ "Native Hawaiian and Other Pacific Islander",
                          grepl("^.*!!Some Other Race alone", label) ~ "Some Other Race",
                          grepl("^.*!!Two or More Races", label) ~ "Two or more races",
                          grepl("Total!!Hispanic or Latino", label) ~ "Hispanic or Latinx",
                          grepl("P005001", variable) ~ "Total",
                          !is.na(label) ~ ""),
         poc = case_when(grepl("Black", race) ~ "poc",
                         grepl("American Indian and Alaska Native", race) ~ "poc",
                         grepl("Asian", race) ~ "poc",
                         grepl("Native Hawaiian and Other Pacific Islander", race) ~ "poc",
                         grepl("Some Other Race", race) ~ "poc",
                         grepl("Two or more races", race) ~ "poc",
                         grepl("Hispanic or Latinx", race) ~ "poc",
                         grepl("Total", race) ~ "Total",
                         !is.na(race) ~ "")) %>%            
  filter(!race == "") %>%
  rename(name = NAME)

p_2010$name <- gsub(" County, Washington", "", p_2010$name)

# Export raw data
file_name_raw <- paste(raw_export, collapse = "_")
file_name_raw <- paste(file_name_raw, ".csv", sep = "")
write.csv(p_2010, file = file.path(export_path, file_name_raw), row.names = FALSE)

# Group by County & R/E, order
table01 <- p_2010 %>%
  group_by(name,race) %>%
  reframe(value = value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(all_of(table_order)) %>%
  slice(match(re_order, race))

# Calculate percentages
# Reference table
table01_prct <- table01 %>%
  subset(race == "Total") %>%
  select(!starts_with("prct"))

# Crunch Percentages for all R/E categories. Regionwide and by county
table01$prct_Region = table01$Region/table01_prct$Region
table01$prct_King = table01$King/table01_prct$King
table01$prct_Kitsap = table01$Kitsap/table01_prct$Kitsap
table01$prct_Pierce = table01$Pierce/table01_prct$Pierce
table01$prct_Snohomish = table01$Snohomish/table01_prct$Snohomish

# Calculate percentage POC by region/county
poc <- p_2010 %>% 
  group_by(name, poc) %>%
  summarise(value=sum(value)) %>%
  filter(!poc == "")

poc <- poc %>%   
  pivot_wider(names_from = poc, values_from = value) %>%
  mutate(prct=(poc/Total))

# Pivot
poc <- poc %>%
  select(-c(Total))
poc_pivot <- pivot_wider(poc, names_from = name, values_from = c(poc, prct))

# Clean columns
poc_pivot$race = "poc"
poc_pivot <- poc_pivot %>% select(race, everything())
names(poc_pivot) = gsub(pattern = "poc_", replacement = "", x = names(poc_pivot))

# Join to Table01
table01 <- rbind(poc_pivot, table01)
table01 <- table01[c(which(table01$race != "poc"), which(table01$race == "poc")), ]
table01$race <- gsub("poc", "People of Color", table01$race)
table01$year <- year

# Rounded estimates
columns_to_round <- grep(paste(integers_to_round_for_final_output, collapse = "|"), names(table01), value = TRUE)
table01[columns_to_round] <- lapply(table01[columns_to_round], function(x) round(x, -2)) # Rounded to nearest 100

columns_to_truncate <- grep("^prct_", names(table01), value = TRUE) # Columns to be truncated
table01[columns_to_truncate] <- lapply(table01[columns_to_truncate], function(x) trunc(x * 1000) / 1000) # Truncate to 3 decimals
