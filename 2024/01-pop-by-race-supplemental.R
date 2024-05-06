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
formatted_export <- c("formatted", "table01")

# 2020 Decennial Data ------------------------------------

year <- (2020)
data_type <- 'P9'

raw_export <- c("raw", year, data_type)

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
p9_2020_final <- P9_2020 %>%
  group_by(name,race) %>%
  reframe(value = value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(all_of(table_order)) %>%
  slice(match(re_order, race))

# Calculate percentages
# Reference table
p9_2020_prct <- p9_2020_final %>%
  subset(race == "Total") %>%
  select(!starts_with("prct"))

# Crunch Percentages for all R/E categories. Regionwide and by county
p9_2020_final$prct_Region = p9_2020_final$Region/p9_2020_prct$Region
p9_2020_final$prct_King = p9_2020_final$King/p9_2020_prct$King
p9_2020_final$prct_Kitsap = p9_2020_final$Kitsap/p9_2020_prct$Kitsap
p9_2020_final$prct_Pierce = p9_2020_final$Pierce/p9_2020_prct$Pierce
p9_2020_final$prct_Snohomish = p9_2020_final$Snohomish/p9_2020_prct$Snohomish

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
p9_2020_final <- rbind(poc_pivot, p9_2020_final)
p9_2020_final <- p9_2020_final[c(which(p9_2020_final$race != "poc"), which(p9_2020_final$race == "poc")), ]
p9_2020_final$race <- gsub("poc", "People of Color", p9_2020_final$race)
p9_2020_final$year <- year

# Rounded estimates
columns_to_round <- grep(paste(integers_to_round_for_final_output, collapse = "|"), names(p9_2020_final), value = TRUE)
columns_to_round <- columns_to_round[!grepl("^prct_", columns_to_round)]
p9_2020_final[columns_to_round] <- lapply(p9_2020_final[columns_to_round], function(x) round(x, -2)) # Rounded to nearest 100

columns_to_truncate <- grep("^prct_", names(p9_2020_final), value = TRUE) # Columns to be truncated
p9_2020_final[columns_to_truncate] <- lapply(p9_2020_final[columns_to_truncate], function(x) trunc(x * 1000) / 1000) # Truncate to 3 decimals


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
p_2010_final <- p_2010 %>%
  group_by(name,race) %>%
  reframe(value = value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(all_of(table_order)) %>%
  slice(match(re_order, race))

# Calculate percentages
# Reference table
p_2010_prct <- p_2010_final %>%
  subset(race == "Total") %>%
  select(!starts_with("prct"))

# Crunch Percentages for all R/E categories. Regionwide and by county
p_2010_final$prct_Region = p_2010_final$Region/p_2010_prct$Region
p_2010_final$prct_King = p_2010_final$King/p_2010_prct$King
p_2010_final$prct_Kitsap = p_2010_final$Kitsap/p_2010_prct$Kitsap
p_2010_final$prct_Pierce = p_2010_final$Pierce/p_2010_prct$Pierce
p_2010_final$prct_Snohomish = p_2010_final$Snohomish/p_2010_prct$Snohomish

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
p_2010_final <- rbind(poc_pivot, p_2010_final)
p_2010_final <- p_2010_final[c(which(p_2010_final$race != "poc"), which(p_2010_final$race == "poc")), ]
p_2010_final$race <- gsub("poc", "People of Color", p_2010_final$race)
p_2010_final$year <- year

# Rounded estimates
columns_to_round <- grep(paste(integers_to_round_for_final_output, collapse = "|"), names(p_2010_final), value = TRUE)
columns_to_round <- columns_to_round[!grepl("^prct_", columns_to_round)]
p_2010_final[columns_to_round] <- lapply(p_2010_final[columns_to_round], function(x) round(x, -2)) # Rounded to nearest 100

columns_to_truncate <- grep("^prct_", names(p_2010_final), value = TRUE) # Columns to be truncated
p_2010_final[columns_to_truncate] <- lapply(p_2010_final[columns_to_truncate], function(x) trunc(x * 1000) / 1000) # Truncate to 3 decimals

# 2000 Decennial Data -------------------------------------

year <- (2000)
data_type <- 'P'

raw_export <- c("raw", year, data_type)
formatted_export <- c("formatted", year, data_type)

# Pull Data
p_2000_raw <- get_decennial_recs(geography = 'county',
                                 sumfile = 'sf1',
                                 table_codes = data_type,
                                 years = year)

# Summarize by race/ethnicity
p_2000 <- p_2000_raw %>%
  filter(concept == "HISPANIC OR LATINO BY RACE",
         !grepl("Two races including Some other race", label),
         !grepl("Two races excluding Some other race, and three or more races", label),
         !grepl("^.*!!Hispanic or Latino!!", label)) %>%
  mutate(race = case_when(grepl("^.*!White alone", label) ~ "White",
                          grepl("^.*!Black or African American alone", label) ~ "Black",
                          grepl("^.*!American Indian and Alaska Native alone", label) ~ "American Indian and Alaska Native",
                          grepl("^.*!Asian alone", label) ~ "Asian",
                          grepl("^.*!Native Hawaiian and Other Pacific Islander alone", label) ~ "Native Hawaiian and Other Pacific Islander",
                          grepl("^.*!!Some other race alone", label) ~ "Some Other Race",
                          grepl("^.*!!Two or more races", label) ~ "Two or more races",
                          grepl("Total!!Hispanic or Latino", label) ~ "Hispanic or Latinx",
                          grepl("P008001", variable) ~ "Total",
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

p_2000$name <- gsub(" County, Washington", "", p_2000$name)

# Export raw data
file_name_raw <- paste(raw_export, collapse = "_")
file_name_raw <- paste(file_name_raw, ".csv", sep = "")
write.csv(p_2000, file = file.path(export_path, file_name_raw), row.names = FALSE)

# Group by County & R/E, order
p_2000_final <- p_2000 %>%
  group_by(name,race) %>%
  reframe(value = value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  select(all_of(table_order)) %>%
  slice(match(re_order, race))

# Calculate percentages
# Reference table
p_2000_prct <- p_2000_final %>%
  subset(race == "Total") %>%
  select(!starts_with("prct"))

# Crunch Percentages for all R/E categories. Regionwide and by county
p_2000_final$prct_Region = p_2000_final$Region/p_2000_prct$Region
p_2000_final$prct_King = p_2000_final$King/p_2000_prct$King
p_2000_final$prct_Kitsap = p_2000_final$Kitsap/p_2000_prct$Kitsap
p_2000_final$prct_Pierce = p_2000_final$Pierce/p_2000_prct$Pierce
p_2000_final$prct_Snohomish = p_2000_final$Snohomish/p_2000_prct$Snohomish

# Calculate percentage POC by region/county
poc <- p_2000 %>% 
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
p_2000_final <- rbind(poc_pivot, p_2000_final)
p_2000_final <- p_2000_final[c(which(p_2000_final$race != "poc"), which(p_2000_final$race == "poc")), ]
p_2000_final$race <- gsub("poc", "People of Color", p_2000_final$race)
p_2000_final$year <- year

# Rounded estimates
columns_to_round <- grep(paste(integers_to_round_for_final_output, collapse = "|"), names(p_2000_final), value = TRUE)
columns_to_round <- columns_to_round[!grepl("^prct_", columns_to_round)]
p_2000_final[columns_to_round] <- lapply(p_2000_final[columns_to_round], function(x) round(x, -2)) # Rounded to nearest 100

columns_to_truncate <- grep("^prct_", names(p_2000_final), value = TRUE) # Columns to be truncated
p_2000_final[columns_to_truncate] <- lapply(p_2000_final[columns_to_truncate], function(x) trunc(x * 1000) / 1000) # Truncate to 3 decimals

# Combine data together -------------------------------------

table01 <- rbind(p_2000_final, p_2010_final, p9_2020_final)

# Export
file_name_formatted <- paste(formatted_export, collapse = "_")
file_name_formatted <- paste(file_name_formatted, ".csv", sep = "")
write.csv(table01, file = file.path(export_path, file_name_formatted), row.names = FALSE)