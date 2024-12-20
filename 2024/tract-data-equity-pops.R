# TITLE: Demographic Profile Mapping - % POC % youth (5-17) % 65+ % in poverty % low income % with a disability % LEP
# GEOGRAPHIES: Tract
# DATA SOURCE: ACS Data
# AUTHOR: Eric Clute

# Assumptions
library(psrccensus)
library(tidycensus)
library(dplyr)
library(tidyr)

year <- 2023
acs_type <- "acs5"
tables_needed <- c("DP05","DP02", "S1701")
export <- "Y:/Demog Profile/2024/Data/Maps/equity_populations_bytract-NEW.csv"

# Functions

pull_tables <- function(table){
  raw <- get_acs_recs(geography = 'tract',
                      table.names = table,
                      years = year,
                      acs.type = acs_type)}

# Pull Tables
raw <- pull_tables(tables_needed)

# DATA BY TRACT ------------
# Select data
data <- raw %>%
  filter(variable == "DP05_0001" | variable == "DP05_0024" | variable == "DP05_0024P" | # %senior
         variable == "DP05_0019" | variable == "DP05_0019P"| variable == "DP05_0005" | variable == "DP05_0005P" | # %under 18 & %under 5
         variable == "DP05_0072" | variable == "DP05_0079" | # %POC
         variable == "DP02_0071" | variable == "DP02_0072" | variable == "DP02_0072P" | # %disability
         variable == "DP02_0112" | variable == "DP02_0115" | variable == "DP02_0115P" | # %LEP
         variable == "S1701_C01_001" | variable == "S1701_C02_001" | variable == "S1701_C03_001" |  # %below 100% poverty
         variable == "S1701_C01_042" # %below 200% poverty
         ) %>%
  select(GEOID, variable, estimate, moe) %>%
  pivot_wider(names_from = variable, values_from = c(estimate, moe)) %>%
  rename(tract_pop_age_est = estimate_DP05_0001, tract_age_pop_moe = moe_DP05_0001,
         tract_pop_race_est = estimate_DP05_0072, tract_race_pop_moe = moe_DP05_0072,
         tract_pop_dis_est = estimate_DP02_0071, tract_dis_pop_moe = moe_DP02_0071,
         tract_pop_lep_est = estimate_DP02_0112, tract_lep_pop_moe = moe_DP02_0112,
         tract_pop_pov_est = estimate_S1701_C01_001, tract_pov_pop_moe = moe_S1701_C01_001,
         
         senior_pop_est = estimate_DP05_0024, senior_pop_moe = moe_DP05_0024,
         senior_prct_est = estimate_DP05_0024P,senior_prct_moe = moe_DP05_0024P,
         
         under5_pop_est = estimate_DP05_0005, under5_pop_moe = moe_DP05_0005,
         under5_prct_est = estimate_DP05_0005P,under5_prct_moe = moe_DP05_0005P,
         
         under18_pop_est = estimate_DP05_0019, under18_pop_moe = moe_DP05_0019,
         under18_prct_est = estimate_DP05_0019P,under18_prct_moe = moe_DP05_0019P,
         
         nhwhite_pop_est = estimate_DP05_0079, nhwhite_pop_moe = moe_DP05_0079,
         
         dis_pop_est = estimate_DP02_0072, dis_pop_moe = moe_DP02_0072,
         dis_prct_est = estimate_DP02_0072P, dis_prct_moe = moe_DP02_0072P,
         
         lep_pop_est = estimate_DP02_0115, lep_pop_moe = moe_DP02_0115,
         lep_prct_est = estimate_DP02_0115P, lep_prct_moe = moe_DP02_0115P,
         
         pov100_pop_est = estimate_S1701_C02_001, pov100_pop_moe = moe_S1701_C02_001,
         pov100_prct_est = estimate_S1701_C03_001, pov100_prct_moe = moe_S1701_C03_001,
         
         pov200_pop_est = estimate_S1701_C01_042, pov200_pop_moe = moe_S1701_C01_042) %>%
  mutate(poc_prct_est = 1 - nhwhite_pop_est/tract_pop_race_est,
         poc_prct_moe = tidycensus::moe_prop(nhwhite_pop_est, tract_pop_race_est,
                                             nhwhite_pop_moe, tract_race_pop_moe),
         
         youth_pop_est = under18_pop_est - under5_pop_est,
         youth_pop_moe = sqrt(under18_pop_moe^2 + under5_pop_moe^2),
         
         youth_prct_est = under18_prct_est - under5_prct_est,
         youth_prct_moe = sqrt(under18_prct_moe^2 + under5_prct_moe^2),
         
         pov200_prct_est = pov200_pop_est / tract_pop_pov_est,
         pov200_prct_moe = tidycensus::moe_prop(pov200_pop_est, tract_pop_pov_est,
                                                pov200_pop_moe, tract_pov_pop_moe)
         )

# Calculate regional values, create percentages
data <- data %>%
  mutate(reg_total_pop_age = sum(tract_pop_age_est),
         reg_total_pop_race = sum(tract_pop_race_est),
         reg_total_pop_dis = sum(tract_pop_dis_est),
         reg_total_pop_lep = sum(tract_pop_lep_est),
         reg_total_pop_pov = sum(tract_pop_pov_est),
         
         reg_pop_senior = sum(senior_pop_est),
         reg_prct_senior = (reg_pop_senior / reg_total_pop_age),
         
         reg_pop_youth = sum(youth_pop_est),
         reg_prct_youth = (reg_pop_youth / reg_total_pop_age),

         reg_pop_poc = reg_total_pop_race - (sum(nhwhite_pop_est)),
         reg_prct_poc = (reg_pop_poc / reg_total_pop_race),
         
         reg_pop_dis = sum(dis_pop_est),
         reg_prct_dis = (reg_pop_dis / reg_total_pop_dis),

         reg_pop_lep = sum(lep_pop_est),
         reg_prct_lep = (reg_pop_lep / reg_total_pop_lep),
         
         reg_pop_pov100 = sum(pov100_pop_est),
         reg_prct_pov100 = (reg_pop_pov100 / reg_total_pop_pov),
         
         reg_pop_pov200 = sum(pov200_pop_est),
         reg_prct_pov200 = (reg_pop_pov200 / reg_total_pop_pov)
         )

# Clean up percent data
data$senior_prct_est <- (data$senior_prct_est / 100)
data$senior_prct_moe <- (data$senior_prct_moe / 100)
data$under5_prct_est <- (data$under5_prct_est / 100)
data$under5_prct_moe <- (data$under5_prct_moe / 100)
data$under18_prct_est <- (data$under18_prct_est / 100)
data$under18_prct_moe <- (data$under18_prct_moe / 100)
data$youth_prct_est <- (data$youth_prct_est / 100)
data$youth_prct_moe <- (data$youth_prct_moe / 100)
data$dis_prct_est <- (data$dis_prct_est / 100)
data$dis_prct_moe <- (data$dis_prct_moe / 100)
data$lep_prct_est <- (data$lep_prct_est / 100)
data$lep_prct_moe <- (data$lep_prct_moe / 100)
data$pov100_prct_est <- (data$pov100_prct_est / 100)
data$pov100_prct_moe <- (data$pov100_prct_moe / 100)

# Extract column names that match the patterns
selected_columns <- c("GEOID")  # Initialize with non-grep columns
for (pattern in c("^senior_", "^under5_", "^under18_", "^youth_", "^nhwhite_", "^poc_", "^dis_", "^lep_", "^pov100_", "^pov200_", "^tract_", "^reg_total_", "^reg_pop_", "^reg_prct_")) {
  selected_columns <- c(selected_columns, grep(pattern, names(data), value = TRUE))
}

# Rearrange dataframe columns
data <- data[, selected_columns]

# Export
write.csv(data, export)
