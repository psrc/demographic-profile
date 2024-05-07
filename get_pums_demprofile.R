library(magrittr)
library(psrccensus)
library(dplyr)
library(tidyr)
library(data.table)
#library(gt)

# 1. Setup: List necessary direct-PUMS variables & table names ------
dyear <- 2022
geolabels <- data.frame(raw=c("King","Kitsap","Pierce","Snohomish","Region"),
              published=c(paste(c("King","Kitsap","Pierce","Snohomish"), "County"),"Central Puget Sound")
                       )

# PUMS variables for population-scale analysis
pvars <- c(
  "AGEP",                   # Age
  "DIS",                    # Disability
  "LNGI",                   # Limited English speaking household
  "LANP",                   # Language spoken at home
  "PRACE",                  # Individual race (PSRC categories)
  "POVPIP",                 # Income-to-poverty ratio
  "RAC2P",                  # Detailed race
  "HINCP",                  # Household income
  "HHT",                    # Household type
  "VEH",                    # Household vehicles
  "ENG",                    # Ability to speak English
  "SEX"
)

# PUMS variables for household-level analysis
hvars <- c(
  "HINCP",                  # Household income
  "HRACE",                  # Household race (PSRC categories)
  "LNGI",                   # Limited English speaking household
  "NP",                     # Number of persons in household
  "HHLANP",                 # Language spoken at home
  "HHT",                    # Household type
  "POVPIP",                 # Income-to-poverty ratio
  "PRACE",                  # Householder race (PSRC categories)
  "VEH",                    # Household vehicles
  "R18",                    # Presence of persons under 18 years in household
  "R65",                    # Presence of persons over 65 years in household
  "HDIS"                    # Presence of disabled persons in household
)

asian_regex <- paste0(c("Asian Indian", "Cambodian", "Chinese", "Taiwanese", "Filipino",
                  "Japanese", "Korean", "Laotian", "Pakistani", "Thai", "Vietnamese"), collapse="|") %>% paste0("(",.,")")

# 2. Setup: Helper functions ----------------------------------------

ctyreg_pums_count <- function(so, groupvars=NULL){                                                 # Function for county + region counts
  rs      <- list()
  rs[[1]] <- psrc_pums_count(so, group_vars=groupvars, incl_na=FALSE)                              # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- psrc_pums_count(so, group_vars=c("COUNTY", groupvars), incl_na=FALSE) %>%
    .[COUNTY!="Region"]                                                                            # Remove duplicate total level
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, COUNTY)                                               # Combine county & regional results
  return(rs)
}

ctyreg_pums_median <- function(so, stat_var, groupvars=NULL){                                      # Function for county + region median
  rs      <- list()
  rs[[1]] <- psrc_pums_median(so, stat_var, groupvars, incl_na=FALSE)                              # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- psrc_pums_median(so, stat_var, group_var=c("COUNTY", groupvars), incl_na=FALSE) %>%
    .[COUNTY!="Region"]                                                                            # Remove duplicate total level
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, COUNTY)                                               # Combine county & regional results
  return(rs)
}

selectvars <- function(df){                                                                        # Drop excess fields before pivot
  selectcols <- grep("(PRACE|COUNTY|count|share|median|moe)", names(df), value=TRUE)
  df %<>% setDT() %>% .[, ..selectcols]
}

pivot_counties <- function(df){                                                                    # When counties are given, displayed horizontally
  df %<>% tidyr::pivot_wider(
            names_from = COUNTY,
            values_from = matches("(count|share|median|moe)", ignore.case=FALSE),
            names_sort = TRUE, names_vary = "slowest") #%>% gt() %>%
        # gt::tab_spanner_delim(
        #     delim="_", columns=matches("(count|share|median|moe)", ignore.case=FALSE),
        #     split="last", limit=1, reverse=TRUE) %>% rm_stubhead()
  return(df)
}

# 3. Main function --------------------------------------------------

# Generate all indicators for a single survey
get_pums_dp <- function(dyear){
  pums_rds <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"                 # Network PUMS location
  pp_df <- get_psrc_pums(5, dyear, "p", pvars, dir=pums_rds)                                  # Retrieve persons data
  hh_df <- get_psrc_pums(5, dyear, "h", hvars, dir=pums_rds)                                  # Retrieve household data

  pp_df %<>% mutate(
    poverty_100 = case_when(is.na(POVPIP) ~ NA_character_,                                    # Create additional analysis variables
                            POVPIP<100 ~ "Yes",
                            TRUE ~ "No"),
    poverty_200 = case_when(is.na(POVPIP) ~ NA_character_,
                            POVPIP<200 ~ "Yes",
                            TRUE ~ "No"),
    age_group = factor(case_when(is.na(AGEP) ~ NA_character_,
                          AGEP>64 ~ "65+",
                          AGEP<18 ~ "< 18",
                          TRUE ~ "18-64")),
    age_detail = factor(case_when(is.na(AGEP) ~ NA_character_,
                          between(AGEP,65,84) ~ "65-84",
                          between(AGEP,5,17)  ~ "5-17",
                          AGEP < 4 ~ "0-4",
                          AGEP > 84 ~ "85+",
                          TRUE ~ "18-64")),
    asian_subgrp = factor(case_when(PRACE!="Asian alone" ~ NA_character_,
                          PRACE=="Asian alone" & grepl(!!asian_regex, as.character(RAC2P)) ~ as.character(RAC2P),
                          PRACE=="Asian alone" ~ "Other Asian",
                          TRUE ~ NA_character_))
  )

  deep_pocket <- list()                                                                       # List will contain all tables
  deep_pocket$"Tbl 0 Poverty Summary" <-                                                      # Item names become export spreadsheet tabs
    ctyreg_pums_count(pp_df, "poverty_200") %>%
    filter(poverty_200=="Yes") %>% select(-any_of(contains("share", ignore.case = FALSE)))

# Table 3
  deep_pocket$"Tbl 3 LowInc Race-Hisp" <-
    ctyreg_pums_count(pp_df, c("PRACE","poverty_200")) %>%
    filter(poverty_200=="Yes") %>% select(-any_of(contains("count", ignore.case = FALSE)))

# Table 4 - Median household income
  deep_pocket$"Tbl 4a MedInc Race-Hisp" <-
    ctyreg_pums_median(hh_df, stat_var="HINCP", c("PRACE")) %>%
    rename_with(.fn= ~stringr::str_replace(., "HINCP_",""), .cols=contains("HINCP"))
  deep_pocket$"Tbl 4b Ratio to Reg MedInc" <-
    ctyreg_pums_median(pp_df, stat_var="HINCP") %>%
    inner_join(deep_pocket$"Tbl 4a", by = join_by(COUNTY)) %>%
    mutate(share_of_overall=median/HINCP_median,
           moe=tidycensus::moe_prop(median, HINCP_median, median_moe, HINCP_median_moe)) %>%
    select(-any_of(contains("median")))

# Table 5 - Asian detail
  asian_detail <- list()
  asian_detail$pop <- psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                         group_vars="asian_subgrp") %>%
    select(-any_of(contains("share")))
  asian_detail$pov100 <- psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                            group_vars=c("asian_subgrp", "poverty_100"), incl_na=FALSE) %>%
    filter(poverty_100=="Yes") %>% select(-any_of(contains("count")))
  asian_detail$pov200 <- psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                            group_vars=c("asian_subgrp", "poverty_200"), incl_na=FALSE) %>%
    filter(poverty_200=="Yes") %>% select(-any_of(contains("count")))
  asian_detail$inc <- psrc_pums_median(filter(pp_df, PRACE=="Asian alone"), "HINCP",
                            group_vars="asian_subgrp", incl_na=FALSE)
  deep_pocket$"Tbl 5 Asian Detail" <-
    Reduce(function(x, y) merge(x, y, all=TRUE), asian_detail)

# Table 6 - Poverty, Race & Sex
  deep_pocket$"Tbl 6a LowInc Race-Hisp Sex 100" <-
    psrc_pums_count(pp_df, group_vars=c("PRACE","poverty_100","SEX")) %>%
    filter(poverty_100=="Yes")
  deep_pocket$"Tbl 6b LowInc Race-Hisp Sex 200" <-
    psrc_pums_count(pp_df, group_vars=c("PRACE","poverty_200","SEX")) %>%
    filter(poverty_200=="Yes")

# Table 8 - Ages 65+
  deep_pocket$"Tbl 8a Pov65+ Summary" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), c("poverty_200")) %>%
    filter(poverty_200=="Yes")
  deep_pocket$"Tbl 8b Pov65+ Age Detail" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), c("age_detail"))
  deep_pocket$"Tbl 8c Pov65+ Race" <-
    psrc_pums_count(filter(pp_df, age_group=="65+"), group_vars=c("PRACE"))

# Table 9 - Ages < 18
  deep_pocket$"Tbl 9a Pov<18 Summary" <-
    ctyreg_pums_count(filter(pp_df, age_group=="< 18"), c("poverty_200")) %>%
    filter(poverty_200=="Yes")
  deep_pocket$"Tbl 9b Pov<18 Age Detail" <-
    ctyreg_pums_count(filter(pp_df, age_group=="< 18"), c("age_detail"))
  deep_pocket$"Tbl 9c Pov<18 Race" <-
    psrc_pums_count(filter(pp_df, age_group=="< 18"), group_vars=c("PRACE"))

# Table 10 - Disability status
  deep_pocket$"Tbl 10a PovDisab Summary" <-
    ctyreg_pums_count(filter(pp_df, grepl("^With ", as.character(DIS))), c("poverty_200")) %>%
    filter(poverty_200=="Yes")
  deep_pocket$"Tbl 10b PovDisab Age Detail" <-
    ctyreg_pums_count(filter(pp_df, grepl("^With ", as.character(DIS))), c("age_detail"))
  deep_pocket$"Tbl 10c PovDisab Race" <-
    psrc_pums_count(filter(pp_df, grepl("^With ", as.character(DIS))), group_vars=c("PRACE"))

  deep_pocket %<>% lapply(setDT)
  deep_pocket[1:4] %<>% lapply(selectvars)
  deep_pocket[c(1:4, 8:9, 11:12, 14:15)] %<>% lapply(pivot_counties)
  return(deep_pocket)
}

# Write all tables to file
write_dprofile_pums_xlsx <- function(result_list){
  openxlsx::write.xlsx(result_list, file = "outfile.xlsx",
    sheetName = names(result_list), rowNames = FALSE)
  return(invisible(NULL))
}

# Example -------------
# x <- get_pums_dp(2022)                         # Returns all tables as separate items in a list
# write_dprofile_pums_xlsx(x)                    # Write the tables to .xlsx