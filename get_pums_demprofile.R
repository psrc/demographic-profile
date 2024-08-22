library(magrittr)
library(psrccensus)
library(dplyr)
library(tidyr)
library(data.table)

# 1. Setup: List necessary direct-PUMS variables & table names ------
geolabels <- data.frame(raw=c("King","Kitsap","Pierce","Snohomish","Region"),
          published=c(paste(c("King","Kitsap","Pierce","Snohomish"), "County"),"Central Puget Sound"))

# PUMS variables for population-scale analysis
pvars <- c(
  "AGEP",                   # Age
  "DIS",                    # Disability
  "LANP",                   # Language spoken at home
  "PRACE",                  # Individual race (PSRC categories)
  "HRACE",                  # Household race (PSRC categories)
  "POVPIP",                 # Income-to-poverty ratio
  "RAC2P",                  # Detailed race
  "HINCP",                  # Household income
  "ENG",                    # Ability to speak English
  "LANX",                   # Language spoken at home
  "SEX"
)

# PUMS variables for household-level analysis
hvars <- c(
  "HINCP",                  # Household income
  "PRACE",                  # Respondent race (PSRC categories)
  "HRACE",                  # Household race (PSRC categories)
  "RAC2P",                  # Detailed race
  "NP",                     # Number of persons in household
  "HHT",                    # Household type
  "POVPIP",                 # Income-to-poverty ratio
  "VEH",                    # Household vehicles
  "R18",                    # Presence of persons under 18 years in household
  "R65",                    # Presence of persons over 65 years in household
  "HDIS",                   # Presence of disabled persons in household
  "AGEP",                   # Respondent Age
  "DIS",                    # Respondent Disability
  "LNGI"                    # Household English proficiency
)

asian_regex <- paste0(
  c("Asian Indian", "Cambodian", "Chinese", "Taiwanese", "Filipino",
    "Japanese", "Korean", "Laotian", "Pakistani", "Thai", "Vietnamese"),
  collapse="|") %>% paste0("(",.,")")

# 2. Setup: Helper functions ----------------------------------------

add_shared_vars <- function(df){
  df %<>% mutate(
     poverty_100 = case_when(is.na(POVPIP) ~ NA_character_,
                             POVPIP<100 ~ "Yes",
                             TRUE ~ "No"),
     poverty_200 = case_when(is.na(POVPIP) ~ NA_character_,
                             POVPIP<200 ~ "Yes",
                             TRUE ~ "No"),
     age_group = factor(
       case_when(is.na(AGEP) ~ NA_character_,
                 AGEP>64 ~ "65+",
                 AGEP<18 ~ "< 18",
                 TRUE ~ "18-64")),
     age_detail = factor(
       case_when(is.na(AGEP) ~ NA_character_,
                 between(AGEP,65,84) ~ "65-84",
                 between(AGEP,5,17)  ~ "5-17",
                 AGEP < 4 ~ "0-4",
                 AGEP > 84 ~ "85+",
                 TRUE ~ "18-64")),
     asian_subgrp = factor(
       case_when(PRACE!="Asian alone" ~ NA_character_,
                 PRACE=="Asian alone" &
                   grepl(!!asian_regex, as.character(RAC2P)) ~ as.character(RAC2P),
                 PRACE=="Asian alone" &
                   grepl("^All combinations", as.character(RAC2P)) ~ "Two or more Asian",
                 PRACE=="Asian alone" ~ "Other Asian",
                 TRUE ~ NA_character_)))
}

add_pp_vars <- function(df){
  df %<>% mutate(
    lep = factor(
      case_when(AGEP<5 ~ NA_character_,
                !stringr::str_detect(ENG, "^Very") ~"Speak English less than 'very well'",
                TRUE ~ "Speak English 'very well'")),
    eng_only=factor(
      case_when(is.na(LANP) ~ "Speak English 'very well'",
                TRUE ~ "Speak English less than 'very well'")))
}

add_hh_vars <- function(df){
  df %<>% mutate(
    zero_veh=factor(
      case_when(grepl("^No ", as.character(VEH)) ~"Yes",
                grepl("^\\d ", as.character(VEH)) ~"No",
                is.na(VEH) ~NA_character_)),
    hh_type=stringr::str_extract(HHT, "^.*e household\\w{0,2}"))
}

ctyreg_pums_count <- function(so, groupvars=NULL){                                                 # Function for county + region counts
  rs      <- list()
  rs[[1]] <- psrc_pums_count(so, group_vars=groupvars, incl_na=FALSE, rr="cv")                     # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- psrc_pums_count(so, group_vars=c("COUNTY", groupvars), incl_na=FALSE, rr="cv") %>%
    filter(COUNTY!="Region")                                                                       # Remove duplicate total level
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, COUNTY)                                               # Combine county & regional results
  return(rs)
}

ctyreg_pums_median <- function(so, stat_var, groupvars=NULL){                                      # Function for county + region median
  rs      <- list()
  rs[[1]] <- psrc_pums_median(so, stat_var, groupvars, incl_na=FALSE, rr="cv")                     # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- psrc_pums_median(so, stat_var, group_var=c("COUNTY", groupvars), incl_na=FALSE, rr="cv") %>%
    filter(COUNTY!="Region")                                                                       # Remove duplicate total level
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, COUNTY)                                               # Combine county & regional results
  return(rs)
}

format_county_stats <- function(df, group_vars=NULL, metric="share"){
  rdigits <- if(metric=="share"){3}else{0}
  if(is.na(group_vars)|is.null(group_vars)){group_vars <- NULL
  }else if(grepl(";", group_vars)){group_vars %<>% stringr::str_split_1(";")}
  selectcols <- c("COUNTY", group_vars, "value", "moe")
  dt <- copy(df) %>% setDT() %>%
    .[, which(grepl(metric, colnames(.))):=lapply(.SD, round, rdigits),
      .SDcols=grepl(metric, colnames(.))] %>%
    .[, `:=`(value=get(metric), moe=get(paste0(metric,"_moe")))] %>%
    .[, `:=`(COUNTY=factor(case_when(COUNTY!="Region" ~ paste(COUNTY, "County"),
                                     COUNTY=="Region" ~ "Central Puget Sound")),
             value=case_when(((moe/1.645)/value) > 0.5 ~ paste("††", value),
                             ((moe/1.645)/value) > 0.3 ~ paste("**", value),
                             TRUE ~ as.character(value)))] %>%
    .[, ..selectcols] %>% setcolorder(c(group_vars, "COUNTY")) %>%
    tidyr::pivot_wider(
    names_from = COUNTY,
    values_from = matches("(value|moe)"),
    names_sort = TRUE, names_vary = "slowest")
  return(dt)
}

filter_poverty_level <- function(df, povlevel){
  pov_var <- paste0("poverty_", povlevel)
  dt <- copy(df) %>% setDT %>% .[(get(pov_var))=="Yes"] %>%
    .[,(pov_var):=NULL]
  return(dt)
}

combine_tbl_elements <- function(rgx, group_varlist=NULL, metric_list){
  x <- list()
  x <- Map(format_county_stats,
           copy(xtrastats[grep(rgx, names(xtrastats))]),
           group_vars=group_varlist, metric=metric_list) %>%
    rbindlist(fill=TRUE, idcol="Stub")
  setcolorder(x, grep("(value|moe)", colnames(x)), after=ncol(x))
  return(x)
}

# 3. Main function --------------------------------------------------

# Generate all indicators for a single survey
get_pums_dp <- function(dyear){

  pums_rds <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"                 # Network PUMS location
  pp_df <- get_psrc_pums(5, dyear, "p", pvars, dir=pums_rds)                                  # Retrieve persons data
  hh_df <- get_psrc_pums(5, dyear, "h", hvars, dir=pums_rds)                                  # Retrieve household data

  pp_df %<>% add_shared_vars() %>% add_pp_vars()
  hh_df %<>% add_shared_vars() %>% add_hh_vars()

  xtrastats <- list()                                                                         # List will contain all tables

  xtrastats$"Tbl 0 Total Count" <-
    ctyreg_pums_count(pp_df, "poverty_100") %>% .[poverty_100=="Total"] %>%
    .[, `:=`(income="Total", DATA_YEAR=NULL)] %>%
    setcolorder("income")
  xtrastats$"Tbl 0 Poverty Share" <-
    ctyreg_pums_count(pp_df, "poverty_100") %>%  filter_poverty_level(100)
  xtrastats$"Tbl 0 LowInc Share" <-
    ctyreg_pums_count(pp_df, "poverty_200") %>% filter_poverty_level(200)

  # Table 3                                                                                     # Item names become export spreadsheet tabs
  xtrastats$"Tbl 3 Total Count" <- copy(xtrastats$"Tbl 0 Total Count")
  xtrastats$"Tbl 3 Poverty Share" <- copy(xtrastats$"Tbl 0 Poverty Share")
  xtrastats$"Tbl 3 Pov Race-Hisp" <-
    ctyreg_pums_count(pp_df, c("PRACE","poverty_100")) %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 3 Pov POC" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone"), "poverty_100") %>%
    filter_poverty_level(100) %>% mutate(PRACE="POC total")
  xtrastats$"Tbl 3 LowInc Share" <- copy(xtrastats$"Tbl 0 LowInc Share")
  xtrastats$"Tbl 3 LowInc Race-Hisp" <-
    ctyreg_pums_count(pp_df, c("PRACE","poverty_200")) %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 3 LowInc POC" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone"), "poverty_200") %>%
    filter_poverty_level(200) %>% mutate(PRACE="POC total")

# Table 4a - Median HH income HRACE
  MedInc_POC_HRACE <-
    ctyreg_pums_median(filter(hh_df, HRACE!="White alone"), stat_var="HINCP") %>%
    rename_with(.fn= ~stringr::str_replace(., "HINCP_",""), .cols=contains("HINCP")) %>%
    mutate(HRACE="POC total - Household")
  xtrastats$"Tbl 4a MedInc Race-Hisp" <-
    ctyreg_pums_median(hh_df, stat_var="HINCP", c("HRACE")) %>%
    rename_with(.fn= ~stringr::str_replace(., "HINCP_",""), .cols=contains("HINCP")) %>%
    rbind(MedInc_POC_HRACE)
  xtrastats$"Tbl 4a Ratio to Reg MedInc" <-
    ctyreg_pums_median(hh_df, stat_var="HINCP") %>%
    inner_join(select(xtrastats$"Tbl 4a MedInc Race-Hisp",-cv), by = join_by(COUNTY,DATA_YEAR)) %>%
    mutate(share=median/HINCP_median,
           share_moe=tidycensus::moe_prop(median, HINCP_median, median_moe, HINCP_median_moe)) %>%
    select(-any_of(contains("median")))

# Table 4b - Median HH income PRACE
  MedInc_POC_PRACE <-
    ctyreg_pums_median(filter(hh_df, PRACE!="White alone"), stat_var="HINCP") %>%
    rename_with(.fn= ~stringr::str_replace(., "HINCP_",""), .cols=contains("HINCP")) %>%
    mutate(PRACE="POC total - Respondent")
  xtrastats$"Tbl 4b MedInc Race-Hisp" <-
    ctyreg_pums_median(hh_df, stat_var="HINCP", c("PRACE")) %>%
    rename_with(.fn= ~stringr::str_replace(., "HINCP_",""), .cols=contains("HINCP")) %>%
    rbind(MedInc_POC_PRACE)
  xtrastats$"Tbl 4b Ratio to Reg MedInc" <-
    ctyreg_pums_median(hh_df, stat_var="HINCP") %>%
    inner_join(select(xtrastats$"Tbl 4b MedInc Race-Hisp",-cv), by = join_by(COUNTY,DATA_YEAR)) %>%
    mutate(share=median/HINCP_median,
           share_moe=tidycensus::moe_prop(median, HINCP_median, median_moe, HINCP_median_moe)) %>%
    select(-any_of(contains("median")))

# Table 5 - Asian detail
  asian <- list()
  asian$pop <-
    psrc_pums_count(filter(pp_df, PRACE=="Asian alone"), rr="cv", incl_na=FALSE) %>%
    select(-any_of(contains("share")))
  asian$pov100 <-
    psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                    group_vars="poverty_100", incl_na=FALSE, rr="cv") %>%
    filter(poverty_100=="Yes") %>%
    select(-c(poverty_100, count, count_moe)) %>%
    rename_at(vars(matches("share|moe|cv")), ~ paste0("pov_", .x, recycle0 = TRUE))
  asian$pov200 <-
    psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                    group_vars="poverty_200", incl_na=FALSE, rr="cv") %>%
    filter(poverty_200=="Yes") %>%
    select(-c(poverty_200, count, count_moe)) %>%
    rename_at(vars(matches("share|moe|cv")), ~ paste0("lowinc_", .x, recycle0 = TRUE))
  asian$medinc_HRACE <-
    psrc_pums_median(filter(hh_df, HRACE=="Asian alone"), "HINCP",
                     incl_na=FALSE, rr="cv") %>%
    rename(HINCP_HRACE_median=HINCP_median) %>%
    rename(HINCP_median_cv=cv)
  asian$medinc_PRACE <-
    psrc_pums_median(filter(hh_df, PRACE=="Asian alone"), "HINCP",
                     incl_na=FALSE, rr="cv") %>%
    rename(HINCP_PRACE_median=HINCP_median) %>%
    rename(HINCP_median_cv=cv)
  asians <- Reduce(function(x, y) merge(x, y, by=c("DATA_YEAR","COUNTY")), asian) %>%
    select(-c("DATA_YEAR","COUNTY")) %>% mutate(asian_subgrp="Total: All Asians")

  asian_detail <- list()
  asian_detail$pop <-
    psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                    group_vars="asian_subgrp", incl_na=FALSE, rr="cv") %>%
    select(-any_of(contains("share")))
  asian_detail$pov100 <-
    psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                    group_vars=c("asian_subgrp", "poverty_100"),
                    incl_na=FALSE, rr="cv") %>%
    filter(poverty_100=="Yes") %>%
    select(-c(poverty_100, count, count_moe)) %>%
    rename_at(vars(matches("share|moe|cv")), ~ paste0("pov_", .x, recycle0 = TRUE))
  asian_detail$pov200 <-
    psrc_pums_count(filter(pp_df, PRACE=="Asian alone"),
                    group_vars=c("asian_subgrp", "poverty_200"), incl_na=FALSE, rr="cv") %>%
    filter(poverty_200=="Yes") %>%
    select(-c(poverty_200, count, count_moe)) %>%
    rename_at(vars(matches("share|moe|cv")), ~ paste0("lowinc_", .x, recycle0 = TRUE))
  asian_detail$medinc_HRACE <-
    psrc_pums_median(filter(hh_df, HRACE=="Asian alone"), stat_var="HINCP",
                     group_vars="asian_subgrp", incl_na=FALSE, rr="cv") %>%
    rename(HINCP_HRACE_median=HINCP_median) %>%
    rename(HINCP_median_cv=cv)
  asian_detail$medinc_PRACE <-
    psrc_pums_median(filter(hh_df, PRACE=="Asian alone"), "HINCP",
                     group_vars="asian_subgrp", incl_na=FALSE, rr="cv") %>%
    rename(HINCP_PRACE_median=HINCP_median) %>%
    rename(HINCP_median_cv=cv)
  asian_details <- Reduce(function(x, y) merge(x, y, by="asian_subgrp"),
         lapply(asian_detail, select, -c("DATA_YEAR","COUNTY")))
  xtrastats$"Tbl 5 Asian Detail" <- rbind(asians, asian_details) %>% relocate(asian_subgrp)

# Table 6 - Poverty, Race & Sex
  xtrastats$"Tbl 6 Poverty Sex" <-
    ctyreg_pums_count(pp_df, c("SEX","poverty_100")) %>%  filter_poverty_level(100)
  xtrastats$"Tbl 6 Pov Race-Hisp Sex 100" <-
    psrc_pums_count(pp_df, group_vars=c("PRACE","SEX","poverty_100"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 6 Pov POC Sex 100" <-
    psrc_pums_count(filter(pp_df, PRACE!="White alone"), group_vars=c("SEX","poverty_100"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(100) %>% mutate(PRACE="POC Total")
  xtrastats$"Tbl 6 LowInc Sex" <-
    ctyreg_pums_count(pp_df, c("SEX","poverty_200")) %>% filter_poverty_level(200)
  xtrastats$"Tbl 6 LowInc Race-Hisp Sex 200" <-
    psrc_pums_count(pp_df, group_vars=c("PRACE","SEX","poverty_200"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 6 LowInc POC Sex 200" <-
    psrc_pums_count(filter(pp_df, PRACE!="White alone"), group_vars=c("SEX","poverty_200"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(200) %>% mutate(PRACE="POC Total")

# Table 7 - Household Type
  xtrastats$"Tbl 7 HH POC count" <-
    ctyreg_pums_count(filter(hh_df, PRACE!="White alone")) %>%
    select(-any_of(contains("share", ignore.case = FALSE)))
  xtrastats$"Tbl 7 HH Type Summary" <-
    ctyreg_pums_count(hh_df, "hh_type") %>%
    select(-any_of(contains("share", ignore.case = FALSE)))
  xtrastats$"Tbl 7 HH Type Pov100" <-
    ctyreg_pums_count(hh_df, c("hh_type","poverty_100")) %>%
    filter(poverty_100=="Yes")
  xtrastats$"Tbl 7 HH Type Pov100 HPOC" <-
    ctyreg_pums_count(filter(hh_df, HRACE!="White alone"), c("hh_type","poverty_100")) %>%
    filter(poverty_100=="Yes")
  xtrastats$"Tbl 7 HH Type Pov100 PPOC" <-
    ctyreg_pums_count(filter(hh_df, PRACE!="White alone"), c("hh_type","poverty_100")) %>%
    filter(poverty_100=="Yes")
  xtrastats$"Tbl 7 HH Type Pov200" <-
    ctyreg_pums_count(hh_df, c("hh_type","poverty_200")) %>%
    filter(poverty_200=="Yes")
  xtrastats$"Tbl 7 HH Type Pov200 HPOC" <-
    ctyreg_pums_count(filter(hh_df, HRACE!="White alone"), c("hh_type","poverty_200")) %>%
    filter(poverty_200=="Yes")
  xtrastats$"Tbl 7 HH Type Pov200 PPOC" <-
    ctyreg_pums_count(filter(hh_df, PRACE!="White alone"), c("hh_type","poverty_200")) %>%
    filter(poverty_200=="Yes")
  xtrastats$"Tbl 7 HH Type Med Inc" <-
    ctyreg_pums_median(hh_df, "HINCP", "hh_type")
  xtrastats$"Tbl 7 HH Type Med Inc HPOC" <-
    ctyreg_pums_median(filter(hh_df, HRACE!="White alone"), "HINCP", "hh_type")
  xtrastats$"Tbl 7 HH Type Med Inc PPOC" <-
    ctyreg_pums_median(filter(hh_df, PRACE!="White alone"), "HINCP", "hh_type")

# Table 8 - Ages 65+
  xtrastats$"Tbl 8 Total Count" <- copy(xtrastats$"Tbl 0 Total Count")
  xtrastats$"Tbl 8 Poverty Share" <- copy(xtrastats$"Tbl 0 Poverty Share")
  xtrastats$"Tbl 8 Pov65+ Summary" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), "poverty_100") %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 8 Pov65+ Age Detail" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), c("age_detail", "poverty_100")) %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 8 Pov65+ Race" <-
    psrc_pums_count(filter(pp_df, age_group=="65+"), group_vars=c("PRACE","poverty_100"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 8 Pov POC 65+" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone" & age_group=="65+"), "poverty_100") %>%
    filter_poverty_level(100) %>% mutate(PRACE="POC total")
  xtrastats$"Tbl 8 LowInc Share" <- copy(xtrastats$"Tbl 0 LowInc Share")
  xtrastats$"Tbl 8 LowInc65+ Summary" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), "poverty_200") %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 8 LowInc65+ Age Detail" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), c("age_detail", "poverty_200")) %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 8 LowInc65+ Race" <-
    psrc_pums_count(filter(pp_df, age_group=="65+"), group_vars=c("PRACE","poverty_200"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 8 LowInc POC 65+" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone" & age_group=="65+"), "poverty_200") %>%
    filter_poverty_level(200) %>% mutate(PRACE="POC total")

# Table 9 - Ages < 18
  xtrastats$"Tbl 9 Total Count" <- copy(xtrastats$"Tbl 0 Total Count")
  xtrastats$"Tbl 9 Poverty Share" <- copy(xtrastats$"Tbl 0 Poverty Share")
  xtrastats$"Tbl 9 Pov<18 Summary" <-
    ctyreg_pums_count(filter(pp_df, age_group=="< 18"), c("poverty_100")) %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 9 Pov<18 Age Detail" <-
    ctyreg_pums_count(filter(pp_df, age_group=="< 18"), c("age_detail","poverty_100")) %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 9 Pov<18 Race" <-
    psrc_pums_count(filter(pp_df, age_group=="< 18"), group_vars=c("PRACE","poverty_100"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 9 Pov <18 POC" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone" & age_group=="< 18"), "poverty_100") %>%
    filter_poverty_level(100) %>% mutate(PRACE="POC total")
  xtrastats$"Tbl 9 LowInc Share" <- copy(xtrastats$"Tbl 0 LowInc Share")
  xtrastats$"Tbl 9 Lowinc<18 Summary" <-
    ctyreg_pums_count(filter(pp_df, age_group=="< 18"), c("poverty_200")) %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 9 Lowinc<18 Age Detail" <-
    ctyreg_pums_count(filter(pp_df, age_group=="< 18"), c("age_detail","poverty_200")) %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 9 Lowinc<18 Race" <-
    psrc_pums_count(filter(pp_df, age_group=="< 18"), group_vars=c("PRACE","poverty_200"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 9 LowInc <18 POC" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone" & age_group=="< 18"), "poverty_200") %>%
    filter_poverty_level(200) %>% mutate(PRACE="POC total")

# Table 10 - Disability status
  xtrastats$"Tbl 10 Total Count" <- copy(xtrastats$"Tbl 0 Total Count")
  xtrastats$"Tbl 10 Poverty Share" <- copy(xtrastats$"Tbl 0 Poverty Share")
  xtrastats$"Tbl 10 PovDisab Summary" <-
    ctyreg_pums_count(filter(pp_df, grepl("^With ", as.character(DIS))), c("poverty_100")) %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 10 PovDisab Age 5-17" <-
    ctyreg_pums_count(filter(pp_df, age_detail=="5-17"), c("DIS","poverty_100")) %>%
    filter_poverty_level(100) %>% filter(grepl("^With ", as.character(DIS)))
  xtrastats$"Tbl 10 PovDisab Age 65+" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), c("DIS","poverty_100")) %>%
    filter_poverty_level(100) %>% filter(grepl("^With ", as.character(DIS)))
  xtrastats$"Tbl 10 PovDisab Race" <-
    psrc_pums_count(filter(pp_df, grepl("^With ", as.character(DIS))), group_vars=c("PRACE","poverty_100"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(100)
  xtrastats$"Tbl 10 Pov DIS POC" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone" & grepl("^With ", as.character(DIS))), "poverty_100") %>%
    filter_poverty_level(100) %>% mutate(PRACE="POC total")
  xtrastats$"Tbl 10 LowInc Share" <- copy(xtrastats$"Tbl 0 LowInc Share")
  xtrastats$"Tbl 10 LowincDisab Summary" <-
    ctyreg_pums_count(filter(pp_df, grepl("^With ", as.character(DIS))), c("poverty_200")) %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 10 LowIncDisab Age 5-17" <-
    ctyreg_pums_count(filter(pp_df, age_detail=="5-17"), c("DIS","poverty_200")) %>%
    filter_poverty_level(200) %>% filter(grepl("^With ", as.character(DIS)))
  xtrastats$"Tbl 10 LowIncDisab Age 65+" <-
    ctyreg_pums_count(filter(pp_df, age_group=="65+"), c("DIS","poverty_200")) %>%
    filter_poverty_level(200) %>% filter(grepl("^With ", as.character(DIS)))
  xtrastats$"Tbl 10 LowIncDisab Race" <-
    psrc_pums_count(filter(pp_df, grepl("^With ", as.character(DIS))), group_vars=c("PRACE","poverty_200"), incl_na=FALSE, rr="cv") %>%
    filter_poverty_level(200)
  xtrastats$"Tbl 10 LowInc DIS POC" <-
    ctyreg_pums_count(filter(pp_df, PRACE!="White alone" & grepl("^With ", as.character(DIS))), "poverty_200") %>%
    filter_poverty_level(200) %>% mutate(PRACE="POC total")

# Table 11 - Zero-vehicle Households
  xtrastats$"Tbl 11 Total Count" <- copy(xtrastats$"Tbl 0 Total Count")
  xtrastats$"Tbl 11 No Veh Summary" <-
    ctyreg_pums_count(hh_df, "zero_veh") %>% filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh POC_HRACE" <-
    ctyreg_pums_count(filter(hh_df, HRACE!="White alone"), "zero_veh") %>%
    filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh POC_PRACE" <-
    ctyreg_pums_count(filter(hh_df, PRACE!="White alone"), "zero_veh") %>%
    filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh Pov" <-
    ctyreg_pums_count(filter(hh_df, poverty_100=="Yes"), "zero_veh") %>%
    filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh LowInc" <-
    ctyreg_pums_count(filter(hh_df, poverty_200=="Yes"), "zero_veh") %>%
    filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh H65+" <-
    ctyreg_pums_count(filter(hh_df, stringr::str_detect(R65, "^\\d ")), "zero_veh") %>%
    filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh P65+" <-
    ctyreg_pums_count(filter(hh_df, age_group=="65+"), "zero_veh") %>%
    filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh HDis" <-
    ctyreg_pums_count(filter(hh_df, stringr::str_detect(HDIS, "^With ")), "zero_veh") %>%
    filter(zero_veh=="Yes")
  xtrastats$"Tbl 11 No Veh PDis" <-
    ctyreg_pums_count(filter(hh_df, stringr::str_detect(DIS, "^With ")), "zero_veh") %>%
    filter(zero_veh=="Yes")

# Table 12 - Common Non-English Languages
  xtrastats$"Tbl 12 Common Languages" <-
    ctyreg_pums_count(pp_df, "LANP") %>% filter(LANP!="Total") %>%
    slice_max(order_by=share, n=12, by=COUNTY, na_rm=TRUE)                                    # Selecting top 12 per geography

# Table 13 - Limited English Proficiency
  xtrastats$"Tbl 13 Pop 5yo+ x LANX" <-
    ctyreg_pums_count(filter(pp_df, AGEP>5), "LANX") %>% rename(lep=LANX)
  xtrastats$"Tbl 13 Pop 5yo+ x LEP" <-
    ctyreg_pums_count(filter(pp_df, AGEP>5), "lep")

# Table 14 - Regional LEP by Language Spoken
  lep_languages <- psrc_pums_count(filter(pp_df, grepl("less than",lep)),
                                   group_vars="LANP",incl_na=FALSE) %>%                                     # First pull to choose languages
    filter(LANP!="Total" & (count + count_moe) > 5000) %>%                                    # -- to enable LEP share per language
    pull(LANP) %>% as.character() %>% unique()
  lep_stats <- filter(pp_df, AGEP>=5 & LANP %in% lep_languages) %>%
    psrc_pums_count(group_vars=c("LANP","lep"), incl_na=FALSE) %>%
    filter(LANP!="Total" & lep!="Speak English 'very well'") %>%                              # LEP share per language
    split(f=.$lep) %>% lapply(select,-lep)
  lep_stats$Total %<>% select(-contains("share")) %>%
    rename_with(~ paste0("total_", .x, recycle0 = TRUE),
    starts_with("count", ignore.case=FALSE))
  lep_stats$`Speak English less than 'very well'` %<>%
    rename_with(~ paste0("lep_", .x, recycle0 = TRUE),
                matches("count|share", ignore.case=FALSE))
  xtrastats$"Tbl 14 LEP Languages" <-
    inner_join(lep_stats$Total, lep_stats$`Speak English less than 'very well'`,
               by=c("LANP","DATA_YEAR","COUNTY")) %>%
    arrange(desc(lep_count)) %>%
    rename_with(.fn= ~paste0(.,"_values"), .cols=matches("(count|share)$")) %>%
    tidyr::pivot_longer(cols=matches("(values|moe)$"),                                        # This results in the right table structure
             names_to = c("metric", ".value"),                                                # -- although R displays counts & shares
             names_pattern = "(.*_)(values|moe)$")                                            # -- with identical numeric format, e.g. decimal places

# Table 15 - Additional stats by largest non-English population total                         # Extra context added last                                        # -- to enable LEP share per language
  lep_languages2 <- psrc_pums_count(pp_df, group_vars="LANP", incl_na=FALSE) %>%              # First pull to choose languages
    filter(LANP!="Total" & (count + count_moe) > 10000) %>% pull(LANP) %>%                    # -- to enable LEP share per language
    as.character() %>% unique()
  lep_stats2 <- filter(pp_df, AGEP>=5 & LANP %in% lep_languages2) %>%
    psrc_pums_count(group_vars=c("LANP","lep"), incl_na=FALSE) %>%
    filter(LANP!="Total" & lep!="Speak English 'very well'") %>%                              # LEP share per language
    split(f=.$lep) %>% lapply(select,-lep)
  lep_stats2$Total %<>% select(-contains("share")) %>%
    rename_with(~ paste0("total_", .x, recycle0 = TRUE),
                starts_with("count", ignore.case=FALSE))
  lep_stats2$"Speak English less than 'very well'" %<>%
    rename_with(~ paste0("lep_", .x, recycle0 = TRUE),
                matches("count|share", ignore.case=FALSE))
  xtrastats$"Tbl 15 Non-English Languages" <-
    inner_join(lep_stats2$Total, lep_stats2$"Speak English less than 'very well'",
               by=c("LANP","DATA_YEAR","COUNTY")) %>%
    arrange(desc(lep_count)) %>%
    rename_with(.fn= ~paste0(.,"_values"), .cols=matches("(count|share)$")) %>%
    tidyr::pivot_longer(cols=matches("(values|moe)$"),                                        # This results in the right table structure
             names_to = c("metric", ".value"),                                                # -- although R displays counts & shares
             names_pattern = "(.*_)(values|moe)$")                                            # -- with identical numeric format, e.g. decimal places

  return(xtrastats)
}

format_for_report <- function(xtrastats){
    report_tables <- list()
    report_tables$"Tbl3 Pov-LowInc Race-Hisp" <-
      combine_tbl_elements(rgx="Tbl 3", group_varlist=c(NA, NA, "PRACE", "PRACE", NA, "PRACE", NA),
                                    metric_list = c(rep(c("count", "share", "count"),2),"count"))
    report_tables$"Tbl4a Med Inc Race-Hisp" <-
      combine_tbl_elements("Tbl 4a", group_varlist=rep.int("HRACE",2),
                                    metric_list=c("median","share"))
    report_tables$"Tbl4b Med Inc Race-Hisp" <-
      combine_tbl_elements("Tbl 4b", group_varlist=rep.int("PRACE",2),
                           metric_list=c("median","share"))
    report_tables$"Tbl5 Asian Detail" <- xtrastats$"Tbl 5 Asian Detail" %>% select(-any_of(contains("cv")))
    report_tables$"Tbl6 Pov-LowInc Race Sex" <-
      combine_tbl_elements("Tbl 6", group_varlist=rep(c("SEX", "PRACE;SEX", "SEX"),2),
                           metric_list=rep("share",6))
    report_tables$"Tbl7 HH Type" <-
      combine_tbl_elements("Tbl 7", group_varlist=c(NA, rep("hh_type", 10)),
                           metric_list=c(rep("count",2) ,rep("share",6), rep("HINCP_median",3)))
    report_tables$"Tbl8 Pov-LowInc 65+" <-
      combine_tbl_elements("Tbl 8",
                           group_varlist=c(NA, rep(c(NA, NA, "age_detail", "PRACE", "PRACE"), 2)),
                           metric_list=c("count", rep("share", 10)))
    report_tables$"Tbl9 Pov-LowInc <18" <-
      combine_tbl_elements("Tbl 9",
                           group_varlist=c(NA, rep(c(NA, NA, "age_detail", "PRACE", "PRACE"), 2)),
                           metric_list=c("count", rep("share", 10)))
    report_tables$"Tbl10 Pov-LowInc Disability" <-
      combine_tbl_elements("Tbl 10", group_varlist=c(rep(NA, 5), "PRACE", "PRACE", rep(NA, 4), "PRACE", "PRACE"),
                                    metric_list=c("count", rep("share", 12)))
    report_tables$"Tbl11 Zero Veh" <-
      combine_tbl_elements("Tbl 11", group_varlist=rep(NA, 10),
                                    metric_list=c("count", rep("share", 9))) %>%
      rbind(combine_tbl_elements("Tbl 11 No Veh", group_varlist=rep(NA, 9),
                                 metric_list=rep("count", 9)))
    report_tables$"Tbl12 Common Languages" <- xtrastats$"Tbl 12 Common Languages"
    report_tables$"Tbl13 Limited English" <-
         rbind(combine_tbl_elements("Tbl 13", group_varlist=rep("lep", 2), metric_list=rep("count", 2)),
         mutate(format_county_stats(xtrastats$"Tbl 13 Pop 5yo+ x LEP", group_vars="lep", metric="share"),
                Stub="Tbl 13 Pop 5yo+ x LEP share"))
    report_tables$"Tbl14 LEP Languages" <- xtrastats$"Tbl 14 LEP Languages"
    report_tables$"Tbl15 Non-English Languages" <- xtrastats$"Tbl 15 Non-English Languages"

    return(report_tables)
}

# Write all tables to file
write_dprofile_pums_xlsx <- function(result_list){
  openxlsx::write.xlsx(result_list, file = "outfile.xlsx",
    sheetName = names(result_list), rowNames = FALSE)
  return(invisible(NULL))
}

# Example -------------
# xtrastats <- get_pums_dp(2022)                          # Returns expanded tables as separate items in a list
# write_dprofile_pums_xlsx(xtrastats)                     # Write the expanded tables to .xlsx
# prettier <- format_for_report(xtrastats)                # Formats existing object (pivoting, asterisks, etc)
# write_dprofile_pums_xlsx(prettier)                      # Write the structured tables to .xlsx (will overwrite, so rename prior write)
