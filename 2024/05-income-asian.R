library(psrccensus)
library(psrcplot)
library(tidyverse)
library(openxlsx)

Sys.getenv("CENSUS_API_KEY")


#---- download 2022 5-year PUMS data with specified variables ----
pums_2022_p <- get_psrc_pums(span = 5,
                             dyear = 2022,
                             level = "p",
                             vars = c("RAC1P", "RAC2P"))  
pums_2022_h <- get_psrc_pums(span = 5,
                             dyear = 2022,
                             level = "h",
                             vars = c("RAC1P", "RAC2P", "HINCP", "BIN_POVRATIO")) 


# get all subgroups with more than 5000 population
asian_subgroup_5000 <- psrc_pums_count(pums_2022_p, group_vars=c("RAC1P","RAC2P")) %>%
  filter(RAC1P=="Asian alone",
         count>5000) 

# assign new asian subgroup groupings
get_new_subgroup <- function(.data, group_list){
  .data %>%
    mutate(asian_subgroup = case_when(RAC2P == "All combinations of Asian races only"~"Two or more Asian",
                                      RAC2P == "Other Asian alone"~"Other Asian",
                                      RAC2P %in% group_list ~RAC2P,
                                      TRUE~"Other Asian"))
}

df_pums <- pums_2022_h %>%
  filter(RAC1P == "Asian alone") %>%
  # make new variables
  mutate(income_poverty_level = case_when(BIN_POVRATIO %in% c("under 0.50","0.50 to 0.99")~"Income below 100% of poverty level",
                                          TRUE~"Income above 100% of poverty level")) %>%
  get_new_subgroup(asian_subgroup_5000$RAC2P)

df_pums_p <- pums_2022_p %>%
  filter(RAC1P == "Asian alone") %>%
  get_new_subgroup(asian_subgroup_5000$RAC2P)


#---- asian alone ----

per_count <- psrc_pums_count(df_pums_p, group_vars=c("RAC1P")) %>%
  filter(RAC1P=="Asian alone")
poverty <- psrc_pums_count(df_pums, group_vars=c("RAC1P","income_poverty_level")) %>%
  filter(income_poverty_level=="Income below 100% of poverty level")
income <- psrc_pums_median(df_pums, stat_var = "HINCP", group_vars=c("RAC1P"))

final_columns <- c("asian_subgroup","count.population","count_moe.population",
                   "share.below_poverty","share_moe.below_poverty","HINCP_median","HINCP_median_moe")
df_asian <- per_count %>%
  left_join(income, by=c("DATA_YEAR","COUNTY","RAC1P")) %>%
  left_join(poverty, by=c("DATA_YEAR","COUNTY","RAC1P"), suffix = c(".population",".below_poverty")) %>%
  rename(asian_subgroup = RAC1P) %>%
  select(all_of(final_columns))
  
#---- asian subgroups ----

per_count_sub <- psrc_pums_count(df_pums_p, group_vars=c("RAC1P","asian_subgroup")) %>%
  filter(RAC1P=="Asian alone")
poverty_sub <- psrc_pums_count(df_pums, group_vars=c("asian_subgroup","income_poverty_level")) %>%
  filter(income_poverty_level=="Income below 100% of poverty level")
# median income + poverty level
income_sub <- psrc_pums_median(df_pums, stat_var = "HINCP", group_vars=c("asian_subgroup"))

df_asian_sub <- per_count_sub %>%
  left_join(income_sub, by=c("DATA_YEAR","COUNTY","asian_subgroup")) %>%
  left_join(poverty_sub, by=c("DATA_YEAR","COUNTY","asian_subgroup"), suffix = c(".population",".below_poverty")) %>%
  filter(asian_subgroup!="Total") %>%
  mutate(asian_subgroup = str_replace(asian_subgroup," alone|, alone",""),
         asian_subgroup = factor(asian_subgroup, levels=c("Asian Indian","Cambodian","Chinese, except Taiwanese",
                                                      "Filipino","Japanese","Korean","Laotian","Pakistani",
                                                      "Taiwanese","Thai","Vietnamese","Two or more Asian","Other Asian"))) %>%
  arrange(asian_subgroup) %>%
  select(all_of(final_columns))

asian_profile <- df_asian %>%
  add_row(df_asian_sub) %>%
  mutate(`count.population` = scales::number(`count.population`,accuracy=100,big.mark = ","),
         `count_moe.population` = scales::number(`count_moe.population`,accuracy=1,big.mark = ","),
         `share.below_poverty` =  scales::percent(`share.below_poverty`,accuracy=0.1),
         `share_moe.below_poverty` =  scales::percent(`share_moe.below_poverty`,accuracy=0.1),
         HINCP_median = scales::number(HINCP_median,accuracy=100,big.mark = ","),
         HINCP_median_moe = scales::number(HINCP_median_moe,accuracy=1,big.mark = ","))

# save to spreadsheet
write.xlsx(asian_profile, 'asian_demo_profile.xlsx')
