# This script will pull data for the Demographic Profile StoryMap project, for the fourth chapter on language
# Demographic Profile report: https://www.psrc.org/media/9039 (p.44)

# load libraries
library(tidyverse)
library(psrccensus)
library(psrcelmer)

dir <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"
dyear <- 2023

# download PUMS data ------
# data dictionary: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2019-2023.pdf 
pums_raw <- get_psrc_pums(5, dyear, "h", 
                          c("HHL", #Household language
                            "HHLANP", #Detailed household language
                            "LANX", #Language other than English spoken at home
                            "LANP", #Language spoken at home
                            "LNGI" #Limited English speaking household
                            ),
                          dir = dir)


# Common Languages Spoken Other than English (Fig 20/Table 12) -----
## simplified hh language
hhl_count <- psrc_pums_count(pums_raw,
                             group_vars = c("HHL"),
                             incl_na = FALSE,
                             rr = TRUE)
hhl_count_region <- hhl_count %>% 
  filter(COUNTY == "Region") %>% 
  arrange(desc(share))

hhl_count_county <- psrc_pums_count(pums_raw,
                                    group_vars = c("COUNTY","HHL"),
                                    incl_na = FALSE,
                                    rr = TRUE)

hhl_count_county_org <- hhl_count_county %>% 
  filter(COUNTY != "Region") %>% 
  arrange(COUNTY, desc(share))

# Asian and Pacific Island languages are the most common household languages after English in each of the four counties, except Pierce, where Spanish is the most common after English

## detailed hh language
hhlanp_count <- psrc_pums_count(pums_raw,
                                group_vars = c("HHLANP"),
                                incl_na = FALSE,
                                rr = TRUE)
hhlanp_count_region <- hhlanp_count %>% 
  filter(COUNTY == "Region") %>% 
  arrange(desc(count))

# •	Common languages spoken other than English
# English-only households represent 73% of the region's households. 
# The next most common languages spoken are Spanish (6.8%), Chinese (2.1%), Korean (1.4%), and Vietnamese (1.4%)

hhlanp_count_county <- psrc_pums_count(pums_raw,
                                       group_vars = c("COUNTY","HHLANP"),
                                       incl_na = FALSE,
                                       rr = TRUE)

hhlanp_count_county_org <- hhlanp_count_county %>% 
  filter(COUNTY != "Region") %>% 
  arrange(COUNTY, desc(share))

# In King: Spanish, Chinese, Vietnamese, Kindi
# In Kitsap: Spanish, Tagalog, Japanese (fair), Filipino (fair)
# In Pierce: Spanish, Korean, Tagalog, German
# In Snohomish: Spanish, Korean, Vietnamese, Russian

# Populations Speaking English Less than “Very Well” (Fig 21) -----
hhlanglep_count <- psrc_pums_count(pums_raw,
                               group_vars = c("HHLANP", "LNGI"),
                               incl_na = FALSE,
                               rr = TRUE)

hhlanplep_count_region <- hhlanglep_count %>% 
  filter(COUNTY == "Region") %>% 
  arrange(desc(count))

# Regionally, for households where "No one in the household 14 and over speaks English only or speaks English 'very well'," the largest number of households speak Spanish, Chinese, Korean, Vietnamese, and Russian 
# Regionally, for households where "No one in the household 14 and over speaks English only or speaks English 'very well'," the languages with the highest proportion of speakers (with good reliability) are Ukranian (35.6%), Vietnamese (32.3%), Korean (32%), Chinese (29.6%), Russian (22.9%), Mandarin (22%), Spanish (14.9%), and Tagalog (7.8%)

hhlanglep_count <- psrc_pums_count(pums_raw,
                                   group_vars = c("COUNTY","HHLANP", "LNGI"),
                                   incl_na = FALSE,
                                   rr = TRUE)

hhlanglep_count_county_org <- hhlanglep_count %>% 
  filter(COUNTY != "Region") %>% 
  arrange(COUNTY, desc(share))
# Regionally, for households where "No one in the household 14 and over speaks English only or speaks English 'very well'," the largest number of households speak
# King: Spanish, Chinese, Vietnamese, Korean
# Kitsap: all weak or unreliable, Tagalog is only fair
# Pierce: Spanish, Korean, Vietnamese (fair), Ukranian (fair)
# Snohomish: Spanish, Korean (fair), Vietnamese (fair), Russian (fair)

# Limited English Proficiency (Table 13) -----
hhlep_count <- psrc_pums_count(pums_raw,
                               group_vars = c("LNGI"),
                               incl_na = FALSE,
                               rr = TRUE)

# Regionally, 4.6% of households have "No one in the household 14 and over speaks English only or speaks English 'very well'."

hhlep_count_county <- psrc_pums_count(pums_raw,
                                      group_vars = c("COUNTY", "LNGI"),
                                      incl_na = FALSE,
                                      rr = TRUE)

# In King, 5.7% of households have "No one in the household 14 and over speaks English only or speaks English 'very well'" - Snohomish is 4.5%, Pierce is 2.8%, and Kitsap is 1% (fair reliability)

# Ability to Speak English by Language Spoken at Home (Table 14) -----
hhlepdetail_count <- psrc_pums_count(pums_raw,
                                     group_vars = c("LNGI", "HHLANP"),
                                     incl_na = FALSE,
                                     rr = TRUE)

# The languages that households where "No one in the household 14 and over speaks English only or speaks English 'very well'" most heavily speak (based on number of households): Spanish, Chinese, Korean, Vietnamese, Russian 

hhlepdetail_count_county <- psrc_pums_count(pums_raw,
                                     group_vars = c("COUNTY", "LNGI", "HHLANP"),
                                     incl_na = FALSE,
                                     rr = TRUE)

hhlepdetail_count_county_simp <- hhlepdetail_count_county %>% 
  filter(reliability == "good",
         grepl("No one", LNGI))

# When filtering for "good" reliability, and when looking at individual counties, the languages that households where "No one in the household 14 and over speaks English only or speaks English 'very well'" most heavily speak varies:
# King: Chinese, Mandarin, Korean, Russian, Spanish, Vietnamese
# Kitsap: N/A
# Pierce: Spanish and Korean
# Snohomish: Spanish


# Language spoken at home -----
hhlanguage_count_region <- psrc_pums_count(pums_raw,
                                           group_vars = c("LANP"),
                                           incl_na = FALSE,
                                           rr = TRUE)

hhlanguage_region_desc <- hhlanguage_count_region %>% 
  arrange(desc(count)) #%>% 
# filter(LANP!="Total") 

# Regionally, the ten most common languages spoken at home, other than English, are Spanish (23.9%), Chinese (8.5%), Vietnamese (4.5%), Korean (5.1%), Hindi (4.7%), Russian (4.1%), Tagalog (3.5%), Mandarin (3.5%), Japanese (2.5%), and French (2.2%). 

hhlanguage_count_county <- psrc_pums_count(pums_raw,
                                    group_vars = c("COUNTY", "LANP"),
                                    incl_na = FALSE,
                                    rr = TRUE)

hhlanguage_county_desc <- hhlanguage_count_county %>% 
  arrange(desc(count)) %>%
  filter(COUNTY!="Region",
         LANP!="Total")
# In King County, the top 5 are: Spanish, Chinese, Hindi, Vietnamese, and Mandarin
# In Kitsap County, the top 5 are: Spanish, Tagalog, Japanese (fair), Filipino (fair), and French (weak)
# In Pierce County, the top 5 are: Spanish, Korean, Tagalog, Russian, Vietnamese
# In Snohomish County, the top 5 are: Spanish, Korean, Vietnamese, Russian, Chinese
