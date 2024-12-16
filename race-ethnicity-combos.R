library(magrittr)
library(psrccensus)
library(dplyr)
library(data.table)

dyear <- 2022
#pums_rds <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"                # Network PUMS location
pums_rds <- "C:/Users/mjensen/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"   # Local PUMS location

# Helper function -------------------------------
combo_count <- function(x){
  hh_df2 <- copy(hh_df) %>% mutate(
    race_combo=paste0(x, collapse="-"),                                                     # Add label field
    relevant=case_when(if_all(all_of({{x}}), ~.x=="Yes") ~"Yes",                            # Add combo field
                    TRUE                                 ~"No"))
  rs <- psrc_pums_count(hh_df2, group_vars=c("race_combo","relevant"), incl_na=FALSE) %>%   # Calculate stats
    filter(relevant=="Yes") %>% select(-relevant)                                           # Drop all but combo
}

# Build the data object -------------------------
pvars <- c("HISP","RAC1P","RACAIAN","RACASN","RACBLK","RACNH","RACPI","RACSOR","RACWHT","RACNUM")
checkbox_vars <- c("HISP","RACAIAN","RACASN","RACBLK","RACNHPI","RACSOR","RACWHT")
byvars <- c("SERIALNO","DATA_YEAR","PRODUCT","COUNTY")

hh_df <- get_psrc_pums(5, dyear, "h", "HRACE", dir=pums_rds)                                # Retrieve household data
pp_df <- get_psrc_pums(5, dyear, "p", pvars, dir=pums_rds) %>%                              # Retrieve persons data
  srvyr::as_tibble() %>% setDT()
for(col in c(checkbox_vars, "RACNH","RACPI"))
  set(pp_df, j=col, value=as.character(pp_df[[col]]))                                       # Character type used in next operations
pp_df[, RACNHPI:=pmax(RACNH,RACPI)] %>% .[, c("RACNH", "RACPI"):=NULL] %>%                 # Keep only combined NHPI
  .[, HISP:=case_when(stringr::str_detect(HISP,"^Not ") ~"No",                              # Switch to dichotomous Hispanic ethnicity
                      !is.na(HISP) ~"Yes")]
hh_x <- pp_df[, lapply(.SD, max, na.rm=TRUE), by=eval(byvars), .SDcols=(checkbox_vars)] %>% # Summarize to household
  setDF()
hh_df[[7]] %<>% dplyr::left_join(hh_x, by=byvars)                                           # Combine w/ srvyr object

# Create combinations and run stats -------------
combo_list <- lapply(1:4, function(x) lapply(as.data.frame(combn(checkbox_vars, x)), c))    # All combinations, from 1 to 4 elements
rs <- lapply(combo_list, function(x) lapply(x, combo_count)) %>% lapply(rbindlist)          # Summaries workhorse
names(rs) <- c("solos","duos","trios","quartets")

# Store results ---------------------------------
saveRDS(rs, "race-ethnicity-combos.rds")
openxlsx::write.xlsx(rs, file = "race-ethnicity-combos.xlsx", sheetName = names(rs), rowNames=FALSE)
