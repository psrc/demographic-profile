# ============================================================
# CORRECTED SCRIPT - Using actual PUMS language codes
# Saves CSV directly to Downloads folder
# ============================================================

# Load libraries
library(dplyr)
library(tidyr)
library(purrr)
library(srvyr)
library(psrccensus)

# Set directory
dir <- "//AWS-PROD-FILE01/Datateam/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"
dyear <- 2024

# Download PUMS data
cat("Step 1/5: Downloading PUMS data...\n")
pums_raw <- get_psrc_pums(
  span = 5,
  dyear = dyear,
  level = "h",
  vars = c("HHLANP"),
  dir = dir
)

# Convert to data frame
cat("Step 2/5: Processing data...\n")
pums_df <- pums_raw$variables %>%
  mutate(COUNTY = as.character(COUNTY),
         HHLANP = as.character(HHLANP))

# Calculate counts by county and language (exclude English code 9500)
lang_county <- pums_df %>%
  filter(HHLANP != "9500", !is.na(HHLANP)) %>%
  group_by(COUNTY, HHLANP) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(COUNTY) %>%
  mutate(
    total = sum(count),
    share = (count / total) * 100,
    rank = rank(desc(count), ties.method = "first")
  ) %>%
  filter(rank >= 2 & rank <= 5) %>%
  mutate(Language_Name = case_when(
    # Spanish and Spanish Creole
    HHLANP %in% c("1200", "1210", "1220", "1230", "1240", "1250", "1260", "1261", "1262", 
                  "1263", "1270", "1273", "1274", "1275", "1276", "1277", "1278", "1280", 
                  "1281", "1282", "1283", "1285", "1288", "1290", "1292", "1293", "1295", 
                  "1296", "1297", "1298", "1300", "1301", "1302", "1303", "1304", "1305", 
                  "1310", "1311", "1312", "1313", "1314", "1315", "1316", "1317", "1318", 
                  "1320", "1321", "1322", "1323", "1324", "1325", "1326", "1327", "1328", 
                  "1330", "1331", "1332", "1333", "1335", "1340", "1345", "1350", "1360") ~ "Spanish",
    # Chinese (includes Mandarin, Cantonese)
    HHLANP %in% c("1960", "1970", "1980", "1990") ~ "Chinese",
    # Mandarin
    HHLANP == "2000" ~ "Mandarin",
    # Korean
    HHLANP == "2050" ~ "Korean",
    # Vietnamese
    HHLANP == "2100" ~ "Vietnamese",
    # Hindi
    HHLANP == "2350" ~ "Hindi",
    # Tagalog
    HHLANP %in% c("2525", "2535", "2540", "2560", "2575") ~ "Tagalog",
    # Japanese
    HHLANP == "2430" ~ "Japanese",
    # Russian
    HHLANP %in% c("2910", "2920", "2930", "2940", "2950", "2960") ~ "Russian",
    # French (including Cajun)
    HHLANP %in% c("1110", "1120", "1130", "1140", "1141", "1142", "1150", "1155", 
                  "1160", "1170") ~ "French",
    # Filipino (Tagalog based)
    HHLANP %in% c("2600", "2610", "2620", "2630", "2640") ~ "Filipino",
    # German
    HHLANP %in% c("1410", "1420", "1430", "1435", "1440", "1450") ~ "German",
    TRUE ~ "Other"
  )) %>%
  mutate(rank_label = case_when(
    rank == 2 ~ "Second",
    rank == 3 ~ "Third",
    rank == 4 ~ "Fourth",
    rank == 5 ~ "Fifth"
  ))

# Pivot to wide format
cat("Step 3/5: Creating wide format...\n")
map_ready <- lang_county %>%
  select(COUNTY, rank_label, Language_Name, share) %>%
  pivot_wider(
    id_cols = COUNTY,
    names_from = rank_label,
    values_from = c(Language_Name, share),
    names_sep = "_"
  )

# Add FIPS codes
map_ready <- map_ready %>%
  mutate(
    FIPS = case_when(
      COUNTY == "King" ~ "53033",
      COUNTY == "Pierce" ~ "53053",
      COUNTY == "Kitsap" ~ "53035",
      COUNTY == "Snohomish" ~ "53061"
    )
  )

# Round percentages
for (rank in c("Second", "Third", "Fourth", "Fifth")) {
  col_name <- paste0("share_", rank)
  if (col_name %in% names(map_ready)) {
    map_ready[[col_name]] <- round(map_ready[[col_name]], 1)
  }
}

# Ensure all columns exist
expected_cols <- c("Language_Name_Second", "Language_Name_Third", 
                   "Language_Name_Fourth", "Language_Name_Fifth",
                   "share_Second", "share_Third", "share_Fourth", "share_Fifth")

for (col in expected_cols) {
  if (!col %in% names(map_ready)) {
    map_ready[[col]] <- NA
  }
}

# Reorder columns
map_ready <- map_ready %>%
  select(
    COUNTY, FIPS,
    Language_Name_Second, share_Second,
    Language_Name_Third, share_Third,
    Language_Name_Fourth, share_Fourth,
    Language_Name_Fifth, share_Fifth
  )

# Save to DOWNLOADS folder
cat("Step 4/5: Saving CSV to Downloads folder...\n")
downloads_path <- file.path(Sys.getenv("USERPROFILE"), "Downloads", "county_languages_by_rank.csv")
write.csv(map_ready, downloads_path, row.names = FALSE)

# Show results
cat("\n========================================\n")
cat("SUCCESS! File saved to:\n")
cat(downloads_path, "\n")
cat("========================================\n\n")

print("Here is your data:")
print(map_ready)

# Open Downloads folder
if (Sys.info()["sysname"] == "Windows") {
  shell.exec(Sys.getenv("USERPROFILE") %>% file.path("Downloads"))
  cat("\nYour Downloads folder should now open.\n")
}

