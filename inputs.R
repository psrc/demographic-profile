# Variables and lists used for data analysis

database.name <- "Elmer"
server.name <- "AWS-PROD-SQL\\Sockeye"

yr <- list(2016)

census.tables <- list("B01001.1yr" = list("B01001","Sex by Age","acs/acs1"),
                      "B02001.1yr" = list("B02001","Race", "acs/acs1"),
                      "B03002.1yr" = list("B03002","Hispanic or Latino by Race", "acs/acs1"),
                      "C16001.1yr" = list("C16001","Language Spoken at Home for the Population 5 Years and Over", "acs/acs1"),
                      "C16004.1yr" = list("C16004","Age by Language Spoken at Home by Ability to Speak English for the Population 5 Years and Over", "acs/acs1"),
                      "C17001.1yr" = list("C17001","Poverty Status in the Past 12 Months by Sex by Age", "acs/acs1"),
                      "C17002.1yr" = list("C17002","Ratio of Income to Poverty Level in the Past 12 Months", "acs/acs1"),
                      "B18101.1yr" = list("B18101","Sex by Age by Disability Status", "acs/acs1"),
                      "B18130.1yr" = list("B18130","Sex by Age by Disability Status by Employment Status for the Civilian Noninstitutionalized Population 5 Years and Over", "acs/acs1"),
                      "B25045.1yr" = list("B25045","Tenure by Vehicles Available by Age of Householder", "acs/acs1"),
                      "B19013.1yr" = list("B19013","Median Household Income in the Past 12 Months", "acs/acs1"),
                      "B19013A.1yr" = list("B19013A","Median Household Income in the Past 12 Months (White Alone Householder)", "acs/acs1"),
                      "B19013B.1yr" = list("B19013B","Median Household Income in the Past 12 Months (Black or African American Alone Householder)", "acs/acs1"),
                      "B19013C.1yr" = list("B19013C","Median Household Income in the Past 12 Months (American Indian and Alaskan Native Alone Householder)", "acs/acs1"),
                      "B19013D.1yr" = list("B19013D","Median Household Income in the Past 12 Months (Asian Alone Householder)", "acs/acs1"),
                      "B19013E.1yr" = list("B19013E","Median Household Income in the Past 12 Months (Native Hawaiian and Other Pacific Islander Alone Householder)", "acs/acs1"),
                      "B19013F.1yr" = list("B19013F","Median Household Income in the Past 12 Months (Some Other Race Householder)", "acs/acs1"),
                      "B19013G.1yr" = list("B19013G","Median Household Income in the Past 12 Months (Two or More Races Householder)", "acs/acs1"),
                      "B19013H.1yr" = list("B19013H","Median Household Income in the Past 12 Months (Hispanic or Latino Householder)", "acs/acs1"),
                      "B19013I.1yr" = list("B19013I","Median Household Income in the Past 12 Months (White Alone, Not Hispanic or Latino Householder)", "acs/acs1"),
                      "B02001.5yr" = list("B02001","Race", "acs/acs5"),
                      "B03002.5yr" = list("B03002","Hispanic or Latino by Race", "acs/acs5"),
                      "B17001.5yr" = list("B17001","Poverty Status in the Past 12 Months by Sex by Age", "acs/acs5"),
                      "B17001A.5yr" = list("B17001A","Poverty Status in the Past 12 Months by Sex by Age (White Alone)", "acs/acs5"),
                      "B17001B.5yr" = list("B17001B","Poverty Status in the Past 12 Months by Sex by Age (Black or African American Alone)", "acs/acs5"),
                      "B17001C.5yr" = list("B17001C","Poverty Status in the Past 12 Months by Sex by Age (American Indian and Alaskan Native Alone)", "acs/acs5"),
                      "B17001D.5yr" = list("B17001D","Poverty Status in the Past 12 Months by Sex by Age (Asian Alone)", "acs/acs5"),
                      "B17001E.5yr" = list("B17001E","Poverty Status in the Past 12 Months by Sex by Age (Native Hawaiian and Other Pacific Islander Alone)", "acs/acs5"),
                      "B17001F.5yr" = list("B17001F","Poverty Status in the Past 12 Months by Sex by Age (Some Other Race Alone)", "acs/acs5"),
                      "B17001G.5yr" = list("B17001G","Poverty Status in the Past 12 Months by Sex by Age (Two or More Races Alone)", "acs/acs5"),
                      "B17001H.5yr" = list("B17001H","Poverty Status in the Past 12 Months by Sex by Age (Hispanic or Latino)", "acs/acs5"),
                      "B17001I.5yr" = list("B17001I","Poverty Status in the Past 12 Months by Sex by Age (White Alone, Not Hispanic or Latino)", "acs/acs5")
)

counties <- list("Central Puget Sound", "King County", "Kitsap County", "Pierce County", "Snohomish County")

race.categories <- c("Geography","Estimate Total",
                     "Estimate Total White alone", "MoE Total White alone",
                     "Estimate Total Black or African American alone", "MoE Total Black or African American alone",
                     "Estimate Total American Indian and Alaska Native alone" , "MoE Total American Indian and Alaska Native alone",
                     "Estimate Total Asian and Pacific Islander", "MoE Total Asian and Pacific Islander",
                     "Estimate Other Race or Two or More Races", "MoE Other Race or Two or More Races",
                     "Hispanic or Latino (of any race)",
                     "Estimate Total Minority", "MoE Total Minority")

race.hispanic.origin.categories <- list ("total-population" = ":",
                                         "white" = ": Not Hispanic or Latino: White alone",
                                         "black" = ": Not Hispanic or Latino: Black or African American alone" ,
                                         "native-american" = ": Not Hispanic or Latino: American Indian and Alaska Native alone",
                                         "asian" = ": Not Hispanic or Latino: Asian alone",
                                         "pacific-islander" = ": Not Hispanic or Latino: Native Hawaiian and Other Pacific Islander alone",
                                         "other" = ": Not Hispanic or Latino: Some other race alone",
                                         "two-or-more" = ": Not Hispanic or Latino: Two or more races:" ,
                                         "hispanice-latinx" = ": Hispanic or Latino:")

poverty.ratio.categories <- list ("total-population" = ":",
                                         "under-50%" = ": Under .50",
                                         "50%-100%" = ": .50 to .99" ,
                                         "100%-125%" = ": 1.00 to 1.24",
                                         "125%-150%" = ": 1.25 to 1.49",
                                         "150%-185%" = ": 1.50 to 1.84",
                                         "185%-200%" = ": 1.85 to 1.99",
                                         "over-200%" = ": 2.00 and over")

tbl1.colnames <- c("Estimate", "Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate","Estimate", "MoE")
