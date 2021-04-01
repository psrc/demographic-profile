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
                      "B19013H.1yr" = list("B19013H","Median Household Income in the Past 12 Months (White Alone, Not Hispanic or Latino Householder)", "acs/acs1"),
                      "B19013I.1yr" = list("B19013I","Median Household Income in the Past 12 Months (Hispanic or Latino Householder)", "acs/acs1"),
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
                      "B17001H.5yr" = list("B17001H","Poverty Status in the Past 12 Months by Sex by Age (White Alone, Not Hispanic or Latino)", "acs/acs5"),
                      "B17001I.5yr" = list("B17001I","Poverty Status in the Past 12 Months by Sex by Age (Hispanic or Latino)", "acs/acs5")
                      
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

poverty.ratio.categories <- c("Geography","Estimate Total",
                              "Estimate under 100%", "MoE under 100%",
                              "Estimate under 150%", "MoE under 150%",
                              "Estimate under 200%", "MoE under 200%")

poverty.race.tables <- list("B17001A" = "White",
                            "B17001B" = "Black or African American",
                            "B17001C" = "American Indian/Alaska Native",
                            "B17001D" = "Asian",
                            "B17001E" = "Native Hawaiian/Other Pacific Islander",
                            "B17001F" = "Some other race",
                            "B17001G" = "Two or more races",
                            "B17001H" = "White, not Hispanic or Latino",
                            "B17001I" = "Hispanic or Latino")

hhincome.race.tables <- list("B19013A" = "White",
                            "B19013B" = "Black or African American",
                            "B19013C" = "American Indian/Alaska Native",
                            "B19013D" = "Asian",
                            "B19013E" = "Native Hawaiian/Other Pacific Islander",
                            "B19013F" = "Some other race",
                            "B19013G" = "Two or more races",
                            "B19013H" = "White, not Hispanic or Latino",
                            "B19013I" = "Hispanic or Latino")

over65.poverty.categories <- c("Geography","Estimate Total", "MoE Total",
                               "Estimate Total Income in the past 12 months below poverty level Female 65 years and over", 
                               "Estimate Total Income in the past 12 months below poverty level Male 65 years and over",
                               "Estimate Total Income in the past 12 months at or above poverty level Female 65 years and over",
                               "Estimate Total Income in the past 12 months at or above poverty level Male 65 years and over",
                               "MoE Total Income in the past 12 months below poverty level Female 65 years and over", 
                               "MoE Total Income in the past 12 months below poverty level Male 65 years and over",
                               "MoE Total Income in the past 12 months at or above poverty level Female 65 years and over",
                               "MoE Total Income in the past 12 months at or above poverty level Male 65 years and over" )

under18.poverty.categories <- c("Geography","Estimate Total", "MoE Total",
                               "Estimate Total Income in the past 12 months below poverty level Female Under 18 years", 
                               "Estimate Total Income in the past 12 months below poverty level Male Under 18 years",
                               "Estimate Total Income in the past 12 months at or above poverty level Female Under 18 years",
                               "Estimate Total Income in the past 12 months at or above poverty level Male Under 18 years",
                               "MoE Total Income in the past 12 months below poverty level Female Under 18 years", 
                               "MoE Total Income in the past 12 months below poverty level Male Under 18 years",
                               "MoE Total Income in the past 12 months at or above poverty level Female Under 18 years",
                               "MoE Total Income in the past 12 months at or above poverty level Male Under 18 years" )

tbl1.colnames <- c("Estimate", "Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate","Estimate", "MoE")
tbl2.colnames <- c("Estimate", "Estimate", "MoE","Estimate", "MoE","Estimate", "MoE")
tbl3.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE")
tbl4.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Estimate", "MoE")
tbl5.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Pct", "MoE")
tbl6.colnames <- c("Estimate", "MoE","Estimate", "MoE","Estimate", "MoE","Pct", "MoE")
