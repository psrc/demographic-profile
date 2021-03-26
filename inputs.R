# Variables and lists used for data analysis

database.name <- "Elmer"
server.name <- "AWS-PROD-SQL\\Sockeye"

yr <- 2019

census.tables <- list("B01001.1yr" = list("B01001","Sex by Age","ACS1"),
                      "B02001.1yr" = list("B02001","Race", "ACS1"),
                      "B03002.1yr" = list("B03002","Hispanic or Latino by Race", "ACS1"),
                      "C16001.1yr" = list("C16001","Language Spoken at Home for the Population 5 Years and Over", "ACS1"),
                      "C16004.1yr" = list("C16004","Age by Language Spoken at Home by Ability to Speak English for the Population 5 Years and Over", "ACS1"),
                      "C17001.1yr" = list("C17001","Poverty Status in the Past 12 Months by Sex by Age", "ACS1"),
                      "C17002.1yr" = list("C17002","Ratio of Income to Poverty Level in the Past 12 Months", "ACS1"),
                      "B18101.1yr" = list("B18101","Sex by Age by Disability Status", "ACS1"),
                      "B18130.1yr" = list("B18130","Sex by Age by Disability Status by Employment Status for the Civilian Noninstitutionalized Population 5 Years and Over", "ACS1"),
                      "B25045.1yr" = list("B25045","Tenure by Vehicles Available by Age of Householder", "ACS1"),
                      "B19013.1yr" = list("B19013","Median Household Income in the Past 12 Months", "ACS1"),
                      "B19013A.1yr" = list("B19013A","Median Household Income in the Past 12 Months (White Alone Householder)", "ACS1"),
                      "B19013B.1yr" = list("B19013B","Median Household Income in the Past 12 Months (Black or African American Alone Householder)", "ACS1"),
                      "B19013C.1yr" = list("B19013C","Median Household Income in the Past 12 Months (American Indian and Alaskan Native Alone Householder)", "ACS1"),
                      "B19013D.1yr" = list("B19013D","Median Household Income in the Past 12 Months (Asian Alone Householder)", "ACS1"),
                      "B19013E.1yr" = list("B19013E","Median Household Income in the Past 12 Months (Native Hawaiian and Other Pacific Islander Alone Householder)", "ACS1"),
                      "B19013F.1yr" = list("B19013F","Median Household Income in the Past 12 Months (Some Other Race Householder)", "ACS1"),
                      "B19013G.1yr" = list("B19013G","Median Household Income in the Past 12 Months (Two or More Races Householder)", "ACS1"),
                      "B19013H.1yr" = list("B19013H","Median Household Income in the Past 12 Months (Hispanic or Latino Householder)", "ACS1"),
                      "B19013I.1yr" = list("B19013I","Median Household Income in the Past 12 Months (White Alone, Not Hispanic or Latino Householder)", "ACS1"),
                      "B02001.5yr" = list("B02001","Race", "ACS5"),
                      "B03002.5yr" = list("B03002","Hispanic or Latino by Race", "ACS5"),
                      "B17001.5yr" = list("B17001","Poverty Status in the Past 12 Months by Sex by Age", "ACS5"),
                      "B17001A.5yr" = list("B17001A","Poverty Status in the Past 12 Months by Sex by Age (White Alone)", "ACS5"),
                      "B17001B.5yr" = list("B17001B","Poverty Status in the Past 12 Months by Sex by Age (Black or African American Alone)", "ACS5"),
                      "B17001C.5yr" = list("B17001C","Poverty Status in the Past 12 Months by Sex by Age (American Indian and Alaskan Native Alone)", "ACS5"),
                      "B17001D.5yr" = list("B17001D","Poverty Status in the Past 12 Months by Sex by Age (Asian Alone)", "ACS5"),
                      "B17001E.5yr" = list("B17001E","Poverty Status in the Past 12 Months by Sex by Age (Native Hawaiian and Other Pacific Islander Alone)", "ACS5"),
                      "B17001F.5yr" = list("B17001F","Poverty Status in the Past 12 Months by Sex by Age (Some Other Race Alone)", "ACS5"),
                      "B17001G.5yr" = list("B17001G","Poverty Status in the Past 12 Months by Sex by Age (Two or More Races Alone)", "ACS5"),
                      "B17001H.5yr" = list("B17001H","Poverty Status in the Past 12 Months by Sex by Age (Hispanic or Latino)", "ACS5"),
                      "B17001I.5yr" = list("B17001I","Poverty Status in the Past 12 Months by Sex by Age (White Alone, Not Hispanic or Latino)", "ACS5")
)

counties <- list("King County", "Kitsap County", "Pierce County", "Snohomish County", "Central Puget Sound")
