# Regional Demographic Profile
The Puget Sound Regional Council (PSRC) is the federally designated metropolitan planning organization, as well as the state-designated regional transportation planning organization, for the central Puget Sound region. PSRC is responsible for developing and regularly updating the region’s long-range transportation plan as well as distributing federal transportation funds to local projects through its transportation improvement program. In 2003, PSRC developed a baseline <b>Regional Demographic Profile </b> as an initial step toward better integrating environmental justice into its transportation work program. That profile is updated to present current demographic data describing the central Puget Sound region each year to identify population groups and communities to be considered for subsequent environmental justice analyses and activities.

## Summary Tables
The data for the <b>Regional Demographic Profile </b> is pulled from a variety of tables from the latest 1-yr and 5yr American Community Survey (ACS) estimates for the four (4) counties that make up PSRC (King, Kitsap, Pierce and Snohomish). In all, there are eleven (11) tables in the Demographic Profile which are mainly derived from ACS 1year data with a few relying on 5yr data and 1 coming from the Public Use Microdata Sample (PUMS) Data. The eleven (11) summary tables in the document are:

1. Population by Race and Hispanic/Latino Origin (1yr)
2. Poverty Statistics (1yr)
3. Poverty Rates by Race and Hispanic/Latino Origin (5yr)
4. Median Household Income by Race and Hispanic/Latino Origin of Householder (1yr)
5. Poverty Rate for the Population Age 65 and Over (1yr)
6. Poverty Rate for the Population Under 18 (1yr)
7. Poverty Rate for Persons with a Disability (1yr)
8. Households with No Vehicle (1yr PUMS data)
9. Persons with Limited English Proficiency (1yr)
10. Language Spoken at Home by Ability to Speak English (5yr)
11. Common Languages Other Than English Spoken in the Central Puget Sound (5yr)

These tables were created from a variety of census data tables. The specific census tables needed for the creation of this report are:

### ACS 1 Year
- Table B01001. Sex by Age
- Table B02001. Race
- Table B03002. Hispanic or Latino by Race
- Table C16001. Language Spoken at Home for the Population 5 Years and Over
- Table C16004. Age by Language Spoken at Home by Ability to Speak English for the Population 5 Years and Over
- Table C17001. Poverty Status in the Past 12 Months by Sex by Age
- Table C17002. Ratio of Income to Poverty Level in the Past 12 Months
- Table B18101. Sex by Age by Disability Status
- Table B18130. Sex by Age by Disability Status by Employment Status for the Civilian Noninstitutionalized Population 5 Years and Over
- Table B19013. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars)
- Table B25045. Tenure by Vehicles Available by Age of Householder
- Table B19013A. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (White Alone Householder)
- Table B19013B. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (Black or African American Alone Householder)
- Table B19013C. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (American Indian and Alaskan Native Alone Householder)
- Table B19013D. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (Asian Alone Householder)
- Table B19013E. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (Native Hawaiian and Other Pacific Islander Alone Householder)
- Table B19013F. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (Some Other Race Householder)
- Table B19013G. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (Two or More Races Householder)
- Table B19013H. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (Hispanic or Latino Householder)
- Table B19013I. Median Household Income in the Past 12 Months (in 2016 Inflation-Adjusted Dollars) (White Alone, Not Hispanic or Latino Householder)

### ACS 5 Year
- Table B02001. Race
- Table B03002. Hispanic or Latino by Race
- Table B17001. Poverty Status in the Past 12 Months by Sex by Age
- Table B17001A. Poverty Status in the Past 12 Months by Sex by Age (White Alone)
- Table B17001B. Poverty Status in the Past 12 Months by Sex by Age (Black or African American Alone)
- Table B17001C. Poverty Status in the Past 12 Months by Sex by Age (American Indian and Alaskan Native Alone)
- Table B17001D. Poverty Status in the Past 12 Months by Sex by Age (Asian Alone)
- Table B17001E. Poverty Status in the Past 12 Months by Sex by Age (Native Hawaiian and Other Pacific Islander Alone)
- Table B17001F. Poverty Status in the Past 12 Months by Sex by Age (Some Other Race Alone)
- Table B17001G. Poverty Status in the Past 12 Months by Sex by Age (Two or More Races Alone)
- Table B17001H. Poverty Status in the Past 12 Months by Sex by Age (Hispanic or Latino)
- Table B17001I. Poverty Status in the Past 12 Months by Sex by Age (White Alone, Not Hispanic or Latino)

### ACS 1 Year PUMS Data
- Vehicles available (zero, 1 or more) by poverty status (universe: occupied housing units)
- Vehicles available (zero, 1 or more) by presence of one or more household members with a disability (universe: occupied housing units)

## Maps
In addition to compiling the <b>Regional Demographic Profile </b> tables described above, PSRC mapped the distributions of minority and low-income populations across the region to identify geographic areas and communities with substantial minority and low-income populations. There are six (6) maps in total:

1. Minority Population, Central Puget Sound
2. Black/African American Population, Central Puget Sound
3. American Indian/Alaskan Native Population, Central Puget Sound
4. Asian/Pacific Islander Population, Central Puget Sound
5. Hispanic/Latino Population, Central Puget Sound
6. Low-Income Population, Central Puget Sound

PSRC drew from ACS data to develop its environmental justice GIS map profiles. The minority population profile maps utilized census tract-level race and Hispanic/Latino origin data from the 5-year data set, and the low-income population profile map utilized census tract-level poverty status data from the same data set. PSRC established a set of regional thresholds to determine whether a census tract had a regionally significant minority or low-income population concentrations.

# Our Challenge
Like most organizations, we have created this product in the past in a fairly straight-forward manner:

1. Manually download data tables from the Census using Census data tools.
2. Clean and organize the data using our old friend Microsoft Excel.
3. Create maps in ArcMap by joining census tract data in csv to Census tract layers in ArcMap.
4. Put all our tables and maps into a Microsoft Word document, send it around for edits and save the final version as a pdf.

We know what tables we use, they are the same from year to year but the process of downloading, saving, copying and pasting is time consuming and risks human error. So why not use our scripting tools in the toolbox to automate this so that we can generate this report automatically? Always sounds easier than it is but we feel that this is a task worth doing - and this repo is our commitment  to it. Our mission as we see it is to:

1. Pull relevant census tables via the Census API and store them in our Central Database (Elmer) so that everyone will know exactly what tables we use.
2. Process/transform the data using <b>R</b> to get the data into a wide form for table creation.
3. Create <b>Excel-like</b> static tables using the Kable or other relevant packages in <b>R</b>.
4. Create <b>GIS-like</b> static maps using ggplot2 in <b>R</b>.
5. Use RMarkdown to knit together a nice pdf report of our demographic profile using our nifty tables and maps from above.

This should be fun - and I am sure a little frustrating - as we try to use our tools to make this product new but feel old!
