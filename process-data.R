# Load Census Tables ------------------------------------------------------

for (analysis.yr in yr) {
  c.yr <- as.character(analysis.yr)
  label.yr <- '2016'
  
  for (c in names(census.tables)) {
    
    results[[c.yr]][["raw-data-tables"]][[census.tables[[c]][[3]]]][[census.tables[[c]][[1]]]] <- get.county.census(c.type=census.tables[[c]][[3]], c.yr = analysis.yr, c.table = census.tables[[c]][[1]])
    
  } # end of loop for census tables
} # end of loop for analysis years


# Create Table 1 ----------------------------------------------------------

for (analysis.yr in yr) {
  c.yr <- as.character(analysis.yr)
  
  pop.by.race <- results[[c.yr]][["raw-data-tables"]][["acs/acs1"]][["B02001"]]
  pop.by.race.hispanic.origin <- results[[c.yr]][["raw-data-tables"]][["acs/acs1"]][["B03002"]]
  
  # Combine Asian and Pacific Islander categories
  pop.by.race["Estimate Total Asian and Pacific Islander"] <- pop.by.race["Estimate Total Asian alone"] + pop.by.race["Estimate Total Native Hawaiian and Other Pacific Islander alone"]
  pop.by.race["MoE Total Asian and Pacific Islander"] <- sqrt((pop.by.race["MoE Total Asian alone"])^2 + (pop.by.race["MoE Total Native Hawaiian and Other Pacific Islander alone"])^2)
  
  # Combine Other and Two or More Race categories
  pop.by.race["Estimate Other Race or Two or More Races"] <- pop.by.race["Estimate Total Some other race alone"] + pop.by.race["Estimate Total Two or more races"]
  pop.by.race["MoE Other Race or Two or More Races"] <- sqrt((pop.by.race["MoE Total Some other race alone"])^2 + (pop.by.race["MoE Total Two or more races"])^2)
  
  # Add in Hispanic or Latino of any race
  pop.by.race["Hispanic or Latino (of any race)"] <- pop.by.race.hispanic.origin["Estimate Total Hispanic or Latino"]
  
  # Total non-white population
  pop.by.race["Estimate Total Minority"] <- pop.by.race.hispanic.origin["Estimate Total Hispanic or Latino"] + 
    pop.by.race.hispanic.origin["Estimate Total Not Hispanic or Latino Black or African American alone"] +
    pop.by.race.hispanic.origin["Estimate Total Not Hispanic or Latino American Indian and Alaska Native alone"] +
    pop.by.race.hispanic.origin["Estimate Total Not Hispanic or Latino Asian alone"] +
    pop.by.race.hispanic.origin["Estimate Total Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone"] +
    pop.by.race.hispanic.origin["Estimate Total Not Hispanic or Latino Some other race alone"] +
    pop.by.race.hispanic.origin["Estimate Total Not Hispanic or Latino Two or more races"]
  
  pop.by.race["MoE Total Minority"] <- sqrt((pop.by.race.hispanic.origin["MoE Total Not Hispanic or Latino Black or African American alone"])^2 +
                                              (pop.by.race.hispanic.origin["MoE Total Not Hispanic or Latino American Indian and Alaska Native alone"])^2 +
                                              (pop.by.race.hispanic.origin["MoE Total Not Hispanic or Latino Asian alone"])^2 +
                                              (pop.by.race.hispanic.origin["MoE Total Not Hispanic or Latino Native Hawaiian and Other Pacific Islander alone"])^2 +
                                              (pop.by.race.hispanic.origin["MoE Total Not Hispanic or Latino Some other race alone"])^2 +
                                              (pop.by.race.hispanic.origin["MoE Total Not Hispanic or Latino Two or more races"])^2)
  
  # Final table with values
  results[[c.yr]][["processed-data-tables"]][["table.1.pop.by.race"]] <- pop.by.race[,race.categories] %>% arrange(Geography)
  
  # Create Formatted Table for document
  temp <- results[[c.yr]][["processed-data-tables"]][["table.1.pop.by.race"]] %>%
    mutate(across(where(is.numeric), label_comma(accuracy = 1)))
  
  # Add in Rows for Shares
  for (c in counties) {
    
    temp2 <- results[[c.yr]][["processed-data-tables"]][["table.1.pop.by.race"]] %>% 
      filter(Geography==c) %>%
      select(-Geography) %>%
      mutate(across(everything()), . / `Estimate Total`) %>%
      mutate(across(everything(), label_percent(accuracy = 0.1))) %>%
      add_column(Geography=c, .before = "Estimate Total")
    
    temp <- bind_rows(temp, temp2)  
  }
  
  temp <- temp %>% setNames(c("Geography",tbl1.colnames))
  
  results[[c.yr]][["formatted-data-tables"]][["table.1.pop.by.race"]] <- kbl(temp, caption = paste0("Population and Race by Hispanic/Latino Origin: ", c.yr), booktabs = T, align = "lcccccccccccccc") %>%
    kable_styling(latex_options = "striped") %>%
    kable_styling(latex_options = "scale_down") %>%
    add_header_above(c(" " = 1, "Total\n Population*"=1, "White" = 2,"Black or African\n American" = 2, "American Indian\n and Alaska Native" = 2, "Asian and Pacific\n Islander" = 2, "Other race or two\n or more races" = 2, "Hispanic\n or Latino\n (of any\n race)*" = 1, "Total Minority\n (non-White\n including\n White/Hispanic"=2)) %>%
    add_header_above(c(" " = 2, "Race (all categories)" = 10, " " = 3)) %>%
    row_spec(1:1, bold = T) %>%
    row_spec(6:6, bold = T) %>%
    pack_rows(index=c("Totals" = 5, "Shares" = 5)) %>%
    footnote(general = paste0("Source: ", c.yr, " American Community Survey 1-Year Estimates"),
             symbol = c("The estimate is controlled. A statistical test for sampling variability is not applicable.")) %>%
    landscape() 
    
  
} #end of year loop

rm(pop.by.race, pop.by.race.hispanic.origin, temp, temp2)

# Create Table 2 ----------------------------------------------------------

for (analysis.yr in yr) {
  c.yr <- as.character(analysis.yr)

  # Load table with ratio of income to poverty level
  pop.by.poverty <- results[[c.yr]][["raw-data-tables"]][["acs/acs1"]][["C17002"]]

  # Create columns for 100%, 150% and 200% of poverty levels
  pop.by.poverty["Estimate under 100%"] <- pop.by.poverty["Estimate Total Under .50"] + pop.by.poverty["Estimate Total .50 to .99"]
  pop.by.poverty["MoE under 100%"] <- sqrt((pop.by.poverty["MoE Total Under .50"])^2 + (pop.by.poverty["MoE Total .50 to .99"])^2)

  pop.by.poverty["Estimate under 150%"] <- pop.by.poverty["Estimate Total Under .50"] + pop.by.poverty["Estimate Total .50 to .99"] +
                                           pop.by.poverty["Estimate Total 1.00 to 1.24"] + pop.by.poverty["Estimate Total 1.25 to 1.49"]

  pop.by.poverty["MoE under 150%"] <- sqrt((pop.by.poverty["MoE Total Under .50"])^2 + (pop.by.poverty["MoE Total .50 to .99"])^2 +
                                      (pop.by.poverty["MoE Total 1.00 to 1.24"])^2 + (pop.by.poverty["MoE Total 1.25 to 1.49"])^2)

  pop.by.poverty["Estimate under 200%"] <- pop.by.poverty["Estimate Total Under .50"] + pop.by.poverty["Estimate Total .50 to .99"] +
                                           pop.by.poverty["Estimate Total 1.00 to 1.24"] + pop.by.poverty["Estimate Total 1.25 to 1.49"] +
                                           pop.by.poverty["Estimate Total 1.50 to 1.84"] + pop.by.poverty["Estimate Total 1.85 to 1.99"]

  pop.by.poverty["MoE under 200%"] <- sqrt((pop.by.poverty["MoE Total Under .50"])^2 + (pop.by.poverty["MoE Total .50 to .99"])^2 +
                                      (pop.by.poverty["MoE Total 1.00 to 1.24"])^2 + (pop.by.poverty["MoE Total 1.25 to 1.49"])^2 +
                                      (pop.by.poverty["MoE Total 1.50 to 1.84"])^2 + (pop.by.poverty["MoE Total 1.85 to 1.99"])^2)

  # Final table with values
  results[[c.yr]][["processed-data-tables"]][["table.2.pop.by.poverty.ratio"]] <- pop.by.poverty[,poverty.ratio.categories] %>% arrange(Geography)

  # Create Formatted Table for document
  temp <- results[[c.yr]][["processed-data-tables"]][["table.2.pop.by.poverty.ratio"]] %>%
    mutate(across(where(is.numeric), label_comma(accuracy = 1)))

  # Add in Rows for Shares
  for (c in counties) {
  
    temp2 <- results[[c.yr]][["processed-data-tables"]][["table.2.pop.by.poverty.ratio"]] %>% 
      filter(Geography==c) %>%
      select(-Geography) %>%
      mutate(across(everything()), . / `Estimate Total`) %>%
      mutate(across(everything(), label_percent(accuracy = 0.1))) %>%
      add_column(Geography=c, .before = "Estimate Total")
  
    temp <- bind_rows(temp, temp2)  
  }
  
  temp <- temp %>% setNames(c("Geography",tbl2.colnames))
  
  results[[c.yr]][["formatted-data-tables"]][["table.2.pop.by.poverty.ratio"]] <- kbl(temp, caption = paste0("Poverty Statistics: ", c.yr), booktabs = T, align = "lccccccc") %>%
    kable_styling(latex_options = "striped") %>%
    kable_styling(latex_options = "scale_down") %>%
    add_header_above(c(" " = 1, "Population for whom\n poverty status is\n determined"=1, "Below 100%\n of poverty level" = 2, "Below 150%\n of poverty level" = 2, "Below 200%\n of poverty level" = 2)) %>%
    add_header_above(c(" " = 2, "Income" = 6)) %>%
    row_spec(1:1, bold = T) %>%
    row_spec(6:6, bold = T) %>%
    pack_rows(index=c("Totals" = 5, "Shares" = 5)) %>%
    footnote(general = paste0("Source: ", c.yr, " American Community Survey 1-Year Estimates")) %>%
    landscape() 

} #end of year loop

rm(pop.by.poverty, temp, temp2)

# Create Table 3 ----------------------------------------------------------
for (analysis.yr in yr) {
  c.yr <- as.character(analysis.yr)

  # Load Total population in poverty from the base table
  temp <- results[[c.yr]][["raw-data-tables"]][["acs/acs5"]][["B17001"]] %>%
    select(Geography , `Estimate Total` , `Estimate Total Income in the past 12 months below poverty level`, `MoE Total`, `MoE Total Income in the past 12 months below poverty level`) %>%
    setNames(c("Geography","Total-Estimate","Estimate-in-Poverty","Total-MoE", "MoE-in-Poverty")) %>%
    mutate(`Share-Estimate-Poverty` = `Estimate-in-Poverty` / `Total-Estimate`) %>%
    mutate(`Share-MoE-Poverty` = `MoE-in-Poverty` / `Total-Estimate`) %>%
    arrange(Geography) %>%
    mutate(Race="All Persons")

  # Read data from each table of poverty statistics by race and append to the total population in poverty table
  for (tbl in names(poverty.race.tables)) {

    temp2 <- results[[c.yr]][["raw-data-tables"]][["acs/acs5"]][[tbl]] %>%
      select(Geography , `Estimate Total` , `Estimate Total Income in the past 12 months below poverty level`, `MoE Total`, `MoE Total Income in the past 12 months below poverty level`) %>%
      setNames(c("Geography","Total-Estimate","Estimate-in-Poverty","Total-MoE", "MoE-in-Poverty")) %>%
      mutate(`Share-Estimate-Poverty` = `Estimate-in-Poverty` / `Total-Estimate`) %>%
      mutate(`Share-MoE-Poverty` = `MoE-in-Poverty` / `Total-Estimate`) %>%
      arrange(Geography) %>%
      mutate(Race=poverty.race.tables[[tbl]])
  
    temp <- bind_rows(temp, temp2)
  }

  # Calculate Total Non-White Minority including White Hispanic
  for (c in counties) {
    tot <- temp %>%
      filter(Race %in% c("All Persons","White, not Hispanic or Latino"), Geography == c) %>%
      select(!contains("MoE")) %>%
      mutate(across(where(is.numeric), ~.-last(.))) %>%
      mutate(`Share-Estimate-Poverty` = `Estimate-in-Poverty` / `Total-Estimate`) %>%
      filter(Race == "All Persons") 
    
    moe <- suppressWarnings(temp %>%
      filter(Race %in% c("All Persons","White, not Hispanic or Latino"), Geography == c) %>%
      select(contains("MoE")) %>%
      summarise_all(moe_sum) %>%
      mutate(Geography=c))
  
    tot <- inner_join(tot, moe, by="Geography") %>%
      mutate(Race = "Total minority (non-White including White Hispanic)") %>%
      mutate(`Share-Estimate-Poverty` = `Estimate-in-Poverty` / `Total-Estimate`) %>% 
      mutate(`Share-MoE-Poverty` = `MoE-in-Poverty` / `Total-Estimate`)
  
    temp <- bind_rows(temp, tot)

  }

  # Convert from long to wide format
  tbl.values <- pivot_wider(temp, id_cols=Race, names_from = Geography, values_from = c("Total-Estimate","Estimate-in-Poverty","Total-MoE", "MoE-in-Poverty", "Share-Estimate-Poverty", "Share-MoE-Poverty"))  

  poverty.race.total.col <- c("Race","Total-Estimate_Central Puget Sound", "Total-MoE_Central Puget Sound",
                            "Total-Estimate_King County", "Total-MoE_King County",
                            "Total-Estimate_Kitsap County", "Total-MoE_Kitsap County",
                            "Total-Estimate_Pierce County", "Total-MoE_Pierce County",
                            "Total-Estimate_Snohomish County", "Total-MoE_Snohomish County")

  poverty.race.share.col <- c("Race","Share-Estimate-Poverty_Central Puget Sound", "Share-MoE-Poverty_Central Puget Sound",
                            "Share-Estimate-Poverty_King County", "Share-MoE-Poverty_King County",
                            "Share-Estimate-Poverty_Kitsap County", "Share-MoE-Poverty_Kitsap County",
                            "Share-Estimate-Poverty_Pierce County", "Share-MoE-Poverty_Pierce County",
                            "Share-Estimate-Poverty_Snohomish County", "Share-MoE-Poverty_Snohomish County")

  pov.pop.tot <- tbl.values[,poverty.race.total.col] %>% filter(Race=="All Persons") %>% mutate(Race="Population for whom poverty status is determined") %>% mutate(across(where(is.numeric), label_comma(accuracy = 1)))
  pov.pop.shr <- tbl.values[,poverty.race.share.col] %>% mutate(across(where(is.numeric), label_percent(accuracy = 0.1))) %>% setNames(poverty.race.total.col)

  # Final table with values
  results[[c.yr]][["processed-data-tables"]][["table.3.poverty.by.race"]] <- rbind(pov.pop.tot,pov.pop.shr)
  
  temp <- results[[c.yr]][["processed-data-tables"]][["table.3.poverty.by.race"]] %>% setNames(c("",tbl3.colnames))

  results[[c.yr]][["formatted-data-tables"]][["table.3.poverty.by.race"]] <- kbl(temp, caption = paste0("Poverty Statistics by Race and Hispanic/Latino Origin: ", analysis.yr-4,"-",analysis.yr), booktabs = T, align='lcccccccccc') %>%
    kable_styling(latex_options = "striped") %>%
    kable_styling(latex_options = "scale_down") %>%
    add_header_above(c(" " = 1, "Central\n Puget Sound"=2, "King\n County" = 2, "Kitsap\n County" = 2, "Pierce\n County" = 2, "Snohomish\n County" = 2)) %>%
    row_spec(1:1, bold = T) %>%
    add_indent(c(3:9), level_of_indent = 4) %>%
    add_indent(c(2,10:12), level_of_indent = 1) %>%
    footnote(general = paste0("Source: ", analysis.yr-4,"-",analysis.yr, " American Community Survey 5-Year Estimates")) %>%
    landscape() 

} #end of year loop

rm(pov.pop.tot, pov.pop.shr, tbl.values, temp, tot, moe, temp2)  
  
# Create Table 4 ----------------------------------------------------------
for (analysis.yr in yr) {
  c.yr <- as.character(analysis.yr)
  
  # Load Total population in poverty from the base table
  temp <- results[[c.yr]][["raw-data-tables"]][["acs/acs1"]][["B19013"]] %>%
    filter(Geography != "Central Puget Sound") %>%
    arrange(Geography) %>%
    mutate(Race="All Households") %>%
    setNames(c("Geography","Estimate","MoE", "Race"))
  
  # Read data from each table of poverty statistics by race and append to the total population in poverty table
  for (tbl in names(hhincome.race.tables)) {
    
    temp2 <- results[[c.yr]][["raw-data-tables"]][["acs/acs1"]][[tbl]] %>%
      filter(Geography != "Central Puget Sound") %>%
      arrange(Geography) %>%
      mutate(Race=hhincome.race.tables[[tbl]]) %>%
      setNames(c("Geography","Estimate","MoE", "Race"))
    
    temp <- bind_rows(temp, temp2) %>%
      mutate(Value="Total")
  }
  
  # Add in Rows for Shares
  for (c in counties) {
    t.hh <- temp %>% filter(Geography==c, Race=="All Households") %>% pull(`Estimate`)
    
    temp2 <- temp %>% 
      filter(Geography==c) %>%
      mutate(`Share-Estimate` = `Estimate`/ t.hh, `Share-MoE` = `MoE`/ t.hh) %>%
      select(Geography, Race, `Share-Estimate`, `Share-MoE`) %>%
      setNames(c("Geography","Race", "Estimate","MoE")) %>%
      mutate(Value="Share")
    
    temp <- bind_rows(temp, temp2)
 
  }

  # Convert from long to wide format
  tbl.values <- pivot_wider(temp, id_cols=Race, names_from = c(Geography,Value), values_from = c("Estimate","MoE"))  
  
  hhincome.total.col <- c("Race",
                          "Estimate_King County_Total", "MoE_King County_Total",
                          "Estimate_Kitsap County_Total", "MoE_Kitsap County_Total",
                          "Estimate_Pierce County_Total", "MoE_Pierce County_Total",
                          "Estimate_Snohomish County_Total", "MoE_Snohomish County_Total")
  
  hhincome.tot <- tbl.values[,hhincome.total.col] %>% mutate(across(where(is.numeric), label_dollar(accuracy = 1)))
  
  hhincome.share.col <- c("Race",
                          "Estimate_King County_Share", "MoE_King County_Share",
                          "Estimate_Kitsap County_Share", "MoE_Kitsap County_Share",
                          "Estimate_Pierce County_Share", "MoE_Pierce County_Share",
                          "Estimate_Snohomish County_Share", "MoE_Snohomish County_Share")
  
  hhincome.shr <- tbl.values[,hhincome.share.col] %>% mutate(across(where(is.numeric), label_percent(accuracy = 0.1))) %>%
    setNames(colnames(hhincome.tot))
  
  # Final table with values
  results[[c.yr]][["processed-data-tables"]][["table.4.median.income.by.race"]] <- rbind(hhincome.tot, hhincome.shr)
  
  temp <- results[[c.yr]][["processed-data-tables"]][["table.4.median.income.by.race"]] %>% setNames(c("",tbl4.colnames))

  results[[c.yr]][["formatted-data-tables"]][["table.4.median.income.by.race"]] <- kbl(temp, caption = paste0("Median Household Income by Race and Hispanic/Latino Origin: ", analysis.yr), booktabs = T, align='lcccccccc') %>%
    kable_styling(latex_options = "striped") %>%
    kable_styling(latex_options = "scale_down") %>%
    add_header_above(c(" " = 1, "King\n County" = 2, "Kitsap\n County" = 2, "Pierce\n County" = 2, "Snohomish\n County" = 2)) %>%
    row_spec(1:1, bold = T) %>%
    add_indent(c(2:8, 12:18), level_of_indent = 4) %>%
    add_indent(c(9:10, 19:20), level_of_indent = 1) %>%
    row_spec(11:11, bold = T) %>%
    footnote(general = paste0("Source: ", analysis.yr, " American Community Survey 1-Year Estimates")) %>%
    landscape()   
}
  
rm(hhincome.shr, hhincome.tot, tbl.values, temp, temp2)

