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
    mutate(across(is.numeric, label_comma(accuracy = 1)))
  
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
  
  results[[c.yr]][["formatted-data-tables"]][["table.1.pop.by.race"]] <- kbl(temp, caption = paste0("Population and Race by Hispanic/Latino Origin: ", c.yr), booktabs = T) %>%
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
    #%>% column_spec(column=2:15, width="5em")
  
} #end of year loop

rm(pop.by.race, pop.by.race.hispanic.origin, temp, temp2)
