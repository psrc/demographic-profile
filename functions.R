# Functions used for Data Analysis and Visualization Creations

stored.procedure.from.db <- function(srv.nm, db.nm, procedure.nm) {
  
  db.con <- dbConnect(odbc::odbc(),
                      driver = "SQL Server",
                      server = srv.nm,
                      database = db.nm,
                      trusted_connection = "yes")
  
  w.tbl <- DBI::dbGetQuery(db.con, procedure.nm)
  odbc::dbDisconnect(db.con)
  as_tibble(w.tbl)
  
  return(w.tbl)
}

get.county.census <- function(c.type, c.yr, c.table, c.geo="county:033,035,053,061", c.state="state:53", l.yr = label.yr) {
  
  # Download Table from API
  tbl.values <- suppressWarnings(getCensus(name = c.type, vintage = c.yr, vars = c("NAME",paste0("group(",c.table,")")),region = c.geo, regionin = c.state) %>%
    select(ends_with(c("E","M"))) %>%
    select(-state) %>%
    rename(Geography=NAME) %>%
    pivot_longer(cols=contains("_"), names_to="name", values_to="value") %>%
    mutate(Geography = str_replace(Geography, ", Washington", "")))
  
  # Get variable labels 
  tbl.vars <- listCensusMetadata(name = c.type, vintage = l.yr, type = "variables", group = c.table) %>%
    filter(grepl("(E|M)$", name)) %>%
    select(name,label) %>%
    mutate(label = gsub("!!"," ", label), label = gsub("Margin of Error","MoE", label), label = gsub(" Total:",":", label))
  
  # JOin values and labels
  tbl.values <- inner_join(tbl.values, tbl.vars, by="name") %>%
    select(-name) %>%
    pivot_wider(names_from = label)
  
  tbl.values[tbl.values == -555555555 ] <- 0
  
  # Add total for region with calculated MoE for county to region aggregation
  region.moe <- suppressWarnings(tbl.values %>% select(contains("MoE")) %>% mutate(PSRC=1) %>% group_by(PSRC) %>% summarise_all(moe_sum))
  region.tot <- tbl.values %>% select(!contains("MOE"),-Geography) %>% mutate(PSRC=1) %>% group_by(PSRC) %>% summarise_all(sum) 
  region <- inner_join(region.tot,region.moe,by="PSRC") %>% mutate(Geography="Central Puget Sound") %>% select(-PSRC)
  
  # Append Region Total to table
  tbl.values <- bind_rows(tbl.values,region)
  
  return(tbl.values)
  
}

return.value <-function(data=results, c.geo=c, c.year=c.yr, acs.typ, c.tbl, c.val ) {
  
  r <- data[[c.year]][['tables']][[acs.typ]][[c.tbl]] %>%
    filter(Geography %in% c.geo) %>%
    pull(c.val) %>%
    sum()
  
  return(r)
}