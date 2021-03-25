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