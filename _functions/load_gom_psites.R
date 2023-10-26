### reads in the planting site data from the GoM server
load_gom_psites <- function() {
  con <- dbConnect(RPostgres::Postgres(),dbname = 'reforest_app',
                   host = '159.203.56.62',
                   port = 5432, # or any other port specified by your DBA
                   user = 'postgres',
                   password = 'BECisGOD2023')
  
  psites <- data.table(dbGetQuery(con,"select * from planting_info"))
  psites
}