library(RPostgres)
library(data.table)
require(dplyr)
require(DataExplorer)
con <- dbConnect(RPostgres::Postgres(),dbname = 'reforest_app',
                 host = '159.203.56.62',
                 port = 5432, # or any other port specified by your DBA
                 user = 'postgres',
                 password = 'BECisGOD2023')

psites <- data.table(dbGetQuery(con,"select * from planting_info"))
planter <- data.table(dbGetQuery(con,"select * from planter_info"))
trials <- data.table(dbGetQuery(con,"select * from trial_info"))
plantedby <- left_join(psites,planter, by = c("planter_id" = "_id")) %>% select(planter_id.y) %>% count(planter_id.y)
fwrite(plantedby, "./outputs/GoM_planted_by_planter.csv")
fwrite(planter, "./outputs/GoM_planters.csv")
dbGetQuery(con,"select distinct species from planting_info")

psites[planter, PlanterName := i.planter_id, on = c(planter_id = "_id")]
#psites <- psites[!PlanterName %in% c("KSD","Coco","kdaust"),]
fwrite(psites, "Test_GOM.csv")

DataExplorer::create_report(psites)

load_GOM_psites <- function() {
  con <- dbConnect(RPostgres::Postgres(),dbname = 'reforest_app',
                   host = '159.203.56.62',
                   port = 5432, # or any other port specified by your DBA
                   user = 'postgres',
                   password = 'BECisGOD2023')
  
  psites <- data.table(dbGetQuery(con,"select * from planting_info"))
  psites
}
psites <- load_GOM_psites()
