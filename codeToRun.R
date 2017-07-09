
# Initialize environment ---------------------------------------------------------------------

user <- Sys.getenv("cdmUser")
password <- Sys.getenv("cdmPassword")
port <- Sys.getenv("cdmServerPort")
server <- Sys.getenv("cdmServer")
webApiPrefix <- Sys.getenv("webApiPrefix")
webApiUseSsl <- Sys.getenv("webApiUseSsl")
scratchTablePrefix = Sys.getenv("scratchTablePrefix")
initializeEnvironment(scratchTablePrefix)

run_id <- 1
errorOnHighCorrelation <- TRUE
stopOnError <- TRUE
fitOutcomeModel <- TRUE
deleteCovariatesSmallCount = 100

cmRootPath <- paste0("output/cohortMethod/run_", run_id)
reportsRootPath <- paste0("output/reports/run_", run_id)
if (!dir.exists(cmRootPath)) { dir.create(path = cmRootPath, recursive = TRUE) }
if (!dir.exists(reportsRootPath)) { dir.create(path = reportsRootPath, recursive = TRUE) }

# Initialize cohorts and concept sets ---------------------------------------------------------

  ## see inst/sql/sql_server/cohorts for SQL defintions of all cohorts
  ## see inst/json for Atlas JSON objects for cohorts and concept sets

cilo  <- 3732
stent <- 3733
ath   <- 3734

outcomeIds <- c(2357, 3177) # Primary, Secondary Outcomes

excludeCovariateIds <- c()

excludedCovariateConceptIds <- c()
for (csvFile in list.files("inst/csv/excludedCovariates", full.names = TRUE))
{
  excludedCovariateConceptIds <- c(excludedCovariateConceptIds, 
                                   (read.csv(file = csvFile, header = TRUE))$CONCEPT_ID)
}

negativeControlConceptIds <- getIncludedConcepts(webApiPrefix = webApiPrefix, 
                                                 conceptSetId = 4719, webApiUseSsl = FALSE)

dcoList <- list(
  createDrugComparatorOutcomes(targetId = ath, comparatorId = cilo,
                               outcomeIds = outcomeIds,
                               excludedCovariateConceptIds = excludedCovariateConceptIds),
  createDrugComparatorOutcomes(targetId = stent, comparatorId = cilo,
                               outcomeIds = outcomeIds,
                               excludedCovariateConceptIds = excludedCovariateConceptIds)
)

# Initialize databases ----------------------------------------------------------------------

scratchDb <- buildDatabase(id = "scratch", 
                           dbms = "pdw",
                           dbName = "scratch", 
                           server = server, port = port, 
                           scratchDatabaseSchema = "scratch.dbo", 
                           user = user, 
                           password = password)
dbList <- list(
  buildDatabase(id = "TRUVENCCAE", dbms = "pdw", dbName = "cdm_truven_ccae_v568",
                server = server, port = port, cdmDatabaseSchema = "cdm_truven_ccae_v568.dbo",
                resultsDatabaseSchema = "cdm_truven_ccae_v568.dbo",
                scratchDatabaseSchema = "scratch.dbo", user = user, password = password),
  buildDatabase(id = "OPTUMDOD", dbms = "pdw", dbName = "cdm_optum_extended_dod_v572",
                server = server, port = port, cdmDatabaseSchema = "cdm_optum_extended_dod_v572.dbo",
                resultsDatabaseSchema = "cdm_optum_extended_dod_v572.dbo",
                scratchDatabaseSchema = "scratch.dbo", user = user, password = password),
  buildDatabase(id = "TRUVENMDCR", dbms = "pdw", dbName = "cdm_truven_mdcr_v570",
                server = server, port = port, cdmDatabaseSchema = "cdm_truven_mdcr_v570.dbo",
                resultsDatabaseSchema = "cdm_truven_mdcr_v570.dbo",
                scratchDatabaseSchema = "scratch.dbo", user = user, password = password)
)

# Iterate through databases to run study ---------------------------------------------------------

for (cdmDb in dbList)
{
  cmOutputPath <- paste(cmRootPath, cdmDb$id, sep = "/")
  if (!dir.exists(cmOutputPath)) { dir.create(path = cmOutputPath, recursive = TRUE) }
  
  reportsOutputPath <- paste(reportsRootPath, cdmDb$id, sep = "/")
  if (!dir.exists(reportsOutputPath)) { dir.create(path = reportsOutputPath, recursive = TRUE) }
  
  generateNegativeControlCohorts(cdmDb = cdmDb,
                                 dcoList = dcoList,
                                 scratchDb = scratchDb,
                                 connectionDetails = cdmDb$connectionDetails,
                                 scratchTablePrefix = scratchTablePrefix,
                                 negativeControlConceptIds = negativeControlConceptIds,
                                 studyStartDate = '2011-06-01')

  runNegativeControls(cdmDb = cdmDb,
                      dcoList = dcoList,
                      scratchDb = scratchDb,
                      connectionDetails = cdmDb$connectionDetails,
                      scratchTablePrefix = scratchTablePrefix,
                      negativeControlConceptIds = negativeControlConceptIds,
                      excludedCovariateConceptIds = excludedCovariateConceptIds,
                      cmOutputPath = cmOutputPath,
                      webApiPrefix = webApiPrefix,
                      deleteCovariatesSmallCount = deleteCovariatesSmallCount,
                      cdmVersion = cdmVersion,
                      fitOutcomeModel = fitOutcomeModel)

  runPrimaryAnalyses(cdmDb = cdmDb,
                     dcoList = dcoList,
                     connectionDetails = cdmDb$connectionDetails,
                     scratchTablePrefix = scratchTablePrefix,
                     outcomeIds = outcomeIds,
                     excludedCovariateConceptIds = excludedCovariateConceptIds,
                     cmOutputPath = cmOutputPath,
                     webApiPrefix = webApiPrefix,
                     webApiUseSsl = webApiUseSsl,
                     fitOutcomeModel = fitOutcomeModel,
                     deleteCovariatesSmallCount = deleteCovariatesSmallCount,
                     excludeCovariateIds = excludeCovariateIds,
                     errorOnHighCorrelation = errorOnHighCorrelation,
                     stopOnError = stopOnError)

  createBalanceFile(run_id = run_id,
                    cdmDb = cdmDb,
                    dcoList = dcoList,
                    cmOutputPath = cmOutputPath,
                    reportsOutputPath = reportsOutputPath,
                    filtered = TRUE)

  generatePsOutputs(connectionDetails = cdmDb$connectionDetails,
                    dcoList = dcoList, cdmDb = cdmDb,
                    scratchTablePrefix = scratchTablePrefix,
                    cmOutputPath = cmOutputPath,
                    reportsOutputPath = reportsOutputPath,
                    webApiPrefix = webApiPrefix,
                    webApiUseSsl = webApiUseSsl,
                    insertCohortsToDb = FALSE)

  createModelSummary(cdmDb = cdmDb,
                     dcoList = dcoList,
                     cmOutputPath = cmOutputPath,
                     reportsOutputPath = reportsOutputPath,
                     negativeControlConceptIds = negativeControlConceptIds,
                     subGroupList = NULL,
                     includeNegControls = TRUE)

  createOutcomeModelPlots(outcomeIds = outcomeIds,
                          reportsOutputPath = reportsOutputPath,
                          cmOutputPath = cmOutputPath)
  
  createTable1(webApiPrefix = webApiPrefix,
               cdmDb = cdmDb,
               scratchDb = scratchDb,
               dcoList = dcoList,
               cmOutputPath = cmOutputPath,
               scratchTablePrefix = scratchTablePrefix)
  
  summarizeFollowUpTime(dcoList = dcoList,
                        cmOutputPath = cmOutputPath,
                        reportsOutputPath = reportsOutputPath)
  
}

