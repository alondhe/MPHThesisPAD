#' @author                     Ajit Londhe
#' @title                      Initialize Environment
#' @details                    Initializes the study environment
#' 
#' @param scratchTablePrefix   A prefix to help tag the FF files                   
#' @export
initializeEnvironment <- function(scratchTablePrefix)
{
  ifelse(.Platform$OS.type == "windows", 
         Sys.setenv("R_ZIPCMD" = "C:/RBuildTools/3.4/bin/zip.exe"), 
         Sys.setenv(R_ZIPCMD = "/usr/bin/zip"))
  ffdir <- paste("/data" , "fftemp", Sys.getenv("USER"), scratchTablePrefix, run_id, sep = "/")
  if (!dir.exists(ffdir)) { dir.create(path = ffdir, recursive = TRUE) }
  options('fftempdir' = ffdir)
}


#' @author                            Ajit Londhe, Jamie Weaver
#' @title                             Generate Negative Controls
#' @details                           Generates the outcome cohorts needed for negative controls
#' 
#' @param cdmDb                       The CDM database object constructed with \code{buildDatabase}
#' @param scratchDb                   The scratch database object constructed with \code{buildDatabase}
#' @param dcoList                     The list of drug-comparator-outcomes
#' @param scratchTablePrefix          The prefix of all scratch tables created
#' @param negativeControlConceptIds   The negative control concept Ids
#' @param studyStartDate              The study start date
#' @export
generateNegativeControlCohorts <- function(cdmDb, scratchDb, dcoList,
                                           scratchTablePrefix, negativeControlConceptIds, studyStartDate)
{
  treatmentIds <- unique(c(lapply(dcoList, '[[', 'targetId'), lapply(dcoList, '[[', 'comparatorId')))
  
  # Create Negative Control Cohorts ---------------------------------
  
  sqlCreateNegControls <- loadRenderTranslateSql("createNegativeControlCohorts.sql", 
                                                 packageName = "MPHThesisPAD", 
                                                 dbms = scratchDb$connectionDetails$dbms,
                                                 scratchDatabaseSchema = scratchDb$scratchDatabaseSchema,
                                                 tablePrefix = paste(scratchTablePrefix, cdmDb$id, sep = "_"),
                                                 studyStartDate = studyStartDate,
                                                 cdmDatabaseSchema = cdmDb$cdmDatabaseSchema,
                                                 treatmentIds = paste(treatmentIds, collapse = ","),
                                                 negConceptIds = paste(negativeControlConceptIds, sep = "", collapse = ","))
  connection <- connect(scratchDb$connectionDetails)
  executeSql(connection = connection, sql = sqlCreateNegControls)
}


#' @author                            Ajit Londhe, Jamie Weaver
#' @title                             Run Negative Controls
#' @details                           Executes CohortMethod for negative control outcomes 
#'                                    by invoking \code{executeFullCm}
#' 
#' @param cdmDb                       The CDM database object constructed with \code{buildDatabase}
#' @param dcoList                     The list of drug-comparator-outcomes
#' @param scratchDb                   The scratch database object constructed with \code{buildDatabase}
#' @param connectionDetails           The connectionDetails object
#' @param scratchTablePrefix          The prefix of all scratch tables created
#' @param negativeControlConceptIds   The negative control concept Ids
#' @param excludedCovariateConceptIds The concept Ids to exclude in Feature Extraction
#' @param cmOutputPath                The path to the CohortMethod files
#' @param webApiPrefix                The URL prefix of the WebAPI instance
#' @param cdmVersion                  The CDM version of the CDM database
#' @param deleteCovariatesSmallCount  The minimum cell size
#' @param fitOutcomeModel             Should the outcome models be fit?
#' @export
runNegativeControls <- function(cdmDb, dcoList, scratchDb, connectionDetails, 
                                scratchTablePrefix, negativeControlConceptIds, 
                                excludedCovariateConceptIds, cmOutputPath, 
                                webApiPrefix, deleteCovariatesSmallCount,
                                fitOutcomeModel = TRUE,
                                cdmVersion = 5)
{
  negativeDcoList <- list(
    createDrugComparatorOutcomes(targetId = dcoList[[1]]$targetId, 
                                 comparatorId = dcoList[[1]]$comparatorId, 
                                 outcomeIds = negativeControlConceptIds,
                                 excludedCovariateConceptIds = excludedCovariateConceptIds),
    createDrugComparatorOutcomes(targetId = dcoList[[2]]$targetId, 
                                 comparatorId = dcoList[[2]]$comparatorId,
                                 outcomeIds = negativeControlConceptIds,
                                 excludedCovariateConceptIds = excludedCovariateConceptIds)
  )
  treatmentIds <- unique(c(lapply(dcoList, '[[', 'targetId'), lapply(dcoList, '[[', 'comparatorId')))
  
  # Execute the full Multiple Cohort Method Analyses ---------------------------------
  
  executeFullCM(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDb$cdmDatabaseSchema,
                dcoList = negativeDcoList,
                excludedCovariateConceptIds = excludedCovariateConceptIds,
                excludeCovariateIds = c(),
                cmOutputPath = paste(cmOutputPath, "negcontrols", sep = "_", collapse = ""),
                exposureDatabaseSchema = cdmDb$cdmDatabaseSchema,
                exposureTable = "cohort",
                outcomeDatabaseSchema = scratchDb$scratchDatabaseSchema,
                outcomeTable = paste(scratchTablePrefix, cdmDb$id, "negcontrols", sep = "_"), 
                errorOnHighCorrelation = TRUE,
                stopOnError = TRUE, 
                fitOutcomeModel = fitOutcomeModel,
                deleteCovariatesSmallCount = deleteCovariatesSmallCount)
}


#' @author                            Ajit Londhe
#' @title                             Run Primary Analyses
#' @details                           Executes CohortMethod for primary outcomes 
#'                                    by invoking \code{executeFullCm}
#' 
#' @param cdmDb                       The CDM database object constructed with \code{buildDatabase}
#' @param dcoList                     The list of drug-comparator-outcomes
#' @param connectionDetails           The connectionDetails object
#' @param excludedCovariateConceptIds The concept Ids to exclude in Feature Extraction
#' @param cmOutputPath                The path to the CohortMethod files
#' @param deleteCovariatesSmallCount  The minimum cell size
#' @param errorOnHighCorrelation      Should an error be thrown if high correlation is found in PS matching?
#' @param stopOnError                 Should the execution be interrupted if an error is thrown?
#' @param fitOutcomeModel             Should the outcome models be fit?
#' @param cdmVersion                  The CDM version of the CDM database
#' @export
runPrimaryAnalyses <- function(cdmDb, dcoList, connectionDetails, 
                               excludedCovariateConceptIds, cmOutputPath, 
                               excludeCovariateIds,
                               deleteCovariatesSmallCount,
                               errorOnHighCorrelation = TRUE, stopOnError = TRUE,
                               fitOutcomeModel = TRUE,
                               cdmVersion = 5)
{
  executeFullCM(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDb$cdmDatabaseSchema,
                dcoList = dcoList,
                excludedCovariateConceptIds = excludedCovariateConceptIds,
                cmOutputPath = cmOutputPath,
                exposureDatabaseSchema = cdmDb$resultsDatabaseSchema,
                exposureTable = "cohort",
                outcomeDatabaseSchema = cdmDb$resultsDatabaseSchema,
                outcomeTable = "cohort",
                fitOutcomeModel = fitOutcomeModel,
                excludeCovariateIds = excludeCovariateIds, 
                deleteCovariatesSmallCount = deleteCovariatesSmallCount,
                errorOnHighCorrelation = errorOnHighCorrelation,
                stopOnError = stopOnError) 
}

#' @author                   Ajit Londhe, Jamie Weaver
#' @title                    Create Balance File
#' @details                  Creates covariate balance file for a CDM's analyses
#' @param run_id             The run id of this execution
#' @param cdmDb              The CDM database object constructed with \code{buildDatabase}
#' @param dcoList            The list of drug-comparator-outcomes
#' @param cmOutputPath       The path to the CohortMethod files
#' @param reportsOutputPath  The path to the reports file root
#' @param onlyUnbalanced     Should only covariates with SMD > 0.1 be included?
#' 
#' @export
createBalanceFile <- function(run_id, cdmDb, dcoList, 
                              cmOutputPath, reportsOutputPath, onlyUnbalanced = FALSE)
{
  wbBalance <- createWorkbook()
  for (dco in dcoList)
  {
    for (outcomeId in dco$outcomeIds)
    {
      balance <- readRDS(paste(cmOutputPath, 
                               paste("Bal", "l1", "s1", "p1", paste0("t", dco$targetId), paste0("c", dco$comparatorId), "s1", 
                                     paste0("o", outcomeId, ".rds"), sep = "_", collapse = ""), sep = "/", collapse = ""))
      
      balance$beforeMatchingStdDiffAbs <- abs(balance$beforeMatchingStdDiff)
      balance$afterMatchingStdDiffAbs <- abs(balance$afterMatchingStdDiff)
      
      if (onlyUnbalanced)
      {
        balance <- balance[balance$afterMatchingStdDiffAbs > 0.1, ]
      }
      
      sheetName <- paste(paste0("t", dco$targetId), paste0("c", dco$comparatorId), 
                         paste0("o", outcomeId),
                         sep = "_")
      
      ws <- addWorksheet(wb = wbBalance, 
                         sheetName = sheetName)
      hs <- createStyle(textDecoration = "Bold")
      writeData(wb = wbBalance, sheet = sheetName, x = balance, 
                startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, headerStyle = hs)
      setColWidths(wb = wbBalance, sheet = sheetName, cols = 1:100, widths = "auto")
    }
  }
  fileName <- paste(run_id, "Balance", cdmDb$id, sep = "_")
  if (filtered)
  {
    fileName <- paste(fileName, "filtered", sep = "_")
  }
  saveWorkbook(wbBalance, 
               paste(reportsOutputPath, 
                     paste(
                       fileName, "xlsx", sep = ".", collapse = ""), 
                     sep = "/", collapse = ""),
               overwrite = TRUE)
}


#' @author                   Ajit Londhe
#' @title                    Create PS Outputs
#' @details                  Creates balance plots, writes propensity-matched data to scratch
#' @param connectionDetails  The connectionDetails object
#' @param dcoList            The list of drug-comparator-outcomes
#' @param cdmDb              The CDM database object constructed with \code{buildDatabase}
#' @param scratchDb          The scratch database object constructed with \code{buildDatabase}
#' @param scratchTablePrefix The prefix of all scratch tables created
#' @param cmOutputPath       The path to the CohortMethod files
#' @param reportsOutputPath  The path to the reports file root
#' @param webApiPrefix       The URL prefix of the WebAPI instance
#' @param webApiUseSsl       Does the WebAPI use SSL?
#' @param numTopCovariates   How many covariates should be included in the Top Covariates balance graph?
#' @param imageExtension     What file image extension to use?
#' @param insertCohortsToDb  Should the PS matched cohorts be inserted to the scratch database?
#' 
#' @export
generatePsOutputs <- function(connectionDetails, dcoList, cdmDb, scratchDb,
                                scratchTablePrefix, cmOutputPath, reportsOutputPath, 
                                webApiPrefix, webApiUseSsl,
                                numTopCovariates = 20, 
                                imageExtension = ".png", insertCohortsToDb = FALSE)
{
  for (dco in dcoList)
  {
    targetId <- dco$targetId
    targetLabel <- targetId
    targetLabel <- getFormattedCohortName(webApiPrefix = webApiPrefix, treatmentId = targetId, webApiUseSsl = webApiUseSsl)
    comparatorId <- dco$comparatorId
    comparatorLabel <- comparatorId
    comparatorLabel <- getFormattedCohortName(webApiPrefix = webApiPrefix, treatmentId = comparatorId, webApiUseSsl = webApiUseSsl)
    for (outcomeId in dco$outcomeIds)
    {
    
      psPop <- readRDS(paste(cmOutputPath, 
                             paste("Ps", "l1", "s1", "p1", paste0("t", targetId), paste0("c", comparatorId),
                                   paste0("o", outcomeId, ".rds"), sep = "_", collapse = ""), sep = "/", collapse = ""))
      
      plotPs(psPop, fileName = paste(reportsOutputPath, 
                                     paste("PsPlot", 
                                           paste0("t", targetId), 
                                           paste0("c", comparatorId), 
                                           paste0("o", outcomeId, imageExtension),
                                           sep = "_", collapse = ""), sep = "/", collapse = ""), treatmentLabel = targetLabel, comparatorLabel = comparatorLabel)
      
      stratPop <- readRDS(paste(cmOutputPath, 
                                paste("StratPop", "l1", "s1", "p1", paste0("t", targetId), paste0("c", comparatorId),
                                      "s1", paste0("o", outcomeId, ".rds"), sep = "_", collapse = ""), sep = "/", collapse = ""))
      
      drawAttritionDiagram(stratPop, fileName = paste(reportsOutputPath,
                                                      paste("StratPop", "l1", "s1", "p1", paste0("t", targetId), paste0("c", comparatorId),
                                                            "s1", paste0("o", outcomeId, imageExtension), sep = "_", collapse = ""), sep = "/", collapse = ""), 
                           treatmentLabel = targetLabel, comparatorLabel = comparatorLabel)
      
      cohortMethodData <- loadCohortMethodData(paste(cmOutputPath,
                                                     paste("CmData", "l1", paste0("t", targetId), paste0("c", comparatorId), sep = "_", collapse = ""),
                                                     sep = "/", collapse = ""))
      
      balance <- readRDS(paste(cmOutputPath, 
                               paste("Bal", "l1", "s1", "p1", paste0("t", targetId), paste0("c", comparatorId), "s1", 
                                     paste0("o", outcomeId, ".rds"), sep = "_", collapse = ""), sep = "/", collapse = ""))
      
      balancePlot <- paste(reportsOutputPath, 
                           paste("BalPlot", 
                                 paste0("t", targetId), 
                                 paste0("c", comparatorId),
                                 paste0("o", outcomeId, imageExtension),
                                 sep = "_", collapse = ""), sep = "/", collapse = "")
      balancePlotTop <- paste(reportsOutputPath, 
                              paste(paste0("Bal", numTopCovariates), 
                                    paste0("t", targetId), 
                                    paste0("c", comparatorId),
                                    paste0("o", outcomeId, imageExtension),
                                    sep = "_", collapse = ""), sep = "/", collapse = "")
      
      plotCovariateBalanceScatterPlot(balance = balance, fileName = balancePlot)
      plotCovariateBalanceOfTopVariables(balance = balance, fileName = balancePlotTop)
      
      if (insertCohortsToDb)
      {
        insertDbPopulation(population = stratPop, cohortIds = c(targetId, comparatorId), 
                           connectionDetails = scratchDb$connectionDetails,
                           cohortDatabaseSchema = scratchDb$scratchDatabaseSchema,
                           cohortTable = paste(scratchTablePrefix, cdmDb$id, "psm",
                                               paste0("t", targetId),
                                               paste0("c", comparatorId),
                                               paste0("o", outcomeId),
                                               sep = "_"),
                           createTable = TRUE,
                           dropTableIfExists = TRUE, cdmVersion = "5")
      }
    }
  }
}


#' @author                             Ajit Londhe, Jamie Weaver
#' @title                              Create Model Summary
#' @details                            Creates the model summary files for a specific CDM
#' @param cdmDb                        The CDM database object constructed with \code{buildDatabase}
#' @param dcoList                      The list of drug-comparator-outcomes
#' @param cmOutputPath                 The path to the CohortMethod files
#' @param negativeControlConceptIds    List of negative control concept Ids
#' @param reportsOutputPath            The path to the reports file root
#' @param subGroupList                 The list of subgroups, built with \code{buildSubGroup}
#' @param includeNegControls           Should the negative control outcome models be exported?
#' 
#' @export
createModelSummary <- function(cdmDb, dcoList, cmOutputPath, negativeControlConceptIds,
                               reportsOutputPath,
                               subGroupList = NULL,
                               includeNegControls = FALSE)
{
  wbModelSummary <- createWorkbook()
  addEmpiricalCalibration <- function(totalSummary, dcoList, negativeControlConceptIds)
  {
    outcomeIds <- unlist(unique(c(lapply(dcoList, '[[', 'outcomeIds'))))
    totalSummary <- totalSummary[complete.cases(totalSummary),]
    for (analysisId in unique(totalSummary$analysisId))
    {
      comparisons <- unique(totalSummary[totalSummary$analysisId == analysisId, c("targetId", "comparatorId")])
      for (i in 1:nrow(comparisons))
      {
        comparison <- comparisons[i,]
        negControlSubset <- totalSummary[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId & totalSummary$outcomeId %in% negativeControlConceptIds, ]
        outcomeSubset <- totalSummary[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId & totalSummary$outcomeId %in% outcomeIds, ]
        null <- EmpiricalCalibration::fitMcmcNull(negControlSubset$logRr, negControlSubset$seLogRr)
        subset <- totalSummary[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId , ]
        
        calibratedP <- EmpiricalCalibration::calibrateP(null, subset$logRr, subset$seLogRr)
        subset$calibratedP <- calibratedP$p
        subset$calibratedP_lb95ci <- calibratedP$lb95ci
        subset$calibratedP_ub95ci <- calibratedP$ub95ci
        mcmc <- attr(null, "mcmc") 
        subset$null_mean <- mean(mcmc$chain[,1])
        subset$null_sd <- 1/sqrt(mean(mcmc$chain[,2]))
        totalSummary$calibratedP[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId ] <- subset$calibratedP
        totalSummary$calibratedP_lb95ci[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId] <- subset$calibratedP_lb95ci
        totalSummary$calibratedP_ub95ci[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId] <- subset$calibratedP_ub95ci
        totalSummary$null_mean[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId] <- subset$null_mean
        totalSummary$null_sd[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId] <- subset$null_sd
        
        negatives <- totalSummary[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId & totalSummary$outcomeId %in% negativeControlConceptIds, ]
        positives <- totalSummary[totalSummary$analysisId == analysisId & totalSummary$targetId == comparison$targetId & totalSummary$comparatorId == comparison$comparatorId & totalSummary$outcomeId %in% outcomeIds, ]
        calPlot <- plotCalibrationEffect(negatives$logRr, negatives$seLogRr, positives$logRr, positives$seLogRr, null)
        calPlot <- calPlot + ggtitle(paste0("Empirical Calibration: ", getFormattedCohortName(webApiPrefix, comparison$targetId),
                                            " vs. ", getFormattedCohortName(webApiPrefix, comparison$comparatorId),
                                            "; ", cdmDb$id))
        ggsave(file = paste(reportsOutputPath, 
                            paste0(paste("calPlot", comparison$targetId, comparison$comparatorId, cdmDb$id, analysisId, sep = "_", collapse = ""), ".jpg"), sep = "/"), 
               plot = calPlot)
      } # for comparisons
    }  # for analyses
    
    # Results <- totalSummary[totalSummary$outcomeId %in% outcomeIds, ]
    # Results$calP_bonf <- p.adjust(Results$calibratedP, method = "bonferroni", n = length(Results$calibratedP))
    
    return(totalSummary)
  }
  
  treatmentIds <- unique(c(lapply(dcoList, '[[', 'targetId'), lapply(dcoList, '[[', 'comparatorId')))
  
  # Obtain Primary Outcome Models ---------------------------------------------------------
  
  outcomeModel <- readRDS(paste(cmOutputPath, "outcomeModelReference.rds", sep = "/", collapse = ""))
  totalSummary <- summarizeAnalyses(outcomeModel)
  createOutcomeModelPlots(outcomeModelSummary = totalSummary,
                          reportsOutputPath = reportsOutputPath, cmOutputPath = cmOutputPath)
  
  
  # Obtain Negative Control Outcome Models ---------------------------------------------------------
  
  if (includeNegControls)
  {
    negOutcomeModel <- readRDS(
      paste(
        paste(cmOutputPath, "negcontrols", sep = "_"), "outcomeModelReference.rds", sep = "/"))
    totalSummary <- rbind(summarizeAnalyses(negOutcomeModel), totalSummary)
    totalSummary <- addEmpiricalCalibration(totalSummary = totalSummary, 
                                            dcoList = dcoList, 
                                            negativeControlConceptIds = negativeControlConceptIds)
    
  }
  
  # Obtain Subgroup Outcome Models -------------------------------------------------------
  
  if (!is.null(subGroupList))
  {
    for (sg in subGroupList)
    {
      for (status in sg$statuses)
      {
        sgStatusOutputPath <- paste(cmOutputPath, "subgroups", sg$id, status, sep = "/")
        sgOm <- readRDS(paste(sgStatusOutputPath, "outcomeModelReference.rds", sep = "/"))
        sgOutcomeModelSummary <- summarizeAnalyses(sgOm)
        sgOutcomeModelSummary$analysisId <- paste(sgOutcomeModelSummary$analysisId, sg$id, status, sep = ", ")
        totalSummary <- rbind.fill(sgOutcomeModelSummary, totalSummary)
      }
    }
  }
  
  
  # Format Model Summary ---------------------------------------------------------
  
  for (treatmentId in treatmentIds)
  {
    if (sum(totalSummary$targetId == treatmentId) > 0)
      totalSummary[totalSummary$targetId == treatmentId,]$targetId <- 
        getFormattedCohortName(webApiPrefix, treatmentId)
    if (sum(totalSummary$comparatorId == treatmentId) > 0)
      totalSummary[totalSummary$comparatorId == treatmentId,]$comparatorId <- 
        getFormattedCohortName(webApiPrefix, treatmentId)
  }
  
  for (outcomeId in cdmDb$outcomeIds)
  {
    if (sum(totalSummary$outcomeId == outcomeId) > 0)
      totalSummary[totalSummary$outcomeId == outcomeId,]$outcomeId <- 
        getFormattedCohortName(webApiPrefix, outcomeId)
  }
  
  totalSummary$eventRateTreated <- (totalSummary$eventsTreated/totalSummary$treatedDays) * 100 * 365
  totalSummary$eventRateComparator <- (totalSummary$eventsComparator/totalSummary$comparatorDays)  * 100 * 365
  
  ws <- addWorksheet(wb = wbModelSummary, sheetName = cdmDb$id)
  hs <- createStyle(textDecoration = "Bold")
  writeData(wb = wbModelSummary, sheet = cdmDb$id, x = totalSummary, 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, headerStyle = hs)
  setColWidths(wb = wbModelSummary, sheet = cdmDb$id, cols = 1:40, widths = 
                 "auto")
  
  saveWorkbook(wbModelSummary, 
               paste(reportsOutputPath, 
                     paste(
                       paste("ModelSummary", cdmDb$id, sep = "_", collapse = ""), "xlsx", sep = ".", collapse = ""), 
                     sep = "/", collapse = ""),
               overwrite = TRUE)
}

#' @author                        Jamie Weaver, Ajit Londhe
#' @title                         Create outcome model visualizations
#' @details                       Creates follow-up time plots, KM curves, RR forest plots
#' @param outcomeIds              An array of the outcome ids
#' @param reportsOutputPath       The path pointing to the folder that should hold charts and tables
#' @param cmOutputPath            The path pointing to the folder that holds the CohortMethod assets
#' 
#' @export
createOutcomeModelPlots <- function(outcomeIds, reportsOutputPath, cmOutputPath)
{
  outcomeModel <- readRDS(paste(cmOutputPath, "outcomeModelReference.rds", sep = "/"))
  outcomeModelSummary <- summarizeAnalyses(outcomeModel)
  outcomeNames <- lapply(outcomeIds, function(outcomeId)
  {
    getFormattedCohortName(webApiPrefix = webApiPrefix, treatmentId = outcomeId)
  })
  outcomeNames <- as.character(outcomeNames)
  
  for (analysisId in unique(outcomeModelSummary$analysisId))
  {
    comparisons <- unique(outcomeModelSummary[outcomeModelSummary$analysisId == analysisId, c("targetId", "comparatorId")])
    for (i in 1:nrow(comparisons))
    {
      comparison <- comparisons[i, ]
      
      targetLabel <- getFormattedCohortName(webApiPrefix, comparison$targetId)
      comparatorLabel <- getFormattedCohortName(webApiPrefix, comparison$comparatorId)
      
      plotForest(logRr = outcomeModelSummary[outcomeModelSummary$analysisId == analysisId &
                                               outcomeModelSummary$targetId == comparison$targetId & 
                                               outcomeModelSummary$comparatorId == comparison$comparatorId, ]$logRr, 
                 seLogRr = outcomeModelSummary[outcomeModelSummary$analysisId == analysisId &
                                                 outcomeModelSummary$targetId == comparison$targetId & 
                                                 outcomeModelSummary$comparatorId == comparison$comparatorId, ]$seLogRr, 
                 names = outcomeNames,
                 title = paste0(cdmDb$id, ": ", targetLabel, " vs ", comparatorLabel),
                 fileName = paste0(reportsOutputPath, "/forestPlot_a", analysisId, "_t", comparison$targetId, "_c", comparison$comparatorId, ".jpg"))
      
      for (outcomeId in unique(outcomeModelSummary$outcomeId))
      {
        stratPop <- readRDS(paste0(cmOutputPath, "/StratPop_l1_s1_p1_t", comparison$targetId, "_c", comparison$comparatorId, "_s1_o", outcomeId, ".rds"))
        outcomeLabel <- getFormattedCohortName(webApiPrefix, outcomeId)

        CohortMethod::plotFollowUpDistribution(population = stratPop,
                                               targetLabel = targetLabel,
                                               comparatorLabel = comparatorLabel,
                                               title = paste0("Follow-up: ", cdmDb$id),
                                               fileName = paste0(reportsOutputPath,
                                                                 "/followUp_a", analysisId,
                                                                 "_t", comparison$targetId,
                                                                 "_c", comparison$comparatorId,
                                                                 "_o", outcomeId,
                                                                 ".png")) #follow-up the same for each outcome
        CohortMethod::plotKaplanMeier(stratPop,
                                      includeZero = FALSE,
                                      treatmentLabel = targetLabel,
                                      comparatorLabel = comparatorLabel,
                                      title = paste0("KM, ", outcomeLabel, ": ", cdmDb$id),
                                      fileName = paste0(reportsOutputPath,
                                                        "/KM_a", analysisId,
                                                        "_t", comparison$targetId,
                                                        "_c", comparison$comparatorId,
                                                        "_o", outcomeId,
                                                        ".png"))
      }
    } # for comparisons
  } # for analyses  
}


#' @author                        Ajit Londhe
#' @title                         Get Median Days of Follow-up time
#' @details                       Parses StratPop files to calculate the median days of follow-up time
#' @param dcoList                 The list of drug-comparator-outcomes
#' @param cmOutputPath            The path pointing to the folder that holds the CohortMethod assets
#' @param reportsOutputPath       The path pointing to the folder that should hold charts and tables
#' 
#' @export
summarizeFollowUpTime <- function(dcoList, cmOutputPath, reportsOutputPath)
{
  wb <- createWorkbook()
  
  df <- data.frame(
    targetId = integer(0),
    comparatorId = integer(0),
    outcomeId = integer(0),
    T_MedianDays = integer(0),
    C_MedianDays = integer(0)
  )
  
  for (dco in dcoList)
  {
    for (outcomeId in dco$outcomeIds)
    {
      stratPop <- readRDS(paste(cmOutputPath, 
                                paste("StratPop", "l1", "s1", "p1", 
                                      paste0("t", dco$targetId), 
                                      paste0("c", dco$comparatorId),
                                      "s1", paste0("o", outcomeId, ".rds"), 
                                      sep = "_", collapse = ""), sep = "/", collapse = ""))
      
      newDf <- data.frame(
        targetId = dco$targetId,
        comparatorId = dco$comparatorId,
        outcomeId = outcomeId,
        T_MedianDays = median(stratPop[stratPop$treatment == 1,]$timeAtRisk),
        C_MedianDays = median(stratPop[stratPop$treatment == 0,]$timeAtRisk)
      )
      df <- rbind.fill(df, newDf)
    }
  }
  
  ws <- addWorksheet(wb = wb, sheetName = cdmDb$id)
  hs <- createStyle(textDecoration = "Bold")
  writeData(wb = wb, sheet = cdmDb$id, x = df, 
            startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, headerStyle = hs)
  setColWidths(wb = wb, sheet = cdmDb$id, cols = 1:40, widths = 
                 "auto")
  
  saveWorkbook(wb, 
               paste(reportsOutputPath, 
                     paste(
                       paste("FollowUpTime", cdmDb$id, sep = "_", collapse = ""), "xlsx", sep = ".", collapse = ""), 
                     sep = "/", collapse = ""),
               overwrite = TRUE)
}
