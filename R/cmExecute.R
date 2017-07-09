#' @author                            Ajit Londhe, Jamie Weaver
#' @title                             Execute Full CohortMethod
#' @details                           Executes Cohort Method using MultipleAnalyses
#' @param connectionDetails           The connectionDetails object
#' @param cdmDatabaseSchema           The fully qualified name of the CDM database schema
#' @param dcoList                     The list of drug-comparator-outcomes
#' @param excludedCovariateConceptIds The list of covariate concept Ids to exclude
#' @param cmOutputPath                The path to the CohortMethod files
#' @param exposureDatabaseSchema      The fully qualified schema name for the exposures
#' @param outcomeTable                The name of the table with the outcome cohorts
#' @param errorOnHighCorrelation      Should CohortMethod throw an error if highly correlated covariates are detected?
#' @param stopOnError                 Should CohortMethod stop on an error?
#' @param fitOutcomeModel             Should the outcome model be fit?

#' @export
executeFullCM <- function(connectionDetails, cdmDatabaseSchema, dcoList, 
                          excludedCovariateConceptIds, cmOutputPath,
                          exposureDatabaseSchema, exposureTable,
                          outcomeDatabaseSchema, outcomeTable, 
                          fitOutcomeModel, excludeCovariateIds,
                          deleteCovariatesSmallCount,
                          errorOnHighCorrelation = TRUE,
                          stopOnError = TRUE)
{
  cores <- detectCores()
  covariateSettings <- createCovariateSettings(useCovariateDemographics = TRUE,
                                               useCovariateDemographicsGender = TRUE,
                                               useCovariateDemographicsRace = TRUE,
                                               useCovariateDemographicsEthnicity = TRUE,
                                               useCovariateDemographicsAge = TRUE, 
                                               useCovariateDemographicsYear = TRUE,
                                               useCovariateDemographicsMonth = TRUE,
                                               useCovariateConditionOccurrence = TRUE,    
                                               useCovariateConditionOccurrenceLongTerm = TRUE,
                                               useCovariateConditionOccurrenceShortTerm = TRUE,
                                               useCovariateConditionOccurrenceInptMediumTerm = TRUE,
                                               useCovariateConditionEra = TRUE, 
                                               useCovariateConditionEraEver = TRUE,
                                               useCovariateConditionEraOverlap = TRUE,
                                               useCovariateConditionGroup = TRUE,
                                               useCovariateConditionGroupMeddra = TRUE,
                                               useCovariateConditionGroupSnomed = TRUE,
                                               useCovariateDrugExposure = TRUE, 
                                               useCovariateDrugExposureLongTerm = TRUE,
                                               useCovariateDrugExposureShortTerm = TRUE, 
                                               useCovariateDrugEra = TRUE,
                                               useCovariateDrugEraLongTerm = TRUE, 
                                               useCovariateDrugEraShortTerm = TRUE,
                                               useCovariateDrugEraOverlap = TRUE, 
                                               useCovariateDrugEraEver = TRUE,
                                               useCovariateDrugGroup = FALSE, 
                                               useCovariateProcedureOccurrence = TRUE,
                                               useCovariateProcedureOccurrenceLongTerm = TRUE,
                                               useCovariateProcedureOccurrenceShortTerm = TRUE,
                                               useCovariateProcedureGroup = FALSE, 
                                               useCovariateObservation = TRUE,
                                               useCovariateObservationLongTerm = TRUE, 
                                               useCovariateObservationShortTerm = TRUE,
                                               useCovariateObservationCountLongTerm = TRUE, 
                                               useCovariateMeasurement = TRUE,
                                               useCovariateMeasurementLongTerm = TRUE, 
                                               useCovariateMeasurementShortTerm = TRUE,
                                               useCovariateMeasurementCountLongTerm = TRUE,
                                               useCovariateMeasurementBelow = TRUE,
                                               useCovariateMeasurementAbove = TRUE, 
                                               useCovariateConceptCounts = FALSE,
                                               useCovariateRiskScores = TRUE, 
                                               useCovariateRiskScoresCharlson = TRUE,
                                               useCovariateRiskScoresDCSI = TRUE, 
                                               useCovariateRiskScoresCHADS2 = TRUE,
                                               useCovariateRiskScoresCHADS2VASc = TRUE,
                                               useCovariateInteractionYear = TRUE, 
                                               useCovariateInteractionMonth = FALSE,
                                               excludedCovariateConceptIds = excludedCovariateConceptIds,
                                               deleteCovariatesSmallCount = deleteCovariatesSmallCount,
                                               addDescendantsToExclude = TRUE,
                                               longTermDays = 180,
                                               mediumTermDays = 120,
                                               shortTermDays = 30)
  
  createPsArgs <- createCreatePsArgs(excludeCovariateIds = excludeCovariateIds,
                                     stopOnError = stopOnError, 
                                     prior = createPrior("laplace", exclude = c(0), 
                                                         useCrossValidation = TRUE),
                                     control = createControl(noiseLevel = "silent", 
                                                             cvType = "auto", 
                                                             tolerance = 2e-07, 
                                                             cvRepetitions = 10, 
                                                             startingVariance = 0.01), 
                                     includeCovariateIds = c(),
                                     errorOnHighCorrelation = errorOnHighCorrelation)
  
  
  matchOnPsArgs <- createMatchOnPsArgs(caliper = 0.25, 
                                       caliperScale = "standardized",
                                       maxRatio = 1, 
                                       stratificationColumns = c())
  
  getDbCmDataArgs <- createGetDbCohortMethodDataArgs(removeDuplicateSubjects = TRUE, 
                                                     firstExposureOnly = FALSE, 
                                                     studyStartDate = "", 
                                                     studyEndDate = "", washoutPeriod = 0,
                                                     excludeDrugsFromCovariates = FALSE,
                                                     covariateSettings = covariateSettings)
  
  fitOutcomeModelArgs <- createFitOutcomeModelArgs(modelType = "cox", 
                                                   stratified = TRUE,
                                                   useCovariates = TRUE, 
                                                   excludeCovariateIds = excludeCovariateIds,
                                                   includeCovariateIds = c(), 
                                                   prior = createPrior("laplace", useCrossValidation = FALSE), 
                                                   control = createControl(cvType = "auto",
                                                                           startingVariance = 0.01, 
                                                                           tolerance = 2e-07, 
                                                                           cvRepetitions = 10, 
                                                                           noiseLevel = "noisy"))
  
  cmAnalysisList <- list(
    createCmAnalysis(analysisId = 1,
                     description = "PP", 
                     getDbCohortMethodDataArgs = getDbCmDataArgs,
                     createStudyPopArgs = createCreateStudyPopulationArgs(
                       removeSubjectsWithPriorOutcome = TRUE, 
                       firstExposureOnly = FALSE, 
                       washoutPeriod = 0, 
                       removeDuplicateSubjects = TRUE, 
                       priorOutcomeLookback = 99999, 
                       minDaysAtRisk = 1, 
                       riskWindowStart = 1,
                       riskWindowEnd = 730,
                       addExposureDaysToStart = FALSE,
                       addExposureDaysToEnd = TRUE
                     ),
                     createPs = TRUE, 
                     createPsArgs = createPsArgs, 
                     matchOnPs = TRUE, 
                     matchOnPsArgs = matchOnPsArgs,
                     computeCovariateBalance = TRUE,
                     fitOutcomeModel = fitOutcomeModel,
                     fitOutcomeModelArgs = fitOutcomeModelArgs)
  )
  
  ## execute analysis
  runCmAnalyses(connectionDetails = connectionDetails,
                cdmDatabaseSchema = cdmDatabaseSchema,
                exposureDatabaseSchema = exposureDatabaseSchema,
                exposureTable = exposureTable,
                outcomeDatabaseSchema = outcomeDatabaseSchema,
                outcomeTable = outcomeTable, 
                cdmVersion = 5,
                outputFolder = cmOutputPath,
                cmAnalysisList = cmAnalysisList,
                drugComparatorOutcomesList = dcoList,
                getDbCohortMethodDataThreads = cores,
                createPsThreads = cores, #max(1, cores/2),
                psCvThreads = cores,
                createStudyPopThreads = cores, #min(3, cores),
                trimMatchStratifyThreads = cores, #min(3, cores),
                fitOutcomeModelThreads = max(1, cores/2),
                outcomeCvThreads = cores)
}

