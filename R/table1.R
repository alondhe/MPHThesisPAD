#' @author                    Ajit Londhe, Jamie Weaver
#' @title                     Build Covariate
#' @details                   Builds a Covariate for supporting evidence
#' @param label               The name of the covariate
#' @param covariateTable      The name of the table holding covariate scores or categories
#' @param scoreColumn         (Optional) The name of the column that holds the covariate's continuous score (if available)
#' @param categoryColumn      (Optional) The name of the column that holds the covariate's categorical levels (if available)
#' @param conceptId           (Optional) The Concept Ids of the covariate
#' @param domainId            (Optional) The domain Id (as defined in the OMOP Vocabulary) of the concept
#' 
#' @return                    A covariate object
#' @export
buildCovariate <- function(label, covariateTable = NULL, scoreColumn = NULL, categoryColumn = NULL, 
                           conceptIds = NULL, 
                           domainId = NULL)
{
  covariate <- {}  
  covariate$label <- label
  covariate$covariateTable <- covariateTable
  covariate$scoreColumn <- scoreColumn
  covariate$categoryColumn <- categoryColumn
  covariate$conceptIds <- conceptIds
  
  covariate$domainId <- domainId
  covariate <- assignDomainSyntax(object = covariate)
  
  return(covariate)
}

#' @author                    Ajit Londhe
#' @title                     Assign Domain Syntax
#' @details                   Assigns the domainTable and start date syntax based on a domainId
#' @param object              An object that will store the domainTable and domainStartDate names, requires a domainId attribute assigned
#' 
#' @return                    An object with domainTable and domainStartDate assigned
#' @export
assignDomainSyntax <- function(object)
{
  if (!is.null(object$domainId))
  {
    domainId <- tolower(object$domainId)
    
    if (domainId == "condition")
    {
      object$domainTable <- "condition_occurrence"
    }
    if (domainId == "drug")
    {
      object$domainTable <- "drug_exposure"
    }
    if (domainId == "procedure")
    {
      object$domainTable <- "procedure_occurrence"
    }
    if (domainId == "device")
    {
      object$domainTable <- "device_exposure"
    }
    if (domainId == "measurement")
    {
      object$domainTable <- "measurement"
    }
    if (domainId == "observation")
    {
      object$domainTable <- "observation"
    }
    if (domainId == "procedure")
    {
      object$domainTable <- "procedure_occurrence"
    }
    if (domainId == "specimen")
    {
      object$domainTable <- "specimen"
    }
    
    if (object$domainTable %in% c("drug_exposure", "device_exposure"))
    {
      object$domainStartDate <- paste(tolower(object$domainTable), "start", "date", sep = "_", collapse = "")
    }
    
    if (object$domainTable %in% c("condition_occurrence"))
    {
      object$domainStartDate <- paste(tolower(object$domainId), "start", "date", sep = "_", collapse = "")
    }
    if (object$domainTable %in% c("measurement", "observation", "procedure_occurrence", "specimen"))
    {
      object$domainStartDate <- paste(tolower(object$domainId), "date", sep = "_", collapse = "")
    }
  }
  return(object)
}

#' @author                     Ajit Londhe, Jamie Weaver
#' @title                      Get Table 1
#' @details                    Generates and exports a Table 1 for all drug-comparator-outcomes
#' @param webApiPrefix         The URL prefix of the WebAPI instance
#' @param webApiUseSsl         Does the WebAPI use HTTPS?
#' @param cdmDb                The CDM database object constructed with \code{buildDatabase}
#' @param scratchDb            The scratch database object constructed with \code{buildDatabase}
#' @param scratchTablePrefix   The prefix of all scratch tables created
#' @param dcoList              The list of drug-comparator-outcomes
#' @param cmOutputPath         The path to the CohortMethod files
#' 
#' @export
createTable1 <- function(webApiPrefix, 
                         webApiUseSsl = FALSE,
                         cdmDb, 
                         scratchDb, 
                         scratchTablePrefix, 
                         dcoList, 
                         cmOutputPath)
{
  # obtain Std Difference of Mean for continuous variables
  getStdDiffContinuous <- function(C_Scores, T_Scores)
  {
    if (length(C_Scores) == 0 | length(T_Scores) == 0) { return(NA)}
    sd <- sqrt((sd(T_Scores) ^ 2 + sd(C_Scores) ^ 2) / 2)
    stddiff <- round((mean(T_Scores) - mean(C_Scores))/sd, 3)
    return(stddiff)
  }
  
  # obtain Std Difference of Mean for categorical variables
  getStdDiffCategorical <- function(C_Scores, T_Scores)
  {
    if (length(C_Scores) == 0 | length(T_Scores) == 0) { return(NA)}
    
    C_Mean <- mean(C_Scores)
    T_Mean <- mean(T_Scores)
    denom <- sd(append(C_Scores, T_Scores))
    stddiff <- round((C_Mean - T_Mean)/denom, 3)
    return(stddiff)  
  }
  

  generateCountValues <- function(label, outcomeName, countData = NULL, categoryData = NULL, target = 1, 
                                  C_BeforeDenom, T_BeforeDenom, C_AfterDenom, T_AfterDenom, calcStdDiffs = FALSE)
  {
    if (missing(categoryData))
    {
      C_BeforeMatching <- countData[countData$TREATMENT == 0 & countData$SCORE == target, ]
      T_BeforeMatching <- countData[countData$TREATMENT == 1 & countData$SCORE == target, ]
      C_AfterMatching <- countData[countData$TREATMENT == 0 & countData$AFTER_MATCH == 1 & countData$SCORE == target, ]
      T_AfterMatching <- countData[countData$TREATMENT == 1 & countData$AFTER_MATCH == 1 & countData$SCORE == target, ]
    }
    else
    {
      C_BeforeMatching <- categoryData[categoryData$TREATMENT == 0 & categoryData$CATEGORY == target, ]
      T_BeforeMatching <- categoryData[categoryData$TREATMENT == 1 & categoryData$CATEGORY == target, ]
      C_AfterMatching <- categoryData[categoryData$TREATMENT == 0 & categoryData$AFTER_MATCH == 1 & categoryData$CATEGORY == target, ]
      T_AfterMatching <- categoryData[categoryData$TREATMENT == 1 & categoryData$AFTER_MATCH == 1 & categoryData$CATEGORY == target, ]
    }
    
    if (calcStdDiffs)
    {
      C_BeforeMatchingScores <- categoryData[categoryData$TREATMENT == 0, ]
      C_BeforeMatchingScores$SCORE <- 0
      if (nrow(C_BeforeMatchingScores[C_BeforeMatchingScores$CATEGORY == target, ]) > 0)
      {
        C_BeforeMatchingScores[C_BeforeMatchingScores$CATEGORY == target, ]$SCORE <- 1
      }
      
      T_BeforeMatchingScores <- categoryData[categoryData$TREATMENT == 1, ]
      T_BeforeMatchingScores$SCORE <- 0
      if (nrow(T_BeforeMatchingScores[T_BeforeMatchingScores$CATEGORY == target, ]) > 0)
      {
        T_BeforeMatchingScores[T_BeforeMatchingScores$CATEGORY == target, ]$SCORE <- 1
      }
      
      C_AfterMatchingScores <- categoryData[categoryData$TREATMENT == 0 & categoryData$AFTER_MATCH == 1, ]
      C_AfterMatchingScores$SCORE <- 0
      if (nrow(C_AfterMatchingScores[C_AfterMatchingScores$CATEGORY == target, ]) > 0)
      {
        C_AfterMatchingScores[C_AfterMatchingScores$CATEGORY == target, ]$SCORE <- 1
      }
      
      T_AfterMatchingScores <- categoryData[categoryData$TREATMENT == 1 & categoryData$AFTER_MATCH == 1, ]
      T_AfterMatchingScores$SCORE <- 0
      if (nrow(T_AfterMatchingScores[T_AfterMatchingScores$CATEGORY == target, ]) > 0)
      {
        T_AfterMatchingScores[T_AfterMatchingScores$CATEGORY == target, ]$SCORE <- 1
      }
    }
    
    newValue <- data.frame(
      Characteristic = label,
      Outcome = outcomeName,
      T_BeforeN = nrow(T_BeforeMatching),
      T_BeforePct = round((nrow(T_BeforeMatching) / T_BeforeDenom) * 100, 3),
      C_BeforeN = nrow(C_BeforeMatching),
      C_BeforePct = round((nrow(C_BeforeMatching) / C_BeforeDenom) * 100, 3),
      BeforeStdDiff = ifelse(calcStdDiffs, getStdDiffCategorical(C_Scores = C_BeforeMatchingScores$SCORE, 
                                                                 T_Scores = T_BeforeMatchingScores$SCORE), NA),
      T_AfterN = nrow(T_AfterMatching),
      T_AfterPct = round((nrow(T_AfterMatching) / T_AfterDenom) * 100, 3),
      C_AfterN = nrow(C_AfterMatching),
      C_AfterPct = round((nrow(C_AfterMatching) / C_AfterDenom) * 100, 3),
      AfterStdDiff = ifelse(calcStdDiffs, getStdDiffCategorical(C_Scores = C_AfterMatchingScores$SCORE, 
                                                                 T_Scores = T_AfterMatchingScores$SCORE), NA)
    )
    return(newValue)
  }
  
  generateContinuousValues <- function(label, outcomeName, scoreData)
  {
    metrics <- c("mean", "sd", "median", "min", "max")
    scoreValues <- data.frame(
      Characteristic = label,
      Outcome = outcomeName,
      C_BeforeN = NA,
      C_BeforePct = NA,
      T_BeforeN = NA,
      T_BeforePct = NA,
      BeforeStdDiff = NA,
      C_AfterN = NA, 
      C_AfterPct = NA,
      T_AfterN = NA, 
      T_AfterPct = NA,
      AfterStdDiff = NA)
    
    C_BeforeMatchingScores <- scoreData[scoreData$TREATMENT == 0, ]$SCORE
    T_BeforeMatchingScores <- scoreData[scoreData$TREATMENT == 1, ]$SCORE
    C_AfterMatchingScores <- scoreData[scoreData$TREATMENT == 0 & scoreData$AFTER_MATCH == 1, ]$SCORE
    T_AfterMatchingScores <- scoreData[scoreData$TREATMENT == 1 & scoreData$AFTER_MATCH == 1, ]$SCORE
    
    for (metric in metrics)
    {
      newValue <- data.frame(
        Characteristic = metric,
        Outcome = outcomeName,
        C_BeforeN = do.call(what = metric, list(x = C_BeforeMatchingScores)),
        C_BeforePct = NA,
        T_BeforeN = do.call(what = metric, list(x = T_BeforeMatchingScores)),
        T_BeforePct = NA,
        BeforeStdDiff = ifelse(metric == "mean", getStdDiffContinuous(C_Scores = C_BeforeMatchingScores, 
                                                                      T_Scores = T_BeforeMatchingScores), NA),
        C_AfterN = do.call(what = metric, list(x = C_AfterMatchingScores)),
        C_AfterPct = NA,
        T_AfterN = do.call(what = metric, list(x = T_AfterMatchingScores)),
        T_AfterPct = NA,
        AfterStdDiff = ifelse(metric == "mean", getStdDiffContinuous(C_Scores = C_AfterMatchingScores, 
                                                                     T_Scores = T_AfterMatchingScores), NA)
      )
      scoreValues <- rbind.fill(scoreValues, newValue)
    }
    
    return(scoreValues)
  }
  
  generateCategoricalValues <- function(label, outcomeName, categoryData, C_BeforeDenom, 
                                        T_BeforeDenom, C_AfterDenom, T_AfterDenom)
  {
    categoricalValues <- data.frame(
      Characteristic = label,
      Outcome = outcomeName,
      C_BeforeN = NA,
      C_BeforePct = NA,
      T_BeforeN = NA,
      T_BeforePct = NA,
      BeforeStdDiff = NA,
      C_AfterN = NA, 
      C_AfterPct = NA,
      T_AfterN = NA, 
      T_AfterPct = NA,
      AfterStdDiff = NA)
      
    categories <- sort(unique(categoryData$CATEGORY))
    
    for (category in categories)
    {
      categoricalValues <- rbind.fill(categoricalValues, 
                                      generateCountValues(label = category, 
                                                          outcomeName = outcomeName,
                                                          categoryData = categoryData, 
                                                          target = category,
                                                          C_BeforeDenom = C_BeforeDenom,
                                                          T_BeforeDenom = T_BeforeDenom,
                                                          C_AfterDenom = C_AfterDenom,
                                                          T_AfterDenom = T_AfterDenom,
                                                          calcStdDiffs = TRUE))
    }
    
    return(categoricalValues)
  }
  
  wbTable1 <- createWorkbook()
  for (dco in dcoList)
  {
    comparatorName <- "Cilo"
    if (dco$targetId == 3733)
    {
      targetName <- "Stent"
    }
    else
    {
      targetName <- "Ath"
    }
    
    # generate categorical statistics -----------------------------------------------
    
    generateAgeGroups(dcoList = dcoList, cdmDb = cdmDb, 
                      scratchDb = scratchDb, scratchTablePrefix = scratchTablePrefix)
    generateChadsScores(dcoList = dcoList, 
                        cdmDb = cdmDb, scratchDb = scratchDb, scratchTablePrefix = scratchTablePrefix)
    generateDayswithPAD(webApiPrefix = webApiPrefix, 
                        webApiUseSsl = webApiUseSsl, dcoList = dcoList, cdmDb = cdmDb, 
                        scratchDb = scratchDb, scratchTablePrefix = scratchTablePrefix)
    
    table1 <- data.frame(
      Characteristic = character(0),
      Outcome = character(0),
      T_BeforeN = integer(0),
      T_BeforePct = integer(0),
      C_BeforeN = integer(0),
      C_BeforePct = numeric(0),
      BeforeStdDiff = numeric(0),
      T_AfterN = integer(0), 
      T_AfterPct = numeric(0),
      C_AfterN = integer(0), 
      C_AfterPct = numeric(0),
      AfterStdDiff = numeric(0))
    
    
    covariates <- list(
      buildCovariate(label = "Age at Index (years)",
                     covariateTable = "cohort_agegroups",
                     scoreColumn = "age_at_index"),
      buildCovariate(label = "Age groups",
                     covariateTable = "cohort_agegroups",
                     categoryColumn = "age_group"),
      buildCovariate(label = "CHADS2",
                     covariateTable = "chads2",
                     scoreColumn = "score"),
      buildCovariate(label = "CHADS2Vasc",
                     covariateTable = "chads2vasc",
                     scoreColumn = "score"),
      buildCovariate(label = "Days with PAD",
                     covariateTable = "pad_days",
                     scoreColumn = "score"),

      buildCovariate(label = "Hypertension",
                     domainId = "Condition",
                     conceptIds = getIncludedConcepts(webApiPrefix = webApiPrefix,
                                                      conceptSetId = 4602, webApiUseSsl = webApiUseSsl)),

      buildCovariate(label = "Smoking",
                     domainId = "Condition",
                     conceptIds = getIncludedConcepts(webApiPrefix = webApiPrefix,
                                                      conceptSetId = 3696, webApiUseSsl = webApiUseSsl)),

      buildCovariate(label = "Hyperlipidemia",
                     domainId = "Condition",
                     conceptIds = getIncludedConcepts(webApiPrefix = webApiPrefix,
                                                      conceptSetId = 4604, webApiUseSsl = webApiUseSsl)),

      buildCovariate(label = "Statins",
                     domainId = "Drug",
                     conceptIds = getIncludedConcepts(webApiPrefix = webApiPrefix,
                                                      conceptSetId = 4675, webApiUseSsl = webApiUseSsl)),

      buildCovariate(label = "clopidogrel",
                     domainId = "Drug",
                     conceptIds = getIncludedConcepts(webApiPrefix = webApiPrefix,
                                                      conceptSetId = 2440, webApiUseSsl = webApiUseSsl)),

      buildCovariate(label = "aspirin",
                     domainId = "Drug",
                     conceptIds = getIncludedConcepts(webApiPrefix = webApiPrefix,
                                                      conceptSetId = 2436, webApiUseSsl = webApiUseSsl))
      
    )
    
    # Calculate Values for each continuous and categorical covariate ------------------------------------------------------------
    
    for (covariate in covariates)
    {
      for (outcomeId in dco$outcomeIds)
      {
        afterMatchingTable <- paste(scratchTablePrefix, cdmDb$id, "psm", 
                                    paste0("t", dco$targetId),
                                    paste0("c", dco$comparatorId),
                                    paste0("o", outcomeId),
                                    sep = "_")
        
        # Concept-Based Covariates ----------------------------------
        if (!is.null(covariate$conceptIds))
        {
          sql <- loadRenderTranslateSql(sqlFilename = "getComorbidities.sql",
                                        packageName = "MPHThesisPAD",
                                        dbms = scratchDb$dbms,
                                        cdmDatabaseSchema = cdmDb$cdmDatabaseSchema,
                                        scratchDatabaseSchema = scratchDb$scratchDatabaseSchema, 
                                        targetId = dco$targetId, 
                                        comparatorId = dco$comparatorId,
                                        tablePrefix = paste(scratchTablePrefix, cdmDb$id, sep = "_"),
                                        afterMatchingTable = afterMatchingTable,
                                        conceptIds = covariate$conceptIds,
                                        domainId = covariate$domainId,
                                        domainTable = covariate$domainTable,
                                        domainStartDate = covariate$domainStartDate)
          
          connection <- connect(connectionDetails = scratchDb$connectionDetails)
          countData <- querySql(connection = connection, sql = sql)
          
          C_BeforeDenom <- nrow(countData[countData$TREATMENT == 0, ])
          T_BeforeDenom <- nrow(countData[countData$TREATMENT == 1, ])
          C_AfterDenom <- nrow(countData[countData$TREATMENT == 0 & countData$AFTER_MATCH == 1, ])
          T_AfterDenom <- nrow(countData[countData$TREATMENT == 1 & countData$AFTER_MATCH == 1, ])
          
          table1 <- rbind.fill(table1, generateCountValues(label = covariate$label, 
                                                           outcomeName = getFormattedCohortName(webApiPrefix = webApiPrefix, treatmentId = outcomeId),
                                                           countData = countData, 
                                                           C_BeforeDenom = C_BeforeDenom, T_BeforeDenom = T_BeforeDenom, 
                                                           C_AfterDenom = C_AfterDenom, T_AfterDenom = T_AfterDenom))
        }
        else
        {
          # Continuous Covariates ----------------------------------
          if (!is.null(covariate$scoreColumn))
          {
            sql <- loadRenderTranslateSql(sqlFilename = "getCovariates.sql",
                                          packageName = "MPHThesisPAD",
                                          dbms = scratchDb$dbms,
                                          scratchDatabaseSchema = scratchDb$scratchDatabaseSchema,
                                          targetId = dco$targetId,
                                          comparatorId = dco$comparatorId,
                                          tablePrefix = paste(scratchTablePrefix, cdmDb$id, sep = "_"),
                                          afterMatchingTable = afterMatchingTable,
                                          covariateTable = covariate$covariateTable,
                                          scoreColumn = covariate$scoreColumn)
            
            connection <- connect(connectionDetails = scratchDb$connectionDetails)
            scoreData <- querySql(connection = connection, sql = sql)
            
            table1 <- rbind.fill(table1, generateContinuousValues(label = covariate$label, 
                                                                  outcomeName = getFormattedCohortName(webApiPrefix = webApiPrefix, treatmentId = outcomeId),
                                                                  scoreData = scoreData))
          }
          
          # Categorical Covariates ----------------------------------
          if (!is.null(covariate$categoryColumn))
          {
            sql <- loadRenderTranslateSql(sqlFilename = "getCovariates.sql",
                                          packageName = "MPHThesisPAD",
                                          dbms = scratchDb$dbms,
                                          scratchDatabaseSchema = scratchDb$scratchDatabaseSchema,
                                          targetId = dco$targetId,
                                          comparatorId = dco$comparatorId,
                                          tablePrefix = paste(scratchTablePrefix, cdmDb$id, sep = "_"),
                                          afterMatchingTable = afterMatchingTable,
                                          covariateTable = covariate$covariateTable,
                                          scoreColumn = covariate$scoreColumn,
                                          categoryColumn = covariate$categoryColumn)
            
            connection <- connect(connectionDetails = scratchDb$connectionDetails)
            categoryData <- querySql(connection = connection, sql = sql)
            
            C_BeforeDenom <- nrow(categoryData[categoryData$TREATMENT == 0, ])
            T_BeforeDenom <- nrow(categoryData[categoryData$TREATMENT == 1, ])
            C_AfterDenom <- nrow(categoryData[categoryData$TREATMENT == 0 & categoryData$AFTER_MATCH == 1, ])
            T_AfterDenom <- nrow(categoryData[categoryData$TREATMENT == 1 & categoryData$AFTER_MATCH == 1, ])
            
            table1 <- rbind.fill(table1, generateCategoricalValues(label = covariate$label, 
                                                                   outcomeName = getFormattedCohortName(webApiPrefix = webApiPrefix, treatmentId = outcomeId),
                                                                   categoryData = categoryData,
                                                                   C_BeforeDenom = C_BeforeDenom, T_BeforeDenom = T_BeforeDenom, 
                                                                   C_AfterDenom = C_AfterDenom, T_AfterDenom = T_AfterDenom))
          }
          
        }
      }
    }
    
    round_df <- function(df, digits) {
      nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
      df[,nums] <- round(df[,nums], digits = digits)
      (df)
    }
    
    table1 <- round_df(table1, 2)
                   
    
    
    analysisName <- paste(cdmDb$id, paste(targetName, comparatorName, sep = " vs " ), sep = " - ")
    ws <- addWorksheet(wb = wbTable1, sheetName = analysisName)
    hs <- createStyle(textDecoration = "Bold")
    writeData(wb = wbTable1, sheet = analysisName, x = table1,  
              startCol = 1, startRow = 1, colNames = TRUE, rowNames = FALSE, headerStyle = hs)
    setColWidths(wb = wbTable1, sheet = analysisName, cols = 1:40, widths = "auto")
  }
  
  saveWorkbook(wbTable1, 
             paste(reportsOutputPath, paste(paste("Table1", cdmDb$id, sep = "_"), "xlsx", sep = "."), sep = "/"),
             overwrite = TRUE)
  
}


#' @title                               Generates Age Groups
#' @author                              Ajit Londhe, Jamie Weaver
#' @details                             Generates age groups
#' @param dcoList                       The list of drug-comparator-outcomes
#' @param cdmDb                         The CDM database object constructed with \code{buildDatabase}
#' @param scratchDb                     The scratch database object constructed with \code{buildDatabase}
#' @param scratchTablePrefix            The prefix of all scratch tables created
#' 
#' @export
generateAgeGroups <- function(dcoList, cdmDb, scratchDb, scratchTablePrefix)
{
  connection <- connect(scratchDb$connectionDetails)
  treatmentIds <- unique(c(lapply(dcoList, '[[', 'targetId'), lapply(dcoList, '[[', 'comparatorId')))
  
  sql <- loadRenderTranslateSql(sqlFilename = "generateAgeGroups.sql", 
                                packageName = "MPHThesisPAD", 
                                dbms = connectionDetails$dbms,
                                cdmDatabaseSchema = cdmDb$cdmDatabaseSchema,
                                scratchDatabaseSchema = scratchDb$scratchDatabaseSchema,
                                tablePrefix = paste(scratchTablePrefix, cdmDb$id, sep = "_"),
                                cohortDefinitionIds = paste(treatmentIds, sep = ","))
  executeSql(connection = connection, sql = sql)
  
}

#' @title                               Generates Days with PAD
#' @author                              Ajit Londhe
#' @details                             Generates Days with PAD
#' @param webApiPrefix                  The URL prefix of the WebAPI instance
#' @param webApiUseSsl                  Does the WebAPI use HTTPS?
#' @param dcoList                       The list of drug-comparator-outcomes
#' @param cdmDb                         The CDM database object constructed with \code{buildDatabase}
#' @param scratchDb                     The scratch database object constructed with \code{buildDatabase}
#' @param scratchTablePrefix            The prefix of all scratch tables created
#' 
#' @export
generateDayswithPAD <- function(webApiPrefix, webApiUseSsl, dcoList, 
                                cdmDb, scratchDb, scratchTablePrefix)
{
  connection <- connect(scratchDb$connectionDetails)
  treatmentIds <- unique(c(lapply(dcoList, '[[', 'targetId'), lapply(dcoList, '[[', 'comparatorId')))
  
  sql <- loadRenderTranslateSql(sqlFilename = "generatePadDays.sql", 
                                packageName = "MPHThesisPAD", 
                                dbms = connectionDetails$dbms,
                                cdmDatabaseSchema = cdmDb$cdmDatabaseSchema,
                                scratchDatabaseSchema = scratchDb$scratchDatabaseSchema,
                                tablePrefix = paste(scratchTablePrefix, cdmDb$id, sep = "_"),
                                treatmentIds = paste(treatmentIds, sep = ","),
                                conceptIds = paste(getIncludedConcepts(webApiPrefix = webApiPrefix, conceptSetId = 3841, webApiUseSsl = webApiUseSsl), collapse = ","))
  executeSql(connection = connection, sql = sql)
  
}


#' @title                    Generates CHADS scores
#' @author                   Ajit Londhe, Jamie Weaver
#' @details                  Generates CHADS scores, both CHADS2 and CHADS2Vasc
#' @param dcoList            The list of drug-comparator-outcomes
#' @param cdmDb              The CDM database object constructed with \code{buildDatabase}
#' @param scratchDb          The scratch database object constructed with \code{buildDatabase}
#' @param scratchTablePrefix The prefix of all scratch tables created
#' @param chadsTypes         Which CHADS score types should be generated?
#' 
#' @export
generateChadsScores <- function(dcoList, cdmDb, scratchDb, scratchTablePrefix, 
                                chadsTypes = c("Chads2", "Chads2Vasc"))
{
  connection <- connect(scratchDb$connectionDetails)
  treatmentIds <- unique(c(lapply(dcoList, '[[', 'targetId'), lapply(dcoList, '[[', 'comparatorId')))
  
  for (chadsType in chadsTypes)
  {
    sql <- loadRenderTranslateSql(sqlFilename = paste0("generate", chadsType, ".sql"), 
                                  packageName = "MPHThesisPAD", 
                                  dbms = connectionDetails$dbms,
                                  cdmDatabaseSchema = cdmDb$cdmDatabaseSchema,
                                  scratchDatabaseSchema = scratchDb$scratchDatabaseSchema,
                                  chads2Table = tolower(chadsType),
                                  tablePrefix = paste(scratchTablePrefix, cdmDb$id, sep = "_"),
                                  cohortDefinitionIds = paste(treatmentIds, sep = ","))
    executeSql(connection = connection, sql = sql)
  }
}