

#' @author                          Ajit Londhe
#' @title                           Build Database
#' @details                         Builds a database object for use in the study
#' @param id                        The unique key for the database
#' @param dbms                      The database platform type ("sql server," "pdw," "redshift," "oracle," "mysql")
#' @param dbName                    The technical name of the database
#' @param server                    The name of the server host
#' @param port                      The SQL port of the server host
#' @param cdmDatabaseSchema         The fully qualified schema name of the CDM schema
#' @param resultsDatabaseSchema     The fully qualified schema name of the results schema
#' @param scratchDatabaseSchema     The fully qualified schema name of the scratch schema
#' @param user                      The user name of the SQL account
#' @param password                  The password of the SQL account
#' @param connectionDetails         The connectionDetails object for this database
#' @return                          A database object
#' 
#' @export
buildDatabase <- function(id, dbms, dbName, server, port, 
                          cdmDatabaseSchema = NULL, 
                          resultsDatabaseSchema = NULL, scratchDatabaseSchema = NULL,
                          user, password, connectionDetails = NULL)
{
  dataSource <- {}
  dataSource$id <- id
  dataSource$dbms <- dbms
  dataSource$dbName <- dbName
  dataSource$server <- server
  dataSource$cdmDatabaseSchema <- cdmDatabaseSchema
  dataSource$resultsDatabaseSchema <- resultsDatabaseSchema
  dataSource$scratchDatabaseSchema <- scratchDatabaseSchema
  dataSource$port <- port
  dataSource$user <- user
  dataSource$password <- password
  dataSource$connectionDetails <- createConnectionDetails(dbms = cdmDb$dbms, user = cdmDb$user, 
                                                            password = cdmDb$password, 
                                                            server = cdmDb$server, port = cdmDb$port)
  return(dataSource)
}

#' @author                  Ajit Londhe
#' @title                   Get Formatted Cohort Name
#' @details                 Gets a formatted name for a cohort
#' @param webApiPrefix      The URL prefix of the WebAPI instance
#' @param treatmentId       The cohort definition Id
#' @param webApiUseSsl      Does the WebAPI instance use SSL?
#' @return                  A string with the formatted cohort name
#' 
#' @export
getFormattedCohortName <- function(webApiPrefix, treatmentId, webApiUseSsl = FALSE) 
{
  return(str_replace_all(str_replace_all(str_replace_all(getCohortDefName(webApiPrefix, treatmentId), " ", "_"), "\\[(.*?)\\]_", ""), "_", " "))
}

#' @author                  Ajit Londhe
#' @title                   Get Included Concepts
#' @details                 Obtains the full list of concept Ids in a concept set
#' @param webApiPrefix      The URL for WebAPI
#' @param conceptSetId      The concept set id in Atlas
#' @param webApiUseSsl      Does the WebAPI instance use SSL?
#' @return                  A list of concept Ids
#' @export
getIncludedConcepts <- function(webApiPrefix, conceptSetId, webApiUseSsl = FALSE)
{
  protocol <- ifelse(webApiUseSsl, "https", "http")
  port <- ifelse(webApiUseSsl, 8443, 8080)
  
  expressionUrl <- paste0(protocol, "://", webApiPrefix, ":", port, "/WebAPI/conceptset/", conceptSetId, "/expression")
  expressionReq <- GET(expressionUrl)
  stop_for_status(expressionReq)
  expressionJson <- content(expressionReq, "text")
  
  conceptUrl <- paste0(protocol, webApiPrefix, ":", port, "/WebAPI/vocabulary/", getVocabName(webApiPrefix), "/resolveConceptSetExpression")
  conceptReq <- POST(conceptUrl, body = expressionJson, add_headers("Content-Type" = "application/json"))
  stop_for_status(conceptReq)
  
  concepts <- unlist(content(conceptReq, "parsed", "application/json"))
  return(concepts)
}

#' @author                Ajit Londhe
#' @title                 Get Cohort Definition Name
#' @details               Obtains the name of a cohort
#' @param webApiPrefix    The URL for WebAPI
#' @param conceptSetId    The concept set id in Atlas
#' @param webApiUseSsl    Does the WebAPI instance use SSL?
#' @return                A list of concept Ids
#' @export
getCohortDefName <- function(webApiPrefix, cohortDefinitionId, webApiUseSsl = FALSE)
{
  protocol <- ifelse(webApiUseSsl, "https", "http")
  port <- ifelse(webApiUseSsl, 8443, 8080)
  
  url <- paste0(protocol, "://", webApiPrefix, ":", port, "/WebAPI/cohortdefinition/", cohortDefinitionId)
  req <- GET(url)
  stop_for_status(req)
  json <- fromJSON(content(req, "text"))
  return(json$name)
}