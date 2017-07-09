getTreatmentName <- function(webApiPrefix, cohortId, lower = TRUE)
{
  treatmentName <- str_replace_all(str_replace_all(getCohortDefName(webApiPrefix, cohortId), " ", "_"), "\\[(.*?)\\]_", "")
  if (lower)
  {
    treatmentName <- tolower(treatmentName)
  }
  return(treatmentName)
}

getVocabName <- function(webApiPrefix, webApiUseSsl = FALSE)
{
  url <- paste0("http://", webApiPrefix, ":8080/WebAPI/source/sources")
  if (webApiUseSsl)
  {
    url <- paste0("https://", webApiPrefix, ":8443/WebAPI/source/sources")
  }
  req <- GET(url)
  stop_for_status(req)
  json <- fromJSON(content(req, "text"))
  return(subset(json, json$sourceName=="Default Vocabulary")$sourceKey)
}

# get all included standard concepts from a concept set
getIncludedConcepts <- function(webApiPrefix, conceptSetId, webApiUseSsl = FALSE)
{
  expressionUrl <- paste("http://", webApiPrefix, ":8080/WebAPI/conceptset/", conceptSetId, "/expression", sep="", collapse="")
  if (webApiUseSsl)
  {
    expressionUrl <- paste("https://", webApiPrefix, ":8443/WebAPI/conceptset/", conceptSetId, "/expression", sep="", collapse="")
  }
  
  expressionReq <- GET(expressionUrl)
  stop_for_status(expressionReq)
  expressionJson <- content(expressionReq, "text")
  
  conceptUrl <- paste0("http://", webApiPrefix, ":8080/WebAPI/vocabulary/", getVocabName(webApiPrefix),"/resolveConceptSetExpression")
  if (webApiUseSsl)
  {
    conceptUrl <- paste0("https://", webApiPrefix, ":8443/WebAPI/vocabulary/", getVocabName(webApiPrefix),"/resolveConceptSetExpression")  
  }
  conceptReq <- POST(conceptUrl, body = expressionJson, add_headers ("Content-Type" = "application/json"))
  stop_for_status(conceptReq)
  
  concepts <- unlist(content(conceptReq, "parsed", "application/json"))
  return(concepts)
}


# get concept set name
getConceptSetName <- function(webApiPrefix, conceptSetId, webApiUseSsl = FALSE)
{
  url <- paste("http://", webApiPrefix, ":8080/WebAPI/conceptset/", conceptSetId, sep="", collapse="")
  if (webApiUseSsl)
  {
    url <- paste("https://", webApiPrefix, ":8443/WebAPI/conceptset/", conceptSetId, sep="", collapse="")
  }
  req <- GET(url)
  stop_for_status(req)
  json <- fromJSON(content(req, "text"))
  return(json$name)
}

# get cohort definition name
getCohortDefName <- function(webApiPrefix, cohort_definition_id, webApiwebApiUseSsl = FALSE)
{
  url <- paste0("http://", webApiPrefix, ":8080/WebAPI/cohortdefinition/", cohort_definition_id)
  if (webApiwebApiUseSsl)
  {
    url <- paste0("https://", webApiPrefix, ":8443/WebAPI/cohortdefinition/", cohort_definition_id)
  }
  req <- GET(url)
  stop_for_status(req)
  json <- fromJSON(content(req, "text"))
  return(json$name)
}

# get rendered Sql from WebAPI
getRenderedSql <- function(webApiPrefix, cohort_definition_id, sql_dialect, webApiwebApiUseSsl = FALSE)
{
  expressionUrl <- paste("http://", webApiPrefix, ":8080/WebAPI/cohortdefinition/", cohort_definition_id, sep="", collapse="")
  if (webApiwebApiUseSsl)
  {
    expressionUrl <- paste("https://", webApiPrefix, ":8443/WebAPI/cohortdefinition/", cohort_definition_id, sep="", collapse="")
  }
  expressionReq <- GET(expressionUrl)
  stop_for_status(expressionReq)
  expressionJson <- fromJSON(content(expressionReq, "text"))$expression
  expressionJson <- sprintf('{"expression": %s}', expressionJson)
  
  templateSqlUrl <- paste("http://", webApiPrefix, ":8080/WebAPI/cohortdefinition/sql", sep="", collapse="")
  if (webApiwebApiUseSsl)
  {
    templateSqlUrl <- paste("https://", webApiPrefix, ":8443/WebAPI/cohortdefinition/sql", sep="", collapse="")
  }
  templateSqlReq <- POST(templateSqlUrl, body = expressionJson, add_headers ("Content-Type" = "application/json"))
  stop_for_status(templateSqlReq)
  templateSql <- str_replace_all(content(templateSqlReq, "parsed", "application/json"), "\n", "")
  
  
  renderJson <- sprintf('{"SQL": "%s",
                        "targetdialect": "%s"}', templateSql, sql_dialect)
  renderSqlUrl <- paste("http://", webApiPrefix, ":8080/WebAPI/sqlrender/translate", sep="", collapse="")
  if (webApiUseSsl)
  {
    renderSqlUrl <- paste("https://", webApiPrefix, ":8443/WebAPI/sqlrender/translate", sep="", collapse="")
  }
  renderSqlReq <- POST(renderSqlUrl, body = renderJson, add_headers ("Content-Type" = "application/json"))
  stop_for_status(renderSqlReq)
  renderedSql <- content(renderSqlReq, "parsed", "application/json")
  
  return (renderedSql)
}