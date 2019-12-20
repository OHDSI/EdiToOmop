# Copyright 2019 Observational Health Data Sciences and Informatics
#
# This file is part of RanitidineCancerRisk
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#' EDI Update
#'
#' @details
#' EDI Update..
#'
#' @param ediData             Restructured EDI data(SUGA, Drug, Device)
#' @param startDate           Start date of concepts
#' @param domainIds           Domain id each concept has
#' @param existingVocaTable   Table of existing data on DB
#' @param connectionDetails   Table of existing data on DB
#' @export

NewEdiUpdate<-function(ediData,
                       startDate,
                       domainIds,
                       existingVocaTable,
                       connectionDetails,
                       useMppBulkLoadS=FALSE
){
  startDate <- lubridate::as_date(startDate)
  validEndDate <- startDate -1
  tableName <- "temp_table"

  if (sum(is.na(ediData$conceptCode))) stop("It is not allowed to have NA in concept code")
  if (sum(is.na(ediData$conceptName))) stop("It is not allowed to have NA in concept name")
  if (sum(is.na(ediData$domainId))) stop("It is not allowed to have NA in domain id")
  if (sum(is.na(ediData$vocabularyId))) stop("It is not allowed to have NA in vocabulary id")
  if (sum(is.na(ediData$conceptClassId))) stop("It is not allowed to have NA in concept class id")
  if (sum(is.na(ediData$validStartDate))) stop("It is not allowed to have NA in valid start date")
  if (sum(is.na(ediData$validEndDate))) stop("It is not allowed to have NA in valid end date")

  #ediData$conceptNameLength <- nchar(ediData$conceptName, allowNA = TRUE, keepNA =FALSE)
  #ediData$conceptSynonymLength <- nchar(ediData$conceptSynonym, allowNA = TRUE, keepNA =FALSE)

  #ediData$conceptName <- substr(ediData$conceptName,1,255)
  #ediData$conceptSynonym <- substr(ediData$conceptSynonym,1,255)

  colnames(ediData)<-SqlRender::camelCaseToSnakeCase(colnames(ediData))
  conn <- DatabaseConnector::connect(connectionDetails)
  sql<-"IF OBJECT_ID('#@table_name', 'U') IS NOT NULL
	DROP TABLE #@table_name;

  CREATE TABLE #@table_name (
  concept_code			  	VARCHAR(50)		NOT NULL ,
  concept_name			  	VARCHAR(2000)	NOT NULL ,  --Please note that we allowed lengthy concept name
  concept_synonym       VARCHAR(2000)	NULL,
  domain_id				      VARCHAR(20)		NOT NULL ,
  vocabulary_id			  	VARCHAR(20)		NOT NULL ,
  concept_class_id			VARCHAR(20)		NOT NULL ,
  valid_start_date			DATE			    NOT NULL ,
  valid_end_date		  	DATE	    		NOT NULL ,
  invalid_reason		  	VARCHAR(1)		NULL ,
  ancestor_concept_code VARCHAR(20)		NULL ,
  previous_concept_code VARCHAR(20)		NULL ,
  material              VARCHAR(1000)  NULL ,
  dosage                FLOAT   		NULL ,
  dosage_unit           VARCHAR(20)		NULL ,
  sanjung_name          VARCHAR(1000)		NULL
);
  "
  sql <- SqlRender::render(sql,
                           table_name=tableName)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(conn,sql)

  ##Insert table
  DatabaseConnector::insertTable(connection = conn,
                                 tableName = tableName,
                                 data = ediData,
                                 dropTableIfExists = FALSE,
                                 createTable = FALSE,
                                 tempTable = TRUE,
                                 progressBar = TRUE,
                                 useMppBulkLoad = useMppBulkLoadS)


  # DatabaseConnector::insertTable(connection = conn,
  #                                tableName = tableName,
  #                                data = ediData,
  #                                dropTableIfExists = TRUE,
  #                                createTable = TRUE,
  #                                tempTable = TRUE,
  #                                progressBar = TRUE,
  #                                useMppBulkLoad = useMppBulkLoadS)

  ##New concept code insertion
  sql <- " SELECT COUNT(*) AS NEW_CODE_COUNT FROM #@table_name AS new_code
        WHERE new_code.concept_code not in (SELECT concept_code from @voca_schema.@existing_voca_table)"
  sql <- SqlRender::render(sql,
                           voca_schema = connectionDetails$schema,
                           table_name=tableName,
                           existing_voca_table = existingVocaTable)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  count <- DatabaseConnector::querySql(connection = conn, sql)

  print(sprintf("Total %d codes were updated",count[[1]]))

  # sql <- " SELECT concept_code, concept_name, concept_synonym FROM #@table_name AS new_code
  #       WHERE new_code.concept_code not in (SELECT concept_code from @voca_schema.@existing_voca_table)
  #         AND (new_code.concept_name_length >=128 OR new_code.concept_synonym_length >=128)"
  # sql <- SqlRender::render(sql,
  #                          voca_schema = connectionDetails$schema,
  #                          table_name=tableName,
  #                          existing_voca_table = existingVocaTable)
  # sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  # lengthConcepts <- DatabaseConnector::querySql(connection = conn, sql)
  #
  # print("The concepts below had too lengthy concept names or synonyms. Watch out!!")
  # print(lengthConcepts)

  sql <- "INSERT INTO @voca_schema.@existing_voca_table
        SELECT CONCEPT_CODE, CONCEPT_NAME, CONCEPT_SYNONYM, DOMAIN_ID, VOCABULARY_ID, CONCEPT_CLASS_ID,
        VALID_START_DATE, VALID_END_DATE, INVALID_REASON, ANCESTOR_CONCEPT_CODE, PREVIOUS_CONCEPT_CODE, MATERIAL, DOSAGE, DOSAGE_UNIT, SANJUNG_NAME
        FROM #@table_name AS new_code
        WHERE new_code.concept_code not in (SELECT concept_code from @voca_schema.@existing_voca_table)"
  sql <- SqlRender::render(sql,
                           voca_schema = connectionDetails$schema,
                           table_name=tableName,
                           existing_voca_table = existingVocaTable)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  DatabaseConnector::executeSql(connection = conn, sql)

  ##Deleted code update
  sql <- "UPDATE @voca_schema.@existing_voca_table
        SET VALID_END_DATE = '@valid_end_date', INVALID_REASON = 'D'
        FROM @voca_schema.@existing_voca_table as existing_table
        WHERE existing_table.concept_code NOT IN (SELECT concept_code from #@table_name)
          AND existing_table.domain_id IN ('@domain_ids')"
  sql <- SqlRender::render(sql,
                           voca_schema = connectionDetails$schema,
                           existing_voca_table = existingVocaTable,
                           valid_end_date = validEndDate,
                           table_name=tableName,
                           domain_ids = paste(domainIds,collapse="','"))
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  DatabaseConnector::executeSql(connection = conn, sql)

  sql <- " SELECT COUNT(*) AS DELETED_CODE_COUNT FROM @voca_schema.@existing_voca_table AS existing_table
        WHERE existing_table.VALID_END_DATE = '@valid_end_date'
        AND existing_table.INVALID_REASON = 'D'
        AND existing_table.domain_id IN ('@domain_ids') "
  sql <- SqlRender::render(sql,
                           voca_schema = connectionDetails$schema,
                           existing_voca_table = existingVocaTable,
                           valid_end_date = validEndDate,
                           domain_ids = paste(domainIds,collapse="','"))
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)
  count <- DatabaseConnector::querySql(connection = conn, sql)

  print(sprintf("The status of Total %d codes was uptodated as 'deleted'",count[[1]]))
  DatabaseConnector::disconnect(conn)
}
