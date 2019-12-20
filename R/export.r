#' Upload masterDf to DB
#'
#' @details
#' Upload masterDf to DB
#'
#' @param ediData    The EDI data in format of data frame to be inserted.
#' @param connectionDetails       connectionDetails produced by "DatabaseConnector::createconnectionDetails"
#' @param vocabularyDatabaseSchema The name of the DB schema where the data frame should be inserted.
#' @param tableName    The name of the table where the data should be inserted.
#' @param useMppBulkLoad    If using Redshift or PDW, use more performant bulk loading techniques. Setting the system environment variable "USE_MPP_BULK_LOAD" to TRUE is another way to enable this mode. Please note, Redshift requires valid S3 credentials; PDW requires valid DWLoader installation. This can only be used for permanent tables, and cannot be used to append to an existing table. Default is FALSE :D
#'
#' @export
#'

GenerateEdiVocaTable<-function(ediData,
                               connectionDetails,
                               vocabularyDatabaseSchema = connectionDetails$schema,
                               tableName,
                               useMppBulkLoadS = FALSE
){
  if (sum(is.na(ediData$conceptCode))) stop("It is not allowed to have NA in concept code")
  if (sum(is.na(ediData$conceptName))) stop("It is not allowed to have NA in concept name")
  if (sum(is.na(ediData$domainId))) stop("It is not allowed to have NA in domain id")
  if (sum(is.na(ediData$vocabularyId))) stop("It is not allowed to have NA in vocabulary id")
  if (sum(is.na(ediData$conceptClassId))) stop("It is not allowed to have NA in concept class id")
  if (sum(is.na(ediData$validStartDate))) stop("It is not allowed to have NA in valid start date")
  if (sum(is.na(ediData$validEndDate))) stop("It is not allowed to have NA in valid end date")

  colnames(ediData)<-SqlRender::camelCaseToSnakeCase(colnames(ediData))

  conn <- DatabaseConnector::connect(connectionDetails)

  sql<-"IF OBJECT_ID('@vocabulary_database_schema.@table_name', 'U') IS NOT NULL
	DROP TABLE @vocabulary_database_schema.@table_name;

  CREATE TABLE @vocabulary_database_schema.@table_name (
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
                           vocabulary_database_schema = vocabularyDatabaseSchema,
                           table_name=tableName)
  sql <- SqlRender::translate(sql, targetDialect = connectionDetails$dbms)

  DatabaseConnector::executeSql(conn,sql)

  ##Insert table
  DatabaseConnector::insertTable(connection = conn,
                                 tableName = tableName,
                                 data = ediData,
                                 dropTableIfExists = FALSE,
                                 createTable = FALSE,
                                 tempTable = FALSE,
                                 progressBar = TRUE,
                                 useMppBulkLoad = useMppBulkLoadS)
  # writing CSV file
  # write.csv(ediData,file="./inst/ediData.csv", fileEncoding="UTF-8")
  DatabaseConnector::disconnect(conn)

  #return(ediData)
}

#' Create CSV
#'
#' @details
#' Generate CSV with snake-case columns
#'
#' @param ediData    The EDI data in format of data frame or ffdf containing the data to be inserted.
#' @param filePath    path and file name where CSV is written
#'
#' @export
#'

CreateCsv<-function(ediData,
                    filePath){
  #ediData<-rbind(deviceData, sugaData, drugData)
  if (sum(is.na(ediData$conceptCode))) stop("It is not allowed to have NA in concept code")
  if (sum(is.na(ediData$conceptName))) stop("It is not allowed to have NA in concept name")
  if (sum(is.na(ediData$domainId))) stop("It is not allowed to have NA in domain id")
  if (sum(is.na(ediData$vocabularyId))) stop("It is not allowed to have NA in vocabulary id")
  if (sum(is.na(ediData$conceptClassId))) stop("It is not allowed to have NA in concept class id")
  if (sum(is.na(ediData$validStartDate))) stop("It is not allowed to have NA in valid start date")
  if (sum(is.na(ediData$validEndDate))) stop("It is not allowed to have NA in valid end date")

  colnames(ediData)<-SqlRender::camelCaseToSnakeCase(colnames(ediData))

  write.csv(ediData, filePath, row.names = FALSE)
  print(sprintf("EDI data is written in csv format at %s with total line number of %d", filePath, nrow(ediData)))
}
