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

#' Pre-processing for DEVICE
#'
#' @details
#' Pre-processing for the Excel EDI file
#'
#' @param exelFilePath    file path for the excel file.
#' @param sheetName       A sheet name of interest.
#' @param materialData    Prepared material data. Default is NULL.
#' @param deviceCode      A column name for device EDI code.
#' @param deviceName      A column name for device.
#' @param dateName        A column name for start date.
#' @param materialName        A column name for the material of device.
#' @param KoreanDictFile  Path for csv file containing translation between Korean and English. If you don't want to translate, please set this value as NULL
#'
#' @export
DeviceProcess<-function(exelFilePath,
                        sheetName,
                        materialData=NULL,
                        deviceCode,
                        deviceName,
                        startDateName,
                        materialName,
                        KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation_ANSI.csv"
){
  if(is.null(materialData)){
    matData <- readxl::read_excel(exelFilePath,
                                  sheet=sheetName,
                                  col_names = TRUE)
  }

  #colnames(sugaData)<-SqlRender::snakeCaseToCamelCase(colnames(sugaData))

  conceptCode<-matData[,deviceCode]
  names(conceptCode)<-"conceptCode"

  conceptName<-matData[,deviceName]
  names(conceptName)<-"conceptName"

  conceptSynonym<-matData[,deviceName]
  names(conceptSynonym)<-"conceptSynonym"

  startDate<-dplyr::pull(matData,startDateName)

  material<-matData[,materialName]
  names(material)<-"material"

  matDf<-data.frame(conceptCode= conceptCode,
                    conceptName=conceptName,
                    conceptSynonym=conceptSynonym,
                    domainId = "Device",
                    vocabularyId = "Korean EDI",
                    conceptClassId = "Therapeutic Materials",
                    validStartDate = ifelse( is.na(startDate),"1970-01-01", as.character(startDate)),
                    validEndDate = "2099-12-31",
                    invalidReason = NA,
                    ancestorConceptCode=NA,
                    previousConceptCode=NA,
                    material=material,
                    dosage=NA,
                    dosageUnit=NA,
                    sanjungName = NA,
                    stringsAsFactors=FALSE
  )

  matDf$validStartDate<-lubridate::as_date(matDf$validStartDate)
  matDf$validEndDate<-lubridate::as_date(matDf$validEndDate)

  if(!is.null(KoreanDictFile)) {
    #nrow(sugaDf2) #270413
    #translation for concept name
    dict<- read.csv(KoreanDictFile, stringsAsFactors= FALSE)
    colnames(dict)[grepl("conceptName",colnames(dict))] <- "conceptNameTr"
    if(!length(unique(dict$conceptSynonym)) == length(dict$conceptSynonym)) stop ("Korean names in the dictionary should be unique")
    matDf <- merge(matDf,dict,by= "conceptSynonym", all.x = TRUE, all.y = FALSE)

    matDf$conceptName <- ifelse(!is.na(matDf$conceptNameTr), matDf$conceptNameTr,matDf$conceptName)
  }
  #remove \r\n
  matDf$conceptName<-gsub("\\r\n","",matDf$conceptName)
  matDf$conceptSynonym<-gsub("\\r\n","",matDf$conceptSynonym)

  #remove unnecessary columns
  matDf<-matDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                 "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","previousConceptCode",
                 "material", "dosage", "dosageUnit","sanjungName")]

  return(matDf)
}

#' Pre-processing for SUGA
#'
#' @details
#' Pre-processing for the Excel EDI file
#'
#' @param exelFilePath    file path for the excel file.
#' @param sheetName       A sheet name of interest.
#' @param sugaData        Prepared material data. Default is NULL.
#' @param sugaCode        A column name for device EDI code.
#' @param KoreanName      A column name for device.
#' @param EnglishName     A column name for device.
#' @param startDateName   A column name for start date.
#' @param sanjungName     A column name for the material of device.
#' @param KoreanDictFile  Path for csv file containing translation between Korean and English. If you don't want to translate, please set this value as NULL
#'
#' @export
SugaProcess<-function(exelFilePath,
                      sheetName,
                      sugaData=NULL,
                      sugaCode,
                      KoreanName,
                      EnglishName,
                      startDateName,
                      sanjungName,
                      KoreanDictFile="./inst/csv/suga_Eng_Kor_translation_ANSI.csv"
){
  if(is.null(sugaData)){
    sugaData <- readxl::read_excel(exelFilePath,
                                   sheet=sheetName,
                                   col_names = TRUE)
  }

  #colnames(mdcData)<-SqlRender::snakeCaseToCamelCase(colnames(mdcData))
  conceptCode <- dplyr::pull(sugaData, sugaCode)
  conceptSynonym <- dplyr::pull(sugaData, KoreanName)
  conceptName <- dplyr::pull(sugaData, EnglishName)
  startDate <- dplyr::pull(sugaData, startDateName)
  sanjungName <- dplyr::pull(sugaData, sanjungName)

  ancestorConceptCode <- dplyr::pull(sugaData, sugaCode)

  #substring upto 5 letters to make ancestors
  ancestorConceptCode<-substring(ancestorConceptCode,1,5)
  conceptClassId <- c()

  sugaDf <- data.frame(conceptCode= conceptCode,
                       conceptName = conceptName,
                       conceptSynonym=conceptSynonym,
                       domainId = "Procedure",
                       vocabularyId = "Korean EDI",
                       conceptClassId = "Procedure",
                       validStartDate = ifelse( is.na(startDate),"1970-01-01", as.character(startDate)),
                       validEndDate = "2099-12-31",
                       invalidReason = NA,
                       ancestorConceptCode=ancestorConceptCode,
                       previousConceptCode=NA,
                       material=NA,
                       dosage=NA,
                       dosageUnit=NA,
                       sanjungName = sanjungName,
                       stringsAsFactors=FALSE)

  sugaDf$validStartDate<-lubridate::as_date(sugaDf$validStartDate)
  sugaDf$validEndDate<-lubridate::as_date(sugaDf$validEndDate)

  #If ancestor is identical to concpet code, then remove the ancestor code
  sugaDf$ancestorConceptCode[sugaDf$ancestorConceptCode==sugaDf$conceptCode] <- NA

  #If ancestor is not included to concpet code, then remove the ancestor code
  sugaDf$ancestorConceptCode[!sugaDf$ancestorConceptCode %in% sugaDf$conceptCode] <- NA

  ##Set domain ID
  sugaDf$domainId[grepl("^A[AH]",sugaDf$conceptCode) |
                    grepl("^[BCEFG]",sugaDf$conceptCode) |
                    grepl("^H[AC]",sugaDf$conceptCode) |
                    grepl("^FA",sugaDf$conceptCode) ] <- "Measurement"

  ##Set concept class ID
  sugaDf$conceptClassId[nchar(sugaDf$conceptCode)>nchar(sugaDf$ancestorConceptCode)] <- "Procedure Details"

  sugaDf$conceptClassId[(sugaDf$domainId=="Measurement") & (sugaDf$conceptClassId=="Procedure")] <- "Measurement"
  sugaDf$conceptClassId[(sugaDf$domainId=="Measurement") & (sugaDf$conceptClassId=="Procedure Details")] <- "Measurement Details"

  if(!is.null(KoreanDictFile)) {
    #nrow(sugaDf2) #270413
    #translation for concept name
    dict<- read.csv(KoreanDictFile, stringsAsFactors= FALSE)
    colnames(dict)[grepl("conceptName",colnames(dict))] <- "conceptNameTr"
    if(!length(unique(dict$conceptSynonym)) == length(dict$conceptSynonym)) stop ("Korean names in the dictionary should be unique")
    sugaDf <- merge(sugaDf,dict,by= "conceptSynonym", all.x = TRUE, all.y = FALSE)

    sugaDf$conceptName <- ifelse(is.na(sugaDf$conceptName), sugaDf$conceptNameTr,sugaDf$conceptName)

    #translation for sanjung name
    dict<- read.csv(KoreanDictFile, stringsAsFactors= FALSE)
    colnames(dict)[grepl("conceptSynonym",colnames(dict))] <- "sanjungName"
    colnames(dict)[grepl("conceptName",colnames(dict))] <- "sanjungTr"

    sugaDf <- merge(sugaDf,dict,by= "sanjungName", all.x = TRUE, all.y = FALSE)
    #sugaDf2$sanjungTr[is.na(sugaDf2$sanjungTr)]<-""
    sugaDf$conceptName <- ifelse(!is.na(sugaDf$sanjungTr),
                                 paste(sugaDf$conceptName,sugaDf$sanjungTr,sep=","),
                                 sugaDf$conceptName)
  }
  #remove unnecessary columns
  sugaDf<-sugaDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                   "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","previousConceptCode",
                   "material", "dosage", "dosageUnit","sanjungName")]

  return(sugaDf)
}

#' Pre-processing for DRUG
#'
#' @details
#' Pre-processing for the Excel EDI file
#'
#' @param exelFilePath        file path for the excel file.
#' @param sheetName           A sheet name of interest. Sheet name for drug is usally NULL (default is NULL)
#' @param drugData            Prepared material data. Default is NULL.
#' @param drugCode            A column name for drug EDI code.
#' @param drugName            A column name for drug
#' @param clinicalDrugcode    A column name for clinical drug code
#' @param drugDosage          A column name for drug dosage.
#' @param drugDosageUnit      A column name for drug dosage unit.
#' @param previousConceptCode        A column name for previous code
#'
#' @export
#'
DrugProcess<-function(exelFilePath,
                      sheetName=NULL,
                      drugData=NULL,
                      drugCode,
                      drugName,
                      clinicalDrugcode,
                      drugDosage,
                      drugDosageUnit,
                      previousConceptCode){
  if(is.null(drugData)){
    drugData <- readxl::read_excel(exelFilePath,
                                   sheet = sheetName,
                                   col_names = TRUE)
  }

  #colnames(sugaData)<-SqlRender::snakeCaseToCamelCase(colnames(sugaData))

  conceptCode <- dplyr::pull(drugData, drugCode)
  ancestorConceptCode <- dplyr::pull(drugData, clinicalDrugcode)
  previousConceptCode <- dplyr::pull(drugData, previousConceptCode)
  drugDosage <- dplyr::pull(drugData, drugDosage)
  drugDosageUnit <- dplyr::pull(drugData, drugDosageUnit)
  conceptSynonym <- dplyr::pull(drugData, drugName)

  mdcDf <- data.frame(conceptCode = conceptCode,
                      conceptName = conceptCode,
                      conceptSynonym = conceptSynonym,
                      domainId = "Drug",
                      vocabularyId = "Korean EDI",
                      conceptClassId = "Branded Drug",
                      validStartDate = "1970-01-01",
                      validEndDate = "2099-12-31",
                      invalidReason = NA,
                      ancestorConceptCode = ancestorConceptCode,
                      previousConceptCode = previousConceptCode,
                      material=NA,
                      dosage = drugDosage,
                      dosageUnit = drugDosageUnit,
                      sanjungName = NA,
                      stringsAsFactors=FALSE
  )

  #Distinguish Clinical Drug from Branded Drug
  mdcDf$vocabularyId[is.na(mdcDf$conceptSynonym)] <- "KDC"
  mdcDf$conceptClassId[mdcDf$vocabularyId=="KDC"] <- "Clinical Drug"

  kdcDf<-mdcDf[mdcDf$vocabularyId=="KDC",]

  drugNameDf <- kdcDf[c("conceptName", "ancestorConceptCode")]
  #paste drug names for the same clinical drug (composite drug)
  drugNameDf <- aggregate(conceptName ~ ancestorConceptCode, data = drugNameDf, paste, collapse = ",")

  #Only Branded Drugs of EDI
  bdgDf <- mdcDf[mdcDf$vocabularyId=="Korean EDI",]
  bdgDf$conceptName <- NULL

  #Set the name of Branded Drug as Clinical Drug Names
  bdgDf <- merge(bdgDf,drugNameDf, by = "ancestorConceptCode", all.x= TRUE, all.y = FALSE)

  bdgDf <- bdgDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                   "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","previousConceptCode",
                   "material", "dosage", "dosageUnit","sanjungName")]

  return(bdgDf)
}

#' Pre-processing for DEVICE deleted in 2019.10
#'
#' @details
#' Pre-processing for the Excel EDI file
#'
#' @param exelFilePath    file path for the excel file.
#' @param sheetName       A sheet name of interest.
#' @param materialData    Prepared material data. Default is NULL.
#' @param deviceCode      A column name for device EDI code.
#' @param deviceName      A column name for device.
#' @param enddateName     A column name for deleted date.
#' @param materialName    A column name for the material of device.
#' @param KoreanDictFile  Path for csv file containing translation between Korean and English. If you don't want to translate, please set this value as NULL
#'
#' @export
#'
#'

DelDeviceProcess<-function(exelFilePath,
                           sheetName,
                           materialData=NULL,
                           deviceCode,
                           deviceName,
                           endDateName,
                           materialName,
                           KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation_ANSI.csv"
){
  if(is.null(materialData)){
    matData <- readxl::read_excel(exelFilePath,
                                  sheet=sheetName,
                                  col_names = TRUE)
  }

  #colnames(sugaData)<-SqlRender::snakeCaseToCamelCase(colnames(sugaData))

  conceptCode<-matData[,deviceCode]
  names(conceptCode)<-"conceptCode"

  conceptName<-matData[,deviceName]
  names(conceptName)<-"conceptName"

  conceptSynonym<-matData[,deviceName]
  names(conceptSynonym)<-"conceptSynonym"

  endDate<-dplyr::pull(matData, endDateName)

  material<-matData[,materialName]
  names(material)<-"material"

  matDelDf<-data.frame(conceptCode= conceptCode,
                    conceptName=conceptName,
                    conceptSynonym=conceptSynonym,
                    domainId = "Device",
                    vocabularyId = "Korean EDI",
                    conceptClassId = "Therapeutic Materials",
                    validStartDate = "1970-01-01",
                    validEndDate = ifelse( is.na(endDate),"2099-12-31", as.character(endDate)),
                    invalidReason = NA,
                    ancestorConceptCode=NA,
                    previousConceptCode=NA,
                    material=material,
                    dosage=NA,
                    dosageUnit=NA,
                    sanjungName = NA,
                    stringsAsFactors=FALSE
  )

  matDelDf$validStartDate<-lubridate::as_date(matDelDf$validStartDate)
  matDelDf$validEndDate<-lubridate::as_date(matDelDf$validEndDate)

  if(!is.null(KoreanDictFile)) {
    dict<- read.csv(KoreanDictFile, stringsAsFactors= FALSE)
    colnames(dict)[grepl("conceptName",colnames(dict))] <- "conceptNameTr"
    if(!length(unique(dict$conceptSynonym)) == length(dict$conceptSynonym)) stop ("Korean names in the dictionary should be unique")
    matDelDf <- merge(matDelDf,dict,by= "conceptSynonym", all.x = TRUE, all.y = FALSE)

    matDelDf$conceptName <- ifelse(!is.na(matDelDf$conceptNameTr), matDelDf$conceptNameTr,matDelDf$conceptName)
  }
  #remove \r\n
  matDelDf$conceptName<-gsub("\\r\n","",matDelDf$conceptName)
  matDelDf$conceptSynonym<-gsub("\\r\n","",matDelDf$conceptSynonym)

  #remove unnecessary columns
  matDelDf<-matDelDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                 "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","previousConceptCode",
                 "material", "dosage", "dosageUnit","sanjungName")]

  matDelDf<-subset(matDelDf, is.na(matDelDf$validEndDate)==FALSE)
  matDelDf$validEndDate<-as.character(as.Date(matDelDf$validEndDate)+lubridate::days(-1))
  matDelDf<-subset(matDelDf, validEndDate=="2019-09-30" )


  return(matDelDf)
}

uploadProcess<-function(dbms,
                        user,
                        password,
                        server,
                        schema
){

  masterData<-rbind(sugaData, drugData, deviceData, DelDeviceData)

  connectionDetail <- DatabaseConnector::createConnectionDetails(
    dbms=dbms,
    user=user,
    schema = schema,
    password=password,
    server=server
  )

  con <- DatabaseConnector::connect(connectionDetail)


  DatabaseConnector::insertTable(connection = con,
                                 tableName = "OMOP_CDM_Vocab",
                                 data = masterData,
                                 dropTableIfExists = TRUE,
                                 createTable = TRUE,
                                 tempTable = FALSE,
                                 progressBar = TRUE,
                                 useMppBulkLoad = FALSE)

  write.csv(masterData,file="./inst/master_data.csv", fileEncoding="UTF-8")

  return(masterData)
}
