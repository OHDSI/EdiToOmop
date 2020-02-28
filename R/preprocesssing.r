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
#' @import readxl
#' @import dplyr
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
                    vocabularyId = "EDI",
                    conceptClassId = "Device",
                    validStartDate = ifelse( is.na(startDate),as.Date("1970-01-01"),as.Date(as.character(startDate))),
                    validEndDate = as.Date("2099-12-31"),
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

  #remove concepts starting after 2019.10.
  matDf<-matDf[matDf$validStartDate < as.Date("2019-11-01"),]

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
                      KoreanDictFile
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
                       vocabularyId = "EDI",
                       conceptClassId = "Proc Hierarchy",
                       validStartDate = ifelse( is.na(startDate),as.Date("1970-01-01"), as.Date(as.character(startDate))),
                       validEndDate = as.Date("2099-12-31"),
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
                    grepl("^[BCDEFG]",sugaDf$conceptCode) |
                    grepl("^H[AC]",sugaDf$conceptCode) |
                    grepl("^FA",sugaDf$conceptCode) ] <- "Measurement"

  ##Set concept class ID
  sugaDf$conceptClassId[nchar(sugaDf$conceptCode)>nchar(sugaDf$ancestorConceptCode)] <- "Procedure"

  sugaDf$conceptClassId[(sugaDf$domainId=="Measurement") & (sugaDf$conceptClassId=="Proc Hierarchy")] <- "Meas Class"
  sugaDf$conceptClassId[(sugaDf$domainId=="Measurement") & (sugaDf$conceptClassId=="Procedure")] <- "Measurement"

  ## Measurement D's concept class id -> Proc Hierarchy
  sugaDf$conceptClassId[sugaDf$domainId == "Measurement"&
                          grepl("^D",sugaDf$conceptCode)]<-"Proc Hierarchy"


  #replace 'Measurement' in domain_id with 'Procedure' where concept_class_id ='Measurement'

  sugaDf$domainId[grepl("Measurement",sugaDf$conceptClassId)] <- "Procedure"

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

  ##  add sanjungName in conceptSynonym
  sugaDf$conceptSynonym<-paste(dplyr::pull(sugaDf, "conceptSynonym"), dplyr::pull(sugaDf, "sanjungName"), sep=", ")

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

  mdcDf <- dplyr::data_frame(conceptCode = conceptCode,
                      conceptName = conceptCode,
                      conceptSynonym = conceptSynonym,
                      domainId = "Drug",
                      vocabularyId = "EDI",
                      conceptClassId = "Drug Product",
                      validStartDate = as.Date("1970-01-01"),
                      validEndDate = as.Date("2099-12-31"),
                      invalidReason = NA,
                      ancestorConceptCode = ancestorConceptCode,
                      previousConceptCode = previousConceptCode,
                      material=NA,
                      dosage = as.numeric(drugDosage),
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
  bdgDf <- mdcDf[mdcDf$vocabularyId=="EDI",]
  bdgDf$conceptName <- NULL

  #Set the name of Branded Drug as Clinical Drug Names
  bdgDf <- merge(bdgDf,drugNameDf, by = "ancestorConceptCode", all.x= TRUE, all.y = FALSE)

  bdgDf <- bdgDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                   "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","previousConceptCode",
                   "material", "dosage", "dosageUnit","sanjungName")]


  return(bdgDf)
}





