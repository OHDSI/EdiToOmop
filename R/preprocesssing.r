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
                        KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation.csv"
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
                      KoreanDictFile="./inst/csv/suga_Eng_Kor_translation.csv"
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
#' @param exelFilePath    file path for the excel file.
#' @param sheetName       A sheet name of interest.
#' @param sugaData        Prepared material data. Default is NULL.
#' @param sugaCode        A column name for device EDI code.
#' @param KoreanName      A column name for device.
#' @param EnglishName     A column name for device.
#' @param startDateName   A column name for start date.
#' @param sanjungName     A column name for the material of device.
#' @param KoreanDict      A csv file translating between Korean and English
#'
#' @export
#'
drugEdiProcess<-function(){

}
