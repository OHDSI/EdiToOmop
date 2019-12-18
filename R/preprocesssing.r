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
#'
#' @export
DeviceProcess<-function(exelFilePath,
                        sheetName = "",
                        materialData=NULL,
                        deviceCode = "",
                        deviceName = "",
                        startDateName="",
                        materialName = ""
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
                    sanjungName = NA
  )

  matDf$validStartDate<-lubridate::as_date(matDf$validStartDate)
  matDf$validEndDate<-lubridate::as_date(matDf$validEndDate)

  class(matDf)<-"ediDf"
  return(matDf)
}
