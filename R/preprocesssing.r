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

#' Pre-processing for SUGA
#'
#' @details
#' Run the CohortMethod package, which implements the comparative cohort design.
#'
#' @param exelFilePath    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param sheetName    Schema name where your patient-level data in OMOP CDM format resides.
#'                             Note that for SQL Server, this should include both the database and
#'                             schema name, for example 'cdm_data.dbo'.
#' @param sugaData      Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#'
#' @import lubridate
#' @import dplyr
#' @export
install.packages("qdap")
library(qdap)
######################################################################################################## 수가

SugaProcess<-function(exelFilePath="C:/Users/AJOU/Desktop/ediToOMOP/소스파일/수가/수정_수가반영내역(19.10.1.기준)_홈페이지.xlsx",
                      sheetName = "의치과_급여_전체",
                      sugaData=NULL,
                      sugaCode = "수가코드",
                      KoreanName = "한글명",
                      EnglishName = "영문명",
                      sanjungName = "산정명칭"){
  if(is.null(sugaData)){
    sugaData <- readxl::read_excel(exelFilePath,
                           sheet=sheetName,
                           col_names = TRUE)
  }

  #colnames(mdcData)<-SqlRender::snakeCaseToCamelCase(colnames(mdcData))
  conceptCode<-sugaData[,sugaCode]
  names(conceptCode)<-"conceptCode"

  conceptSynonym<-sugaData[,KoreanName]
  names(conceptSynonym)<-"conceptSynonym"

  sanjungName<-sugaData[,sanjungName]
  names(sanjungName)<-"sanjungName"

  conceptName<-sugaData[,EnglishName]
  names(conceptName)<-"conceptName"


  ancestorConceptCode<-sugaData[,sugaCode]

  ancestorConceptCode<-sapply(ancestorConceptCode, substring, 1, 5)
  ancestorConceptCode<-as.data.frame(ancestorConceptCode)
  names(ancestorConceptCode)<-"ancestorConceptCode"

  conceptClassId <- c()
  sugaDf<-data.frame(conceptCode= conceptCode,
                     conceptName = conceptName,
                     conceptSynonym=conceptSynonym,
                     sanjungName=sanjungName,
                     validStartDate = ifelse( is.na(sugaData$"적용일자"),"1970-01-01",  sugaData$"적용일자"),
                     validEndDate = "2099-12-31",
                     vocabularyId = "Korean EDI",
                     ancestorConceptCode=ancestorConceptCode,
                     invalidReason = NA

  )

  ##############################################need to modified
  for(i in 1:nrow(sugaDf)){
    if(sugaDf$ancestorConceptCode[i] == sugaDf$conceptCode[i]){
      sugaDf$ancestorConceptCode[i]<-NA
    }
  }

  for(i in 1:nrow(sugaDf)){
    if(is.na(match(sugaDf$ancestorConceptCode[i], sugaDf$conceptCode))){
      sugaDf$ancestorConceptCode[i]<-NA
    }
  }
  ##############################################

  sugaDf$conceptClassId<-"Procedure"
  sugaDf$conceptClassId[grepl("^A[AH]",sugaDf$conceptCode) |
                          grepl("^[BCEFG]",sugaDf$conceptCode) |
                          grepl("^H[AC]",sugaDf$conceptCode) |
                          grepl("^FA",sugaDf$conceptCode) ] <-"Measurement"
  sugaDf$domainId<-"Procedure"
  sugaDf$domainId[grepl("^A[AH]",sugaDf$conceptCode) |
                    grepl("^[BCEFG]",sugaDf$conceptCode) |
                    grepl("^H[AC]",sugaDf$conceptCode) |
                    grepl("^FA",sugaDf$conceptCode) ] <-"Measurement"
  #tmp1 <- unlist(sugaData[,KoreanName], use.names = FALSE)
  #tmp2 <- unlist(sugaData[,sanjungName], use.names = FALSE)
  #conceptSynonym<-paste(tmp1, ",", tmp2)
  #conceptSynonym<-gsub(", NA","",conceptSynonym)
  sugaDf$conceptCodeBf<-NA
  sugaDf$material<-NA
  sugaDf$dosage<-NA
  sugaDf$dosageUnit<-NA

  ##


  sugaDf<-sugaDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                   "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","conceptCodeBf","material", "dosage", "dosageUnit", "sanjungName" )]
  rm(conceptSynonym, sanjungName, sugaCode, KoreanName, sugaData, conceptCode, conceptName, ancestorConceptCode)
}

#' Pre-processing for Medicine
#'
#' @details
#' Run the CohortMethod package, which implements the comparative cohort design.
#'
#' @param exelFilePath    An object of type \code{connectionDetails} as created using the
#'                             \code{\link[DatabaseConnector]{createConnectionDetails}} function in the
#'                             DatabaseConnector package.
#' @param mdcData      Schema name where intermediate data can be stored. You will need to have
#'                             write priviliges in this schema. Note that for SQL Server, this should
#'                             include both the database and schema name, for example 'cdm_data.dbo'.
#' @export


######################################################################################################## 약제

mdcProcess<-function(exelFilePath="C:/Users/AJOU/Desktop/ediToOMOP/소스파일/약제/약제급여목록및급여상한금액표_(2019.10.1.)(23,231)_10.14.수정.xlsx",
                     mdcData=NULL,
                     mdcCode = "제품코드",
                     mdcName = "제품명",
                     ingrCode = "주성분코드",
                     mdcdosage = "규격",
                     mdcdosageUnit="단위",
                     mdcconceptCodeBf="목록정비전코드"){
  if(is.null(mdcData)){
    mdcData <- readxl::read_excel(exelFilePath,
                                   col_names = TRUE)
  }

  #colnames(sugaData)<-SqlRender::snakeCaseToCamelCase(colnames(sugaData))

  conceptCode<-mdcData[,mdcCode]
  names(conceptCode)<-"conceptCode"

  ancestorConceptCode <-mdcData[,ingrCode]
  names(ancestorConceptCode)<-"ancestorConceptCode"

  conceptCodeBf<-mdcData[,mdcconceptCodeBf]
  names(conceptCodeBf)<-"conceptCodeBf"

  dosage<-mdcData[,mdcdosage]
  names(dosage)<-"dosage"

  dosageUnit<-mdcData[,mdcdosageUnit]
  names(dosageUnit)<-"dosageUnit"

  conceptSynonym<-mdcData[,mdcName]
  names(conceptSynonym)<-"conceptSynonym"

  mdcDf<-data.frame(conceptCode= conceptCode,
                    conceptName=conceptCode,
                    conceptSynonym=conceptSynonym,
                    domainId = "Drug",
                    vocabularyId = "Korean EDI",
                    conceptClassId = "Drug",
                    validStartDate = "1970-01-01",
                    validEndDate = "2099-12-31",
                    invalidReason = NA,
                    ancestorConceptCode=ancestorConceptCode,
                    conceptCodeBf=conceptCodeBf,
                    material=NA,
                    dosage=dosage,
                    dosageUnit=dosageUnit
  )
  colnames(mdcDf)[2] <- "conceptName"

  for(i in 1:nrow(mdcDf)){
    if(grepl("[a-z]",mdcDf$conceptName[i])==F){
      mdcDf$conceptName[i]<-mdcDf$conceptName[i-1]
    }
  }

  mdcDf<-subset(mdcDf, is.na(conceptSynonym)==FALSE)
  rm(mdcData, conceptCode,ancestorConceptCode,conceptCodeBf,dosage,dosageUnit,conceptSynonym, mdcName, mdcCode, mdcconceptCodeBf )

}


######################################################################################################## 주성분

igrProcess<-function(exelFilePath="C:/Users/AJOU/Desktop/ediToOMOP/소스파일/약제/약제급여목록및급여상한금액표_(2019.10.1.)(23,231)_10.14.수정.xlsx",
                     igrData=NULL,
                     mdcCode = "제품코드",
                     mdcName = "제품명",
                     ingrCode = "주성분코드",
                     mdcdosage = "규격",
                     mdcdosageUnit="단위",
                     mdcconceptCodeBf="목록정비전코드"){
  if(is.null(igrData)){
    igrData <- readxl::read_excel(exelFilePath,
                                  col_names = TRUE)
  }

  #colnames(sugaData)<-SqlRender::snakeCaseToCamelCase(colnames(sugaData))

  conceptName<-igrData[,mdcCode]
  names(conceptName)<-"conceptName"

  conceptCode<-igrData[,ingrCode]
  names(conceptCode)<-"conceptCode"

  conceptSynonym<-igrData[,mdcName]
  names(conceptSynonym)<-"conceptSynonym"

  mdcDf<-data.frame(conceptCode= conceptCode,
                    conceptName=conceptName,
                    conceptSynonym=conceptSynonym,
                    domainId = "Drug",
                    vocabularyId = "KDC",
                    conceptClassId = "General Drug",
                    validStartDate = "1970-01-01",
                    validEndDate = "2099-12-31",
                    invalidReason = NA,
                    ancestorConceptCode=NA,
                    conceptCodeBf=NA,
                    material=NA,
                    dosage=NA,
                    dosageUnit=NA
  )

  igrDf<-subset(mdcDf, is.na(conceptSynonym))
  rm(igrData,conceptCode, conceptName, conceptSynonym)

}

######################################################################################################## 치재

matProcess<-function(exelFilePath="C:/Users/AJOU/Desktop/ediToOMOP/소스파일/치료재료/2019.10.1.적용_치료재료대(인체조직포함)_파일(급여)_게시용_최종.xlsx",
                     matData=NULL,
                     sheetName = "급여품목(인체조직포함)",
                     matCode = "코 드",
                     matName = "품 명",
                     material = "재 질"
                     ){
  if(is.null(matData)){
    matData <- readxl::read_excel(exelFilePath,
                                  sheet=sheetName,
                                  col_names = TRUE)
  }

  #colnames(sugaData)<-SqlRender::snakeCaseToCamelCase(colnames(sugaData))

  conceptCode<-matData[,matCode]
  names(conceptCode)<-"conceptCode"

  conceptName<-matData[,matName]
  names(conceptName)<-"conceptName"

  conceptSynonym<-matData[,matName]
  names(conceptSynonym)<-"conceptSynonym"

  material<-matData[,material]
  names(material)<-"material"

  matDf<-data.frame(conceptCode= conceptCode,
                    conceptName=conceptName,
                    conceptSynonym=conceptSynonym,
                    domainId = "Device",
                    vocabularyId = "Korean EDI",
                    conceptClassId = "Therapeutic Materials",
                    validStartDate = ifelse( is.na(matData$"적용일자"),"1970-01-01", as.character(matData$"적용일자")),
                    validEndDate = "2099-12-31",
                    invalidReason = NA,
                    ancestorConceptCode=NA,
                    conceptCodeBf=NA,
                    material=material,
                    dosage=NA,
                    dosageUnit=NA
  )

  rm(matData,conceptCode, conceptName, conceptSynonym, material)

}

######################################################################################################## 치재삭제시트

matDelProcess<-function(exelFilePath="C:/Users/AJOU/Desktop/ediToOMOP/소스파일/치료재료/2019.10.1.적용_치료재료대(인체조직포함)_파일(급여)_게시용_최종.xlsx",
                     matData=NULL,
                     sheetName = "삭제 및 삭제예정 품목",
                     matCode = "코 드",
                     matName = "품 명",
                     material = "재 질"
){
  if(is.null(matData)){
    matDelData <- readxl::read_excel(exelFilePath,
                                  sheet=sheetName,
                                  col_names = TRUE)
  }

  #colnames(sugaData)<-SqlRender::snakeCaseToCamelCase(colnames(sugaData))

  conceptCode<-matDelData[,matCode]
  names(conceptCode)<-"conceptCode"

  conceptName<-matDelData[,matName]
  names(conceptName)<-"conceptName"

  conceptSynonym<-matDelData[,matName]
  names(conceptSynonym)<-"conceptSynonym"

  material<-matDelData[,material]
  names(material)<-"material"

  matDelDf<-data.frame(conceptCode= conceptCode,
                    conceptName=conceptName,
                    conceptSynonym=conceptSynonym,
                    domainId = "Device",
                    vocabularyId = "Korean EDI",
                    conceptClassId = "Therapeutic Materials",
                    validStartDate = "1970-01-01",
                    validEndDate = ifelse(as.character(matDelData$"적용일자")=="2019-10-01", as.character(matDelData$"적용일자"),NA),
                    invalidReason = NA,
                    ancestorConceptCode=NA,
                    conceptCodeBf=NA,
                    material=material,
                    dosage=NA,
                    dosageUnit=NA
  )

  matDelDf<-subset(matDelDf, is.na(matDelDf$validEndDate)==FALSE)
  matDelDf$validEndDate<-as.character(as.Date(matDelDf$validEndDate)+lubridate::days(-1))

  rm(conceptSynonym, conceptCode, conceptName, matDelData, material)

}

###########################################################################
appendingTMTrans<-fucntion(exelFilePath="C:/Users/AJOU/Desktop/ediToOMOP/소스파일/TMTransList_ANSI.csv",
                           dicData = NULL){
  if(is.null(dicData)){
    dicData <- read.csv(exelFilePath,
                        stringsAsFactors = F)
  }
  matDf<-rbind(matDf, matDelDf)
  ##############################################need to modified
  matDf <-merge(x = matDf,y = dicData, by='conceptSynonym', all.x = T)
  for(i in 1:nrow(matDf)){
    if(is.na(matDf$conceptName.y[i])==F){
      matDf$conceptName.x[i]<-matDf$conceptName.y[i]
    }
  }
  ##############################################
  matDf$conceptName.y<-NULL
  names(matDf)[3]<-c("conceptName")
  matDf<-matDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                 "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","conceptCodeBf","material", "dosage", "dosageUnit" )]
  rm(matDelDf)
}
###############################################################################
appendingSugaTrans<-fucntion(exelFilePath="C:/Users/AJOU/Desktop/ediToOMOP/소스파일/sugaTransList_ANSI.csv",
                             dicData = NULL){
  if(is.null(dicData)){
    dicData <- read.csv(exelFilePath,
                        stringsAsFactors = F)
  }

  sugaDfclone<-sugaDf
  sugaDfclone <-merge(x = sugaDfclone,y = dicData, by='conceptSynonym', all.x = T)

  colnames(dicData)[1] <- "sanjungName"
  sugaDfclone <-merge(x = sugaDfclone,y = dicData, by='sanjungName', all.x = T)

  ##############################################need to modified
  #sugaDfclone$conceptName.y ##번역한 영문명
  #sugaDfclone$conceptName.x ##원래 영문명
  #sugaDfclone$conceptName ## 산정이름 영문명

  aa<-as.data.frame(cbind(sugaDfclone$conceptSynonym, sugaDfclone$sanjungName,sugaDfclone$conceptName.x,sugaDfclone$conceptName.y, sugaDfclone$conceptName))
  colnames(aa)=c("ko","sanjung","en","transEn","sanjungEn")

  aa <- data.frame(lapply(aa, as.character), stringsAsFactors=FALSE)

  for(i in 1:nrow(aa)){
    if(is.na(aa$transEn[i])==F){
      aa$en[i]<-aa$transEn[i]
    }
  }
  aa$transEn<-NULL
  aa$ko<-NULL
  aa$sanjung<-NULL

  aa<-as.data.frame(apply(aa, 1, paste, collapse=", "))
  colnames(aa)=c("en")

  aa<-as.data.frame(gsub(", NA$", "", aa$en))

  aa<-as.data.frame(cbind(sugaDfclone$conceptCode ,aa))
  names(aa)[1]<-c("conceptCode")
  names(aa)[2]<-c("conceptName")

  sugaDf <-merge(x = sugaDf,y = aa, by='conceptCode', all.x = T)
  sugaDf$conceptName.x<-NULL
  names(sugaDf)[15]<-c("conceptName")
  ##############################################

  rm(aa, sugaDfclone)
  sugaDf<-sugaDf[c("conceptCode", "conceptName", "conceptSynonym", "domainId", "vocabularyId", "conceptClassId",
                   "validStartDate", "validEndDate", "invalidReason","ancestorConceptCode","conceptCodeBf","material", "dosage", "dosageUnit", "sanjungName" )]

  sugaDf<-as.data.frame(sugaDf[,-15])

rm(dicData)


}
######################################################################################################## merge, upload DB
uploadProcess<-function(){
  masterData<-rbind(sugaDf, mdcDf, igrDf, matDf)
  aa<-masterData


  ##############################################need to modified
  ##append null list in invalid reason

  lapply(aa$invalidReason, )
  ##############################################

  #write.csv(masterData,file="C:/Users/AJOU/Desktop/ediToOMOP/결과파일/master_data.csv", fileEncoding="UTF-8")

  dbms<-"sql server"
  user<-"ycseong07"
  password<-"zmffhqj1!"
  server<-"128.1.99.58"
  schema <- 'ediToOmop.dbo'

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
}


