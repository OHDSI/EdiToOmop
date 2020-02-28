#rm(list=ls())
library(EdiToOmop)

##Environment Settings
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = "sql server",
                                                                server = Sys.getenv("server_ip_17"),
                                                                schema = Sys.getenv("ediToOmop"),
                                                                user = Sys.getenv("USER_ID"),
                                                                password = Sys.getenv("PASSWORD_17")
)

vocaTableName = "test_table" ##Table name for vocabulary
###########################


#### preprocessing ####

deviceData<-EdiToOmop::DeviceProcess(exelFilePath="./inst/excels/Device2019.10.1.xlsx",
                                     sheetName = "급여품목(인체조직포함)",
                                     materialData=NULL,
                                     deviceCode = "코 드",
                                     deviceName = "품 명",
                                     startDateName="적용일자",
                                     materialName = "재 질",
                                     KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation_ANSI.csv")


sugaData <- EdiToOmop::SugaProcess(exelFilePath = "./inst/excels/Suga2019.10.1.xlsx",
                                   sheetName = "의치과_급여_전체",
                                   sugaData=NULL,
                                   sugaCode = "수가코드",
                                   KoreanName = "한글명",
                                   EnglishName = "영문명",
                                   startDateName = "적용일자",
                                   sanjungName = "산정명칭",
                                   KoreanDictFile="./inst/csv/suga_Eng_Kor_translation_ANSI.csv"
)

drugData<-EdiToOmop::DrugProcess(exelFilePath = "./inst/excels/Drug2019.10.1.xlsx",
                                 sheetName=NULL,
                                 drugData=NULL,
                                 drugCode = "제품코드",
                                 drugName = "제품명",
                                 clinicalDrugcode = "주성분코드",
                                 drugDosage = "규격",
                                 drugDosageUnit = "단위",
                                 previousConceptCode = "목록정비전코드")

## delete korean in concept_name of drugdata
drugData$conceptName <- gsub("[ㄱ-힣]","", drugData$conceptName)

ediData=rbind(deviceData,sugaData,drugData)

#ediData<-ediData[order(ediData$concept_code),]

max(nchar(ediData$conceptName)) # we will allow lengthy concept name

## del duplicated
dupl<-ediData[duplicated(ediData$conceptCode) | duplicated(ediData$conceptCode, fromLast=TRUE),]
dupl_del<-dupl[dupl$domainId !="Device",]
dupl_add<-dupl[dupl$domainId =="Device",]

ediData<-ediData[!(ediData$conceptCode %in% dupl$conceptCode),]
ediData<-rbind(ediData, dupl_add)

rm(dupl, dupl_add, dupl_del)


#We will insert these data into the database.
#Be careful! This function will remove the table(tableName) and re-generate it.

EdiToOmop::GenerateEdiVocaTable(ediData = drugData,
                                connectionDetails = connectionDetails,
                                vocabularyDatabaseSchema = connectionDetails$schema,
                                tableName = vocaTableName,
                                useMppBulkLoadS = FALSE
)

CreateCsv(ediData = ediData,
          filePath = "./inst/EdiData/EdiData.csv"
)



#### update ####

##Create new Device dataframe

newDeviceData<-EdiToOmop::DeviceProcess(exelFilePath="./inst/excels/Device2019.11.1.xlsx",
                                        sheetName = "급여품목(인체조직포함)",
                                        materialData=NULL,
                                        deviceCode = "코 드",
                                        deviceName = "품 명",
                                        startDateName="적용일자",
                                        materialName = "재 질",
                                        KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation_ANSI.csv")
##Update the existing table

EdiToOmop::NewEdiUpdate(ediData = newDeviceData,
                        startDate = "2019-11-01",
                        domainIds = c("Device"),
                        existingVocaTable = "ediVocaTable",
                        connectionDetails = connectionDetails
)

##Create new Drug dataframe
newDrugData<-EdiToOmop::DrugProcess(exelFilePath = "./inst/excels/Drug2019.11.1.xlsx",
                                    sheetName=NULL,
                                    drugData=NULL,
                                    drugCode = "제품코드",
                                    drugName = "제품명",
                                    clinicalDrugcode = "주성분코드",
                                    drugDosage = "규격",
                                    drugDosageUnit = "단위",
                                    previousConceptCode = "목록정비전코드")

##Update the existing table

EdiToOmop::NewEdiUpdate(ediData = newDrugData,
                        startDate = "2019-11-01",
                        domainIds = c("Drug"),
                        existingVocaTable = "ediVocaTable",
                        connectionDetails = connectionDetails
)

##Create new Suga dataframe
newSugaData<-EdiToOmop::SugaProcess(exelFilePath = "./inst/excels/Suga2019.11.1.xlsx",
                                    sheetName = "의·치과_급여_전체",       ##Watch out! the name of the target sheet was changed.
                                    sugaData=NULL,
                                    sugaCode = "수가코드",
                                    KoreanName = "한글명",
                                    EnglishName = "영문명",
                                    startDateName = "적용일자",
                                    sanjungName = "산정명칭",
                                    KoreanDictFile="./inst/csv/suga_Eng_Kor_translation_ANSI.csv"
)

sum(is.na(newSugaData$conceptName)) #There are concept names not trasnlated into English.

##Update the existing table
# EdiToOmop::NewEdiUpdate(ediData = newSugaData,
#                         startDate = "2019-11-01",
#                         domainIds = c("Procedure", "Measurement"),
#                         existingVocaTable = "ediVocaTable",
#                         connectionDetails = connectionDetails
# )

######

