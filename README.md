# EdiToOmop
The package to convert Korean EDI code to the OMOP vocabulary


##HOW TO RUN

### Preprocessing for the EDI codes

`
library(EdiToOmop)

##Environment Settings
connectionDetails <- DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                                user=Sys.getenv("USER_ID"),
                                                                schema=Sys.getenv("MY_SCHEMA"),
                                                                password=Sys.getenv("PASSWORD"),
                                                                server=Sys.getenv("MY_SERVER"))
vocaTableName = "ediVocaTable" ##Table name for vocabulary


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

ediData=rbind(deviceData,sugaData,drugData)
`

### Insert EDI codes into the database or export them as csv file

`
#Be careful! This function will remove the table(tableName) and re-generate it.
EdiToOmop::GenerateEdiVocaTable(ediData = ediData,
                                connectionDetails = connectionDetails,
                                vocabularyDatabaseSchema = connectionDetails$schema,
                                tableName = vocaTableName,
                                useMppBulkLoadS = FALSE
)

EdiToOmop::CreateCsv(ediData = ediData,
          filePath = "./inst/EdiData/EdiData.csv"
)
`

### Update
`


newDeviceData<-EdiToOmop::DeviceProcess(exelFilePath="./inst/excels/Device2019.11.1.xlsx",
                                        sheetName = "급여품목(인체조직포함)",
                                        materialData=NULL,
                                        deviceCode = "코 드",
                                        deviceName = "품 명",
                                        startDateName="적용일자",
                                        materialName = "재 질",
                                        KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation_ANSI.csv")
###Update the existing table

EdiToOmop::NewEdiUpdate(ediData = newDeviceData,
                        startDate = "2019-11-01",
                        domainIds = c("Device"),
                        existingVocaTable = "ediVocaTable",
                        connectionDetails = connectionDetails
)

newDrugData<-EdiToOmop::DrugProcess(exelFilePath = "./inst/excels/Drug2019.11.1.xlsx",
                                    sheetName=NULL,
                                    drugData=NULL,
                                    drugCode = "제품코드",
                                    drugName = "제품명",
                                    clinicalDrugcode = "주성분코드",
                                    drugDosage = "규격",
                                    drugDosageUnit = "단위",
                                    previousConceptCode = "목록정비전코드")


EdiToOmop::NewEdiUpdate(ediData = newDrugData,
                        startDate = "2019-11-01",
                        domainIds = c("Drug"),
                        existingVocaTable = "ediVocaTable",
                        connectionDetails = connectionDetails
)


`
