rm(list=ls())

#### preprocessing ####

deviceData<-DeviceProcess(exelFilePath="./inst/excels/Device2019.10.1.xlsx",
                          sheetName = "급여품목(인체조직포함)",
                          materialData=NULL,
                          deviceCode = "코 드",
                          deviceName = "품 명",
                          startDateName="적용일자",
                          materialName = "재 질",
                          KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation_ANSI.csv")

sugaData <- SugaProcess(exelFilePath = "./inst/excels/Suga2019.10.1.xlsx",
                        sheetName = "의치과_급여_전체",
                        sugaData=NULL,
                        sugaCode = "수가코드",
                        KoreanName = "한글명",
                        EnglishName = "영문명",
                        startDateName = "적용일자",
                        sanjungName = "산정명칭",
                        KoreanDictFile="./inst/csv/suga_Eng_Kor_translation_ANSI.csv"
)

drugData<-DrugProcess(exelFilePath = "./inst/excels/Drug2019.10.1.xlsx",
                      sheetName=NULL,
                      drugData=NULL,
                      drugCode = "제품코드",
                      drugName = "제품명",
                      clinicalDrugcode = "주성분코드",
                      drugDosage = "규격",
                      drugDosageUnit = "단위",
                      previousConceptCode = "목록정비전코드")

delDeviceData<-DelDeviceProcess(exelFilePath="./inst/excels/Device2019.10.1.xlsx",
                          sheetName = "삭제 및 삭제예정 품목",
                          materialData=NULL,
                          deviceCode = "코 드",
                          deviceName = "품 명",
                          endDateName="적용일자",
                          materialName = "재 질",
                          KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation_ANSI.csv")

# masterData <- UploadProcess(dbmsName<-"sql server",
#                         userName<-"ycseong07",
#                         passwordName<-"zmffhqj1!",
#                         serverName<-"128.1.99.58",
#                         schemaName <- 'ediToOmop.dbo'
#
# )


#### update ####

newDeviceData<-NewDeviceProcess(exelFilePath="./inst/excels/Device2019.11.1.xlsx",
                          sheetName = "급여품목(인체조직포함)",
                          materialData=NULL,
                          deviceCode = "코 드",
                          deviceName = "품 명",
                          startDateName="적용일자",
                          materialName = "재 질",
                          startMonth = "2019-11-01"
)

newSugaData <- NewSugaProcess(exelFilePath = "./inst/excels/Suga2019.11.1.xlsx",
                        sheetName = "의·치과_급여_전체",
                        sugaData=NULL,
                        sugaCode = "수가코드",
                        KoreanName = "한글명",
                        EnglishName = "영문명",
                        startDateName = "적용일자",
                        sanjungName = "산정명칭"
)

newDrugData <- NewDrugProcess(exelFilePath = "./inst/excels/Drug2019.11.1.xlsx",
                      sheetName=NULL,
                      drugData=NULL,
                      drugCode = "제품코드",
                      drugName = "제품명",
                      clinicalDrugcode = "주성분코드",
                      drugDosage = "규격",
                      drugDosageUnit = "단위",
                      previousConceptCode = "목록정비전코드"
)

newDelDeviceData <- NewDelDeviceProcess(exelFilePath="./inst/excels/Device2019.11.1.xlsx",
                                sheetName = "삭제 및 삭제예정 품목",
                                materialData=NULL,
                                deviceCode = "코 드",
                                deviceName = "품 명",
                                endDateName="적용일자",
                                materialName = "재 질",
                                delMonth = "2019-10-31"
)

newData<-CreatingNewTable()


# exData <- ImportingProcess(dbms<-"sql server",
#                               user<-"ycseong07",
#                               password<-"zmffhqj1!",
#                               server<-"128.1.99.58",
#                               schema <- 'ediToOmop.dbo'
#
# )

newConcept <- ExtractNew()

deletedConcept <- ExtractDel()

