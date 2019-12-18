rm(list=ls())
deviceData<-DeviceProcess(exelFilePath="./inst/excels/Device2019.10.1.xlsx",
                          sheetName = "급여품목(인체조직포함)",
                          materialData=NULL,
                          deviceCode = "코 드",
                          deviceName = "품 명",
                          startDateName="적용일자",
                          materialName = "재 질",
                          KoreanDictFile="./inst/csv/tmt_Eng_Kor_translation.csv")

sugaData <- SugaProcess(exelFilePath = "./inst/excels/Suga2019.10.1.xlsx",
                        sheetName = "의치과_급여_전체",
                        sugaData=NULL,
                        sugaCode = "수가코드",
                        KoreanName = "한글명",
                        EnglishName = "영문명",
                        startDateName = "적용일자",
                        sanjungName = "산정명칭",
                        KoreanDictFile="./inst/csv/suga_Eng_Kor_translation.csv"
)

drugData<-drugProcess(exelFilePath = "./inst/excels/Drug2019.10.1.xlsx",
                      sheetName=NULL,
                      drugData=NULL,
                      drugCode = "제품코드",
                      drugName = "제품명",
                      clinicalDrugcode = "주성분코드",
                      drugDosage = "규격",
                      drugDosageUnit = "단위",
                      previousConceptCode = "목록정비전코드")
