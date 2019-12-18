## initial data

installr::install.java()
install.packages("rJava")
install.packages('readxl')
install.packages('dplyr')
install.packages("pkgconfig")
install.packages("writexl")
install.packages("DatabaseConnector")
install.packages("openxlsx")
install.packages("lubridate")

install.packages("drat")
drat::addRepo("OHDSI")
install.packages("DatabaseConnector", INSTALL_opts="--no-multiarch")

##devtools::install_github("OHDSI/DatabaseConnector")

getwd()
setwd("C:/Users/syc/Desktop/아주대/ediToOMOP")

library(rJava)
library(readxl)
library(dplyr)
library(writexl)
library(DatabaseConnector)
library(openxlsx)
library(tidyverse)
library(lubridate)

############################################################################
#### Data Upload

#수가: 의/치과 급여
suga_data<-readxl::read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/수가/수정_수가반영내역(19.10.1.기준)_홈페이지.xlsx",
                      sheet="의치과_급여_전체",
                      col_names = TRUE)

#약제
drug_data<-readxl::read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/약제/약제급여목록및급여상한금액표_(2019.10.1.)(23,231)_10.14.수정.xlsx",
                      sheet="19년10월1일_(23,231)",
                      col_names = TRUE)

#치료재료: 급여
mat_data_tmp<-readxl::read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/치료재료/2019.10.1.적용_치료재료대(인체조직포함)_파일(급여)_게시용_최종(수정.xlsx",
                         sheet="급여품목(인체조직포함)",
                         col_names = TRUE)

# mat_del_data_tmp<-read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/치료재료/2019.10.1.적용_치료재료대(인체조직포함)_파일(급여)_게시용_최종.xlsx",
#                              sheet="삭제 및 삭제예정 품목",
#                              col_names = TRUE)



############################################################################
#### Data append

###########################################################################
##수가

CONCEPT_CODE<-suga_data$"수가코드"

ANCESTOR_CONCEPT_CODE<-substr(CONCEPT_CODE, 1,5)

temp1<-suga_data$"한글명"
temp2<-suga_data$"산정명칭"

KOREAN_NAME<-paste(temp1, ",", temp2)
KOREAN_NAME<-gsub(", NA","",KOREAN_NAME)

rm(temp1, temp2)

CONCEPT_CLASS_ID<-c()
for(i in 1:nrow(suga_data)){
  if(startsWith(CONCEPT_CODE[i],"AA")|startsWith(CONCEPT_CODE[i],"AH")|startsWith(CONCEPT_CODE[i],"CA")|
     startsWith(CONCEPT_CODE[i],"FA")|startsWith(CONCEPT_CODE[i],"HA")|startsWith(CONCEPT_CODE[i],"HC")|
     startsWith(CONCEPT_CODE[i],"B")|startsWith(CONCEPT_CODE[i],"C")|startsWith(CONCEPT_CODE[i],"E")|
     startsWith(CONCEPT_CODE[i],"F")|startsWith(CONCEPT_CODE[i],"G")){
    CONCEPT_CLASS_ID[i]<-"Measurement"
  }else{
    CONCEPT_CLASS_ID[i]<-"Procedure"
  }
}

VALID_START_DATE<-c()
for(i in 1:nrow(suga_data)){
  if(is.na(suga_data$"적용일자")){
    VALID_START_DATE[i]<-"1970-01-01"
  }else
    VALID_START_DATE<-suga_data$"적용일자"
}


VALID_END_DATE<-c()
for(i in 1:nrow(suga_data)){
  VALID_END_DATE[i]<-"2099-12-31"
}

DOMAIN_ID<-c()
for(i in 1:nrow(suga_data)){
  if(startsWith(CONCEPT_CODE[i],"AA")|startsWith(CONCEPT_CODE[i],"AH")|startsWith(CONCEPT_CODE[i],"CA")|
     startsWith(CONCEPT_CODE[i],"FA")|startsWith(CONCEPT_CODE[i],"HA")|startsWith(CONCEPT_CODE[i],"HC")|
     startsWith(CONCEPT_CODE[i],"B")|startsWith(CONCEPT_CODE[i],"C")|startsWith(CONCEPT_CODE[i],"E")|
     startsWith(CONCEPT_CODE[i],"F")|startsWith(CONCEPT_CODE[i],"G")){
    DOMAIN_ID[i]<-"Measurement"
  }else{
    DOMAIN_ID[i]<-"Procedure"
  }
}

#data frame
fee_data <- data.frame(CONCEPT_CODE=CONCEPT_CODE, CONCEPT_CODE_BF=NA, ANCESTOR_CONCEPT_CODE=ANCESTOR_CONCEPT_CODE,
                       CONCEPT_NAME=NA, KOREAN_NAME=KOREAN_NAME, MATERIAL=NA, DOSAGE=NA, DOSAGE_UNIT=NA, DOMAIN_ID=DOMAIN_ID,
                       VOCABULARY_ID="Korean EDI",  CONCEPT_CLASS_ID=CONCEPT_CLASS_ID,
                       VALID_START_DATE=VALID_START_DATE, VALID_END_DATE=VALID_END_DATE, INVALID_REASON=NA)

rm(suga_data)


##########################################################################
##약제
mdc_data_tmp<-data.frame()

for(i in 1:nrow(drug_data)){
  if(is.na(drug_data$"제품명"[i])==FALSE){
    mdc_data_tmp<-rbind(mdc_data_tmp,drug_data[i,])
  }
}

CONCEPT_CODE<-mdc_data_tmp$"제품코드"
CONCEPT_CODE_BF<-mdc_data_tmp$"목록정비전코드"
ANCESTOR_CONCEPT_CODE<-mdc_data_tmp$"주성분코드"
KOREAN_NAME<-mdc_data_tmp$"제품명"
DOSAGE<-mdc_data_tmp$"규격"
DOSAGE_UNIT<-mdc_data_tmp$"단위"

CONCEPT_CLASS_ID<-c()
for(i in 1:nrow(mdc_data_tmp)){
  CONCEPT_CLASS_ID[i]<-"Drug"
}

DOMAIN_ID<-c()
for(i in 1:nrow(mdc_data_tmp)){
  DOMAIN_ID[i]<-"Drug"
}

VALID_START_DATE<-c()
for(i in 1:nrow(mdc_data_tmp)){
  VALID_START_DATE[i]<-"1970-01-01"
}

VALID_END_DATE<-c()
for(i in 1:nrow(mdc_data_tmp)){
  VALID_END_DATE[i]<-"2099-12-31"
}
##api에 정보 있으면 추후 수정

#data frame
mdc_data <- data.frame(CONCEPT_CODE=CONCEPT_CODE, CONCEPT_CODE_BF=CONCEPT_CODE_BF, ANCESTOR_CONCEPT_CODE=ANCESTOR_CONCEPT_CODE,
                       CONCEPT_NAME=NA, KOREAN_NAME=KOREAN_NAME, MATERIAL=NA, DOSAGE=DOSAGE, DOSAGE_UNIT=DOSAGE_UNIT,
                       DOMAIN_ID=DOMAIN_ID, VOCABULARY_ID="Korean EDI", CONCEPT_CLASS_ID=CONCEPT_CLASS_ID,
                       VALID_START_DATE=VALID_START_DATE, VALID_END_DATE=VALID_END_DATE, INVALID_REASON=NA)

##################################################################################
##주성분

mdc_data_tmp<-data.frame()

for(i in 1:nrow(drug_data)){
  if(is.na(drug_data$"제품명"[i])==TRUE){
    mdc_data_tmp<-rbind(mdc_data_tmp,drug_data[i,])
  }
}

CONCEPT_CODE<-mdc_data_tmp$"주성분코드"

ANCESTOR_CONCEPT_CODE<-mdc_data_tmp$"주성분코드"

CONCEPT_NAME<-mdc_data_tmp$"제품코드"

DOMAIN_ID<-c()
for(i in 1:nrow(mdc_data_tmp)){
  DOMAIN_ID[i]<-"Drug"
}

CONCEPT_CLASS_ID<-c()
for(i in 1:nrow(mdc_data_tmp)){
  CONCEPT_CLASS_ID[i]<-"Korean General Drug Code"
}

VALID_START_DATE<-c()
for(i in 1:nrow(mdc_data_tmp)){
  VALID_START_DATE[i]<-"1970-01-01"
}

VALID_END_DATE<-c()
for(i in 1:nrow(mdc_data_tmp)){
  VALID_END_DATE[i]<-"2099-12-31"
}


#data frame
ingr_data <- data.frame(CONCEPT_CODE=CONCEPT_CODE, CONCEPT_CODE_BF=NA, ANCESTOR_CONCEPT_CODE=ANCESTOR_CONCEPT_CODE,
                        CONCEPT_NAME=CONCEPT_NAME, KOREAN_NAME=NA, MATERIAL=NA, DOSAGE=NA, DOSAGE_UNIT=NA, DOMAIN_ID=DOMAIN_ID,
                        VOCABULARY_ID="KDC", CONCEPT_CLASS_ID=CONCEPT_CLASS_ID,
                        VALID_START_DATE=VALID_START_DATE, VALID_END_DATE=VALID_END_DATE, INVALID_REASON=NA)
rm(mdc_data_tmp)


#################################################################################
##치재

CONCEPT_CODE<-mat_data_tmp$"코드"
CONCEPT_NAME<-mat_data_tmp$"품명"
KOREAN_NAME<-mat_data_tmp$"품명"
MATERIAL<-mat_data_tmp$"재질"

CONCEPT_CLASS_ID<-c()
for(i in 1:nrow(mat_data_tmp)){
  CONCEPT_CLASS_ID[i]<-"Therapeutic materials"
}

DOMAIN_ID<-c()
for(i in 1:nrow(mat_data_tmp)){
  DOMAIN_ID[i]<-"Device"
}

VALID_START_DATE<-c()
for(i in 1:nrow(mat_data_tmp)){
  if(is.na(mat_data_tmp$"적용일자")){
    VALID_START_DATE[i]<-"1970-01-01"
  }else{
    VALID_START_DATE[i]<-mat_data_tmp$"적용일자"
  }
}

VALID_END_DATE<-c()
for(i in 1:nrow(mat_data_tmp)){
  if(is.na(mat_data_tmp$"삭제일자"[i])){
    VALID_END_DATE[i]<-"2099-12-31"
  }else{
    VALID_END_DATE[i]<-mat_data_tmp$"삭제일자"
  }
}

INVALID_REASON<-c()
for(i in 1:nrow(mat_data_tmp)){
  if(is.na(mat_data_tmp$"삭제일자"[i])){
    INVALID_REASON[i]<-NA
  }else{
    INVALID_REASON[i]<-"D"
  }
}

#data frame
mat_data<-data.frame()
mat_data <- data.frame(CONCEPT_CODE=CONCEPT_CODE, CONCEPT_CODE_BF=NA, ANCESTOR_CONCEPT_CODE=NA,
                       CONCEPT_NAME=CONCEPT_NAME, KOREAN_NAME=KOREAN_NAME, MATERIAL=MATERIAL, DOSAGE=NA, DOSAGE_UNIT=NA, DOMAIN_ID=DOMAIN_ID,
                       VOCABULARY_ID="Korean EDI", CONCEPT_CLASS_ID=CONCEPT_CLASS_ID,
                       VALID_START_DATE=VALID_START_DATE, VALID_END_DATE=VALID_END_DATE, INVALID_REASON=INVALID_REASON)



#### rbind
master_data<-rbind(fee_data, mdc_data, ingr_data, mat_data)

for(i in 1:nrow(master_data)){
  if(is.na(master_data$INVALID_REASON)){
    master_data$INVALID_REASON<-list(NULL)
  }
}

rm(fee_data, mdc_data, ingr_data, mat_data, drug_data)
rm(ANCESTOR_CONCEPT_CODE, CONCEPT_CLASS_ID, CONCEPT_CODE, CONCEPT_CODE_BF, CONCEPT_NAME, DOMAIN_ID, DOSAGE, DOSAGE_UNIT, i, INVALID_REASON, KOREAN_NAME
   , MATERIAL, VALID_END_DATE, VALID_START_DATE)



##########################################
##데이터 확인용 csv 파일
write.csv(master_data,file="C:/Users/AJOU/Desktop/ediToOMOP/결과파일/master_data.csv", fileEncoding="UTF-8")

####ansi로 다시 저장해야하고, 상위코드 다시 짜야함.

#########################################
#### Database Connection
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
                               data = master_data,
                               dropTableIfExists = TRUE,
                               createTable = TRUE,
                               tempTable = FALSE,
                               progressBar = TRUE,
                               useMppBulkLoad = FALSE)

DatabaseConnector::insertTable(connection = con,
                               tableName = "OMOP_CDM_Vocab",
                               data = master_data[299998:nrow(master_data),],
                               dropTableIfExists = FALSE,
                               createTable = FALSE,
                               tempTable = FALSE,
                               progressBar = TRUE,
                               useMppBulkLoad = FALSE)

zz <- DatabaseConnector::querySql(con,'SELECT * FROM OMOP_CDM_Extension')

zz <- DatabaseConnector::querySql(con,'SELECT COUNT(*) FROM OMOP_CDM_Vocab')

class(master_data)
nrow(master_data)

