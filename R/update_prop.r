############ 추가해야할 사항들 ##################

# 1. 고시 파일 업로드되면 자동으로 특정 디렉토리에 다운받아서 R 돌리게하는 코드 필요
# 2. 치재 한글/영문명 구분
# 3. 수가 상위코드 추출 제대로 되어야함
# 4. 영문명 다 되면 삽입해야함
# 5. concept_code_bf 처리방법 필요
# 6. 변경은 고려 안했는데, 혹시 수가말고 중요한 내용 변동도 변동에 들어가는지 확인해야함
# 7. 신설 항목에 대한 영문명 or 한글명 없으면 어떡하냐
# 8. 비급여까지 올리면 급여중지 이런거 신경 안써도 됨
# 9. 치재에는 U를 넣을만한 데이터가 없음


# 수가, 약물
# 기존데이터와 비교해 새로생긴거 넣고, 빠진거 d처리, 내용 바뀐거 있으면 확인해서 u처리
# 
# 치재
# 신설: 기존데이터와 비교해 새로생긴거 넣고, 내용 바뀐거 있으면 확인해서 u처리
# 삭제: 삭제 시트에 앞으로 삭제될 애들 다들어가 있음. 매번 삭제시트를 받은 다음에, 지금 달에 해당하는 애들만 d처리.

##참고
# 날짜 받을 때 toString으로 받는게 속편하다


############################################################################## library

install.packages('readxl')
install.packages('dplyr')

install.packages("drat")
drat::addRepo("OHDSI")
install.packages("DatabaseConnector", INSTALL_opts="--no-multiarch")
##devtools::install_github("OHDSI/DatabaseConnector")

library(readxl)
library(dplyr)
library(DatabaseConnector)

#### Data table load

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

ExTable <- DatabaseConnector::querySql(con,'SELECT * FROM OMOP_CDM_Vocab')

############################################################################## load new data

#수가: 의/치과 급여
suga_data<-read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/수가/추가반영)_수가반영내역(19.11.1.기준)_홈페이지.xlsx",
                      sheet="의·치과_급여_전체",
                      col_names = TRUE)

#약제
drug_data<-read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/약제/약제급여목록및급여상한금액표_(2019.11.1.)(23,565)_11.1.수정.xlsx",
                      sheet="19년11월1일_(23,565)",
                      col_names = TRUE)

#치료재료: 급여
mat_data_tmp<-read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/치료재료/2019.11.1.적용_치료재료대(인체조직포함)_파일(급여)_수정용.xlsx",
                         sheet="급여품목(인체조직포함)",
                         col_names = TRUE)

#치료재료: 삭제 및 삭제예정
mat_del_data_tmp<-read_excel("C:/Users/AJOU/Desktop/ediToOMOP/소스파일/치료재료/2019.11.1.적용_치료재료대(인체조직포함)_파일(급여)_수정용.xlsx",
                         sheet="삭제 및 삭제예정 품목",
                         col_names = TRUE)


############################################################################## merging updated data frame 

####수가 
concept_code<-suga_data$"수가코드"

ancestor_concept_code<-substr(concept_code, 1,5)

temp1<-suga_data$"한글명"
temp2<-suga_data$"산정명칭"

korean_name<-paste(temp1, ",", temp2)
korean_name<-gsub(", NA","",korean_name)

rm(temp1, temp2)

concept_class_id<-c()
for(i in 1:nrow(suga_data)){
  concept_class_id[i]<-"Fee"
}

valid_start_date<-c()
for(i in 1:nrow(suga_data)){
  if(is.na(suga_data$"적용일자")){
    valid_start_date[i]<-"1970-01-01"
  }else
    valid_start_date<-suga_data$"적용일자"
}


valid_end_date<-c()
for(i in 1:nrow(suga_data)){
  valid_end_date[i]<-"2099-12-31"
}

domain_id<-c()
for(i in 1:nrow(suga_data)){
  if(startsWith(concept_code[i],"AA")|startsWith(concept_code[i],"AH")|startsWith(concept_code[i],"CA")|
     startsWith(concept_code[i],"FA")|startsWith(concept_code[i],"HA")|startsWith(concept_code[i],"HC")|
     startsWith(concept_code[i],"B")|startsWith(concept_code[i],"C")|startsWith(concept_code[i],"E")|
     startsWith(concept_code[i],"F")|startsWith(concept_code[i],"G")){
    domain_id[i]<-"Measurement"
  }else{
    domain_id[i]<-"Procedure"
  }
}

#data frame
fee_data <- data.frame(concept_code=concept_code, concept_code_bf=NA, ancestor_concept_code=ancestor_concept_code,
                       concept_name=NA, korean_name=korean_name, material=NA, dosage=NA, dosage_unit=NA, domain_id=domain_id,
                       vocabulary_id="Korean EDI",  concept_class_id=concept_class_id,
                       valid_start_date=valid_start_date, valid_end_date=valid_end_date, invalid_reason=NA)

rm(suga_data)


####약제
mdc_data_tmp<-data.frame()

for(i in 1:nrow(drug_data)){
  if(is.na(drug_data$"제품명"[i])==FALSE){
    mdc_data_tmp<-rbind(mdc_data_tmp,drug_data[i,])
  }
}

concept_code<-mdc_data_tmp$"제품코드"
concept_code_bf<-mdc_data_tmp$"목록정비전코드"
ancestor_concept_code<-mdc_data_tmp$"주성분코드"
korean_name<-mdc_data_tmp$"제품명"
dosage<-mdc_data_tmp$"규격"
dosage_unit<-mdc_data_tmp$"단위"

concept_class_id<-c()
for(i in 1:nrow(mdc_data_tmp)){
  concept_class_id[i]<-"Drug"
}

domain_id<-c()
for(i in 1:nrow(mdc_data_tmp)){
  domain_id[i]<-"Drug"
}

valid_start_date<-c()
for(i in 1:nrow(mdc_data_tmp)){
  valid_start_date[i]<-"1970-01-01"
}

valid_end_date<-c()
for(i in 1:nrow(mdc_data_tmp)){
  valid_end_date[i]<-"2099-12-31"
}


#data frame
mdc_data <- data.frame(concept_code=concept_code, concept_code_bf=concept_code_bf, ancestor_concept_code=ancestor_concept_code,
                       concept_name=NA, korean_name=korean_name, material=NA, dosage=dosage, dosage_unit=dosage_unit,
                       domain_id=domain_id, vocabulary_id="Korean EDI", concept_class_id=concept_class_id,
                       valid_start_date=valid_start_date, valid_end_date=valid_end_date, invalid_reason=NA)

####주성분

mdc_data_tmp<-data.frame()

for(i in 1:nrow(drug_data)){
  if(is.na(drug_data$"제품명"[i])==TRUE){
    mdc_data_tmp<-rbind(mdc_data_tmp,drug_data[i,])
  }
}

concept_code<-mdc_data_tmp$"주성분코드"

ancestor_concept_code<-mdc_data_tmp$"주성분코드"

concept_name<-mdc_data_tmp$"제품코드"

korean_name<-mdc_data_tmp$"제품코드"

domain_id<-c()
for(i in 1:nrow(mdc_data_tmp)){
  domain_id[i]<-"Drug"
}

concept_class_id<-c()
for(i in 1:nrow(mdc_data_tmp)){
  concept_class_id[i]<-"Korean General Drug Code"
}

valid_start_date<-c()
for(i in 1:nrow(mdc_data_tmp)){
  valid_start_date[i]<-"1970-01-01"
}

valid_end_date<-c()
for(i in 1:nrow(mdc_data_tmp)){
  valid_end_date[i]<-"2099-12-31"
}


#data frame
ingr_data <- data.frame(concept_code=concept_code, concept_code_bf=NA, ancestor_concept_code=ancestor_concept_code,
                        concept_name=concept_name, korean_name=korean_name, material=NA, dosage=NA, dosage_unit=NA, domain_id=domain_id,
                        vocabulary_id="KDC", concept_class_id=concept_class_id,
                        valid_start_date=valid_start_date, valid_end_date=valid_end_date, invalid_reason=NA)


rm(mdc_data_tmp)


####치재

concept_code<-mat_data_tmp$"코 드"
concept_name<-mat_data_tmp$"품 명"
korean_name<-mat_data_tmp$"품 명"
material<-mat_data_tmp$"재 질"

concept_class_id<-c()
for(i in 1:nrow(mat_data_tmp)){
  concept_class_id[i]<-"Therapeutic Materials"
}

domain_id<-c()
for(i in 1:nrow(mat_data_tmp)){
  domain_id[i]<-"Device"
}

valid_start_date<-c()
for(i in 1:nrow(mat_data_tmp)){
    valid_start_date[i]<-toString(mat_data_tmp$"적용일자"[i])
}

valid_end_date<-c()
for(i in 1:nrow(mat_data_tmp)){
  valid_end_date[i]<-"2099-12-31"
}

invalid_reason<-c()
for(i in 1:nrow(mat_data_tmp)){
  invalid_reason[i]<-NA
}

#data frame
mat_data<-data.frame()
mat_data <- data.frame(concept_code=concept_code, concept_code_bf=NA, ancestor_concept_code=NA,
                       concept_name=concept_name, korean_name=korean_name, material=material, dosage=NA, dosage_unit=NA, domain_id=domain_id,
                       vocabulary_id="Korean EDI", concept_class_id=concept_class_id,
                       valid_start_date=valid_start_date, valid_end_date=valid_end_date, invalid_reason=invalid_reason)

rm(mat_data_tmp)


####치재삭제테이블

concept_code<-mat_del_data_tmp$"코 드"
concept_name<-mat_del_data_tmp$"품 명"
korean_name<-mat_del_data_tmp$"품 명"
material<-mat_del_data_tmp$"재 질"

concept_class_id<-c()
for(i in 1:nrow(mat_del_data_tmp)){
  concept_class_id[i]<-"Therapeutic Materials"
}

domain_id<-c()
for(i in 1:nrow(mat_del_data_tmp)){
  domain_id[i]<-"Device"
}

valid_start_date<-c()
for(i in 1:nrow(mat_del_data_tmp)){
    valid_start_date[i]<-"1970-01-01"
}

valid_end_date<-c()
for(i in 1:nrow(mat_del_data_tmp)){
  valid_end_date[i]<-toString(mat_del_data_tmp$"적용일자"[i])
}

invalid_reason<-c()
for(i in 1:nrow(mat_del_data_tmp)){
  invalid_reason[i]<-NA
}

#data frame
matDelTable<-data.frame()
matDelTable <- data.frame(concept_code=concept_code, concept_code_bf=NA, ancestor_concept_code=NA,
                       concept_name=concept_name, korean_name=korean_name, material=material, dosage=NA, dosage_unit=NA, domain_id=domain_id,
                       vocabulary_id="Korean EDI", concept_class_id=concept_class_id,
                       valid_start_date=valid_start_date, valid_end_date=valid_end_date, invalid_reason=invalid_reason)

rm(mat_del_data_tmp)

#### create new table

NewTable<-rbind(fee_data, mdc_data, ingr_data, mat_data)
rm(fee_data, mdc_data, ingr_data, mat_data, drug_data)
rm(ancestor_concept_code, concept_class_id, concept_code, concept_code_bf, concept_name, domain_id, dosage, dosage_unit, i, invalid_reason, korean_name
   , material, valid_end_date, valid_start_date)

############################################################################## add new concept

##extable에 없는 코드가 newtable에 있으면 모든 정보 append 굿굿
c<-data.frame()

for(i in 1:nrow(NewTable)){
  if(is.na(match(NewTable$"concept_code"[i], ExTable$"CONCEPT_CODE"))){
    c <-rbind(c, NewTable[i,])
  }
}


############################################################################## update concept


############################################################################## delete concept

##치재: extable에 있던게 newtable에서 사라지고, matDelTable로 옮겨졌다? D처리

##ex에 있는데 new에 없는거(만약 이중에 삭제내역으로 안옮겨지는 항목이 있다면 처리 더 필요)
e<-data.frame()

for(i in 1:nrow(ExTable)){
  if(is.na(match(ExTable$"CONCEPT_CODE"[i], NewTable$"concept_code"))){
    e <- rbind(e, ExTable[i,])
  }
}

f<- subset(e, DOMAIN_ID=="Device")



##수가, 약물: extable에 있던게 newtable에서 사라졌다? D 처리 굿굿
d<-data.frame()

for(i in 1:nrow(ExTable)){
  if(is.na(match(ExTable$"CONCEPT_CODE"[i], NewTable$"concept_code"))){
    d <-rbind(d, ExTable[i,])
  }
}



###일단 결과 다 보고 업데이트 처리 


#### 치재 삭제는 삭제테이블에서, 신규는 newtable에서
#### U 일단 거르고
#### 수가, 약물은 신규, 삭제 다 newtable에서
#### 테이블 두개 비교해서 새로생긴거 넣고, 빠진거 d처리, 내용 바뀐거 있으면 확인해서 u처리



setwd("C:/Users/AJOU/Desktop/ediToOMOP")
save.image()

save.image(file="C:/Users/AJOU/Desktop/ediToOMOP/update.rData")
load("update.rData")
