install.packages("rJava")
install.packages('readxl')
install.packages('dplyr')
install.packages("pkgconfig")
install.packages("writexl")

##install.packages("drat")
##drat::addRepo("OHDSI")
##devtools::install_github("OHDSI/DatabaseConnector")

getwd()
setwd("C:/Users/syc/Desktop/rt-rod/수가정리")

library(rJava)
library(readxl)
library(dplyr)
library(writexl)
#########################################
#### Data Upload
data<-read_excel("C:/Users/syc/Desktop/rt-rod/수가정리/수정_수가반영내역(19.10.1.기준)_홈페이지.xlsx",
                 sheet="의치과_급여_전체",
                 col_names = TRUE)


data2<-read_excel("C:/Users/syc/Desktop/rt-rod/수가정리/추가반영)_수가반영내역(19.11.1.기준)_홈페이지.xlsx",
                  sheet="의·치과_급여_삭제",
                  col_names = TRUE)

##파일명 추출 함수 필요
data2_name<-"추가반영)_수가반영내역(19.11.1.기준)_홈페이지.xlsx"




#########################################


# 1. 상위코드: 수가 앞 5자리
sugaCode<-data$"수가코드"
sugaCodeAnc<-substr(sugaCode, 1,5)


# 2. 산정명칭
SJname<-data$"산정명칭"
SJname_romit<-na.omit(SJname)

length(SJname_romit)


# 3. 영어가 같은데 한글이 다른거

SGK<-data$"한글명"
SGE<-data$"영문명"

##비교용
SGKo<-data$"한글명"
SGEn<-data$"영문명"
SGKo2<-data$"한글명2"
SGEn2<-data$"영문명2"

SGKo<-c(".",SGKo[1:length(SGKo)])
SGEn<-c(".",SGEn[1:length(SGEn)])
SGKo2<-c(SGKo2[1:length(SGKo2)], ".")
SGEn2<-c(SGKo2[1:length(SGEn2)], ".")

SGName = c(SGKo,SGEn,SGKo2,SGEn2)
dim(SGName)<-c(length(SGName)/4,4)

diffKoEn<-c()

for(i in 1:nrow(SGName)-1){
  if(isTRUE(ifelse(SGName[i,1] != SGName[i+1,1], TRUE, FALSE)) && isTRUE(ifelse(SGName[i,2] == SGName[i+1,2], TRUE, FALSE))){
    diffKoEn<-c(diffKoEn,SGName[i,1], SGName[i+1,1])
    
  } 
}

length(diffKoEn)


# 4. 영어가 없는거
NoEn<-c()
for(i in 1:nrow(SGName)){
  if(is.na(SGName[i,1]) == FALSE && is.na(SGName[i,2]) == TRUE){
    NoEn<-c(NoEn,SGName[i,1])
  } 
}

length(NoEn)


# 5. 다 합쳐서 중복값 제거

result = c(SJname_romit, diffKoEn, NoEn)
result<-unique(result)

length(result)


## 6. 적용일
apply_date <- data$"적용일자"


##7.삭제일

del_code<-data2$"수가코드"
##Sys.setlocale('LC_ALL','C')
del_date <- substr(data2_name, regexpr("내역\\(",data2_name)+3, regexpr('기준)',data2_name)-1 )

del_data<-data.frame(col1=del_code, col2=del_date)
colnames(del_data) = c("수가코드", "삭제일자")


#########################################
#### excel append

append_temp <- data.frame(col1=sugaCodeAnc,col2=sugaCode, col3=SGK, col4=SGE, col5=SJname, col6=apply_date)
colnames(append_temp) = c("상위코드", "수가코드", "한글명", "영문명", "산정명칭", "적용일자")
append <-merge(append_temp, del_data, by= "수가코드", all=TRUE)

write.csv(append,file="정리파일.csv", fileEncoding="UTF-8")
write.csv(result,file="번역파일.csv", fileEncoding="UTF-8")


#########################################
#### Database Connection
connectionDetail <- DatabaseConnector::createConnectionDetail(dbms="sql server / postgre",user="id", password="qwer1231", server="128.1.888")

con <- DatabaseConnector::connect(connectionDetail)

DatabaseConnector::inserTable(connection = connection, 
                              tableName = "scratch.somedata", 
                              data = data, 
                              dropTableIfExists = TRUE, 
                              createTable = TRUE, 
                              tempTable = FALSE, 
                              useMppBulkLoad = TRUE)