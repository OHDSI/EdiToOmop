# EdiToOmop
Project to convert Korean EDI code to the OMOP vocabulary

수가정리.R 코드 사용법

건강보험심사평가원 홈페이지
https://www.hira.or.kr/rd/insuadtcrtr/InsuAdtCrtrList.do?pgmid=HIRAA030069000400
-> 월별 수가반영내역 엑셀파일을 다운받고, 패키지 설치 후 Rstudio에서 Data Upload 부분에 세팅해야함

작업을 진행할 파일의 전체 급여 시트를 data에, 그 다음 달 파일의 급여 삭제 부분 시트를 data2에 설정
 
추출된 파일의 한글이 깨지면 encoding-ANSI 적용하여 다시 저장.

DB 업로드 후 코드 수정 예정
