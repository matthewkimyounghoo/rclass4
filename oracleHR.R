library(rJava)
library(DBI)
library(RJDBC)
library(XML)
library(memoise)
library(KoNLP)
library(wordcloud)
library(dplyr)
library(ggplot2)
library(ggmap)
library(rvest)
library(RColorBrewer)
library(data.table)
library(reshape)
library(stringr)

drv <- JDBC(
  
  "oracle.jdbc.driver.OracleDriver",
  
  "ojdbc6.jar"
  
)

conn <- dbConnect(drv,
                  
                  "jdbc:oracle:thin:@localhost:1521:xe",
                  
                  "hr",
                  
                  "oracle")
####접속코드
tab <- dbGetQuery(conn,"SELECT * FROM TAB")

tname <- tab$TNAME
tname
View(tab)

cnt <- data.frame(dbGetQuery(conn,"SELECT * FROM COUNTRIES"))
dep <- data.frame(dbGetQuery(conn,"SELECT * FROM DEPARTMENTS"))
emp <- data.frame(dbGetQuery(conn,"SELECT * FROM EMPLOYEES"))
empd <- data.frame(dbGetQuery(conn,"SELECT * FROM EMP_DETAILS_VIEW"))
job <- data.frame(dbGetQuery(conn,"SELECT * FROM JOBS"))
jobh <- data.frame(dbGetQuery(conn,"SELECT * FROM JOB_HISTORY"))
loc <- data.frame(dbGetQuery(conn,"SELECT * FROM LOCATIONS"))
reg <- data.frame(dbGetQuery(conn,"SELECT * FROM REGIONS"))

#######################################################
## 문제 5 cnt 의 컬럼명을 한글로 전환하시오
## 국가아이디 = COUNTRY_ID
## 국가명 = COUNTRY_NAME
## 지역아이디 = REGION_ID
cnt <- cnt %>% 
  dplyr::rename(국가아이디 = COUNTRY_ID,국가명=COUNTRY_NAME,지역아이디=REGION_ID)
#######################################################
#######################################################
## 문제 6 dep 의 컬럼명을 한글로 전환하시오
## 부서아이디 = DEPARTMENT_ID
## 부서명 = DEPARTMENT_NAME
## 매니저아이디 = MANAGER_ID
## 위치아이디 = LOCATION_ID
dep <- dep %>% 
  dplyr::rename(부서아이디=DEPARTMENT_ID,부서명=DEPARTMENT_NAME,매니저아이디=MANAGER_ID,위치아이디=LOCATION_ID)
#######################################################
#######################################################
## 문제 7 emp 의 컬럼명을 한글로 전환하시오.
## 그리고 First Name 과 Last Name 을
## 붙여서 이름 으로 된 컬럼을 추가하시오
## 단, 이름 간격은 띄울것. ex) James Dean
## 직원아이디 = EMPLOYEE_ID
## 이메일 = EMAIL
## 전화번호 = PHONE_NUMBER
## 채용일 = HIRE_DATE
## 업무아이디 = JOB_ID
## 연봉 = SALARY
## 커미션비율 = COMMISSION_PCT
## 매니저아이디 = MANAGER_ID
## 부서아이디 = DEPARTMENT_ID
emp <- emp %>% 
  dplyr::rename(직원아이디 = EMPLOYEE_ID,이메일 = EMAIL,전화번호 = PHONE_NUMBER,채용일 = HIRE_DATE,업무아이디 = JOB_ID,연봉 = SALARY,커미션비율 = COMMISSION_PCT,매니저아이디 = MANAGER_ID,부서아이디 = DEPARTMENT_ID)
emp<-emp %>% 
  dplyr::mutate(이름=paste(FIRST_NAME,LAST_NAME)) 
#######################################################
#######################################################
## 문제 8  emp 의 First Name 과 Last Name 컬럼 두개를
## 삭제하시오.
if(is.data.frame(emp)){emp<-subset(emp,select=-c(FIRST_NAME,LAST_NAME))}
#######################################################
#######################################################
## 문제 9
## 매달 지급하는 월급여(연봉 / 12)를 보여주는
## 월급 이라는 컬럼을
## 추가시키시오.(0단위 절삭)
emp<-emp %>% 
  dplyr::mutate(월급=(연봉%/%12))
#######################################################
#######################################################
## 문제 10 job 의 컬럼명을 한글로 전환하시오
## 업무아이디 = JOB_ID
## 업무명 = JOB_TITLE
## 최소연봉 = MIN_SALARY
## 최대연봉 = MAX_SALARY
job<-job %>% 
  dplyr::rename(업무아이디 = JOB_ID,업무명 = JOB_TITLE,최소연봉 = MIN_SALARY,최대연봉 = MAX_SALARY)
#######################################################
#######################################################
## 문제 11 jobh 의 컬럼명을 한글로 전환하시오
## 직원아이디 = EMPLOYEE_ID
## 업무시작일 = START_DATE
## 업무종료일 = END_DATE
## 업무아이디 = JOB_ID
## 부서아이디 = DEPARTMENT_ID
jobh<-jobh %>% 
  dplyr::rename(직원아이디 = EMPLOYEE_ID,업무시작일 = START_DATE,업무종료일 = END_DATE,업무아이디 = JOB_ID,부서아이디 = DEPARTMENT_ID)
#######################################################
#######################################################
## 문제 12 loc 의 컬럼명을 한글로 전환하시오
# 위치아이디 = LOCATION_ID
# 거리주소 = STREET_ADDRESS
# 우편번호 = POSTAL_CODE
# 도시 = CITY
# 주 = STATE_PROVINCE
# 국가아이디 = COUNTRY_ID
loc <- loc %>% 
  dplyr::rename(위치아이디 = LOCATION_ID,거리주소 = STREET_ADDRESS,우편번호 = POSTAL_CODE,도시 = CITY,주 = STATE_PROVINCE,국가아이디 = COUNTRY_ID)
#######################################################
#######################################################
## 문제 13 reg 의 컬럼명을 한글로 전환하시오
## 지역아이디 = REGION_ID
## 지역명 = REGION_NAME
reg <- reg %>% 
  dplyr::rename(지역아이디 = REGION_ID,지역명 = REGION_NAME)
#######################################################
## 문제 14. 연봉이 10000불 이상인
## 사원(emp)의 목록을 이름, 직원아이디, 연봉을
## 연봉 내림차순으로 보여주세요.
emp %>% 
  dplyr::filter(연봉>=10000) %>%
  dplyr::select(이름,직원아이디,연봉) %>% 
  dplyr::arrange(desc(연봉))
#######################################################
## 문제 15. 연봉이 3000 미만인
## 사원에게 보너스로 급여의 1%를 지급하겠다고 합니다
## 대상자의 목록을 이름, 직원아이디, 연봉을 기재하고
## 아이디 오름차순으로 보여주시오. 단 보너스지급내역서
## 라는 이름의 데이터프레임으로 작성한 후 삭제하시오.
## 보너스에는 각 금액에 만원단위를 첨부합니다.
보너스지급내역서 <- emp %>%
  dplyr::filter(연봉<3000) %>%
  dplyr::select(이름,직원아이디,연봉) %>%
  dplyr::arrange(직원아이디) %>% 
  dplyr::mutate(보너스=sprintf("%0.0f 만원",연봉*0.01))
write.csv(보너스지급내역서,'보너스지금내역서.csv')
#######################################################
#######################################################
## 문제 16. 직원중에서 급여가 가장 높은 사람이
## CEO 라고 합니다. 이름이 무엇입니까?
## apply(object, direction, function to apply)
## 적용방향 -> 1:가로방향, 2: 세로방향
k<-apply(emp %>% select(연봉),2,max)
emp%>%
  dplyr::filter(연봉==k) %>%
  dplyr::select(이름)
#######################################################
## 문제 17. 연봉이 10000이 넘는 직원의 부서명, 이름,
## 연보을 출력하시오.
emp %>% 
  dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::filter(연봉>=10000) %>% 
  dplyr::select(부서명,이름,연봉) %>% 
  dplyr::arrange(desc(연봉))
#######################################################

#### [문제 18]
#### 연봉이 12000 이 넘는 직원의 부서명,이름,연봉,직책
#### 을 기재하시오.
View(emp %>% 
  dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::left_join(job,by='업무아이디') %>% 
  dplyr::filter(연봉>=12000) %>% 
  dplyr::select(부서명,이름,연봉,업무명) ) %>% 
  dplyr::arrange(desc(연봉))
######################################################
#######################################################
#### [문제 19]
#### 부서명 별로 연봉 평균을 구하시오
##group_by
View(emp %>% 
  dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::group_by(부서명,부서아이디) %>% 
  dplyr::summarise(연봉평균=mean(연봉,na.rm=T)) %>% #mutate는 개별자료만, summarise는 group_by와 함께
  dplyr::arrange(desc(연봉평균)))
######################################################
#######################################################
#### [문제 20]
#### 이 회사의 부서 수를 기재하시오.
dep %>%
  dplyr::distinct(부서명) %>% 
  count
######################################################
#문제 21. 부서명, 도시, 각 부서별사원수,
## 각 부서 별 평균 연봉을 조회한다.
## 평균 연봉은 소수점 2 자리까지만 표현한다.
## emp: 연봉
## dep: 부서명
## loc: 도시
## [힌트] left_join, group_by, summarise

q<-emp %>% 
  dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::left_join(loc,by='위치아이디') %>% 
  dplyr::group_by(부서명,도시) %>% 
  dplyr::summarise(사원수=length(직원아이디),평균연봉=sprintf('%0.2f만원',mean(연봉)))
View(q)

##문제 22. 부서별로 가장 높은 연봉을
#### 부서아이디, 부서명, 최대연봉으로
#### 표시되도록 하세요.

q<-emp %>% 
  dplyr::left_join(dep,by='부서아이디') %>% 
  dplyr::group_by(부서아이디,부서명) %>% 
  dplyr::summarise(최대연봉=max(연봉))


##문제 23. 부서아이디를 발급받지 않으면
#### 신입입니다.
#### 신입의 이름과 연봉, 부서아이디 없음을 출력하시오.
test <- c('apple','banana','cherry','Apple','Pineapple',NA)
ifelse(stringr::str_detect(test,'A'),'Good','Bad')

emp %>% 
  dplyr::filter(is.na(emp$부서아이디)) %>% 
  dplyr::select(이름,연봉,부서아이디)

## 문제 24. 직원중에서 이름에 대문자 S 와 T
## 가 포함된 직원을 출력하시오.
ifelse(stringr::str_detect(emp$이름,'S'),'Y','N')

emp %>%
  dplyr::filter(ifelse(stringr::str_detect(emp$이름,'S'),'Y','N')=='Y' & ifelse(stringr::str_detect(emp$이름,'T'),'Y','N')=='Y') %>%
  dplyr::select(이름)
  
emp %>%
  dplyr::filter(str_detect(emp$이름,'S'),str_detect(emp$이름,'T')) %>% 
  dplyr::select(이름)

         
  