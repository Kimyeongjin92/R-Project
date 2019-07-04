## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [크롤링을 위해 Selenium을 사용해보자]              ================================
## 2019-07-02(화) / Selenium                          ================================
## ===================================================================================


# 가상의 브라우저(크롬)로서 흉내를 낸다.
# 로그인이 필요한 부분에 대해 

# 1. https://sancj.tistory.com/62에 들어가서 필요한 자료를 받는다.
# 2. cmd를 관리자 권한으로 실행한 후, dir을 입력하여 해당 java 버전으로 다운받는다.

## 0. 패키지 설치
install.packages('RSelenium')
library(RSelenium)
library(rvest)
library(stringr)
trim <- function(x) gsub("^\\s+|\\s+$","",x)

## 1. 크롬창 연결.
remDr <- remoteDriver(remoteServerAddr='localhost',port=4445L,browserName='chrome')
remDr$open() 

# 여기에서 $는 객체[attribute(변수)  method(동작,함수)]를 지원하는 
# 자동차라고 하면 동작으로 speed-up down stop 등이 있으나 fly는 할 수가 없다. 즉 쓸 수 없다.
# 객체 안에서 사용(동작)할 수 있는 함수를 미리 만들어 놓는다. 
# 여기서 remoteDriver가 일종의 객체인데 open navigate findElement setElementAttribute 등의 method를 쓸 수 있다.

## 2. 로그인 창 연결과 루트 설정 
remDr$navigate('https://nid.naver.com/nidlogin.login')   # 로그인 창으로 바뀐다.
txt_id    <- remDr$findElement(using="css selector", '#id')       # css selector를 사용해서 #id값을 찾겠다. html_node 같은 역할을 한다.
txt_pw    <- remDr$findElement(using="id", value='pw')            # input type이 password로 되어있는 것은 **** 이렇게 된다는 뜻.
login_btn <- remDr$findElement(using="class", value='btn_global') # submit : 제출하는 버튼이다. / title alt value는 css를 사용해서 선택할 수 없다. class를 찾자.
# 여기서 using= css selector는 기존의 html처럼 사용하면 된다. #(샾)은 id를 뜻한다.               (css selector, '#pw')
# 만약 using= css selector를 사용할 때 class를 사용하려면 '.btn_global'과 같이 .(점)을 붙여준다. (css selector, '.btn_global')

## 3. 아이디 패스워드 입력
txt_id$setElementAttribute('value','dudwlsrla21') # 아이디 입력
txt_pw$setElementAttribute('value','***********') # 비밀번호 입력
login_btn$clickElement()                          # 로그인이 된다.

url_item <- remDr$getPageSource()[[1]]
url_item <- read_html(url_item, encoding = "UTF-8")
item <- url_item %>% 
  html_node("#list_for_view") %>% 
#  html_node(".mailList preview_none sender_context") %>% 
  html_text()


## 4. html_nodes() 필요한 자료 선택하기.
remDr$navigate("https://mail.naver.com/")
mail_texts <- remDr$findElement(using="id", value="list_for_view") # (using='css selector', 'subject')
mail_texts

## 5. html_text() 긁어오기
mail_texts <-mail_texts$getElementText()
mail_texts

## 6.
tmp <- str_split(mail_texts, '\n') %>% .[[1]]

## 7.
sender  <- NULL
subject <- NULL
time    <- NULL
for(i in 1:20){
  sender  <- c(sender,  tmp[3*i-2]) # 1 4 7 10...
  subject <- c(subject, tmp[3*i-1]) # 2 5 8 11...
  time    <- c(time,    tmp[3*i])   # 3 6 9 12...
}

df_mail   <- data.frame(보낸이=sender,
                        제목=subject,
                        시간=time)
df_mail
remDr$close  # open을 했으면 항상 close를 해줘야 한다.
