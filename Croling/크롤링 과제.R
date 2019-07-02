library('rvest')

# read_html()
# read_table()

# html_node()  : 매칭되는 한 요소만 반환
# html_nodes() : 모든 요소를 반환한다.
# 즉, id를 찾을 경우에는 html_node()를 사용하면 되고, 
# tag,class로 같은 요소를 모두 추출하고자 할 경우에는 html_nodes()를 사용한다.
# ex) class가 title인 요소들 전부를 추출할 경우 html_nodes()를 사용.

## 1. 기본 추출 (tvcast 제목)
html_tvcast <- read_html("http://tvcast.naver.com/jtbc.youth",encoding = "UTF-8")
# <dt class = "title">이란 태그안에 제목이 들어가 있다.
# class가 title인 부분에서 안에 있는 태그('a')에 해당하는 내용을 추출한다.
html_tvcast %>% html_nodes(".title a")
# 그 안에 있는 text만 추출한다. 
html_tvcast %>% html_nodes(".title a") %>% html_text()
# 데이터프레임으로 바꿔주고 저장한다.
html_tvcast %>% html_nodes(".title a") %>% html_text() %>% data.frame()


## 2(1) table 추출 
html_wiki <- read_html('http://en.wikipedia.org/wiki/Student%27s_t-distribution')
# <table class="wikitable">
html_wiki %>% html_nodes(".wikitable") %>% html_table() %>% data.frame()


## 2(2) table 추출 (승객이 가장 많았던 공항 50군데)
html.airports <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic")
df <- html_table(html_nodes(html.airports, "table")[[1]], fill = T)


## 3. 기사 크롤링하기 (https://kuduz.tistory.com/1041)

# http://news.donga.com/search?query=%EA%B7%80%EB%86%8D&more=1&range=1&p=1
basic_url <- 'http://news.donga.com/search?query=%EA%B7%80%EB%86%8D&more=1&range=1&p='

urls      <- NULL
for(x in 0:100){
  urls[x+1] <- paste0(basic_url, x*15+1)
}
length(urls)
# farmer  <- read_html(urls[1])
# farmer2 <- html_nodes(farmer, '.searchCont')
# farmer3 <- html_nodes(farmer2, 'a')
# links   <- html_attr(farmer3, 'href')

library(dplyr)
#links   <- farmer %>% 
#  html_nodes('.searchCont') %>% 
#  html_nodes('a') %>%
#  html_attr('href') %>%    # 속성을 가져오는 attr함수
#  unique()                 # 중복자료 정리.

# grep("pdf",links)                   # 특정한 문자(열)가 들어간 자료 순서를 알려줌.
# links <- links[-grep("pdf",links)]  # 특정 자료만 제거. 

links <- NULL 
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% 
               html_nodes('.searchCont') %>% 
               html_nodes('a') %>% 
               html_attr('href') %>% 
               unique())
}
links <- links[-grep("pdf",links)]
length(links)

txts <- NULL
for(link in links){
  html <- read_html(link)
  txts <- c(txts, html %>% 
              html_nodes('.article_title') %>%
              html_nodes('.title') %>% 
              html_text() %>% 
              unique())
}

txts
