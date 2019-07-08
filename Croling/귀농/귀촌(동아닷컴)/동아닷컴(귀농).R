library(rvest)
library(dplyr)
library(xlsx)
library(stringr)
## 3. 동아닷컴(귀농) (https://kuduz.tistory.com/1041)

#http://www.donga.com/news/search?p=1&query=%EA%B7%80%EB%86%8D&check_news=1&more=1&sorting=1&search_date=1&v1=&v2=&range=1
basic_url <- 'http://www.donga.com/news/search?p='
category  <- '&query=%EA%B7%80%EB%86%8D&check_news=1&more=1&sorting=1&search_date=1&v1=&v2=&range=1'

urls      <- NULL
for(x in 0:97){
  urls[x+1] <- paste0(basic_url, x*15+1, category)
}
length(urls)
# farmer  <- read_html(urls[1])
# farmer2 <- html_nodes(farmer, '.searchCont')
# farmer3 <- html_nodes(farmer2, 'a')
# links   <- html_attr(farmer3, 'href')

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
               html_nodes('.searchList') %>% 
               html_node('a') %>% 
               html_attr('href') %>% 
               unique())
}

# links <- links[-grep("pdf",links)]
length(links)

trim <- function(x) gsub("^\\s+|\\s+$","",x)

title <- NULL
txt   <- NULL
date  <- NULL

for(link in links){
  html  <- read_html(link)
  title <- c(title, html %>% 
              html_node('.article_title') %>%
              html_node('.title') %>% 
              html_text())
  txt   <- c(txt, html %>% 
               html_node('.article_txt') %>%
               html_text() %>% trim())
  date  <- c(date, html %>% 
               html_node('.title_foot') %>%
               html_node('.date01') %>%
               html_text())
}

donga_farm <- data.frame(기사제목 = title,
                            기사내용 = txt,
                            기사입력날짜 = str_sub(date,start=4))

write.xlsx(donga_farm, file = "동아닷컴_귀농.xlsx", row.names=F)
