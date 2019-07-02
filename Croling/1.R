## ===================================================================================
## [빅데이터를 활용한 스마트 데이터 전문가 양성과정]  ================================
## [R로 웹 데이터를 가져오는 방법]                    ================================
## 2019-07-02(화) / 크롤링                            ================================
## ===================================================================================

install.packages("rvest")
install.packages("dplyr")
install.packages("stringr")
library(rvest)
library(dplyr)
library(stringr)
trim <- function(x) gsub("^\\s+|\\s+$","",x) # 공백제거를 위한 함수


## 1. 한빛아카데미 _ 컴퓨터공학 ======================================================

base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
page     <- 'page='
category <- '&cate_cd=004007&srt=p_pub_date'
computer_books <- data.frame(title=c(),writer=c(),price=c())

for (i in 1:6){
  url      <- paste0(base_url,page,i,category)
  html     <- read_html(url)

  #html     <- read_html(base_url)
  #book_list <- html_node(html, '.sub_book_list_area')
  #lis       <- html_nodes(book_list, 'li')
  
  html %>% 
    html_node('.sub_book_list_area') %>% 
    html_nodes('li') -> lis
  
  # 빈 리스트를 먼저 설정해줘야 벡터하나씩이 아닌 리스트로 나온다.
  price <- c()
  title <- c()
  writer<- c()
  for(li in lis){
    pr <- html_node(li,'.price') %>% html_text()
    pr <- gsub("\\\\","",pr)
    price <- c(price, pr)
    title <- c(title, html_node(li,'.book_tit') %>% html_text())
    writer<- c(writer,html_node(li,'.book_writer') %>% html_text())
    #  cat(title,writer,price,'\n')
  }
  books <- data.frame(title=title, writer=writer, price=price)
  computer_books <- rbind.data.frame(computer_books, books)
}
computer_books

## 2. 한빛아카데미 _ 정보통신/전기/전자 ======================================================

base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
page     <- 'page='
category1 <- '&cate_cd=00400'
category2 <- '&srt=p_pub_date'
information_books <- data.frame(title=c(),writer=c(),price=c())
for(k in 1:6){
  
}

for (i in 1:4){
  url      <- paste0(base_url,page,i,category)
  html     <- read_html(url)
  
  html %>% 
    html_node('.sub_book_list_area') %>% 
    html_nodes('li') -> lis
  
  price <- c()
  title <- c()
  writer<- c()
  for(li in lis){
    pr <- html_node(li,'.price') %>% html_text()
    pr <- gsub("\\\\","",pr)
    price <- c(price, pr)
    title <- c(title, html_node(li,'.book_tit') %>% html_text())
    writer<- c(writer,html_node(li,'.book_writer') %>% html_text())
    #  cat(title,writer,price,'\n')
  }
  books <- data.frame(title=title, writer=writer, price=price)
  information_books <- rbind.data.frame(computer_books, books)
}
information_books


base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'