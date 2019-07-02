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

## 2. 한빛아카데미 _ 카테고리별 (페이지 포함) ======================================================

install.packages('openxlsx')
library(rvest)
library(dplyr)
library(stringr)
#library(xlsx)
library(openxlsx)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)

cat_names = c('컴퓨터공학', '정보통신_전기_전자', '수학_과학_공학', '프로그래밍_웹',
              '그래픽_디자인', 'OA_활용', '전기기본서', '전기기사',
              '전기산업기사', '전기공사기사', '전기공사산업기사')
categories = c('004007', '004008', '004003', '004004', '004005', '004006',
               '005005', '005001', '005002', '005003', '005004')
pages = c(6, 4, 3, 3, 1, 2, 2, 2, 1, 1, 1)

base_url <- 'http://www.hanbit.co.kr/academy/books/category_list.html?'
page <- 'page='
category <- '&cate_cd='
sort <- '&srt=p_pub_date'

wb <- createWorkbook()
for (cat_no in 1:11) {
  df_books <- data.frame(title=c(), writer=c(), price=c())
  print(cat_no)
  
  for (i in 1:pages[cat_no]) {
    url <- paste0(base_url, page, i, category, categories[cat_no], sort)
    html <- read_html(url)
    
    html %>%
      html_node('.sub_book_list_area') %>%
      html_nodes('li') -> lis
    lis
    
    price <- c()
    title <- c()
    writer <- c()
    for (li in lis) {
      pr <- html_node(li, '.price') %>% html_text()
      pr <- gsub("\\\\", "", pr)
      price <- c(price, pr)
      title <- c(title, html_node(li, '.book_tit') %>% html_text())
      writer <- c(writer, html_node(li, '.book_writer') %>% html_text())
    }
    books <- data.frame(title=title, writer=writer, price=price)
    df_books <- rbind.data.frame(df_books, books)
  }
  # filename <- paste0("D:/Workspace/R_Project/01_Crawling/books/", cat_no, ".xlsx")
  # write.xlsx(df_books, file=filename, 
  #            sheetName=cat_names[cat_no],
  #            col.names=TRUE, row.names=FALSE, append=TRUE)
  
  addWorksheet(wb, cat_names[cat_no])
  writeDataTable(wb, cat_names[cat_no], df_books)
} 
saveWorkbook(wb, file="D:/dudwlsrla92/R-Project/books.xlsx")

