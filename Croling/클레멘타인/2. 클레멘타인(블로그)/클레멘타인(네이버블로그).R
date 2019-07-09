##                                                 
## ==============================================================================================
##                               클레멘타인 네이버 블로그 리뷰. 
## ==============================================================================================

# 뻘짓 ==========================================================================================

#a <- 'https://search.naver.com/search.naver?date_from=201'
#b <- '0101&date_option=8&date_to=201'
#c <- '1231&dup_remove=0&nso=p%3Afrom201'
##d <- '0101to201'
#e <- '1231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20-%EC%9D%B4%ED%84%B0%EB%84%90&sm=tab_pge&srchby=all&st=sim&where=post&start='

#urls  <- NULL
#urlss <- NULL
#i <- 0
#k <- 0

#for(i in 0:2){
#  urls[i+1] <- paste0(a,i,b,i,c,i,d,i,e)
#  print(urls)
#  for(k in 0:3){
#    urlss[k+1] <- paste0(urls, k*10+1)}}
#    print(urlss)
# }
#}

#for(i in 0:2){
#  urls[i+1] <- paste0(a,i,b,i,c,i,d,i,e)}
#urlss <- paste0(rep(urls,each=100),rep(seq(1,1000,10),3))
#urlss

#urls<-NULL
#for(i in 0:3){
#    urls[i+1] <- paste0(a,i+4,b,i+4,c,i+4,d,i+4,e)
#}

# 뻘짓 =========================================================================================

library(stringr)
library(lubridate)
library(rvest)

# 영화 클레멘타인 +영화 -이터널 -Eternal -짐캐리 -황야 -향수 : 총 4,107건

# 2004~2010 : 550건  54
base_url1 <- 'https://search.naver.com/search.naver?date_from=20040521&date_option=8&date_to=20101231&dup_remove=1&nso=p%3Afrom20040521to20101231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20%2B%EC%98%81%ED%99%94%20-%EC%9D%B4%ED%84%B0%EB%84%90%20-Eternal%20-%EC%A7%90%EC%BA%90%EB%A6%AC%20-%ED%99%A9%EC%95%BC%20-%ED%96%A5%EC%88%98&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2011~2014 : 1088건 99
base_url2 <- 'https://search.naver.com/search.naver?date_from=20110101&date_option=8&date_to=20141231&dup_remove=1&nso=p%3Afrom20110101to20141231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20%2B%EC%98%81%ED%99%94%20-%EC%9D%B4%ED%84%B0%EB%84%90%20-Eternal%20-%EC%A7%90%EC%BA%90%EB%A6%AC%20-%ED%99%A9%EC%95%BC%20-%ED%96%A5%EC%88%98&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2015~2016 : 850건 84
base_url3 <- 'https://search.naver.com/search.naver?date_from=20150101&date_option=8&date_to=20161231&dup_remove=1&nso=p%3Afrom20150101to20161231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20%2B%EC%98%81%ED%99%94%20-%EC%9D%B4%ED%84%B0%EB%84%90%20-Eternal%20-%EC%A7%90%EC%BA%90%EB%A6%AC%20-%ED%99%A9%EC%95%BC%20-%ED%96%A5%EC%88%98&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2017~2018(9월) : 1013건 97
base_url4 <- 'https://search.naver.com/search.naver?date_from=20170101&date_option=8&date_to=20180930&dup_remove=1&nso=p%3Afrom20170101to20180930&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20%2B%EC%98%81%ED%99%94%20-%EC%9D%B4%ED%84%B0%EB%84%90%20-Eternal%20-%EC%A7%90%EC%BA%90%EB%A6%AC%20-%ED%99%A9%EC%95%BC%20-%ED%96%A5%EC%88%98&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2018(10월)~2019(7월) : 350건 34
base_url5 <- 'https://search.naver.com/search.naver?date_from=20181030&date_option=8&date_to=20190709&dup_remove=1&nso=p%3Afrom20181030to20190709&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20%2B%EC%98%81%ED%99%94%20-%EC%9D%B4%ED%84%B0%EB%84%90%20-Eternal%20-%EC%A7%90%EC%BA%90%EB%A6%AC%20-%ED%99%A9%EC%95%BC%20-%ED%96%A5%EC%88%98&sm=tab_pge&srchby=all&st=sim&where=post&start='

i <- 0
urls <- NULL
for(i in 0:34){
  urls[i+1] <- paste0(base_url5, i*10+1) 
}

title <- NULL
txt   <- NULL
date  <- NULL

i <- 0
for(url in urls){
  i <- i + length(url)
  print(i)
  
  html <- read_html(url)
  title <- c(title, html %>%
               html_nodes('#main_pack') %>%
               html_nodes('#elThumbnailResultArea') %>%
               html_nodes('dt') %>%
               html_node('a') %>%
               html_text())
  date   <- c(date, html %>%
                html_nodes('#main_pack') %>%
                html_nodes('#elThumbnailResultArea') %>%
                html_nodes('.txt_inline') %>%
                html_text())
  txt    <- c(txt, html %>%
                html_nodes('#main_pack') %>%
                html_nodes('#elThumbnailResultArea') %>%
                html_nodes('.sh_blog_top') %>%
                html_nodes('dl') %>%
                html_node('.sh_blog_passage') %>%
                html_text()) 
}



Cl1 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
View(Cl2)
Cl2 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
View(Cl2)
Cl3 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
View(Cl3)
Cl4 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
View(Cl4)
Cl5 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
View(Cl5)

N_blog_Cl <- rbind(Cl1,Cl2,Cl3,Cl4,Cl5)
head(N_blog_Cl)
str(N_blog_Cl)

N_blog_Cl$날짜 <- N_blog_Cl$날짜 %>% 
  str_replace_all("\\.","-") %>% 
  str_sub(1,10) %>%
  as.Date() 

N_blog_Cl$년   <- N_blog_Cl$날짜 %>% year()
N_blog_Cl$월   <- N_blog_Cl$날짜 %>% month()
N_blog_Cl$일   <- N_blog_Cl$날짜 %>% day()
N_blog_Cl$요일 <- N_blog_Cl$날짜 %>% wday(T)
View(N_blog_Cl)  
N_blog_Cl<-N_blog_Cl[!is.na(N_blog_Cl$날짜),]

write.table(N_blog_Cl,"클레멘타인(네이버블로그).txt")
write.csv(N_blog_Cl,"클레멘타인(네이버블로그).csv")

cl_txt <- read.table("클레멘타인(네이버블로그).txt")
cl_csv <- read.csv("클레멘타인(네이버블로그).csv")

##                                                 
## ==============================================================================================
##                               클레멘타인(블로그) 워드 클라우드. 
## ==============================================================================================

install.packages('KoNLP')        # 한글 형태소 분석
install.packages('wordcloud')
install.packages('wordcloud2')
install.packages('RColorBrewer') # 색깔 팔레트
install.packages('rJava')
install.packages("extrafont")
library(extrafont)
library(KoNLP)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(rJava)

# 0. 사전 추가
useSejongDic()

# 1. 데이터 불러오기 ---------------------------------------------
setwd('D:/dudwlsrla92/R-Project/Croling/클레멘타인/2. 클레멘타인(블로그)')
getwd()
Clementine<-read.csv('클레멘타인(네이버블로그).csv')
Clementine_title<-as.character(Clementine$제목)
Clementine_txt<-as.character(Clementine$내용)

# 2. 사전에 단어 추가하기. ---------------------------------------
mergeUserDic(data.frame(c("노잼"), "ncn"))

add_dic <- readLines("클레멘타인_추가단어.txt")
for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i],"ncn"))
}

# 2. 명사 추출 (extractNoun) -------------------------------------
Clementine_title_Noun   <- sapply(Clementine_title, extractNoun, USE.NAMES = F)
Clementine_txt_Noun   <- sapply(Clementine_txt, extractNoun, USE.NAMES = F)
head(Clementine_title_Noun)
head(Clementine_txt_Noun)

# 3. 중간저장 (unlist) --------------------------------------------
write(unlist(Clementine_title_Noun),"클레멘타인_전처리(title).txt")
write(unlist(Clementine_txt_Noun),"클레멘타인_전처리(txt).txt")

Cl_title_unlist <- readLines("클레멘타인_전처리(title).txt")
Cl_txt_unlist <- readLines("클레멘타인_전처리(txt).txt")
head(Cl_title_unlist)
head(Cl_txt_unlist)

# 4. 전처리 (1)_정규식 --------------------------------------------
Cl_filter <- gsub("\\d+","",Cl_txt_unlist)
Cl_filter <- gsub("ㄱ-ㅎ","",Cl_filter)
Cl_filter <- gsub("ㅜ|ㅠ","",Cl_filter)
Cl_filter <- gsub('[~^!"@#$%&*()_+=?<>]','',Cl_filter)
Cl_filter <- gsub(" ","",Cl_filter)

Cl_filter  <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 6},Cl_filter)

# 4. 전처리 (2)_gsub --------------------------------------------
txt <- readLines("클레멘타인gsub.txt")

i <- 0
for(i in 1:length(txt)){
  Cl_filter <- gsub((txt[i]),"",Cl_filter)
}

Cl_filter2  <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 5},Cl_filter)
head(Cl_filter2)

# 5. 중간저장 write ---------------------------------------------
write(Cl_filter2,"클레멘타인_전처리2(txt).txt")
Cl <- readLines("클레멘타인_전처리2(txt).txt")
head(Cl)

# 4. table ------------------------------------------------------
Cl_table <- table(Cl)
head(Cl_table)

# 5. sort -------------------------------------------------------
Cl_top    <- sort(Cl_table, decreasing = T)
head(Cl_top,100)

# 6. wordcloud ---------------------------------------------------

windowsFonts(baedal=windowsFont("배달의민족 도현"))

palete <- brewer.pal(9,"Set3") 
pal <- brewer.pal(5,"YlGn")
pal1<- brewer.pal(7,"YlOrRd")
green <- brewer.pal(6,"RdYlGn")
red <- brewer.pal(7,"YlOrRd")
blue <- brewer.pal(6,"Blues")
set <- brewer.pal(7, "Set3")
pp <- brewer.pal(7,"RdYlGn")

par(bg="black")
wordcloud(head(names(Cl_top),1000),
          freq=Cl_top,   #
          scale=c(3,0.1),   # 빈도가 가장 큰 단어와 가장 빈도가 작은 단어 폰트 사이 크기
          rot.per=0.25,     # 90도 회정해서 보여줄 단어 비율.
          min.freq=1,       # 이 값 이상 언급된 단어만 출력.
          max.words=1000,   # 빈도 3이상 100미만 단어 표현.
          random.order=F,   # (F)빈도가 큰 단어를 중앙에 두도록 함.
          random.color=F,   # (T)색상랜덤/(F)빈도수순으로 색상표현.
          colors=palete,
          family="baedal")
legend('top',1,"클레멘타인_WordCloud",
       cex=0.8,
       fill=NA,
       border=NA,
       bg="white",
       text.col="red",
       text.font=2,
       box.col="red")


# 6. wordcloud2 --------------------------------------------------

# 전처리와 sort가 완료된 table 파일로 만든다.
a <- c("white", rep("gold", length(Cl_top)))
wordcloud2(Cl_top,
           size=0.7,
           minSize=0,
           gridSize=8,                # cloud 크기(2)
           col=a,           # 색 변경 random-dark, random-light, rep(brewer.pal(8, "Dark2"), length.out=100)
           rotateRatio=0,             # 회전 정도 조절
           backgroundColor = "black", # 배경 색 
           shape = 'star',            # circle
           shuffle = T,
           fontFamily = 'baedal',
           fontWeight = 'bold',
           ellipticity = 0.65)         # 그림의 평형정도
#           figPath="movie2.png",

install.packages('devtools')
library(devtools)
install.packages('wordcloud2')
library(wordcloud2)