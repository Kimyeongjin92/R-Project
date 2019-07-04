library(rvest)
library(dplyr)
library(stringr)
library(lubridate)
library(xlsx)

Clementine_url <- 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=37886&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='

urls <- NULL
for(i in 1:10){
  urls[i] <- paste0(Clementine_url,i)
}

total <- NULL
page  <- NULL
score <- NULL 
reple <- NULL
name  <- NULL
date  <- NULL
like  <- NULL
hate  <- NULL
date  <- NULL

i <- 0
for(url in urls){
  i <- i + length(url)
  if(i %% 10 == 0)
  print(i)
  
  html  <- read_html(url)
  total <- html %>% 
             html_nodes('.total') %>% 
             html_nodes('em') %>% 
           .[2] %>% 
             html_text() %>%
             gsub(",","",.) %>%
             as.numeric()
  page  <- ceiling(total/10)
  score <- c(score, html %>%
               html_nodes('.score_result') %>%
               html_nodes('.star_score') %>%
               html_node('em') %>%             # .이 있으면 class 없으면 tag
               html_text())
  reple <- c(reple, html %>%
               html_nodes('.score_result') %>%
               html_nodes('.score_reple') %>%
               html_node('p') %>%
               html_text())
  name  <- c(name, html %>%
               html_nodes('.score_result') %>%
               html_nodes('.score_reple') %>%
               html_node('dt') %>%
               html_node('a') %>%
               html_node('span') %>%
               html_text())
  like  <- c(like, html %>% 
               html_nodes('.btn_area') %>%       
               html_node('strong') %>%
               html_node('span') %>%
               html_text())
  hate  <- c(hate ,html %>% 
               html_nodes('.btn_area') %>% 
               html_nodes('strong') %>% 
               .[grep('notSympathy',.)] %>%
               html_text())
  date  <- c(date, html %>% 
               html_nodes('.score_result') %>% 
               html_nodes('.score_reple') %>% 
               html_node('dt') %>% 
               html_nodes('em') %>% 
               .[-grep('onclick',.)] %>% 
               html_text())
}



Naver_CINE_Review <- data.frame(날짜   = 날짜 <- date %>% str_replace_all("\\.","-") %>% as.Date(),
                                년도   = year(날짜),
                                월     = month(날짜),
                                일     = day(날짜),
                                시간   = date %>% str_sub(-5),
                                요일   = paste0(wday(날짜,T),'요일'),
                                점수   = score,
                                좋아요 = like,
                                싫어요 = hate,
                                이름   = name,
                                리플   = reple)

head(View(Naver_CINE_Review),100)

write.xlsx(Naver_CINE_Review, file = "클레멘타인_네이버 리뷰.xlsx",
           sheetName="클레멘타인",
           col.names=T,row.names=F,append=F)


##                                                 
## ==============================================================================================
##                               클레멘타인 워드 클라우드. 
## ==============================================================================================
## https://kutar37.tistory.com/entry/R%EC%9D%84-%EC%9D%B4%EC%9A%A9%ED%95%9C-%ED%85%8D%EC%8A%A4%ED%8A%B8%EB%A7%88%EC%9D%B4%EB%8B%9D-%EC%9B%8C%EB%93%9C%ED%81%B4%EB%9D%BC%EC%9A%B0%EB%93%9C

# scale 가장 빈도가 큰 단어와 가장 빈도가 작은단어 폰트사이의 크기차이 scale=c(10,1)
# minfreq 출력될 단어들의 최소빈도
# maxwords 출력될 단어들의 최대빈도(inf:제한없음)
# random.order(true면 랜덤, false면 빈도수가 큰 단어를 중앙에 배치하고 작은순)
# random.color(true면 색 랜덤, false면 빈도순)
# rot.per(90도 회전된 각도로 출력되는 단어비율)
# colors 가장 작은빈도부터 큰 빈도까지의 단어색
# family 글씨체



install.packages('KoNLP')        # 한글 형태소 분석
install.packages('wordcloud')
install.packages('wordcloud2')
install.packages('RColorBrewer') # 색깔 팔레트
install.packages('rJava')
library(KoNLP)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(rJava)

# 0. 사전 추가
useSejongDic()

# 1. 데이터 불러오기
setwd('D:/dudwlsrla92/R-Project/Croling/네이버 영화리뷰')
getwd()
Clementine<-read.csv('클레멘타인_네이버 리뷰(2).csv')
Clementine_reple<-as.character(Clementine$리플)
length(Clementine_reple)
str(Clementine_reple)

# 2. extractNoun으로 명사 추출
Clementine_Noun   <- sapply(Clementine_reple, extractNoun, USE.NAMES = F)
head(Clementine_Noun)

# 3. list -> unlist
Clementine_unlist <- unlist(Clementine_Noun)

# 4. table 
Clementine_table  <- table(Clementine_unlist)

# 5. sort
Clementine_top    <- head(sort(Clementine_table, decreasing = T),100)

# 6. wordcloud
wordcloud(names(Clementine_top),Clementine_top,family="baedal")

# 7. 단어 등록(1) : 단일로 등록할 때
mergeUserDic(data.frame(c("노잼"), "ncn"))

# 8. 단어 등록(2) : 파일로 등록할 때
add_dic <- readLines("클레멘타인_추가단어.txt")
for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i],"ncn"))
}
