##                                                 
## ==============================================================================================
##                               클레멘타인 네이버 영화 리뷰. 
## ==============================================================================================

install.packages('rvest')
install.packages('dplyr')
install.packages('stringr')
install.packages('lubridate')
install.packages('xlsx')
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
##                               클레멘타인(평점) 워드 클라우드. 
## ==============================================================================================
## https://kutar37.tistory.com/entry/R%EC%9D%84-%EC%9D%B4%EC%9A%A9%ED%95%9C-%ED%85%8D%EC%8A%A4%ED%8A%B8%EB%A7%88%EC%9D%B4%EB%8B%9D-%EC%9B%8C%EB%93%9C%ED%81%B4%EB%9D%BC%EC%9A%B0%EB%93%9C
## https://cran.r-project.org/web/packages/wordcloud2/wordcloud2.pdf  

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
setwd('D:/dudwlsrla92/R-Project/Croling/네이버 영화리뷰')
getwd()
Clementine<-read.csv('클레멘타인_네이버 리뷰(2).csv')
Clementine_reple<-as.character(Clementine$리플)
length(Clementine_reple)
str(Clementine_reple)

# 2. 사전에 단어 추가하기. ---------------------------------------
mergeUserDic(data.frame(c("노잼"), "ncn"))

add_dic <- readLines("클레멘타인_추가단어.txt")
for(i in 1:length(add_dic)){
  mergeUserDic(data.frame(add_dic[i],"ncn"))
}

# 2. 명사 추출 (extractNoun) -------------------------------------
Clementine_Noun   <- sapply(Clementine_reple, extractNoun, USE.NAMES = F)
head(Clementine_Noun)

# 3. 중간저장 (unlist) --------------------------------------------
write(unlist(Clementine_Noun),"클레멘타인_전처리1.txt")
Cl_unlist <- readLines("클레멘타인_전처리1.txt")
head(Cl_unlist)

# 4. 전처리 (1)_정규식 --------------------------------------------
Cl_filter <- gsub("\\d+","",Cl_unlist)
Cl_filter <- gsub("ㄱ-ㅎ","",Cl_filter)
Cl_filter <- gsub("ㅜ|ㅠ","",Cl_filter)
Cl_filter <- gsub('[~^!@#$%&*()_+=?<>]','',Cl_filter)
Cl_filter <- gsub("클레멘타인\\S*","클레멘타인",Cl_filter)
Cl_filter <- gsub("나았습니\\S*","병이나았어",Cl_filter)
Cl_filter <- gsub("감사합니\\S*","감사해",Cl_filter)
Cl_filter <- gsub('[~^!@#$%&*()_+=?<>]','',Cl_filter)
Cl_filter <- gsub(" ","",Cl_filter)

Cl_filter  <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 6},Cl_filter)

# 4. 전처리 (2)_gsub --------------------------------------------
txt <- readLines("클레멘타인gsub.txt")

i <- 0
for(i in 1:length(txt)){
  Cl_filter <- gsub((txt[i]),"",Cl_filter)
}

Cl_filter2  <- Filter(function(x){nchar(x) >= 2 & nchar(x) <= 6},Cl_filter)
head(Cl_filter2)

# 5. 중간저장 write ---------------------------------------------
write(Cl_filter2,"클레멘타인_전처리2.txt")
Cl <- readLines("클레멘타인_전처리2.txt")
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
a <- "white"
a <- c(a, rep("gold", length.out=Cl_top))
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
##                                                 
## ==============================================================================================
##                               클레멘타인 그래프 시각화. 
## ==============================================================================================

# 1. 


