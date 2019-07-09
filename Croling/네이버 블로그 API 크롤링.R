## https://developers.naver.com/main/ # 네이버 개발자 계정 홈페이지 

urlStr <- "https://openapi.naver.com/v1/search/blog.xml?" # 기본 url 생성 
searchString <- "query=육아" # 쿼리생성 
searchString <- iconv(searchString, to="UTF-8") # 인코딩 
searchString <- URLencode(searchString)
searchString

etcString <- "&display=100&start=1&sort=sim"

reqUrl <- paste(urlStr, searchString, etcString, sep="")
reqUrl # 요청할 url 생성 

#install.packages("httr")
library(httr)

clientid <- "054n6IEfBZCo9_YTAgR7" # 개인 api id 값
clientSecret <- "BejMyaYzl0" # 개인 apu secret 값

apiResult <- GET(reqUrl, add_headers("X-Naver-Client-Id"=clientid, 
                                     "X-Naver-Client-Secret"=clientSecret))

apiResult # Status 값이 200이어야 정상. 500 이면 시스템 에러 
str(apiResult)

apiResult$content

result <- rawToChar(apiResult$content)
result
Encoding(result) <- "UTF-8"
result

refineStr <- result

refineStr <- gsub("<(\\/?)(\\w+)*([^<>]*)>", " ", refineStr)
refineStr <- gsub("[[:punct:]]", " ", refineStr)
refineStr <- gsub("[a-z]", " ", refineStr)
refineStr <- gsub("[0-9]", " ", refineStr)
refineStr <- gsub(" +", " ", refineStr)

typeof(refineStr)

library(rJava)
library(KoNLP)
library(dplyr)
library(ggplot2)
useSejongDic()

nouns <- extractNoun(refineStr)
str(nouns)

nouns2 <- nouns
nouns2 <- gsub("육아", " ", nouns2)
nouns2 <- gsub("보육", " ", nouns2)
nouns2 <- gsub("부모", " ", nouns2)
nouns2 <- gsub("엄마", " ", nouns2)
nouns2 <- gsub("아빠", " ", nouns2)
nouns2 <- gsub("오늘", " ", nouns2)
nouns2 <- gsub("^ㅎ", " ", nouns2)
nouns2 <- gsub("^ㅋ^ㅋ^ㅋ^ㅋ^ㅋ", " ", nouns2)
nouns2 <- gsub("아기", " ", nouns2)
nouns2 <- gsub("아이", " ", nouns2)



nouns2 <- nouns2[nchar(nouns2) > 1]
wordT <- sort(table(nouns2), decreasing = T)[1:50]

wordT

library(wordcloud2)

wordcloud2(wordT, size=0.5, shape='diamond')


Termdocume

install.packages('tm')
library(tm)

cps <- Corpus(VectorSource(refineStr))

tdm <- TermDocumentMatrix(cps)
as.matrix(tdm)

findAssocs(tdm, "휴직", 0.3)
