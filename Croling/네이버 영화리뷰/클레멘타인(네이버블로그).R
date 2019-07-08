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

library(lubridate)
library(rvest)

# 영화 클레멘타인 -이터널 : 총 5,929건

# 2004~2010 : 547건
base_url1 <- 'https://search.naver.com/search.naver?date_from=20040521&date_option=8&date_to=20101231&dup_remove=1&nso=p%3Afrom20040521to20101231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20%2B%EC%98%81%ED%99%94%20-%EC%9D%B4%ED%84%B0%EB%84%90%20-Eternal%20-%EC%A7%90%EC%BA%90%EB%A6%AC%20-%ED%99%A9%EC%95%BC%20-%ED%96%A5%EC%88%98&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2011~2013 : 762건
base_url2 <- 'https://search.naver.com/search.naver?date_from=20110101&date_option=8&date_to=20131231&dup_remove=1&nso=p%3Afrom20110101to20131231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20-%EC%9D%B4%ED%84%B0%EB%84%90&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2014~2015 : 999건
base_url3 <- 'https://search.naver.com/search.naver?date_from=20140101&date_option=8&date_to=20151231&dup_remove=1&nso=p%3Afrom20140101to20151231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20-%EC%9D%B4%ED%84%B0%EB%84%90&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2016~2017 : 1065건
base_url4 <- 'https://search.naver.com/search.naver?date_from=20160101&date_option=8&date_to=20171231&dup_remove=1&nso=p%3Afrom20160101to20171231&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20-%EC%9D%B4%ED%84%B0%EB%84%90&sm=tab_pge&srchby=all&st=sim&where=post&start='
# 2016~2017 : 933건
base_url5 <- 'https://search.naver.com/search.naver?date_from=20180101&date_option=8&date_to=20190708&dup_remove=1&nso=p%3Afrom20180101to20190708&post_blogurl=&post_blogurl_without=&query=%EC%98%81%ED%99%94%20%ED%81%B4%EB%A0%88%EB%A9%98%ED%83%80%EC%9D%B8%20-%EC%9D%B4%ED%84%B0%EB%84%90&sm=tab_pge&srchby=all&st=sim&where=post&start='

i <- 0
urls <- NULL
for(i in 0:54){
  urls[i+1] <- paste0(base_url1, i*10+1) 
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
View(Cl1)
Cl2 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
Cl3 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
Cl4 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)
Cl5 <- data.frame(제목 = title,
                  내용 = txt,
                  날짜 = date)

N_blog_Cl <- rbind(Cl1,Cl2,Cl3,Cl4,Cl5)

View(N_blog_Cl)


str_replace_all("\\.","-") %>% 
  str_sub(1,10) %>%
  as.Date())

