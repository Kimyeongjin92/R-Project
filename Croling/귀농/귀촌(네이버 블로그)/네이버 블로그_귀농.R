library(rvest)
# 귀촌 -매매
https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EA%B7%80%EC%B4%8C%20-%EB%A7%A4%EB%A7%A4&sm=tab_pge&srchby=all&st=sim&where=post&start=1
https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EA%B7%80%EC%B4%8C&sm=tab_pge&srchby=all&st=sim&where=post&start=1
base_url <- 'https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EA%B7%80%EC%B4%8C%20-%EB%A7%A4%EB%A7%A4&sm=tab_pge&srchby=all&st=sim&where=post&start='
20672
urls <- NULL
for(i in 0:1000){
  urls[i+1] <- paste0(base_url, i*10+1)
  print(i)
}
length(urls)

# links <- NULL
# for(url in urls){
#  html  <- read_html(url)
#  links <- c(links, html %>%
#               html_nodes('#main_pack') %>%
#               html_nodes('#elThumbnailResultArea') %>%
#               html_node('a') %>%
#               html_attr('href') %>%
#               unique()) 
#}

#length(links)

  
title <- NULL 
txt   <- NULL
date  <- NULL

i <- 0
for(url in urls){
  i <- i + length(url)
  # if(i %% 10 == 0)
  print(i)

  html  <- read_html(url)
  title <- c(title, html %>%
               html_nodes('#main_pack') %>%
               html_nodes('#elThumbnailResultArea') %>%
               html_nodes('dt') %>%
               html_nodes('a') %>%
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
                html_nodes('.sh_blog_passage') %>%
               html_text()) 
}

N_blog_farm <- data.frame(제목 = title,
                          내용 = txt,
                          날짜 = date)

View(N_blog_farm)
N_blog_farm$날짜[-grep("전 | 어제",N_blog_farm$날짜)]
N_blog_farm$날짜[-grep("어제",N_blog_farm$날짜)]
                              
