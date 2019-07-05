library(rvest)

https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EA%B7%80%EB%86%8D&sm=tab_pge&srchby=all&st=sim&where=post&start=1
https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EA%B7%80%EB%86%8D&sm=tab_pge&srchby=all&st=sim&where=post&start=11
https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EA%B7%80%EB%86%8D&sm=tab_pge&srchby=all&st=sim&where=post&start=21

base_url <- 'https://search.naver.com/search.naver?date_from=&date_option=0&date_to=&dup_remove=1&nso=&post_blogurl=&post_blogurl_without=&query=%EA%B7%80%EB%86%8D&sm=tab_pge&srchby=all&st=sim&where=post&start='

urls <- NULL
for(i in 1:53520){
  urls[i] <- paste0(base_url, i)
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
  if(i %% 10 == 0)
  print(i)

  html  <- read_html(url)
  title <- c(title, html %>%
               html_nodes('#main_pack') %>%
               html_nodes('#elThumbnailResultArea') %>%
               html_nodes('dt') %>%
               html_nodes('a') %>%
               html_attr('title')) 
  date   <- c(date, html %>%
               html_nodes('#main_pack') %>%
               html_nodes('#elThumbnailResultArea') %>%
               html_nodes('.txt_inline') %>%
               html_text())
  txt    <- c(txt, html %>%
                html_nodes('#main_pack') %>%
                html_nodes('#elThumbnailResultArea') %>%
                html_nodes('.sh_blog_passage') %>%
               html_text()) 
}
N_blog_farm <- data.frame(제목 = title,
                          내용 = txt,
                          날짜 = date)

                         년도 = 
                           월 =
                           일 = 
                         요일 = )


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