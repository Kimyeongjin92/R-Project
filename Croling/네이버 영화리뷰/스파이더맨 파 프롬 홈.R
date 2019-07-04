library(rvest)
library(dplyr)
library(stringr)
library(lubridate)

cine_url <- 'https://movie.naver.com/movie/bi/mi/pointWriteFormList.nhn?code=173123&type=after&isActualPointWriteExecute=false&isMileageSubscriptionAlready=false&isMileageSubscriptionReject=false&page='

urls <- NULL
for(x in 1:360){
  urls[x] <- paste0(cine_url, x)
}
length(urls)

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
  i <- i+length(url)
    if(i %% 10 == 0)
        print(i)
    html  <- read_html(url)
    total <- c(total, html %>% 
                 html_nodes('.total') %>% 
                 html_nodes('em') %>% 
                 .[2] %>% 
                 html_text() %>%
                 gsub(",","",.) %>%
                 as.numeric())
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
                                시간   = date %>% str_sub(-5),
                                요일   = paste0(wday(날짜,T),'요일'),
                                점수   = score,
                                좋아요 = like,
                                싫어요 = hate,
                                리플   = reple,
                                이름   = name)

head(View(Naver_CINE_Review),100)

write.xlsx(Naver_CINE_Review, file = "네이버 시네 리뷰.xlsx", row.names=T)

