library(rvest)

# read_html()
# read_table()

# html_node()  : 매칭되는 한 요소만 반환
# html_nodes() : 모든 요소를 반환한다.
# 즉, id를 찾을 경우에는 html_node()를 사용하면 되고, 
# tag,class로 같은 요소를 모두 추출하고자 할 경우에는 html_nodes()를 사용한다.
# ex) class가 title인 요소들 전부를 추출할 경우 html_nodes()를 사용.

## 1. 기본 추출 (tvcast 제목)
html_tvcast <- read_html("http://tvcast.naver.com/jtbc.youth",encoding = "UTF-8")
# <dt class = "title">이란 태그안에 제목이 들어가 있다.
# class가 title인 부분에서 안에 있는 태그('a')에 해당하는 내용을 추출한다.
html_tvcast %>% html_nodes(".title a")
# 그 안에 있는 text만 추출한다. 
html_tvcast %>% html_nodes(".title a") %>% html_text()
# 데이터프레임으로 바꿔주고 저장한다.
html_tvcast %>% html_nodes(".title a") %>% html_text() %>% data.frame()


## 2(1) table 추출 
html_wiki <- read_html('http://en.wikipedia.org/wiki/Student%27s_t-distribution')
# <table class="wikitable">
html_wiki %>% html_nodes(".wikitable") %>% html_table() %>% data.frame()


## 2(2) table 추출 (승객이 가장 많았던 공항 50군데)
html.airports <- read_html("https://en.wikipedia.org/wiki/List_of_busiest_airports_by_passenger_traffic")
df <- html_table(html_nodes(html.airports, "table")[[1]], fill = F)

