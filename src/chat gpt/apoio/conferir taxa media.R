Sys.setlocale("LC_CTYPE", "English_United States.utf8")
library(jsonlite)
url <- "https://www.stat-search.boj.or.jp/api/v1/getDataCode?format=json&lang=en&db=FM02&code=STRACLUCON&startDate=202608"
x <- fromJSON(url)
print(x$RESULTSET$VALUES)
