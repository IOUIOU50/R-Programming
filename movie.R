install.packages('rvest')
library(rvest)

url_base <- 'https://movie.daum.net/moviedb/grade?movieId=68593&type=netizen&page='
url_base

all.reviews <- c()
for(page in 1:20){
  url <- paste(url_base, page, sep='')
  htxt <- read_html(url)
  table <- html_nodes(htxt, '.review_info')
  content <- html_nodes(table, '.desc_review')
  reviews <- html_text(content)
  if(length(reviews) == 0){break}
  all.reviews <- c(all.reviews, reviews)
  print(page)
}

write.table(all.reviews, 'daum1.txt')
