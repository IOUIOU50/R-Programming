getwd()
dir_data <- 'c:/Rtest'
setwd(dir_data)
getwd()

install.packages('wordcloud')

library(KoNLP)
library(wordcloud)

mergeUserDic(data.frame('빅데이터', 'ncn'))

txt <- file('bigdata.txt', encoding = 'UTF-8')
text <- readLines(txt)
close(txt)
text
big <- sapply(text, extractNoun, USE.NAMES = F)
big

head(unlist(big), 30)
f<- unlist(big)
f

big <- Filter(function(x){nchar(x) >= 3}, f)
big

big <- gsub('같은경우는', '', big)
big <- gsub('어느정도', '', big)
big <- gsub('우리나라', '', big)
big <- gsub('찌르는거', '', big)
big <- gsub('생각들어보고싶습니다', '', big)
big <- gsub('그자체로', '', big)
big <- gsub('어마어마', '', big)
big <- gsub('하여', '', big)
big <- gsub('것들이', '', big)
big <- gsub('관심을받', '', big)
big <- gsub('다양한분석기술들도', '', big)
big <- gsub('을', '', big)
big <- gsub('에서', '', big)
big <- gsub('28기가바이트밖에', '', big)
big <- gsub('난감한', '', big)
big <- gsub('같은경우는', '', big)
big <- gsub('어려워질경우', '', big)
big <- gsub('그러한정보를', '', big)
big <- gsub('어느곳보다', '', big)
big

write(unlist(big), 'bigdata_2.txt')

re <- read.table('bigdata_2.txt')
nrow(re)

textcount <- table(re)
head(sort(textcount, decreasing = T), 30)

palete <- brewer.pal(9, 'Set1')
wordcloud(names(textcount), freq=textcount, scale = c(5, 1), rot.per=0.25, min.freq = 1, random.order = F, random.color = T, colors = palete)
