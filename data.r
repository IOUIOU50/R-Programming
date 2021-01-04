getwd()
setwd('c:/Rtest')

library(KoNLP)
library(RColorBrewer)
library(wordcloud)

useSejongDic()

park <- file('2017trump.txt', encoding = 'UTF-8') #park : 한글 연설문
speech <- readLines(park) #speech : park 한 줄의 집합
close(park)
head(speech, 5)
tail(speech, 5)

pword <- sapply(speech, extractNoun, USE.NAMES = F) #pword : 한 줄의 연설문 집합들에서 명사를 추출한 '리스트'
pword 

text <- unlist(pword) #text : 리스트 -> 벡터 변환
head(text, 20)
tail(text, 20)

text2 <- Filter(function(x){nchar(x) <= 2}, text)
head(text2, 20)
text3 <- Filter(function(x){nchar(x) <= 3}, text)
head(text3, 20)
text4 <- Filter(function(x){nchar(x) <= 4}, text)
head(text4, 20)

text2 <- gsub('것', '', text2)
text2 <- gsub('들', '', text2)
text2 <- gsub('수', '', text2)
text2 <- gsub('년', '', text2)
text2 <- gsub('이', '', text2)
text2 <- gsub('국', '', text2)
text2 <- gsub('하게', '', text2)
text2 <- gsub('선', '', text2)
text2 <- gsub('나', '', text2)
text2 <- gsub('한', '', text2)
text2 <- gsub('것', '', text2)
text2 <- gsub('저', '', text2)
text2 <- gsub('앞', '', text2)
text2 <- gsub('분', '', text2)
text2 <- gsub('\\n', '', text2)
text2 <- gsub('\\d', '', text2)
text2 <- gsub('\\.', '', text2)

write(unlist(text2), 'mytext.txt')
myword <- read.table('mytext.txt')
nrow(myword)


wordcount <- table(myword)
head(sort(wordcount, decreasing = T), 20)
palete <- brewer.pal(9, 'Set1')
x11()
wordcloud(
  names(wordcount),
  freq = wordcount,
  scale = c(5, 1),
  rot.per = 0.5,
  min.freq = 7,
  random.order = F,
  random.color = T,
  colors = palete
)

a <- head(sort(wordcount, decreasing = T), 20)
pie(a, col = rainbow(10), radius = 1)

pct <- round(a/sum(a)*100, 1)
names(a)
lab <- paste(names(a), '\n', pct, '%')
pie(a, main = '트럼프 대통령 연설문', col = rainbow(10), cex = 0.8, labels = lab)

par(new = T)
pie(a, radius = 0.6, col = 'white', lables = NA, border = NA)

install.packages(c('KONLP', 'arules', 'igraph', 'combinat'))
library(KoNLP)
library(arules)
library(igraph)
library(combinat)

rule <- file('2017trump.txt', encoding = 'UTF-8')
rules <- readLines(rule)
close(rule)
head(rules, 10)

tran <- Map(extractNoun, rules)
tran <- unique(tran)
tran <- sapply(tran, unique)
tran <- sapply(tran, function(x){Filter(function(y){nchar(y) <= 4 && nchar(y) > 1 && is.hangul(y)}, x)})
tran <- Filter(
  function(x){
    length(x) >= 2}, 
  tran)
tran

names(tran) <- paste('Tr', 1:length(tran), sep = '')
names(tran)
wordtran <- as(tran, 'transactions')
wordtran

wordtab <- crossTable(wordtran)
wordtab

ares <- apriori(wordtran, parameter = list(supp = 0.1, conf = 0.2))

inspect(ares)
rules <- labels(ares, ruleSep = ' ')
rules <- sapply(rules, strsplit, ' ', USE.NAMES = F)
rulemat = do.call('rbind', rules)

ruleg <- graph.edgelist(rulemat[-c(1:6),], directed = F)
plot.igraph(ruleg, vertex.label = V(ruleg)$name, vertex.label.cex = 1, vertex.size = 30, layout = layout.fruchterman.reingold.grid)

closen <- closeness(ruleg)
plot(closen, clo = 'red', xaxt = 'n', lty = 'solid', type = 'b', xlab = '단어', ylab = 'closeness')

points(closen, pch = 16, col = 'navy')

axis(1, seq(1, length(closen)), V(ruleg)$name, cex = 5)
