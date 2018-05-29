rm(list=ls())
install.packages('rJava')
dyn.load('/Library/Java/JavaVirtualMachines/jdk-10.0.1.jdk/Contents/Home/lib/server/libjvm.dylib')
library(rJava)

require("NLP")
require("openNLP")
library(NLP)
library(tm)
library(openNLP)
library(tm)
install.packages("RColorBrewer")
library(wordcloud)

# eyeball and extract some non-food words which appeared very frequently as "mystop", to make the code running more efficient.
mystop <- c("www","http","https","html","link","pin","click","com","post","page","christmas","dec","december","love","thanks","gift","gifts","holiday","holidays","recipe","recipes","food","foods","breakfast","lunch","dinner","time","year","years","season",
            "month","week","day","days","night","today","tonight","hours ","minutes","minute","tomorrow","morning","weekend","family","home","friends","kids","people","anyone","guys","everyone","everything","something",
            "anything","one","thing","things","share","help","life","store","market","kitchen","place","order","use","way","les","que","need","try","work","cut")

# define control for all the terms I will choose
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stopwords=c(stopwords("english"), mystop), stripWhitespace=T)

sent_token_annotator <- Maxent_Sent_Token_Annotator()
word_token_annotator <- Maxent_Word_Token_Annotator()
pos_tag_annotator <- Maxent_POS_Tag_Annotator()


#The code below shows 2011 are similar to the code in 2012/2013/2014/2015, I did this because using another loop to iterate each year requires a lot computing effort.
#=======================================================2011==============================================================
#read all Facebook tweets in different months in 2011.

setwd("~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2011")
filenames <- list.files(path = "~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2011",ignore.case = FALSE)

#use loop to read all 12 months' tweets in year of 2011 and store them in "alldocs".
#"alldocs.index" indicates each tweet belongs to which month.
alldocs <- c()
alldocs.index <- c()
j <- 1

for (i in filenames){
  post <- read.csv(i,header = FALSE)
  n <- nrow(post)
  alldocs[j] <- as.String(readLines(i, n, warn = FALSE))
  alldocs.index[j] <- i
  j <- j+1
}

#use Regular expression to change file names to year and month, and to indicate each tweet's time.
ptn <- "fpost-"
kk <- gsub(ptn, "", alldocs.index)
ptn <- ".csv"
alldocs.index <- gsub(ptn, "", kk)

#create Document Term Matrix, each document/row is a month, each term/column is the words which have been mentioned in the tweets
docs <- Corpus(VectorSource(alldocs))

dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm.full,0.999)
idx <- colSums(as.matrix(dtm))>0
dtm = dtm[,idx]
X <- as.matrix(dtm)

#only select terms which appeared more time 50 times of that year
freq = sort(colSums(X), decreasing=TRUE )
high_freq = freq[freq>50]

#only select terms which are nouns(food is noun)
str <- as.String(names(high_freq))
a3 <- annotate(str, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")

mypos <- a3w[tags=="NN"|tags=="NNS"|tags=="NNP"|tags=="NNPS"]
noun <- str[mypos]
popolar2011 <- noun # the nouns which appeared frequently over year of 2011
X.noun <- X[,noun]

# reorder columns to make date column at first
X.noun <- data.frame(X.noun)
columns <- c('year_month', colnames(X.noun))
X.noun$year_month <- alldocs.index
X.noun <- X.noun[, columns]
X.noun2011 <- X.noun#final table of year 2011

#for a particular month, what are the most frequent nouns
freq_month1 = sort(X.noun[1,2:ncol(freq_month1)], decreasing=TRUE )
wordcloud(names(freq_month1),freq_month1, max.words=30)
freq_month1[1:30]

#=======================================================2012==============================================================
#read all Facebook tweets in different months in 2012. The code IS similar to the code in 2011

setwd("~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2012")
filenames <- list.files(path = "~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2012",ignore.case = FALSE)

alldocs <- c()
alldocs.index <- c()
j <- 1

for (i in filenames){
  post <- read.csv(i,header = FALSE)
  n <- nrow(post)
  alldocs[j] <- as.String(readLines(i, n, warn = FALSE))
  alldocs.index[j] <- i
  j <- j+1
}

ptn <- "fpost-"
kk <- gsub(ptn, "", alldocs.index)
ptn <- ".csv"
alldocs.index <- gsub(ptn, "", kk)

docs <- Corpus(VectorSource(alldocs))

dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm.full,0.999)
idx <- colSums(as.matrix(dtm))>0
dtm = dtm[,idx]
X <- as.matrix(dtm)

freq = sort(colSums(X), decreasing=TRUE )
high_freq = freq[freq>50]

str <- as.String(names(high_freq))
a3 <- annotate(str, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")

#find all the nones which have appered in tweets, in order to find food's names
mypos <- a3w[tags=="NN"|tags=="NNS"|tags=="NNP"|tags=="NNPS"]
noun <- str[mypos]
popolar2012 <- noun # the nouns which appeared frequently over year of 2012
X.noun <- X[,noun]

X.noun <- data.frame(X.noun)
columns <- c('year_month', colnames(X.noun))
X.noun$year_month <- alldocs.index
X.noun <- X.noun[, columns]
X.noun2012 <- X.noun#final table of year 2012

#for a particular month, what are the most frequent nouns
freq_month2 = sort(X.noun[1,2:ncol(freq_month1)], decreasing=TRUE )
wordcloud(names(freq_month2),freq_month2, max.words=30)
freq_month2[1:30]


#=======================================================2013==============================================================
#read all Facebook tweets in different months in 2013.

setwd("~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2013")
filenames <- list.files(path = "~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2013",ignore.case = FALSE)

alldocs <- c()
alldocs.index <- c()
j <- 1

for (i in filenames){
  post <- read.csv(i,header = FALSE)
  n <- nrow(post)
  alldocs[j] <- as.String(readLines(i, n, warn = FALSE))
  alldocs.index[j] <- i
  j <- j+1
}

ptn <- "fpost-"
kk <- gsub(ptn, "", alldocs.index)
ptn <- ".csv"
alldocs.index <- gsub(ptn, "", kk)

docs <- Corpus(VectorSource(alldocs))

dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm.full,0.999)
idx <- colSums(as.matrix(dtm))>0
dtm = dtm[,idx]
X <- as.matrix(dtm)

freq = sort(colSums(X), decreasing=TRUE )
high_freq = freq[freq>50]

str <- as.String(names(high_freq))
a3 <- annotate(str, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")

#find all the nones which have appered in tweets, in order to find food's names
mypos <- a3w[tags=="NN"|tags=="NNS"|tags=="NNP"|tags=="NNPS"]
noun <- str[mypos]
popolar2013 <- noun # the nouns which appeared frequently over year of 2013
X.noun <- X[,noun]

X.noun <- data.frame(X.noun)
columns <- c('year_month', colnames(X.noun))
X.noun$year_month <- alldocs.index
X.noun <- X.noun[, columns]
X.noun2013 <- X.noun#final table of year 2013

#for a particular month, what are the most frequent nouns
freq_month3 = sort(X.noun[1,2:ncol(freq_month1)], decreasing=TRUE )
wordcloud(names(freq_month3),freq_month3, max.words=30)
freq_month3[1:30]


#=======================================================2014==============================================================
#read all Facebook tweets in different months in 2014.

setwd("~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2014")
filenames <- list.files(path = "~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2014",ignore.case = FALSE)

alldocs <- c()
alldocs.index <- c()
j <- 1

for (i in filenames){
  post <- read.csv(i,header = FALSE)
  n <- nrow(post)
  alldocs[j] <- as.String(readLines(i, n, warn = FALSE))
  alldocs.index[j] <- i
  j <- j+1
}

ptn <- "fpost-"
kk <- gsub(ptn, "", alldocs.index)
ptn <- ".csv"
alldocs.index <- gsub(ptn, "", kk)

docs <- Corpus(VectorSource(alldocs))

dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm.full,0.999)
idx <- colSums(as.matrix(dtm))>0
dtm = dtm[,idx]
X <- as.matrix(dtm)

freq = sort(colSums(X), decreasing=TRUE )
high_freq = freq[freq>50]

str <- as.String(names(high_freq))
a3 <- annotate(str, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")

#find all the nones which have appered in tweets, in order to find food's names
mypos <- a3w[tags=="NN"|tags=="NNS"|tags=="NNP"|tags=="NNPS"]
noun <- str[mypos]
popolar2014 <- noun # the nouns which appeared frequently over year of 2014
X.noun <- X[,noun]

X.noun <- data.frame(X.noun)
columns <- c('year_month', colnames(X.noun))
X.noun$year_month <- alldocs.index
X.noun <- X.noun[, columns]
X.noun2014 <- X.noun#final table of year 2014

#for a particular month, what are the most frequent nouns
freq_month4 = sort(X.noun[1,2:ncol(freq_month1)], decreasing=TRUE )
wordcloud(names(freq_month4),freq_month4, max.words=30)
freq_month4[1:30]


#=======================================================2015==============================================================
#read all Facebook tweets in different months in 2015.

setwd("~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2015")
filenames <- list.files(path = "~/Desktop/6.Social Media Analytics/Final/B/Facebook Posts 2015",ignore.case = FALSE)

alldocs <- c()
alldocs.index <- c()
j <- 1

for (i in filenames){
  post <- read.csv(i,header = FALSE)
  n <- nrow(post)
  alldocs[j] <- as.String(readLines(i, n, warn = FALSE))
  alldocs.index[j] <- i
  j <- j+1
}

ptn <- "fpost-"
kk <- gsub(ptn, "", alldocs.index)
ptn <- ".csv"
alldocs.index <- gsub(ptn, "", kk)

docs <- Corpus(VectorSource(alldocs))

dtm.full <- DocumentTermMatrix(docs, control=dtm.control)
dtm = removeSparseTerms(dtm.full,0.999)
idx <- colSums(as.matrix(dtm))>0
dtm = dtm[,idx]
X <- as.matrix(dtm)

freq = sort(colSums(X), decreasing=TRUE )
high_freq = freq[freq>50]

str <- as.String(names(high_freq))
a3 <- annotate(str, list(sent_token_annotator, word_token_annotator, pos_tag_annotator))
a3w <- subset(a3, type == "word")
tags <- sapply(a3w$features, `[[`, "POS")

#find all the nones which have appered in tweets, in order to find food's names
mypos <- a3w[tags=="NN"|tags=="NNS"|tags=="NNP"|tags=="NNPS"]
noun <- str[mypos]
popolar2015 <- noun # the nouns which appeared frequently over year of 2015
X.noun <- X[,noun]

X.noun <- data.frame(X.noun)
columns <- c('year_month', colnames(X.noun))
X.noun$year_month <- alldocs.index
X.noun <- X.noun[, columns]
X.noun2015 <- X.noun#final table of year 2015

#for a particular month, what are the most frequent nouns
freq_month5 = sort(X.noun[1,2:ncol(freq_month1)], decreasing=TRUE )
wordcloud(names(freq_month5),freq_month5, max.words=30)
freq_month5[1:30]

#=====================================================================================================================
#combine all years' Facebook noun occurrency tables together and analyze the trend over the years and months.

# As shown below, each year has diffrent number of tweets, so if we use frequence of times each food noun mentioned each year, 
# there will be bias. Thus we use the ratio of food noun mentioned that year.(percentage of occurrence)
length(popolar2011) #4904
length(popolar2012) #7977
length(popolar2013) #12939
length(popolar2014) #14678
length(popolar2015) #17036

#extract the nouns which occurred in all 5 years, and combine all years' Facebook noun occurrence tables according to the common nouns
common.noun <- Reduce(intersect, list(colnames(X.noun2011),colnames(X.noun2012),colnames(X.noun2013),colnames(X.noun2014),colnames(X.noun2015)))

X.noun2011_2 <- X.noun2011[,common.noun]
X.noun2011_2[,2:ncol(X.noun2011_2)] <- X.noun2011_2[,2:ncol(X.noun2011_2)]/length(popolar2011)#percentage of occurrence

X.noun2012_2 <- X.noun2012[,common.noun]
X.noun2012_2[,2:ncol(X.noun2012_2)] <- X.noun2012_2[,2:ncol(X.noun2012_2)]/length(popolar2012)#percentage of occurrence

X.noun2013_2 <- X.noun2013[,common.noun]
X.noun2013_2[,2:ncol(X.noun2013_2)] <- X.noun2013_2[,2:ncol(X.noun2013_2)]/length(popolar2013)#percentage of occurrence

X.noun2014_2 <- X.noun2014[,common.noun]
X.noun2014_2[,2:ncol(X.noun2014_2)] <- X.noun2014_2[,2:ncol(X.noun2014_2)]/length(popolar2014)#percentage of occurrence

X.noun2015_2 <- X.noun2015[,common.noun]
X.noun2015_2[,2:ncol(X.noun2015_2)] <- X.noun2015_2[,2:ncol(X.noun2015_2)]/length(popolar2015)

#overall 5 years' Facebook noun occurrence percentage tables with the common nouns
overal.table <- rbind(X.noun2011_2, X.noun2012_2, X.noun2013_2, X.noun2014_2, X.noun2015_2)

#change the first column to Date type
library(zoo)
overal.table$year_month <- as.Date(as.yearmon(overal.table$year_month))
#reorder rows according to Date, and reorder the indexes
overal.table <- overal.table[order(overal.table[,1]),]
rownames(overal.table) = 1:nrow(overal.table)

#Time Series
install.packages('forecast')
library('forecast')
install.packages('tseries')
library('tseries')

#Plot and analyze turkeys' trend: it matches with that gingerbread will be mentioned most frequently around Thanksgiving, which is around Nov. and Dec.
percentage_of_occurrence <- ts(overal.table$turkeys, start=c(2011, 1), frequency=12) 
plot(percentage_of_occurrence, main = "Time Series")


#Plot and analyze gingerbread's trend: it matches with that gingerbread will be mentioned most frequently around Christmas.
percentage_of_occurrence <- ts(overal.table$gingerbread, frequency=12) #12 months, time series
plot(percentage_of_occurrence, main = "Time Series")
# people tend to mention a lot more salad earlier in the year, around Feb.

#Interestingly, people tend to "gobble" at the end of each year
percentage_of_occurrence <- ts(overal.table$gobble, frequency=12) #12 months, time series
plot(percentage_of_occurrence, main = "Time Series")

#Plot and analyze apple's trend: it matches with that apple's harvest time is generally September through October, or from late summer through early spring
percentage_of_occurrence <- ts(overal.table$apple, frequency=12) #12 months, time series
plot(percentage_of_occurrence, main = "Time Series")

#Plot and analyze ice's trend: it matches with that ice will be mentioned most frequently around Sep., which is around summer
percentage_of_occurrence <- ts(overal.table$ice, frequency=12) #12 months, time series
plot(percentage_of_occurrence, main = "Time Series")

#Plot and analyze cookies' trend: it matches with that people will consume more cookies at the end of each year because of festivals such as Christmas.
percentage_of_occurrence <- ts(overal.table$cookies, frequency=12) #12 months, time series
plot(percentage_of_occurrence, main = "Time Series")

