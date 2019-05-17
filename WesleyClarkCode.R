#This exercise requires the following packages to be installed
#1. Install them only once
install.packages("tm")
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("fpc")
install.packages("cluster")
install.packages("ggplot2")

#Remove numbers
#2. Set working directory and build the corpus
library("tm")      #Load the tm package in memory
setwd("~/Documents/650/Text")   #Set working directory
dir(".")           #List the files in working directory
docs <- Corpus(DirSource("."))  #Build the corpus
summary(docs)                   #List the files in the corpus

#3. Data preprocessing
#Remove URL
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
docs <- tm_map(docs, content_transformer(removeURL))

#Remove whitespace
docs <- tm_map(docs, stripWhitespace)
inspect(docs)

#Remove punctuation
docs <- tm_map(docs, removePunctuation)
inspect(docs)
docs <- tm_map(docs, removeNumbers)
inspect(docs[])

#Remove special characters
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "&")
#ocs <- tm_map(docs, toSpace, "@")
#docs <- tm_map(docs, toSpace, "*")

#Convert to lower case
docs <- tm_map(docs, tolower)
inspect(docs[1])

#Ensure that all the documents are Plain Text
#docs <- tm_map(docs, PlainTextDocument)

#Remove stop words
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("america", "american"))  
inspect(docs[])

docs <- tm_map(docs, removeWords, stopwords("SMART"))
inspect(docs[])

# Stemming
library(SnowballC)
docs <- tm_map(docs, stemDocument)
inspect(docs[])



dtm <- DocumentTermMatrix(docs)
dtm
freq <- colSums(as.matrix(dtm))   
freq
#Number of distinct terms in all documents
length(freq)   
m <- as.matrix(dtm)   
dim(m)   


#4. Term frequencies
#Build the Document term matrix
dtm <- DocumentTermMatrix(docs)
dtm
freq <- colSums(as.matrix(dtm))   
freq
#Number of distinct terms in all documents
length(freq)   
m <- as.matrix(dtm)   
dim(m)   

#List the terms that appear at least 10 times
findFreqTerms(dtm, lowfreq=40)

#Remove sparse terms
dtms <- removeSparseTerms(dtm, 0.7) 
dtms    # display matrix properties after the sparse terms have been removed

#Several ways to List the document names and the terms
dtms$dimnames
dimnames(dtms)

#Several ways to List only the terms
dtms$dimnames[2]
dtms$dimnames$Terms
dtms$dimnames['Terms']
dimnames(dtms)[2]
dimnames(dtms)$Terms
dimnames(dtms)['Terms']

#5. Words Associations
findAssocs(dtm, c("design", "tool", "success"), corlimit=0.75)

#7. Word frequencies plot
library("ggplot2")
wf <- data.frame(word=names(freq), freq=freq)
p <- ggplot(subset(wf, freq>5), aes(word, freq))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))
p

#8. Build a word cloud
library(wordcloud)

wordcloud(names(freq), freq, min.freq=5)
dtms <- removeSparseTerms(dtm, 0.7) # Prepare the data (max 70% empty space)   
freq <- colSums(as.matrix(dtms))    # Find word frequencies   
dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=200, rot.per=0.2, colors=dark2)    

#9. k-means clustering.  Use k=4
dtms <- removeSparseTerms(dtm, 0.75) 
d <- dist(t(dtms), method="euclidian")   
kfit <- kmeans(d, 2)   
kfit
library(cluster)   #Load the cluster package
clusplot(as.matrix(d), kfit$cluster, color=T, shade=T, labels=2, lines=0)  
kfit


#Elbow method
#Plot the total distance between clusters and within clusters by k value
bss<-integer(length(2:15))
for (i in 2:15) bss[i] <- kmeans(d,centers=i)$betweenss
plot(1:15, bss, type="b", xlab="Number of Clusters",
     ylab="Sum of squares", col="blue") 
wss<-integer(length(2:15))
for (i in 2:15) wss[i] <- kmeans(d,centers=i)$tot.withinss
lines(1:15, wss, type="b" ) 

#10. Cluster dendogram
library("cluster")
library ("fpc")

dtms <- removeSparseTerms(dtm, 0.01) # Many terms were removed to make the dendogram legible.
d <- dist(t(dtms), method="euclidian")
fit <- hclust(d=d, method="ward.D2")
plot(fit, hang=-1)

#Add red boxes
plot.new()
plot(fit, hang=-1)
groups <- cutree(fit, k=4)
rect.hclust(fit, k=2, border="red")
