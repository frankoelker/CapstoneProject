## Initial Settings and Librarys
Sys.setlocale("LC_ALL",locale = "English")
library(stringi)
library(stringr)
library(tm)
library(RWeka)
library(ggplot2)
library(dplyr)
library(R.utils)

## Load the Data and print some informations
file <- "Coursera-SwiftKey.zip"
if(!file.exists(file)) {
  url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
  download.file(url = url, destfile = file)
  unzip(file)
}
blogsfile <- "./final/en_US/en_US.blogs.txt"
blogs <- file(blogsfile, "r")
blogslines <- readLines(blogs, encoding = "UTF-8", skipNul = TRUE)
close(blogs)
newsfile <- "./final/en_US/en_US.news.txt"
news <- file(newsfile, "r")
newslines <- readLines(news, encoding = "UTF-8", skipNul = TRUE)
close(news)
twitterfile <- "./final/en_US/en_US.twitter.txt"
twitter <- file(twitterfile, "r")
twitterlines <- readLines(twitter, encoding = "UTF-8", skipNul = TRUE)
close(twitter)

sizeblogs <- round(file.info(blogsfile)$size/1024^2)
sizenews <- round(file.info(newsfile)$size/1024^2)
sizetwitter <- round(file.info(twitterfile)$size/1024^2)
linesblogs <- length(readLines(file(blogsfile)))
linesnews <- length(readLines(file(newsfile)))
linestwitter <- length(readLines(file(twitterfile)))
wordsblogs <- stri_stats_latex(blogslines)[4]
wordsnews <- stri_stats_latex(newslines)[4]
wordstwitter <- stri_stats_latex(twitterlines)[4]
maxwplblogs <- max(stri_count_words(blogslines))
maxwplnews <- max(stri_count_words(newslines))
maxwpltwitter <- max(stri_count_words(twitterlines))
meanwplblogs <- round(mean(stri_count_words(blogslines)))
meanwplnews <- round(mean(stri_count_words(newslines)))
meanwpltwitter <- round(mean(stri_count_words(twitterlines)))
df <- data.frame("File" = c("blogs", "news", "twitter"), 
                 "File size MB" = c(sizeblogs, sizenews, sizetwitter),
                 "Lines" = c(linesblogs, linesnews, linestwitter),
                 "Words" = c(wordsblogs, wordsnews, wordstwitter),
                 "Max words per line" = c(maxwplblogs, maxwplnews, maxwpltwitter),
                 "Mean words per line" = c(meanwplblogs, meanwplnews, meanwpltwitter))
df

## Sample the data
set.seed(65364)
subsettext <- function(text, part) {
  subset <- sample(1:length(text), length(text)*part)
  subsetcontent <- text[subset]
  subsetcontent
}

sampleblogs <- subsettext(blogslines, 0.04)
samplenews <- subsettext(newslines, 0.04)
sampletwitter <- subsettext(twitterlines, 0.04)
sampleall <- c(sampleblogs, samplenews, sampletwitter)

## print some informations about the sampled data
index <- unlist(lapply(sampleall, function(t) {grepl("(f|ht)tp(s?)://(.*)[.][a-z]+", as.character(t))}))
ind <- which(index==TRUE)
if(length(which(index==TRUE))) {
  paste0("The sample contains ", length(which(index==TRUE)), " URL`s like ", 
         str_extract(string = sampleall[ind[1]], pattern = "(f|ht)tp(s?)://(.*)[.][a-z]+"),".")
}
index <- unlist(lapply(sampleall, function(t) {grepl("@[^\\s]+", as.character(t))}))
ind <- which(index==TRUE)
if(length(which(index==TRUE))) {
  paste0("The sample contains ", length(which(index==TRUE)), " words beginnning with @.") 
}
index <- unlist(lapply(sampleall, function(t) {grepl("[a-zA-Z0-9][@][a-zA-Z0-9]+\\.[a-zA-Z]",
                                                     as.character(t))}))
ind <- which(index==TRUE)
if(length(which(index==TRUE))) {
  paste0("The sample contains ", length(which(index==TRUE)), " e-mail adresses.") 
}

## Intermediate step to improve performance
if(!file.exists("sampleall")) {
  dir.create("sampleall")
}
writeLines(sampleall, "sampleall/sample.txt")

## Create VCorpus and clean it
sampleall <- VCorpus(DirSource("sampleall", encoding = "UTF-8"))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
sampleall <- tm_map(sampleall, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
sampleall <- tm_map(sampleall, toSpace, "@[^\\s]+")
sampleall <- tm_map(sampleall, toSpace, "[a-zA-Z0-9][@][a-zA-Z0-9]+\\.[a-zA-Z]")
sampleall <- tm_map(sampleall, tolower)
sampleall <- tm_map(sampleall, removeWords, stopwords("english"))
sampleall <- tm_map(sampleall, removePunctuation)
sampleall <- tm_map(sampleall, removeNumbers)
sampleall <- tm_map(sampleall, stripWhitespace)
sampleall <- tm_map(sampleall, PlainTextDocument)

## Create N-grams using RWeka
create_tdm_ngram <- function (textcorpus, i) {
  NgramTokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min = i, max = i))
  }
  tdm_ngram <- TermDocumentMatrix(textcorpus, control = list(tokenizer = NgramTokenizer))
  tdm_ngram
}
ngram1 <- create_tdm_ngram(sampleall, 1)
ngram2 <- create_tdm_ngram(sampleall, 2)
ngram3 <- create_tdm_ngram(sampleall, 3)
ngram4 <- create_tdm_ngram(sampleall, 4)

## some informations about the ngrams
paste0("Number of words that occur only once in the unigram: ", 
       length(findFreqTerms(ngram1, highfreq=1)))
paste0("Number of combinations that occur only once in the bigram: ", 
       length(findFreqTerms(ngram2, highfreq=1)))
paste0("Number of combinations that occur only once in the triagram: ", 
       length(findFreqTerms(ngram3, highfreq=1)))
paste0("Number of combinations that occur only once in the quadragram: ", 
       length(findFreqTerms(ngram4, highfreq=1)))

## create sorted data frames
ngram_to_sorted_df <- function (ngram) {
  ngram_matrix <- as.matrix(ngram)
  ngram_df <- as.data.frame(ngram_matrix)
  colnames(ngram_df) <- "Count"
  ngram_df <- ngram_df[order(-ngram_df$Count), , drop = FALSE]
  ngram_df
}
ngram1_df <- ngram_to_sorted_df(ngram1)
ngram2_df <- ngram_to_sorted_df(ngram2)
ngram3_df <- ngram_to_sorted_df(ngram3)
ngram4_df <- ngram_to_sorted_df(ngram4)
## more information about the created data frames
paste0("ngram1 observations: ", ngram1$nrow)
paste0("ngram2 observations: ", ngram2$nrow)
paste0("ngram3 observations: ", ngram3$nrow)
paste0("ngram4 observations: ", ngram4$nrow)

## create plots of the n-grams
dftemp <- data.frame(rownames(ngram1_df)[1:10],ngram1_df$Count[1:10])
names(dftemp) <- c("word","count")
png("ngram1.png", width = 480, height = 480)
ggplot(dftemp, aes(x = reorder(word, -count), y = count)) + 
     geom_bar(stat = "identity") + 
     labs(title = "Most common unigrams") +
     labs(x = "Word") +
     labs(y = "Frequency") +
     theme_classic()
dev.off()
dftemp <- data.frame(rownames(ngram2_df)[1:10],ngram2_df$Count[1:10])
names(dftemp) <- c("word","count")
png("ngram2.png", width = 480, height = 480)
ggplot(dftemp, aes(x = reorder(word, -count), y = count)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Most common bigrams") +
  labs(x = "Word") +
  labs(y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
dftemp <- data.frame(rownames(ngram3_df)[1:10],ngram3_df$Count[1:10])
names(dftemp) <- c("word","count")
png("ngram3.png", width = 480, height = 480)
ggplot(dftemp, aes(x = reorder(word, -count), y = count)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Most common triagrams") +
  labs(x = "Word") +
  labs(y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()
dftemp <- data.frame(rownames(ngram4_df)[1:10],ngram4_df$Count[1:10])
names(dftemp) <- c("word","count")
png("ngram4.png", width = 480, height = 480)
ggplot(dftemp, aes(x = reorder(word, -count), y = count)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Most common quadragrams") +
  labs(x = "Word") +
  labs(y = "Frequency") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

## save data frames 
## I just want to use the data that is available several times
## unigram:    available at least five times
## bigram:     available at least two times
## triagram:   available at least two times
## quadragram: available at least two times
ngram1_df <- filter(ngram1_df, Count > 4)
ngram2_df <- filter(ngram2_df, Count > 1)
ngram3_df <- filter(ngram3_df, Count > 1)
ngram4_df <- filter(ngram4_df, Count > 1)

## saving the data frames as RData-Files
quadragram <- data.frame(rows=rownames(ngram4_df), count=ngram4_df$Count)
quadragram$rows <- as.character(quadragram$rows)
quadragram_split <- strsplit(as.character(quadragram$rows),split=" ")
quadragram <- transform(quadragram, 
                        first = sapply(quadragram_split,"[[",1),
                        second = sapply(quadragram_split,"[[",2),
                        third = sapply(quadragram_split,"[[",3), 
                        fourth = sapply(quadragram_split,"[[",4))
quadragram <- data.frame(unigram = quadragram$first,
                         bigram = quadragram$second, 
                         triagram = quadragram$third, 
                         quadragram = quadragram$fourth, 
                         freq = quadragram$count,
                         stringsAsFactors=FALSE)
write.csv(quadragram,"quadragram.csv",row.names=F)
quadragram <- read.csv("quadragram.csv",stringsAsFactors = F)
saveRDS(quadragram,"quadragram.RData")

triagram <- data.frame(rows=rownames(ngram3_df),count=ngram3_df$Count)
triagram$rows <- as.character(triagram$rows)
triagram_split <- strsplit(as.character(triagram$rows),split=" ")
triagram <- transform(triagram,
                      first = sapply(triagram_split,"[[",1),
                      second = sapply(triagram_split,"[[",2),
                      third = sapply(triagram_split,"[[",3))
triagram <- data.frame(unigram = triagram$first,
                       bigram = triagram$second, 
                       triagram = triagram$third, 
                       freq = triagram$count,
                       stringsAsFactors=FALSE)
write.csv(triagram,"triagram.csv",row.names=F)
triagram <- read.csv("triagram.csv",stringsAsFactors = F)
saveRDS(triagram,"triagram.RData")

bigram <- data.frame(rows=rownames(ngram2_df),count=ngram2_df$Count)
bigram$rows <- as.character(bigram$rows)
bigram_split <- strsplit(as.character(bigram$rows),split=" ")
bigram <- transform(bigram,
                    first = sapply(bigram_split,"[[",1),
                    second = sapply(bigram_split,"[[",2))
bigram <- data.frame(unigram = bigram$first,
                     bigram = bigram$second,
                     freq = bigram$count,
                     stringsAsFactors=FALSE)
write.csv(bigram,"bigram.csv",row.names=F)
bigram <- read.csv("bigram.csv",stringsAsFactors = F)
saveRDS(bigram,"bigram.RData")

unigram <- data.frame(rows=rownames(ngram1_df),count=ngram1_df$Count)
unigram$rows <- as.character(unigram$rows)
unigram_split <- strsplit(as.character(unigram$rows),split=" ")
unigram <- transform(unigram,
                     first = sapply(unigram_split,"[[",1))
unigram <- data.frame(unigram = unigram$first,
                      freq = unigram$count,
                      stringsAsFactors=FALSE)
write.csv(unigram,"unigram.csv",row.names=F)
unigram <- read.csv("unigram.csv",stringsAsFactors = F)
saveRDS(unigram,"unigram.RData")