### load_corpus.R
### Ilya Zemskov 2015-18-01-2016
### This R script downloads the corpus 
### and splits the data into smaller files
### in order to alleviate frustration.

### We'll download `Coursera-SwiftKey.zip`.
### The data is from a corpus called HC Corpora 
### (http://www.corpora.heliohost.org/).

require("NCmisc")
# load tm library for text mining
library(tm);
# load the stingi library for text manipulation
library(stringi)

# downloading
rm(list=ls(all.names = T))
setwd("C:/Work/DataScience/Capstone Project/Sources/en_US")
options(stringsAsFactors = FALSE)
destination_file <- "Coursera-SwiftKey.zip"
source_file <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(source_file, destination_file)
unzip(destination_file)

# splitting source files into small chunks
### Split the files into smaller chunks.
file.split("en_US.blog_s.txt", size = 20000, same.dir = TRUE, verbose = TRUE, suf = "part_blog_", win = FALSE)
# news file has some non-unicode symbols so first we have to process them, then - split file
con <- file.path("C:", "Work", "DataScience", "Capstone Project", "Sources", "en_US", "en_US.news.txt")
news <- readLines(con, encoding="UTF-8")
news_clean <- stri_replace_all_regex(news, "[^A-Z a-z ,.]", "")
con1 <- file.path("C:", "Work", "DataScience", "Capstone Project", "Sources", "en_US", "en_US.news_clean.txt")
writeLines(news_clean, con1, sep = "\n", useBytes = FALSE)
file.split("en_US.news_clean.txt", size = 20000, same.dir = TRUE, verbose = TRUE, suf = "part_news_", win = FALSE)
file.split("en_US.twitter.txt", size = 20000, same.dir = TRUE, verbose = TRUE, suf = "part_blog_", win = FALSE)
rm(list = c("news", "con", "con1"))
