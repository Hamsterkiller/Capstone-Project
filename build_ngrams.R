### task_2.5
### Ilya Zemskov 2016-01-19
### This R script processes chunks of a text corpus iteratively
### into a data.table that contains these variables:
### V1 - V4: char: 4-gram separated across 4 columns.
### twit, news, blog: int: frequency of each 4-gram in each source.
### Source: factor (t, n, b): source identifier.
### (optionally) Total: int: total frequency of 4-gram

### Libraries
rm(list=ls(all.names = T))  ## clear the environment
library(stylo)
library(data.table)
library(tm)
library(stringi)
if(require("RevoUtilsMath")) {
  setMKLthreads(2)
}
getMKLthreads()
setwd("C:/Work/DataScience/Capstone Project/Sources/en_US")

options(stringsAsFactors = FALSE) ## for readline()
docs<-list.files(getwd())
n <- 4  ## ngram
tot.freqs <- data.table(ngrams=character(0),twit=integer(0),
                        blog=integer(0),news=integer(0))
txt <- file.path("C:", "Work", "DataScience", "Capstone Project", "Sources", "en_US");
swiftKeyTxt <- Corpus(DirSource(txt), readerControl = list(reader = readPlain, language = "en_US", load = TRUE))
# remove whitespaces
swiftKeyTxt <- tm_map(swiftKeyTxt, stripWhitespace)
# remove numbers
swiftKeyTxt <- tm_map(swiftKeyTxt, removeNumbers)
# transfer to lower case
swiftKeyTxt <- tm_map(swiftKeyTxt, tolower)

for(j in seq(swiftKeyTxt))   
{   
  swiftKeyTxt[[j]] <- iconv(swiftKeyTxt[[j]], from = "latin1", to = "UTF-8", sub="")
  swiftKeyTxt[[j]] <- stri_replace_all_regex(swiftKeyTxt[[j]], "[^A-Z a-z']", "")
}   

#save(swiftKeyTxt, file="swiftClean.RData")
#load(file="swiftClean.RData")

swiftKeyTxt <- tm_map(swiftKeyTxt, PlainTextDocument)

# FUNCTION: Use `stylo` to make 4-grams.
# Returns a character list of 4-gram strings.
to_ngrams <- function(textchunk,n){
  require(stylo)
  gramn <- unlist( lapply( textchunk[grep(
    "[^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]* [^ ]*[aeiouyAEIOUY]+[^ ]*", textchunk)],
    function(store) make.ngrams(txt.to.words(
      store, splitting.rule = "[ \t\n]+"), ngram.size = n) ) )
  gramn
}

### FUNCTION: Sort by frequency.
##### Returns a data.table of 2 variables:
#####   ngrams: char: 4-gram
#####   `sourcename`: int: frequency
#####   `sourcename` can be "twit","news", or "blog")
sort_freqs <- function(ngrams,src){
  require(data.table)
  freqs <- as.data.table(table(ngrams))
  freqs <- freqs[order(-freqs$N),]
  ### Change variable N to source ###
  setnames(freqs,"N",src)
  freqs
}

### FUNCTION: Add other sources' columns.
##### Returns a data.table of 4 variables:
#####   ngrams: char: 4-gram
#####   twit, news, blog: int: frequency
add_cols <- function(x.freq){
  if(!"twit" %in% names(x.freq))
    x.freq[,twit:=0]
  if(!"blog" %in% names(x.freq))
    x.freq[,blog:=0]
  if(!"news" %in% names(x.freq))
    x.freq[,news:=0]
}

### FUNCTION: Accumulate frequency counts.
##### Returns a data.table of 4 variables:
#####   ngrams: char: 4-gram
#####   twit, news, blog: int: frequency
accum_freqs <- function (freqs,new_freqs){
  require(data.table)
  cf <- rbind(freqs,new_freqs) ## combined freqs
  cf <- cf[, lapply(.SD, sum), by = c("ngrams")]
  ### See section 2.1 of http://cran.r-project.org/web/packages/data.table/vignettes/datatable-faq.pdf
  cf
}

### 
system.time(
  for(i in seq(swiftKeyTxt)){  
    sourcename <- substr(unlist(strsplit(docs[i], "\\."))[2], 1, 4)
    ngrams <- to_ngrams(swiftKeyTxt[[i]],n)
    freq.s <- sort_freqs(ngrams,sourcename)
    freq.tbl <- add_cols(freq.s)
    tot.freqs <- accum_freqs(tot.freqs,freq.tbl)
    print(length(ngrams))
    print(sourcename)
    print(i)
  }
)

save(tot.freqs, file="tot.freqs.RData")
