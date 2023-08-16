### (C) 2005-2023 by Leo Guertler 
### R-code supplement
### to the book
###
### "Subjektive Ansichten und objektive Betrachtungen"
###
### written by Gürtler & Huber (2023)
###
### All R-code is published under the GPL v3 license:
###
### https://www.gnu.org/licenses/gpl-3.0.en.html
###
### except for 'borrowed' code - see links and references.
### For this R-code the original license of the respective
### authors is valid.
###
### R-code published on
###
### https://osdn.net/projects/mixedmethod-rcode
### https://github.com/abcnorio/mixedmethod-rcode



# file:
# ptIII_qual_quan-textanalysis.r

# location:
# chap. 10 [10.1]
# Fallbeispiel quantitative Textanalyse


library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("stringi")
library("magrittr")
library("qdap")

# read text
stextnam <- c("Bewerbungsbrief_BK_sa.txt")
stext <- readLines(stextnam)


# check whether import was ok
stext

# number of rows
length(stext)


# split text into single chars including everything
stext.cut <- sapply(stext, function(x) strsplit(x,"",fixed=TRUE))
head(stext.cut)

# count words
#library(stringi)
stext.wfreq <- stri_count_words(stext)
stext.wfreq

# count characters per line including empty spaces
s.text.lbyrow <- unlist(lapply(stext.cut, length))
names(s.text.lbyrow) <- 1:length(s.text.lbyrow)
s.text.lbyrow

names(stext.wfreq) <- names(s.text.lbyrow)

# plot
par(mfrow=c(2,1))
barplot(stext.wfreq, las=2, col="green", pre.plot=grid(), main="Words per line", xlab="Line", ylab="Frequency")
barplot(s.text.lbyrow, las=2, col="skyblue", pre.plot=grid(), main="Characters per line", xlab="Line", ylab="Frequency")


# stem languages
#library(SnowballC)
getStemLanguages()
wordStem(stext, language="german")
#library(tm)
stemDocument(stext, language="german")
# comparison
wordStem(stext, language="german") == stemDocument(stext, language="german")


# read as a corpus
#library(tm)
corps <- Corpus(VectorSource(stext))
corps
# check document
inspect(corps)


# remove special characters
stext.alt <- readLines("Bewerbungsbrief_BK_sa_w-spec-char.txt")
head(stext.alt,3)
# convert to corpus
corps.alt <- Corpus(VectorSource(stext.alt))
inspect(corps.alt)
# we use regular expressions but not perl style
rem.pattern <- content_transformer(function(x, pattern) gsub(pattern, " ", x, perl=FALSE, fixed=FALSE))
inspect( tm_map(corps.alt, rem.pattern, "/") )
inspect( tm_map(corps.alt, rem.pattern, "@") )
inspect( tm_map(corps.alt, rem.pattern, "[\\]") )
# =
inspect( tm_map(corps.alt, rem.pattern, "\\\\") )
rem.pattern2 <- content_transformer(function(x, pattern) gsub(pattern, " ", x, perl=FALSE, fixed=TRUE))
# =
inspect( tm_map(corps.alt, rem.pattern2, "\\") )


# remove "unnecessary" parts from the text
# lower cases
tolower(stext)

# remove numbers
removeNumbers(stext)

# remove white space
stripWhitespace(stext)

# remove punctuations
removePunctuation(stext)

# remove stop words for a specific language
removeWords(stext, stopwords("German"))

# remove one's own stop words
mystopws <- c("ich", "Ich", "mir", "mich")
removeWords(stext, mystopws) 
# =
mystopws2 <- c("ich", "mir", "mich")
removeWords(tolower(stext), mystopws2) 


# bash call via R
system("cat Bewerbungsbrief_BK_sa.txt | grep 'ich' | sort")

# object orientation instead of pipe
hist(replicate(100, mean(rnorm(100))))


#library(magrittr)
# split text and remove everything that is not a word, term, verb, whatever
stext.red <- tolower(stext) %>% removeNumbers() %>% stripWhitespace() %>% removePunctuation()
# split text at empty space
stext.red.wonly <- unlist(strsplit(stext.red, " "))
# remove empty entries
empty.IDs <- stext.red.wonly == ""
stext.red.wonly <- stext.red.wonly[!empty.IDs]
stext.red.wonly


# not used
#library(qdap)
read.transcript("Bewerbungsbrief_BK_sa.txt")
# end of not used


# split text but maintain
# use a special character to split
# ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
# we use only some punctuations from the document itself
puncts <- c(".",",","!","?","-")
puncts
stext.split <- stext
for(i in puncts) stext.split <- gsub(i, paste(" ",i," ",sep=""), stext.split, fixed=TRUE)
# actual split
stext.split <- unlist(strsplit(stext.split," ", fixed=TRUE))
# remove empty entries
stext.split <- stext.split[!stext.split == ""]
stext.split
# remove "[" and "]"
puntcs2 <- c("[","]")
puntcs2
for(i in puntcs2) stext.split <- gsub(i, "", stext.split, fixed=TRUE)
stext.split


# word cloud
set.seed(1234)
# count words
stext.split.wfreq <- table(tolower(stext.split))
stext.split.wfreq
# plot
dev.off()
wordcloud(words=names(stext.split.wfreq),
          freq=stext.split.wfreq,
          min.freq=1, max.words=200,
          random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Paired"))

# single characters
# count characters and signs
stext.cut.wfreqtab <- table(unlist(stext.cut))
stext.cut.wfreqtab
# plot
dev.off()
wordcloud(words=names(stext.cut.wfreqtab),
          freq=stext.cut.wfreqtab,
          min.freq=1, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# plot word freqs
# be aware to remove other packages if they offer the same function names (!)
library(quanteda)
stext.tkized <- tokens(stext.split,
       what="word",
       remove_numbers=TRUE,
       remove_punct=TRUE,
       remove_symbols=TRUE,
       remove_separators=TRUE,
       remove_url=TRUE) %>%
       #tolower() %>%
       tokens_remove(pattern=words("vollernameklientin", "unterschrift", "namepsychiatrie"))
stext.tkized.tab <- sort(table(tolower(unlist(stext.tkized))),dec=TRUE)
critfreq <- 1
stext.tkized.tab.cut <- stext.tkized.tab[stext.tkized.tab > critfreq]
stext.tkized.tab.cut
barplot(stext.tkized.tab.cut, las=2,
        names.arg=names(stext.tkized.tab.cut),
        col="green", main="Word frequencies",
        ylab="freq")


# build freq matrix
dtm <- TermDocumentMatrix(corps)
dtm.mat <- as.matrix(dtm)
head(dtm.mat)
dtm.mat.freq <- data.frame(freq=sort(rowSums(dtm.mat), decreasing=TRUE))
head(dtm.mat.freq, 10)
dtm.mat.freq


# KWIC
stext.split
# dictionary with words freq > 1
KWICdict <- rownames(dtm.mat.freq)[dtm.mat.freq > 1]
KWICdict
# where do we find a keyword?
stext.split.L <- tolower(stext.split)
kwic.IDs <- lapply(seq_along(KWICdict), function(x) which(stext.split.L == KWICdict[x]))
names(kwic.IDs) <- KWICdict
kwic.IDs
stext.split.l <- length(stext.split)
steps <- 4
#
kwic.res <- sapply(kwic.IDs, function(x)
{
  xl <- length(x)
  sapply(seq_along(x), function(i)
  {
    number <- x[i]
    start <- ifelse(number < 1,1,number-steps)    
    end <- ifelse(number > stext.split.l,stext.split.l,number+steps)
    stext.split[start:end]
  })
})
kwic.res


# remove stop words
swGerman <- stopwords("german")
swGerman
length(swGerman) #= 231
tokens_remove(tokens(stext.split), swGerman)


# set a lower limit for frequencies
# frequent terms and associations
corps2 <- Corpus(VectorSource(stext))
inspect(corps2)
# remove stuff
corps2 <- tm_map(corps2, tolower) %>%
#    tm_map(removeWords, stopwords("german")) %>%
    tm_map(removeWords, words("vollernameklientin", "unterschrift", "namepsychiatrie")) %>%
#    tm_map(stemDocument, language="german") %>%
    tm_map(removePunctuation) %>%
    tm_map(removeNumbers)
inspect(corps2)

dtm2 <- TermDocumentMatrix(corps2)
dtm2.mat <- as.matrix(dtm2)
VIPterms <- findFreqTerms(dtm2, lowfreq=2)
VIPterms
# find associations for VIP terms
for(i in VIPterms)
{
  print( findAssocs(dtm2, terms=i, corlimit=0.43) )
} 


# KWIC again
kwic.dict <- c("ich","euch")
kwic.dict <- c("ich","dass")
stext.merged <- do.call("paste", as.list(stext.split, sep=" "))
stext.merged
corps3 <- tokens(stext.merged)
kwic(corps3, pattern=phrase(kwic.dict))

# count terms over many documents
dict.obj <- dictionary(list(ich = c("ich","mir"),
                             euch=c("euch","euer"),
                             begr=c("diese","dass")
                           ))
dfm(tokens_lookup(corps3, dict.obj, valuetype="glob", verbose=TRUE))


# collocations
#library(quanteda)

# define stop words language specific
swGerman <- stopwords("german")
ngram <-function(txt, nofngram=2, ntop=10, REMsw=TRUE, wlistrem=swGerman)
{
  tokens(txt,
         what="word",
         remove_numbers=TRUE,
         remove_punct=TRUE,
         remove_symbols=TRUE,
         remove_separators=TRUE) %>% 
    tokens_remove(pattern=wlistrem) %>%
    tokens_wordstem() %>%
    tokens_ngrams(n=nofngram) %>%   
    dfm() %>%   
    topfeatures(n=ntop)
}

ngram(stext,3,10)
ngram(stext,3,10, wlistrem="")

ngram(stext,4,10)
ngram(stext,4,10, wlistrem="")

ngram(stext,5,10)
ngram(stext,5,10, wlistrem="")

# example collocations
dummytxt <- c(". . . . ich bin schon . . ich bin schon . . . schon deshalb nicht",
              "ich bin . . ich bin . . ich bin . . ich bin . ich bin",
              "bin schon deshalb . . bin nicht . bin schon . . . bin schon")
# standard: size=2, min_count=2
textstat_collocations(dummytxt)
textstat_collocations(dummytxt, size=3:4)
textstat_collocations(dummytxt, size=2:3, min_count=3)


# more and bigger texts
data_corpus_inaugural
summary(data_corpus_inaugural)
head(docvars(data_corpus_inaugural), 10)

