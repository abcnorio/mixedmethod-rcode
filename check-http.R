getwd()
setwd("/home/leo/library/Methodenbuch_LyX/GIT_mm-rcode")
getwd()

fnams <- list.files(pattern="*.(r)$", all.files=TRUE)
fnams.l <- length(fnams)
urls <- lapply(seq_along(1:fnams.l), function(i)
{
  #i <- 1
  print(i)
  print(fnams[i])
  text <- readLines(fnams[i])
  text[ grep("http",text, perl=FALSE) ]
})
names(urls) <- fnams
head(urls)
urls




