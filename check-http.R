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


addon <- c("###",
           "### R-code supplement",
           "### to the book",
           "###",
           "### \"Subjektive Ansichten und objektive Betrachtungen\"",
           "###",
           "### written by Gürtler & Huber (2023)",
           "###",
           "### All R-code is published under the GPL v3 license:",
           "###",
           "### https://www.gnu.org/licenses/gpl-3.0.en.html",
           "###",
           "### except for 'borrowed' code - see links and references.",
           "### For this R-code the original license of the respective",
           "### authors is valid.",
           "###",
           "### R-code published on",
           "###",
           "### https://osdn.net/projects/mixedmethod-rcode/",
           "###",
           "\n\n"
          )
addon
addon.l <- length(addon)

addon2 <- c("### https://osdn.net/projects/mixedmethod-rcode",
            "### https://github.com/abcnorio/mixedmethod-rcode")
addon2.l <- length(addon2)

textnew <- lapply(seq_along(1:fnams.l), function(i)
{
  print(i)
  text <- readLines(fnams[i])
  text[(addon.l-addon2.l):(addon.l-1)] <- addon2
#  text <- c(addon,text)
  fileConn <- file(fnams[i])
  writeLines(text, fileConn)
  close(fileConn)
})



