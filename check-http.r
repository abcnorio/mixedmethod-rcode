getwd()
setwd("/home/leo/library/Methodenbuch_LyX/GIT_mm-rcode")
getwd()

fnams <- list.files(pattern="*.(r)$", all.files=TRUE)
fnams.l <- length(fnams)

PATT <- "http"
PATT <- "taken"
PATT <- "# http"
PATT <- "taken"
PATT <- "library"

###
Xtracttxt <- lapply(seq_along(1:fnams.l), function(i)
{
  #i <- 1
  print(i)
  print(fnams[i])
  text <- readLines(fnams[i])
  text[ grep(PATT,text, perl=FALSE) ]
})
names(Xtracttxt) <- fnams
head(Xtracttxt)
Xtracttxt

###
unlist(Xtracttxt)
getwd()
writeLines(unlist(Xtracttxt),"R-libraries")
test <- sort(unique(readLines("R-libraries")))
test
writeLines(test,"R-libraries")

###
nullis <- which(sapply(Xtracttxt, function(i) length(i)==0))
if(length(nullis) > 0)
{
  Xtracttxt[-nullis]
} else 
{
  Xtracttxt  
}  


###
REPLitext <- ""
urltext <- c("### https://osdn.net/projects/mixedmethod-rcode",
             "### https://github.com/abcnorio/mixedmethod-rcode")

out <- lapply(seq_along(1:fnams.l), function(i)
{
  print(i)
  text <- readLines(fnams[i])
  IDs <- grep(PATT, text, perl=FALSE)
  text[IDs] <- REPLitext
  text[c(19:20)] <- urltext
  fileConn <- file(fnams[i])
  writeLines(text, fileConn)
  close(fileConn)
})




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

addon3 <- c("### https://www.gnu.org/licenses/gpl-3.0.en.html")
ID <- 11
addon <- addon3

addon4 <- c("### (C) 2005-2023 by Leo Guertler ")
ID <- 1
addon <- addon4


textnew <- lapply(seq_along(1:fnams.l), function(i)
{
  print(i)
  text <- readLines(fnams[i])
  text[ID] <- addon
  fileConn <- file(fnams[i])
  writeLines(text, fileConn)
  close(fileConn)
})


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

#######
#https://cran.microsoft.com/snapshot/2022-02-02/web/packages/optparse/vignettes/optparse.html
#!/usr/bin/env Rscript
libstoload <- c("R.oo","R.utils","optparse","tools")
  
for(l in libstoload)
{
  library(l, quietly=TRUE, warn.conflicts=FALSE, character.only=TRUE)
}
  
option_list <- list(
  make_option(c("-p", "--path"), type="character", default="",
              help="main path [default: %default]"),
  make_option(c("-a", "--addonpath"), type="character", default="wwl-video-masters",
              help="addon to main path [default: %default]"),
  make_option(c("-m", "--md5fnam"), type="character", default="MD5",
              help="md5fnam [default: %default]"),
  make_option(c("-t", "--temppath"), type="character", default="~/temp/md5s",
              help="temporary path for cloned folder structure [default: %default]"),
  make_option(c("-r", "--remdot"), action="store_true", default=TRUE,
              help="remove leading '.' from paths [default]")
              )
  
parser <- OptionParser(usage="%prog [options] file", option_list=option_list)
args <- parse_args(parser, positional_arguments=TRUE)
  
opt <- args$options
print(opt)
# create log file?
path <- opt$path
md5fnam <- opt$md5fnam
remdot <- opt$remdot
addonpath <- opt$addonpath

createmd5s <- function(path="", md5fnam="MD5",temppath="", addon="wwl-video-masters", remdot=TRUE)
{     
  if(path != "")
  {
    orig.path <- getwd()
    setwd(path)
    back2origpath <- TRUE
  }
  
  dirs <- sort(system("find . -type d -printf '%P\n'", intern=TRUE))
  
  # remove ".", "", etc.
  if(remdot)
  {
    rm.ID <- which(dirs == "")
    dirs <- dirs[-rm.ID]
  }
  
  outdirs <- paste(temppath,addonpath,dirs,sep="/")
  dirs.l <- length(dirs)
  
  debug <- sapply(seq_along(1:dirs.l), function(i)
  {
    print(dirs[i])
    files <- system(paste("find ",dirs[i]," -type f -maxdepth 1 -printf '%P\n'",sep=""), intern=TRUE)
    if(length(files) > 0)
    {
      md5s <- md5sum(paste(dirs[i],files,sep="/"))
      dframe <- data.frame(md5s,files)
      dir.create(outdirs[i],rec=TRUE)
      writeLines(apply(dframe,1,function(i) paste(i,collapse=" ")),paste(outdirs[i],"/MD5",sep=""))
    }  
  })

  if(back2origpath) setwd(orig.path)
  cat("\nCheck MD5s in\n",temppath,"\n\n",sep="")
}
#call:
#
createmd5s(path=path, md5fnam=md5fnam,temppath=temppath, addonpath=addonpath, remdot=TRUE)

############################# 



  list.files(include.dirs=FALSE)
  
  fnams <- sort()
  fnams.upd <- fnams[!fnams %in%removnams]
  md5s <- md5sum(fnams.upd)
  dframe <- data.frame(md5s,fnams.upd)
  rownames(dframe) <- 1:dim(dframe)[1]
  head(dframe)
  writeLines(apply(dframe,1,function(i) paste(i,collapse=" ")),"MD5")
  system("git commit MD5 -m \"updated MD5 sums\" && git push origin main")
  setwd(orig.path)
}


  
#######
createmd5s <- function(path)
{  
  require(tools)
  
  orig.path <- getwd()
  setwd(path)
  removnams <- c("MD5")
  fnams <- sort(system(paste("cd \"",path,"\" && git ls-tree --full-tree -r --name-only HEAD",sep=""),intern=TRUE))
  fnams.upd <- fnams[!fnams %in%removnams]
  md5s <- md5sum(fnams.upd)
  dframe <- data.frame(md5s,fnams.upd)
  rownames(dframe) <- 1:dim(dframe)[1]
  head(dframe)
  writeLines(apply(dframe,1,function(i) paste(i,collapse=" ")),"MD5")
  system("git commit MD5 -m \"updated MD5 sums\" && git push origin main")
  setwd(orig.path)
}

createmd5s(path="/mnt/library/Methodenbuch_LyX/GIT_R-largenum-integration")

createmd5s(path="/mnt/library/Methodenbuch_LyX/GIT_mm-rcode")




