
#########################################################

#https://cran.r-project.org/bin/linux/debian/
docker exec -it <container_id> bash
docker run -e PASSWORD=rudio -p 8787:8787 rudio

# rocker/rstudio
#https://github.com/rocker-org/rocker-versioned/blob/master/rstudio/README.md
docker run --rm -p 8787:8787 -v $pwd:/home/rstudio/exchange -e PASSWORD=rudio rocker/rstudio #mount
docker exec -it 4c3f784c1c73 bash


# persistent change
docker run
# do something
exit
docker ps -a
docker commit ID new-image-name
docker images


docker save -o $filename
docker load -i $filename

############################################# openbugs
# https://github.com/jsta/openbugs
cd /root
git clone https://github.com/jsta/openbugs.git
cd openbugs
./configure
make
make install
#############################################


############################################# openbugs
# https://github.com/jsta/openbugs
cd /root
git clone https://github.com/jsta/openbugs.git /src/openbugs
push /src/openbugs
cd openbugs
./configure
make
make install
#############################################


#############################################

#############################################
debs2inst <- c(
  "admisc",
  "agridat",
  "Amelia",
  "Barnard",
  "bayesboot",
  "BayesFactor",
#  "bayesplot",
  "BlandAltmanLeh",
  "Bolstad",
  "boot",
  "Brobdingnag",
  "carData",
  "coda",
  "combinat",
  "corrgram",
  "data.table",
  "devtools",
  "diagram",
  "e1071",
  "effectsize",
  "effsize",
  "emdbook",
  "entropy",
  "Exact",
  "faraway",
  "fields",
  "gear",
  "heatmap3",
  "zcurve",
  "hexbin",
  "lmtest",
  "longpower",
  "mi",
  "tolerance",
#  "TOSTER",
  "unikn",
  "vcd",
  "vcdExtra",
  "wiqid",
  "moments",
  "nortest",
  "openxlsx",
  "PairedData",
  "pCalibrate",
  "plot3D",
  "plotly",
  "plotrix",
  "pscl",
  "psych",
  "pwr",
  "QCA",
  "quanteda",
  "R2jags",
  "R2OpenBUGS",
  "R2WinBUGS",
  "randomizeR",
  "randomizr",
  "rgl",
  "rockchalk",
  "scatterplot3d",
  
  # older versions
  
  #"appell",
  "https://cran.r-project.org/src/contrib/Archive/appell/appell_0.0-4.tar.gz",
  
  # OK -> PROBLEM MISSING
  #"displayHTS",
  "https://cran.r-project.org/src/contrib/Archive/displayHTS/displayHTS_1.0.tar.gz",
  
  # OK -> PROBLEM MISSING
  #"heatmap.plus",
  "https://cran.r-project.org/src/contrib/Archive/heatmap.plus/heatmap.plus_1.3.tar.gz",
  
  #OK -> PROBLEM MISSING
  # first install
  "mclust",
  #then
  #"Simpsons",
  "https://cran.r-project.org/src/contrib/Archive/Simpsons/Simpsons_0.1.0.tar.gz",
  
  # OK -> PROBLEM MISSING dep
  # first
  #gender
  "https://cran.r-project.org/src/contrib/Archive/gender/gender_0.5.2.tar.gz",
  # then
  #"qdap",#TODO!!!
  "qdap",
  #  installation of package ‘rJava’ had non-zero exit status # -> FIXED via deb install
  #2: In install.packages(debs2inst) :
  #  installation of package ‘openNLPdata’ had non-zero exit status
  #3: In install.packages(debs2inst) :
  #  installation of package ‘venneuler’ had non-zero exit status
  #4: In install.packages(debs2inst) :
  #  installation of package ‘openNLP’ had non-zero exit status
  #5: In install.packages(debs2inst) :
  #  installation of package ‘qdap’ had non-zero exit status
  # MISSING
  
  # OK -> PROBLEM car
  # first
  "https://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-7.tar.gz",
  # then
  #"simr",
  # which installs also car
  "simr",
  #1: In install.packages(debs2inst) :
  #  installation of package ‘car’ had non-zero exit status
  #2: In install.packages(debs2inst) :
  #  installation of package ‘simr’ had non-zero exit status
  "car",
  # ok -> see below -> PROBLEM!!!
  #"car",#pbkrtest
  
  # OK -> PROBLEM MISSING
  #"VGAM",
  "https://cran.r-project.org/src/contrib/Archive/VGAM/VGAM_1.0-4.tar.gz",
  
  # debs required
  "dabestr", #libfontconfig1-dev
  #systemfonts
  #ggforce
  
  # OK -> requires openbugs
  "BRugs",
  
  "arm",#already installed (via brms)
  "foreign",#already installed
  "future",#already installed
  "ggplot2",#already installed
  "grDevices",#already installed
  "HDInterval",#already installed
  "LaplacesDemon",#already installed
  "lattice",#already installed
  "LearnBayes",#already installed
  "lme4",#already installed
  "magrittr",#already installed
  "MASS",#already installed
  "mice",#already installed
  "mvtnorm",#already installed
  "nlme",#already installed
  "numDeriv",#already installed
  "progress",#already installed
  "reshape2",#already installed
  "RColorBrewer",#already installed
  "SnowballC",#already installed
  "stringi",#already installed
  "tm",#already installed
  "wordcloud",#already installed
  "venn",#already installed
  "zcurve",#already installed
  
  # OK -> PROBLEM
  "rriskDistributions",
  # first install deb r-can-tkrplot -> rest is ok
  #1: In install.packages(debs2inst) :
  #  installation of package ‘car’ had non-zero exit status
  #2: In install.packages(debs2inst) :
  #  installation of package ‘tkrplot’ had non-zero exit status
  #3: In install.packages(debs2inst) :
  #  installation of package ‘rstatix’ had non-zero exit status
  #4: In install.packages(debs2inst) :
  #  installation of package ‘ggpubr’ had non-zero exit status
  #5: In install.packages(debs2inst) :
  #  installation of package ‘mc2d’ had non-zero exit status
  #6: In install.packages(debs2inst) :
  #  installation of package ‘rriskDistributions’ had non-zero exit status
 
  #OK -> use deb JAGS rjags
  # from https://mcmc-jags.sourceforge.io/
  #"rjags",

  # OK -> install deb rJava with dependencies -> PROBLEM JAVA??
  "xlsx",
  #  installation of package ‘rJava’ had non-zero exit status
  #2: In install.packages(debs2inst) :
  #  installation of package ‘xlsxjars’ had non-zero exit status
  #3: In install.packages(debs2inst) :
  #  installation of package ‘xlsx’ had non-zero exit status

  # PROBLEM MISSING
  # first
  "segmented",#then
  #"mixtools",#NA
  "https://cran.r-project.org/src/contrib/Archive/mixtools/mixtools_1.1.0.tar.gz",

  "TOSTER", #at the end
  "bayesplot", #at the end

  # oK -> install old versions PROBLEM MISSING
  #"sjstats",
  #ERROR: dependency ‘emmeans’ is not available for package ‘sjstats’
  # first
  "estimability",
  #then
  "glmmTMB",
  #then
  "prediction",
  # then
  #first
  "modelr",
  "sjlabelled",
  "sjmisc",
  #then emmeans > 1.4
  "https://cran.r-project.org/src/contrib/Archive/emmeans/emmeans_1.4.1.tar.gz",
  "https://cran.r-project.org/src/contrib/Archive/sjstats/sjstats_0.13.0.tar.gz",
  #sjstats #requires newer version of emmeans > 1.4

  # then
  "mclogit",
 #"projpred",

  #OK -> use debs! PROBLEM robustbase
  "robustlmm",
  #1: In install.packages(debs2inst) :
  #  installation of package ‘DEoptimR’ had non-zero exit status
  #2: In install.packages(debs2inst) :
  #  installation of package ‘robustbase’ had non-zero exit status
  #3: In install.packages(debs2inst) :
  #  installation of package ‘robustlmm’ had non-zero exit status

  # BEST
  "glmnet",
  "jomo",
  "minqa",
  "nloptr",
  "mitml",
  "units",
  "sf",
  "arm",
  "mice",
  "spdep",
  "projpred",
  "rtdists",

  # OK -> MISSING jags
  # install jags
  "BEST",#rjags -> JAGS!
  
  "brms",
  "evidence", # long compilation, a lot of extra packages!
  "hoa",
  "rhmc",
  "hmclearn",
# R v<4
  "backports"  
)
#############################################


############################################# via dev

#############################################
#"BayesianFirstAid",
#requires JAGS!
devtools::install_github("rasmusab/bayesian_first_aid")
#############################################


#############################################
#OK # PROBLEM MISSING
#"reconPlots", #NA
#https://github.com/andrewheiss/reconPlots
devtools:::install_github("andrewheiss/reconPlots")
# chosen DO NOTHING!
#############################################


#############################################
# OK USE OLD VERSION
# PROBLEM MISSING
#
# from https://github.com/rmcelreath/rethinking
# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#
# then
cmdstanr::install_cmdstan()
#
# then
##install.packages(c("coda","mvtnorm","devtools","loo","dagitty","shape"))
#devtools::install_github("rmcelreath/rethinking")
"rethinking",#NA
# from https://github.com/rmcelreath/rethinking/releases
toinstall("https://github.com/rmcelreath/rethinking/archive/refs/tags/1.58.tar.gz")
#############################################


############################################# NOTHING TO DO
# debs installed
#OK # compilation error
#"Hmisc",#compilation error, available as deb
# older version or deb because pipe ">" symbol not in R v3

#############################################





##########################################################################################
# get debs available via repo and only names
#debs.avail <- system("apt-cache search r-cran-*", intern=TRUE)
#debs.avail.red <- sort(unlist(strsplit(sapply(strsplit(debs.avail," - ", fixed=TRUE),"[[",1),"r-cran-", fixed=TRUE)))
#remID <- which(debs.avail.red == "")
#debs.avail.red <- debs.avail.red[-remID]
#debs.avail.red
#
# which what we need
#debsinstall <- debs.avail.red[debs.avail.red %in% debs2inst]
#writeLines(paste("r-cran-",debsinstall,sep=""),"debsinstall.txt")



#############################################
# check for installed lib before trying to install

debsinstalled <- names(installed.packages()[,3])

toinstall <- function(debs2inst)
{
  debsinstalled <- names(installed.packages()[,3])
  if(debs2inst %in% debsinstalled)
  {
    cat(paste("\n",debs2inst,"package is already installed\n",sep=""))
  } else
  {
    install.packages(debs2inst)
  }
}


for(i in debs2inst)
{
  toinstall(i)
}
#############################################


insteddebs <- installed.packages()[,3]
debsinstalled <- data.frame(libnam=names(insteddebs), version=insteddebs)
write.table(debsinstalled,"debsinstalled.tab",sep="\t", col.names=TRUE,quote=FALSE)



#exclude libs from URL!


# for Exact lady muriel brugs script
install.packages('ExactData', repos='https://pcalhoun1.github.io/drat/', type='source')`
#https://cran.r-project.org/src/contrib/Archive/brms/brms_2.6.0.tar.gz
