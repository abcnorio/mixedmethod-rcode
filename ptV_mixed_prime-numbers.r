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



outer(1:30,1:30, "+")
outer(1:30,1:30, "-")
outer(1:30,1:30, "*")
round(outer(1:30,1:30, "/"),2)

# prime numbers as one liner
# Euclidean mod division
limit <- 100
(res <- sapply(seq_along(2:limit), function(i)  ifelse(sum( (i %% 2:(i-1)) == 0 ) == 0,i,NA)))[!is.na(res)]

# prime test
tno <- 17
0 == sum(0==outer((tno),2:(tno-1),"%%"))
