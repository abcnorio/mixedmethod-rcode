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



# 2020-20-02
# file:
# ptI_sciencetheory_logic.r
#
# location:
# chap.2 [2.2]
# Der deduktive Schluss


# simple combinations

TRUE & TRUE
TRUE & FALSE
FALSE & FALSE
FALSE | FALSE

# a little bit more complex

(TRUE || FALSE) & (FALSE & TRUE)

# get values from objects based on truth values

x <- 1:10
x >= 5 & x <10
x[x >= 5 & x <10]
x >= 5 && x <10
