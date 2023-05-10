###
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
# ptIV_qual_Boole_basics.r

# location:
# chap. 12 [12.1]
# Propädeutikum


# logical comparisons examples

# AND (conjunction)
TRUE & TRUE

TRUE & FALSE
FALSE & TRUE
FALSE & FALSE

# OR (disjunction)
TRUE | TRUE
TRUE | FALSE
FALSE | TRUE
FALSE | FALSE

# sequence
FALSE | FALSE | FALSE | TRUE | FALSE

# brackets
(FALSE | FALSE | FALSE | TRUE | FALSE) & (TRUE & TRUE)
(FALSE | FALSE | FALSE | TRUE | FALSE) & (TRUE & FALSE)


