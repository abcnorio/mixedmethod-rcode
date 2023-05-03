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
