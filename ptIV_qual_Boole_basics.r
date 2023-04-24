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


