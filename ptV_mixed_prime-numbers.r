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
