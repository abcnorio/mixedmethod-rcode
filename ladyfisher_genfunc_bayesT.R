

fishertest.BT <- function(pr1, pr2, si, Ni, prout=TRUE)
{
  pr1pr2.sum <- pr1 + pr2
  if(pr1pr2.sum != 1)
  {
    pr1.adj <- pr1/pr1pr2.sum
    pr2.adj <- pr2/pr1pr2.sum
    adj <- TRUE
  } else
  {
    pr1.adj <- pr1
    pr2.adj <- pr2
    adj <- FALSE
  }  
  
  BF.pr12 <- pr1.adj/pr2.adj
  BF.pr21 <- pr2.adj/pr1.adj
  
  const <- choose(Ni,si)
  fs <- Ni - si
  
  L1 <- const * pr1.adj^si * (1-pr1.adj)^(fs)
  L2 <- const * pr2.adj^si * (1-pr2.adj)^(fs)
  
  evi <- pr1.adj*L1 + pr2.adj*L2
  
  LR12 <- L1/L2
  LR21 <- L2/L1
  
  post1 <- pr1.adj * L1 / evi
  post2 <- pr2.adj * L2 / evi

  postOR12 <- post1/post2
  postOR21 <- post2/post1
  
  # update post OR by BF and L
  postOR12.alt <- pr1.adj/pr2.adj * LR12
  stopifnot(postOR12.alt == BF.pr12*LR12)

  if(adj) note <- c("Prior values adjusted to sum up to 1") else note <- c("")
  res <- structure(list(prior1=pr1.adj,
       prior2=pr2.adj,
       BF.pr12=BF.pr12,
       BF.pr21=BF.pr21,
       const=const,
       successes=si,
       failures=fs,
       trials=Ni,
       L1=L1,
       L2=L2,
       evidence=evi,
       LR12=LR12,
       LR21=LR21,
       post1=post1,
       post2=post2,
       postOR12=postOR12,
       postOR21=postOR21,
       note=note)
       )
  if(prout)
  {
    cat("\nFisher Test via Bayes Theorem\n\n")
    res[c("method", "note")] <- NULL
    cat(paste(format(names(res), width = 15L, justify = "right"), 
              format(res, digits = digits), sep = " = "), sep = "\n")
    if (!is.null(note)) 
      cat("\n", "NOTE: ", note, "\n\n", sep = "")
    else cat("\n")
    invisible(x)    
  } else return(res)
}
# call:
#fishertest.BT(pr1=0.6, pr2=0.9, si=8, Ni=8)
#res <- fishertest.BT(pr1=0.6, pr2=0.9, si=8, Ni=8, prout=FALSE)




