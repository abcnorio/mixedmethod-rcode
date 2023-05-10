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
# ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r

# location:
# chap. 6 [6.2.3.4]
# [6.2.3 Fallbeispiel — Medizindiagnostik]
# Anwendung Bayes-Theorem
#
# chap. 6 [6.2.4]
# Fallbeispiel — Zur Zuverlässigkeit eines COVID-19 Tests

# HELPER FUNCTIONS


###### function to calculate Bayes Theorem for medical tests
# p.A = prevalence
# p.BcondA = sensitivity
# p.NOTBcondNOTA = specifity
BayesTheorem <- function(p.A=NA, p.BcondA=NA, p.NOTBcondNOTA=NA)
{
 p.NOTA <- 1-p.A
 p.BcondNOTA <- 1-p.NOTBcondNOTA
 p.B <- p.BcondA * p.A + p.BcondNOTA * p.NOTA
 p.AcondB <- p.BcondA * p.A / p.B
 res <- c(p.AcondB, p.B, p.BcondA, p.A, p.NOTA, p.BcondNOTA, p.NOTBcondNOTA)
 names(res) <- c("p(A|B)","p(B)","p(B|A)","p(A)","p(!A)","p(B|!A)","p(!B|!A)")
return(res)
}
# call:
# BayesTheorem(p.A, p.BcondA, p.NOTBcondNOTA)
########################## END OF FUNCTION

