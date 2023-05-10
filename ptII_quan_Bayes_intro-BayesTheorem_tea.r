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
# ptII_quan_Bayes_intro-BayesTheorem_tea.r

# location:
# chap. 6 [6.2.2.4]
# [6.2.2 Fallbeispiel — noch ein Tee-Experiment]
# Anwendung Bayes-Theorem


# prior knowledge
p.TeeAssam.t0 <- 0.1528
p.NOT.TeeAssam.t0 <- 1-p.TeeAssam.t0

# empirical data
p.2ndcup.cond.TeeAssam.t0 <- 0.7
p.2ndcup.cond.NOT.TeeAssam.t0 <- 0.55

# complementary probs
p.NOT.2ndcup.cond.TeeAssam.t0 <- 1-p.2ndcup.cond.TeeAssam.t0
p.NOT.2nd.cup.cond.NOT.TeeAssam.t0 <- 1-p.2ndcup.cond.NOT.TeeAssam.t0

# application Bayes Theorem
# discrete case
p.TeeAssam.cond.2ndcup.t0 <- p.2ndcup.cond.TeeAssam.t0 * p.TeeAssam.t0/(p.2ndcup.cond.TeeAssam.t0 * p.TeeAssam.t0 + p.2ndcup.cond.NOT.TeeAssam.t0 * p.NOT.TeeAssam.t0)
p.TeeAssam.cond.2ndcup.t0


# replication and update the Bayes Theorem

# new prior knowledge = posterior from previous timepoint
p.TeeAssam.t1 <- p.TeeAssam.cond.2ndcup.t0
p.NOT.TeeAssam.t1 <- 1-p.TeeAssam.t1

# new empirical data
p.2ndcup.cond.TeeAssam.t1 <- 0.9
p.2ndcup.cond.NOT.TeeAssam.t1 <- 0.35

p.TeeAssam.cond.2ndcup.t1 <- p.2ndcup.cond.TeeAssam.t1 * p.TeeAssam.t1/(p.2ndcup.cond.TeeAssam.t1 * p.TeeAssam.t1 + p.2ndcup.cond.NOT.TeeAssam.t1 * p.NOT.TeeAssam.t1)
p.TeeAssam.cond.2ndcup.t1

# ratio first time point versus t1
# = ratio posteriors from t0 to t1
((.9*.187)/(.9*.187+.35*.813))/((.70*.1528)/(.70*.1528+.55*.8472))
p.TeeAssam.cond.2ndcup.t1 / p.TeeAssam.cond.2ndcup.t0

