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
# ptIII_qual_code-paradigm_table-analysis.r

# location:
# chap. 9 [9.4]
# Tabellenanalysen nach Miles und Huberman


# G = German kind of schools
kindofschoolG <- c("Hauptschule", "Realschule", "Gymnasium", "Sonderschule")
sex <- c("f","m")
age <- c("[25–35]", "[35–45]", "[45–55]", "[55–67]")
subject <- c("music", "naturalsciences", "language", "math", "religion", "sport", "art-and-craft", "IT", "history-and-politics")
tab <- expand.grid(kindofschoolG=kindofschoolG, sex=sex, age=age, subject=subject)

head(tab)
tail(tab)
dim(tab)
tab


