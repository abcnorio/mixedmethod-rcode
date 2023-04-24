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


