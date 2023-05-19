# R code used in the book "Subjektive Betrachtungen und objektive Ansichten. Qual, Quan und Logik" written by Gürtler & Huber (2023).

## Overview

The book itself is about mixed methodology especially if it comes to data analysis. Although the book is written in German (original language), the R code can be used without any German language skills. The notes and comments in the code are written in English. The code is not organized as a R package and that's not the intention here. However, some functions may be useful for this or that purpose. Comments here and there in the code should help to understand the main aim if the book is neither available or cannot be understood.

## License

The R code is licensed under [GNU GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html). Please feel free to use, modify or share the code if it is helpful for you.

## Background

The book itself is about mixed methodology (quantitative, qualitative, boolean logic) and will be published in 2023 freely on some open library platform.

## Filenames

The files work in the following manner so that most files are directly related to a chapter of the book and a specific task or data set:

*ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r*
*ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r*

This means taking the example filenames above and breaking them down to its parts:

- **pt II** = part II
- **quan** = quantitative
- **Bayes** = Bayesian statistics
- **BayesFactors** = subtopic
- **dependence-on-N-sim** = topic discussed, here the dependence of the Bayes Factor from the sample size N by using simulation to demonstrate this relationship
- **helpfuncs** [optional] = general term used for functions used in the scripts (in most cases the non-helpfuncs.r files do not contain any R functions)
- **ptall_generalfuncs** = collection of functions of general usage and not (just) data set specific

Some rare files do not follow that filename pattern:

- **ADDON_Lindley-Paradox.r** = small addon to the Lindley-Paradox
- **DiM_Bretthorst_PG.r** = implementation of the paper "Difference in means" from GL Bretthorst 1993 (an analytical Bayesian solution to the Behrens-Fisher problem) -> this R script is a conversion of the original Mathematica script written and published by Gregory 2005 ("Bayesian logical data analysis for the physical sciences")
- **DiM_Bretthorst_UMS.r** = the same as above but an implementation of the original Mathematica code written by UM Studer in the 1990's (context: evaluation research study of the Suisse drug therapy center 'start again')
- **ladyfisher_genfunc_bayesT.r**  = solution to the 'Lady tea experiment' (that gave rise to the exact Fisher test) by using the Bayes Theorem
- **model.txt** = Lady Bristol BUGS model to reproduce the exact Fisher test based on the original data from the 'Lady tea experiment' (with fixed margin totals for rows and cols)

## Links in the R code

It is impossible to write any R code without external role models and code taken, borrowed, learned, etc. from other people. To give respect to that fact and to allow to deepen this or that understanding of R or one of the topic of the code, at some selected points the scripts contain URLs to external webpages. We cannot guarantee that those links still exist, because the internet changes too fast. They did when the scripts were written.

## Data sets

Due to legal and licence issues not all data sets discussed in the book can be published here as well. Data sets should be located in the same folder as the script to load the data. The following data sets are included in the following folders below 'data':

- **AAH** = data from a research study about collaborative learning
- **LG** = data from a research study about the usage of word counts and a small experiment about clairvoyance to demonstrate something that does not show any kind of effect
- **school_success** = logical table about school success (fictional data just for demonstration)
- **Spain-edu** = data from a Spanish research study on leadership in education
- **startagain_appl-letter-addiction** = application letter for a place in drug therapy rehabilitation written from detox in psychiatry (originally sent via fax, names and places are fully anonymized)
- **startagain_successrates** = success rates to pass through a drug rehabilitation program in the Suisse drug therapy center *start again* between 1992-2017
- **Titanic_survivors** = well-known data of the sinking of the Titanic and its survivors resp. their characteristics
- **wikipedia_presidential-heights** = data from wikipedia about the relationship of body height of US presidential candidates and later winners (presidents)

Place datasets in the main folder to use them along with the scripts.

## External data sets

Some data sets used are taken directly from R like the **bupa** data set or the one from Annette Dobson (1990) about **plant weight data**, the famous **iris** data set, etc. (see book for further references). Other set like **crime data** are from external sources (e.g. UCLA) and others are not published due to a missing license required for public access (e.g. data about the **chiropractice research study** or about **women in parliament** from ML Krook 2010).

## External R code

Some R code was not taken from R packages but various locations on the net. From that selection some scripts were also tweaked to fit to our needs here. Mostly, those scripts are from:

- **bayesian2beta.r** (from: Sverdlov O, Ryeznik Y, Wu S.2015.  Exact Bayesian Inference Comparing Binomial Proportions, With Application to Proof-of-Concept Clinical Trials. *Therapeutic Innovation & Regulatory Science*, *49*(1), p.163-174.)
- **DBDA2E-utilities.R** and **Jags-Ymet-XmetMulti-Mrobust.R** (from: Kruschke, J. 2014. *Doing Bayesian Data Analysis. 2nd ed.* Academic Press.)

The corresponding *.r file contains the URL where to download the R script. Download and just place them in the main folder. External R code is used in the following way as (parts of) R functions:
- R packages and R functions that are modified to meet our needs here (e.g. some functions went missing from one R version to the next one like code from   [heatmap.plus](https://github.com/cran/heatmap.plus), [sjstats](https://github.com/strengejacke/sjstats), [rhmc](https://github.com/cran/rhmc), ...).
- Some R code (e.g. from [Bolstad](https://github.com/cran/Bolstad)) was slightly changed due to bugs in the original code at that time. That may be different now.
- Then there is a lot of R code found on forums, tutorials, etc. to enhance the graphical output or solve this or that problem.
- Some other code just follows papers and can be seen as an implementation of algorithms (e.g. about p-hacking, z-curves, etc.). This is referenced en detail in the book itself.

From time to time some code is put in sections like

```
### not run
...
### end of not run
```

Such code is optional or sometimes not fully related to the book or just gives another (maybe even redundant) perspective.

## R version

All R scripts were written and tested under R v3. "In theory" they should run with later versions of R as well. However, sometimes packages are not maintained anymore and then they are dropped or something changes so heavily in a package that previous functions either do not exist anymore or at least not in the way they should and are used here. Therefor, one can create a virtual machine, install R v3 and everything should run fine independent from using Linux or Windows. All scripts were developed under Linux, but also tested under win7.

## R libraries

Many scripts require external R packages and external libraries. Sometimes they require especially under Linux some compilation of libraries. Such compilations under Linux should be done directly by running R from the commandline and not via using some GUI like RStudio, because experience shows that the compilation tends to break and fails if such a GUI is used. Compilation directly via R on the commandline works pretty well, and afterwards the GUI can be used again. Under windows, most libraries do not require any compilation. If those compiled packages should be made available to all local users, start R as root and install then packages.

## Disclaimer

Although all R scripts were tested heavily we cannot rule out any possible errors. So we do not guarantee anything but to advice that users should use their common sense whether a result makes sense and is done properly or not. Some R code and examples make only sense in combination with the book, because the few notes in the code are not sufficient. All R scripts are simple and pure text files. Still, usage is always at one's own risk. R code is provided "as is".

## Errors in the R code

If one finds a bug please contact us with a short and clear example so we can try to reproduce it and fix the error. Important is to understand that sometimes due to demonstration goals the code is not perfect and is revised a few lines later. Those incidents are no bugs but intentional for educational purposes to show that things evolve and do not happen accidentially, but statistical work means (ideally!) slow and steady progress towards a goal that sometimes even changes. In general, the R code is there for educational purposes and adjusted to the tasks (e.g. data sets) and the specific topics of the book. The R code does not fulfill the requirements of traditional R packages like some universal library. Nevertheless, some of the functions may be useful in various changing contexts.

## AQUAD 7 files

One data set (application letter for a place in a drug addiction rehabilitation center) is taken from qualitative data analysis along with the windows binary from [AQUAD 7](https://www.aquad.de), a free and open source QDA software. The folder AQUAD7 contains all working files along with the older AQUAD 7 version that was used for analysis. AQUAD is now on v8.

## Citation

If you ever refer to any part of the R code, please cite it as:

Gürtler, Leo (2023). R code supplement for Gürtler & Huber (2023). *Subjektive Betrachtungen und objektive Ansichten. Qual, Quan und Logik.* R code published on [Github](https://github.com/abcnorio/mixedmethod-rcode) and [OSDN](https://osdn.net/projects/mixedmethod-rcode).

