# R code used in the book "Subjektive Betrachtungen und objektive Ansichten. Qual, Quan und Logik" written by Gürtler & Huber (2023).

## Overview

The book itself is about mixed methodology especially if it comes to data analysis. Although the book is written in German (original language), the R code can be used without any German language skills. The notes and comments in the code are written in English. The code is not organized as a R package and that's not the intention here. However, some functions may be useful for this or that purpose. Comments here and there in the code should help to understand the main aim if the book is neither available or cannot be understood.

## Background

The book itself is about mixed methodology (quantitative, qualitative, boolean logic) and will be published in 2023 freely on some open library platform.

## Run the R code

Use a GUI like RStudio, R Commander, JGR, Emacs with ESS, Deducer, Eclipse StatET, RKWard, Rattle or Tinn-R. Use what suits you.

## Filenames

The files work in the following manner so that most files are directly related to a chapter of the book and a specific task or data set:

- `ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r`
- `ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r`

This means taking the example filenames above and breaking them down to its parts, e.g.:

| Filename part | Description |
| --- | --- |
| `pt II` | part II [possible values: `part all`, `I`, `II`, `III`, `IV`, `V`] |
| `quan` | quantitative [possible values: `generalfuncs`, `scientifictheory`, `quantitative`, `qualitative`] |
| `Bayes` | Bayesian statistics [possible values: `classicstats`, `EDA`, `code-paradigm`, `quan-textanalysis`, `Boole`, `mixed`] |
| `BayesFactors` | subtopic [various topics possible] |
| `dependence-on-N-sim` | topic discussed, here the dependence of the Bayes Factor from the sample size N by using simulation to demonstrate this relationship |
| `helpfuncs` [optional] | general term used for functions used in the scripts (in most cases the non-helpfuncs.r files do not contain any R functions) |
| `ptall_generalfuncs` | collection of functions of general usage and not (just) data set specific |

Some rare files do not follow that filename pattern:

| Scriptname | Description |
| --- | --- |
| `ADDON_Lindley-Paradox.r` | small addon to the Lindley-Paradox
| `ADDON_ladyfisher_genfunc_bayesT.r` | solution to the 'Lady tea experiment' (that gave rise to the exact Fisher test) by using the Bayes Theorem
| `DiM_Bretthorst_PG.r` | R implementation of the paper "Difference in means" from GL Bretthorst 1993 (an analytical Bayesian solution to the Behrens-Fisher problem) modelled after Gregory 2005
| `DiM_Bretthorst_UMS.r` | same as above but an implementation ater UM Studer (1998)
| `model.txt` | Lady Bristol BUGS model to reproduce the exact Fisher test based on the original data from the 'Lady tea experiment' (with fixed margin totals for rows and cols)


The following table contains condensed information about each script.

<details>

<summary>Click here to see the table</summary>

| Scriptname | Description |
| --- | --- |
| **all parts** | general functions |
| ptall_generalfuncs.r |  |
| ptall_generalfuncs_Bayes_Beta_determine.r |  |
| ptall_generalfuncs_Bayes_binomial.r |  |
| ptall_generalfuncs_Bayes_binomial-prop-test.r |  |
| ptall_generalfuncs_brob-integral.r |  |
| **Part I** | Scientific Theory |
| ptI_sciencetheory_logic.r |  |
| **Part II** | Classical Statistics |
| | **Central limit theorem** |
| ptII_quan_classicstats_centrallimittheorem.r |  |
| ptII_quan_classicstats_centrallimittheorem_helpfuncs.r |  |
| | **Power** |
| ptII_quan_classicstats_dealingwithpower.r |  |
| | **Effect sizes** |
| ptII_quan_classicstats_effectsizes.r |  |
| ptII_quan_classicstats_effectsizes_helpfuncs.r |  |
| ptII_quan_classicstats_effectsizes_helpfuncs_sjstats.r |  |
| | **Never one method to rule them all** |
| ptII_quan_classicstats_equivalentmethods.r |  |
| ptII_quan_classicstats_equivalentmethods_helpfuncs.r |  |
| | **Fisher** |
| ptII_quan_classicstats_Fisher_ladyteataste.r |  |
| | **type S and M error** |
| ptII_quan_classicstats_GandC_type-S-M-error.r |  |
| ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r |  |
| | **Jefferys-Lindley Paradox** |
| ptII_quan_classicstats_JeffreysLindleyparadox.r |  |
| | **Missing Data (NA)** |
| ptII_quan_classicstats_missingdata.r |  |
| | **Distributions** |
| ptII_quan_classicstats_normaldist_residuals.r |  |
| ptII_quan_classicstats_normal-vs-t.r |  |
| | **Neyman-Pearson Confidence Intervals** |
| ptII_quan_classicstats_N-P_confint.r |  |
| ptII_quan_classicstats_N-P_confint_bayesboot.r |  |
| ptII_quan_classicstats_N-P_confint_errorbars.r |  |
| ptII_quan_classicstats_N-P_confint_helpfuncs.r |  |
| ptII_quan_classicstats_N-P_confint_p-t-value.r |  |
| ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r |  |
| ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r |  |
| | **Simulating data (empirical example)** |
| ptII_quan_classicstats_N-P_example-soccer-sim.r |  |
| ptII_quan_classicstats_N-P_example-soccer-sim_helpfuncs.r |  |
| | **Null Hypothesis Significance Testing** |
**| ptII_quan_classicstats_NHST_nulldist.r |  |
| ptII_quan_classicstats_N-P_nulldist-hypotest.r |  |
| ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r |  |
**| ptII_quan_classicstats_nullritual.r |  |
| | **Neyman-Pearson** |
| ptII_quan_classicstats_N-P_powerfunc.r |  |
| ptII_quan_classicstats_N-P_SE-N-dep.r |  |
| ptII_quan_classicstats_N-P_simulation.r |  |
| ptII_quan_classicstats_N-P_stat-signif-isNOT-practsignif.r |  |
| | **Outliers** |
| ptII_quan_classicstats_outliers-and-influentialpoints.r |  |
| ptII_quan_classicstats_outliers-and-influentialpoints_helpfuncs.r |  |
| | **p-values and p-hacking** |
| ptII_quan_classicstats_p-hacking-sim.r |  |
| ptII_quan_classicstats_p-hacking-sim_helpfuncs.r |  |
| ptII_quan_classicstats_p-t-df-relationship.r |  |
| ptII_quan_classicstats_pvalue-as-base.r |  |
| | **Randomization** |
| ptII_quan_classicstats_randomization.r |  |
| | **R-Index and z-curves** |
| ptII_quan_classicstats_R-index_z-curve.r |  |
| ptII_quan_classicstats_R-index_z-curve_helpfuncs.r |  |
| | **Simpson paradox** |
| ptII_quan_classicstats_Simpsonparadox.r |  |
| ptII_quan_classicstats_Simpsonparadox_helpfuncs.r |  |
| | **Variances** |
| ptII_quan_classicstats_varianceestimation.r |  |
| ptII_quan_classicstats_varianceestimation_helpfuncs.r |  |
| ptII_quan_classicstats_variancehomogeneity.r |  |
| | |
| **Part II** | Exploratory Data Analysis (EDA) |
| | **Basics and robust methods** |
| ptII_quan_EDA_intro_overviewrobust.r |  |
| ptII_quan_EDA_intro_overviewrobust_helpfuncs.r |  |
| | **Empirical examples** |
| ptII_quan_EDA_case_Anderson_iris-species-in-biology.r |  |
| ptII_quan_EDA_case_Chiro_heartrate-variability.r |  |
| ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r |  |
| ptII_quan_EDA_case_German-states-population.r |  |
| ptII_quan_EDA_case_Spain_leadership-in-education.r |  |
| ptII_quan_EDA_case_Spain_leadership-in-education_helpfuncs.r |  |
| ptII_quan_EDA_case_Suisse-fertility.r |  |
| ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r |  |
| ptII_quan_EDA_case_Titanic_death-and-dying.r |  |
| ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r |  |
| | **Simulating pi** |
| ptII_quan_simulate-pi.r |  |
| ptII_quan_simulate-pi_helpfuncs.r |  |
| **Part II** | Bayesian Statistics |
| | **Bayes Factors** |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r |  |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r |  |
| ptII_quan_Bayes_BayesFactors_further-remarks.r |  |
| ptII_quan_Bayes_BayesFactors_test-hypos.r |  |
| | **Reflections on the Be studym** |
| ptII_quan_Bayes_Bem-study-aspects.r |  |
| | **Beta distribution** |
| ptII_quan_Bayes_Beta-distribution.r |  |
**| ptII_quan_Bayes_find-Beta-distribution-shapeparams.r |  |
| | **Empirical examples** |
| ptII_quan_Bayes_case_exp-extra-sensual-perception.r |  |
| ptII_quan_Bayes_case_presidential-debates_UPDATE.r |  |
| ptII_quan_Bayes_case_presidential-heights.r |  |
| ptII_quan_Bayes_case_presidential-heights_helpfuncs.r |  |
| ptII_quan_Bayes_case_startagain-successrates.r |  |
| ptII_quan_Bayes_case_startagain-successrates-longterm.r |  |
| ptII_quan_Bayes_case_wordcounts-PPC.r |  |
| ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r |  |
| | **Entropy** |
| ptII_quan_Bayes_Entropy_KullbackLeibler.r |  |
| ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r |  |
| | **Lady Bristol tea experiment** |
| ptII_quan_Bayes_Fisher_LadyBristol-Beta-disttribution.r |  |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS.r |  |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r |  |
| | **Gamma Distribution** |
| ptII_quan_Bayes_Gamma-distribution.r |  |
| | **Markov Chain Monte Carlo (MCMC) examples** |
**| ptII_quan_Bayes_MC-simulation_binom-norm.r |  |
**| ptII_quan_Bayes_MetropolisHastings_example-normdist.r |  |
**| ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r |  |
| ptII_quan_Bayes_GibbsSampling_example-normdist.r |  |
**| ptII_quan_Bayes_JAGS_example-norm.r |  |
**| ptII_quan_Bayes_HMC.r |  |
**| ptII_quan_Bayes_HMC_helpfuncs.r |  |
| | **Highest Density Interval (HDI)** |
| ptII_quan_Bayes_HDI.r |  |
| ptII_quan_Bayes_HDI_helpfuncs.r |  |
| | **Information criteria** |
| ptII_quan_Bayes_information-criteria.r |  |
| ptII_quan_Bayes_information-criteria_helpfuncs.r |  |
| | **Bayes Theorem (general)** |
| ptII_quan_Bayes_intro-BayesTheorem_covid19-test.r |  |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r |  |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r |  |
| ptII_quan_Bayes_intro-BayesTheorem_tea.r |  |
| | **Loss functions** |
| ptII_quan_Bayes_lossfun_startagain.r |  |
| | **Maximum Entropy** |
| ptII_quan_Bayes_MaximumEntropy.r |  |
| ptII_quan_Bayes_MaximumEntropy_helpfuncs.r |  |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice.r |  |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r |  |
| | **Over- and underfitting** |
| ptII_quan_Bayes_over-and-underfitting.r |  |
| | **posterior** |
| ptII_quan_Bayes_posterior.r |  |
| | **Posterior Predictive Checks** |
| ptII_quan_Bayes_PPC_model-check.r |  |
| ptII_quan_Bayes_PPC_model-check_helpfuncs.r |  |
| ptII_quan_Bayes_PPC_model-check-graph.r |  |
| | **Relationship of prior, likelihood, and posterior** |
| ptII_quan_Bayes_Prior-Likeli-Post_relationship.r |  |
| | **Local minima** |
| ptII_quan_Bayes_problem-local-minima.r |  |
| | **Random Walk** |
| ptII_quan_Bayes_RandomWalk.r |  |
| ptII_quan_Bayes_RandomWalk_helpfuncs.r |  |
| | **Regularization** |
| ptII_quan_Bayes_regularization.r |  |
| | **Region of practical equivalence (ROPE)** |
| ptII_quan_Bayes_ROPE-BayesFactor.r |  |
| ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r |  |
| | **Estimation mean** |
| ptII_quan_Bayes_simple-estimation-mean-post.r |  |
| ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r |  |
| | **Simulating pi** |
| ptII_quan_Bayes_simulate-pi.r |  |
| ptII_quan_Bayes_simulate-pi_helpfuncs.r |  |
| **Part III** | Qualitative Data Analysis |
| | **Text Analysis and coding paradigm** |
| ptIII_qual_code-paradigm_table-analysis.r |  |
| ptIII_qual_quan-textanalysis.r |  |
| **Part IV** | Mixed Methods |
| | **Boolean algebra and Qualitative Comparative Analysis (QCA)** |
| ptIV_qual_Boole_basics.r |  |
| ptIV_qual_Boole_case_Krook_women-in-parliament.r |  |
| ptIV_qual_Boole_case_school-success.r |  |
| ptIV_qual_Boole_case_Titanic_death-and-dying.r |  |
| ptIV_qual_Boole_fuzzy-logic.r |  |
| ptIV_qual_Boole_logical-minimization.r |  |
| | **Something different** |
| ptV_mixed_prime-numbers.r |  |

</details>

## Links in the R code

It is impossible to write any R code without external role models and code taken, borrowed, learned, etc. from other people. To give respect to that fact and to allow to deepen this or that understanding of R or one of the topic of the code, at some selected points the scripts contain URLs to external webpages. We cannot guarantee that those links still exist, because the internet changes too fast. They did when the scripts were written.

## Data sets

Due to legal and licence issues not all data sets discussed in the book can be published here as well. Data sets should be located in the same folder as the script to load the data. The following data sets are included in the following folders below 'data':

| Dataset | Description |
| --- | --- |
| `AAH` | data from a research study about collaborative learning |
| `LG` | data from a research study about the usage of word counts and a small experiment about clairvoyance to demonstrate something that does not show any kind of effect |
| `school_success` | logical table about school success (fictional data just for demonstration) |
| `Spain-edu` | data from a Spanish research study on leadership in education |
| `startagain_appl-letter-addiction` | application letter for a place in drug therapy rehabilitation written from detox in psychiatry (originally sent via fax, names and places are fully anonymized) |
| `startagain_successrates` | success rates to pass through a drug rehabilitation program in the Suisse drug therapy center *start again* between 1992-2017 |
| `Titanic_survivors` | well-known data of the sinking of the Titanic and its survivors resp. their characteristics |
| `wikipedia_presidential-heights` | data from wikipedia about the relationship of body height of US presidential candidates and later winners (presidents) |

Place datasets in the main folder to use them along with the scripts.

## External data sets

Some data sets used are taken directly from R like the **bupa** data set or the one from Annette Dobson (1990) about **plant weight data**, the famous **iris** data set, etc. (see book for further references). Other set like **crime data** are from external sources (e.g. UCLA) and others are not published due to a missing license required for public access (e.g. data about the **chiropractice research study** or about **women in parliament** from ML Krook 2010).

## External R code

Some R code was not taken from R packages but various locations on the net. From that selection some scripts were also tweaked to fit to our needs here. Mostly, those scripts are from:
- `bayesian2beta.r` (from: Sverdlov O, Ryeznik Y, Wu S.2015.  Exact Bayesian Inference Comparing Binomial Proportions, With Application to Proof-of-Concept Clinical Trials. *Therapeutic Innovation & Regulatory Science*, *49*(1), p.163-174.)
- `DBDA2E-utilities.R` and `Jags-Ymet-XmetMulti-Mrobust.R` (from: Kruschke, J. 2014. *Doing Bayesian Data Analysis. 2nd ed.* Academic Press.)

The corresponding `*.r` file contains the URL where to download the R script. Download and just place them in the main folder. External R code is used in the following way as (parts of) R functions:
- R packages and R functions that are modified to meet our needs here (e.g. some functions went missing from one R version to the next one like code from   [heatmap.plus](https://github.com/cran/heatmap.plus), [sjstats](https://github.com/strengejacke/sjstats), [rhmc](https://github.com/cran/rhmc), ...).
- Some R code (e.g. from [Bolstad](https://github.com/cran/Bolstad)) was slightly changed due to bugs in the original code at that time. That may be different now.
- Some other code just follows papers and can be seen as an implementation of algorithms (e.g. about p-hacking, z-curves, etc.). This is referenced en detail in the book itself.

From time to time some code is put in sections like

```
### not run
...
### end of not run
```

Such R code is optional or sometimes not fully related to the book or just gives another (maybe even redundant) perspective.

## R version

All R scripts were written and tested under R v3. "In theory" they should run with later versions of R as well. However, sometimes packages are not maintained anymore and then they are dropped or something changes so heavily in a package that previous functions either do not exist anymore or at least not in the way they should and are used here. Therefor, one can create a virtual machine, install R v3 and everything should run fine independent from using Linux or Windows. All scripts were developed under Linux, but also tested under win7.

## R libraries

Many scripts require external R packages and external libraries. Sometimes they require especially under Linux some compilation of libraries. Such compilations under Linux should be done directly by running R from the commandline and not via using some GUI like RStudio, because experience shows that the compilation tends to break and fails if such a GUI is used. Compilation directly via R on the commandline works pretty well, and afterwards the GUI can be used again. Under windows, most libraries do not require any compilation. If those compiled packages should be made available to all local users, start R as root and install then packages.

## Errors in the R code

If one finds a bug please contact us with a short and clear example so we can try to reproduce it and fix the error.

## Usage of the R code

Important is to understand that sometimes due to demonstration goals the code is not perfect and is revised a few lines later. Those incidents are no bugs but intentional for educational purposes to show that things evolve and do not happen accidentially, but statistical work means (ideally!) slow and steady progress towards a goal that sometimes even changes. In general, the R code is adjusted to the tasks (e.g. data sets) and the specific topics of the book. It's not an R package - the R code does not fulfill the requirements of traditional R packages like being useful as a universal library. Nevertheless, some of the functions may be helpful in various changing contexts and can be used independently from the book, the datasets, etc.

## AQUAD 7 files

One data set (application letter for a place in a drug addiction rehabilitation center) is taken from qualitative data analysis along with the windows binary from [AQUAD 7](https://www.aquad.de), a free and open source QDA software. The folder AQUAD7 contains all working files along with the older AQUAD 7 version that was used for analysis. AQUAD is now on v8.

## Licenses and Credits

The R code is licensed under [GNU GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html). Please feel free to use, modify or share the code if it is helpful for you.
The functions taken from other packages have their own license. Details can be found in the license file.
The AQUAD7 binary `aquad7_170117.exe` has a (C) by GL Huber (2017). The more resent version of AQUAD8 can be found freely on the [AQUAD 8](https://www.aqua.de) webpage.

## References


## Citation

If you ever refer to any part of the R code, please cite it as:

Gürtler, Leo (2023). R code supplement for Gürtler & Huber (2023). *Subjektive Betrachtungen und objektive Ansichten. Qual, Quan und Logik.* R code published on [Github](https://github.com/abcnorio/mixedmethod-rcode) and [OSDN](https://osdn.net/projects/mixedmethod-rcode).

## Disclaimer

Although all R scripts were tested heavily we cannot rule out any possible errors. So we do not guarantee anything but to advice that users should use their intelligence and experience whether a result makes sense and is done properly or not. Some R code and examples make only sense in combination with the book, because the few notes in the code are not sufficient. However, it is provided "as is".
Use common sense to compare results with expectations. NO WARRANTY of any kind is involved here. There is no guarantee that the software is free of error or consistent with any standards or even meets your requirements. Do not use the software or rely on it to solve problems if incorrect results may lead to hurting or injurying living beings of any kind or if it can lead to loss of property or any other possible damage to the world. If you use the software in such a manner, you are on your own and it is your own risk.