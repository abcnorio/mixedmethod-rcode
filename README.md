# R code used in the book "Subjektive Betrachtungen und objektive Ansichten. Qual, Quan und Logik" written by Gürtler & Huber (2023).

## Languages

A Spanish version of this README.md can be found at XXX.

## Overview

The book's main topic covers mixed methodology especially if it comes to data analysis. Although the book is written in German (original language) and translated to Spanish, the R code can be used without any German language skills. The notes and comments in the scripts are written in English. The code itself is not organized as an R package and that's not the intention here. However, some functions may be useful for this or that purpose. Comments here and there in the scripts should help to understand the main aim if the book is neither available or cannot be understood.

## Background

The book deals with mixed methodology (quantitative, qualitative, and Boolean logic) and will be published in 2023 freely on some open library platform. The statistical part covers classical statistics (Fisher, Neyman-Pearson), exploratory data analysis sensu JW Tukey, as well as Bayesian statistics. The Boolean logic is used for implicant analysis of qualitative comparative analysis. The qualitative part covers quantitative as well as qualitative textanalysis. The latter uses coding paradigm and sequential analysis originating from Objective Hermeneutics sensu Oevermann and colleagues.

## Run the R code

Either use a GUI like RStudio, R-Commander, JGR, Emacs with ESS, Deducer, Eclipse StatET, RKWard, Rattle or Tinn-R. Use what suits you.

## Filenames

The files work in the following manner so that most files are directly related to a chapter of the book and a specific task or data set (see table below).

- `ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r`
- `ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r`

This means we take the example filenames above and break them down to their parts, e.g.:

| Filename part | Description |
| --- | --- |
| `pt II` | part II [possible values: `part all`, `I`, `II`, `III`, `IV`, `V`] |
| `quan` | quantitative [possible values: `generalfuncs`, `scientifictheory`, `quantitative`, `qualitative`] |
| `Bayes` | Bayesian statistics [possible values: `classicstats`, `EDA`, `code-paradigm`, `quan-textanalysis`, `Boole`, `mixed`] |
| `BayesFactors` | subtopic [various topics possible...] |
| `dependence-on-N-sim` | topic discussed, here the dependence of the Bayes Factor from the sample size N by using simulation to demonstrate such a relationship |
| `helpfuncs` [optional] | general term used for functions used in the scripts (in most cases the non-helpfuncs.r files do not contain R functions) |
| `ptall_generalfuncs` | collection of functions of general usage which are not (just) data set specific |

Some rare files do not follow that filename pattern:

| Scriptname | Description |
| --- | --- |
| `ADDON_Lindley-Paradox.r` | small addon to the Lindley-Paradox |
| `ADDON_ladyfisher_genfunc_bayesT.r` | solution to the 'Lady tea experiment' (that gave rise to the exact Fisher test) by using the Bayes Theorem |
| `DiM_Bretthorst_PG.r` | R implementation of the paper "Difference in means" from GL Bretthorst 1993 (an analytical Bayesian solution to the Behrens-Fisher problem) modelled after Gregory 2005 |
| `DiM_Bretthorst_UMS.r` | same as above but an implementation ater UM Studer (1998) |
| `model.txt` | Lady Bristol BUGS model to reproduce the exact Fisher test based on the original data from the 'Lady tea experiment' (with fixed margin totals for rows and cols) |

For the Bretthorst approach there is a dedicated repo at https://github.com/abcnorio/diffinmeans which updated and enhanced code.

To facilitate the relationship of R script file and book chapter, the following table contains the match of chapter and R script. It is ordered in accordance to book chapters.

<details>

<summary>Click here to see the table with a the matching of book chapter and R script filename</summary>

| Book chapter | Chapter title | R script filename |
| --- | --- | --- |
| | | |

</details>

The following table contains condensed information about each script using some keywords. The files are clustered in accordance ot the topic and do not necessarily represent the order of appearance in the book.

<details>

<summary>Click here to see the table with a short description of the R code scripts</summary>

| Scriptname | Description |
| --- | --- |
| **all parts** | general functions |
| ptall_generalfuncs.r | Cohen's d, descriptive statistics of all kinds in one table, Tukey's fivenum with labels, convert Aquad style tables to truth tables and v.v., print prime implicants from QCA objects, full distance matrix, optimal cut through a proximity matrix after Oldenbürger and plot it, plot prototypes in 2d + 3d, plot eigenvalues (MDS), correlation and p-values |
XXX CHECK WITH SOME SCRIPT BELOW| ptall_generalfuncs_Bayes_Beta_determine.r | determine and plot beta distribution values from three quantile points via optimization |
| ptall_generalfuncs_Bayes_binomial.r | calculate, summarize, and plot prior from successes/ failures, convert to beta disztribution values and v.v., update binomial prior with likelihood to beta (posterior) and plot, tweaked bayes.prop.test summary from BayesianFirstAid and plot theta_diff|
| ptall_generalfuncs_Bayes_binomial-prop-test.r | Bayesian proportion test: summary, MCMC plot, simulation from posterior, grid approximation via brute force, exact (binomial difference) tests (Evan Miller, Chris Stucchio, the approach by Sverdlov, Ryeznik, Wu (2015) from bayesian2beta.r in a tweaked form to work with log values or brob objects and plot results, numerical integration (Simpson rule) for brob objects, brute force comparison of rbetas vs. dbetas and plot it |
| ptall_generalfuncs_brob-integral.r | useful functions for brob objects, convert a list to a vector, calculate the scalarproduct, numerical integration (Simpson rule), more numerical integration methods for brob objects can be found at https://github.com/abcnorio/R-largenum-integration |
| **Part I** | Scientific Theory |
| ptI_sciencetheory_logic.r | simple true/ false statements |
| **Part II** | Classical Statistics |
| | **Central limit theorem** |
| ptII_quan_classicstats_centrallimittheorem.r | simulate central limit theorem (CLT) und varying conditions |
| ptII_quan_classicstats_centrallimittheorem_helpfuncs.r | basic functions to simulate CLT |
| | **Power** |
| ptII_quan_classicstats_dealingwithpower.r | calculate, simulate, and plot power under varying models |
| | **Effect sizes** |
| ptII_quan_classicstats_effectsizes.r | relate N, p-value, Cohens' d, odds ratio, and risk ratio with each other graphically |
| ptII_quan_classicstats_effectsizes_helpfuncs.r | function to relate N, p-value, and Cohen's d |
| ptII_quan_classicstats_effectsizes_helpfuncs_sjstats.r | taken and twekaed from package sjtats as it disappeared from the package, convert odds ratio to risk ratio |
| | **Never one method to rule them all** |
| ptII_quan_classicstats_equivalentmethods.r | comparison with Bland-Altman stats and plot (mean-diff plot), TOST from package TOSTER, Pitman-Morgan Test, tests from packages BayesFactor, BEST, usage of implementation of GL Bretthorst (1993) 'On the difference in mean', using e.g. original data from Bland-Altman |
| ptII_quan_classicstats_equivalentmethods_helpfuncs.r | mean-difference plot after Tukey (equals Bland-Altman plot) |
| | **Fisher** |
| ptII_quan_classicstats_Fisher_ladyteataste.r | Lady Bristol tea taste experiment by Fisher using his methods using hypergeometric distribution, fisher.test, and manual calculation using factorials |
| | **type S and M error** |
| ptII_quan_classicstats_GandC_type-S-M-error.r | reproduce Gelman & Carlin (2014), retrodesign power analysis |
| ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r | plot effect size and power for retrodesign after Gelman & Carlin (2014), plot sample distribution under H_0 and hypothesis of true effect size |
| | **Jefferys-Lindley Paradox** |
| ptII_quan_classicstats_JeffreysLindleyparadox.r | Jeffrey's-Lindley's paradox with data examples |
| | **Missing Data (NA)** |
| ptII_quan_classicstats_missingdata.r | handling missing data and associated changes with and without imputation methods, comparison of methods of imputation with each other and with brms (Bayesian linear model) |
| | **Distributions** |
| ptII_quan_classicstats_normal-vs-t.r | relationship of normal and t distribution |
| ptII_quan_classicstats_normaldist_residuals.r | investigation of residuals from linear models for various characteristics (normality, skewness, kurtosis, various sample sizes |
| | **Neyman-Pearson Confidence Intervals** |
| ptII_quan_classicstats_N-P_confint.r | working and simulating confidence intervals (CI) |
| ptII_quan_classicstats_N-P_confint_helpfuncs.r | calculate CI for mean, difference in means, and plot them |
| ptII_quan_classicstats_N-P_confint_bayesboot.r | bootstrap CIs and other statistics via simulation, simulate difference in means, use Bayesian bootstrap |
| ptII_quan_classicstats_N-P_confint_errorbars.r | CI evolution in relation to varying sample sizes |
| ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r | change of CIs along with error bars, calculate covered CIs |
| ptII_quan_classicstats_N-P_confint_p-t-value.r | simulate t-test with p- and t-values |
| ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r | simulate t-tests, compare with Cohen's d |
| | **Simulating data (empirical example)** |
| ptII_quan_classicstats_N-P_example-soccer-sim.r | simulate an empirical real run of how to get all cards of a soccer card album |
| ptII_quan_classicstats_N-P_example-soccer-sim_helpfuncs.r | simulate how many cards are still to go using chance and not commone sense like card exchange, etc. |
| | **Null Hypothesis Significance Testing** |
**| ptII_quan_classicstats_NHST_nulldist.r | plot NHST decision making using acceptance/ rejection regions, alpha and beta error rates |
| ptII_quan_classicstats_N-P_nulldist-hypotest.r | plot H_0, H_1 for one- and two-sided tests |
| ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r | functions to plot H_0, H_1 for hypothesis directions: two-sided, less, greater, and plot alpha and beta error rates for t-distributed densities |
**| ptII_quan_classicstats_nullritual.r | all the non-sense around the Null ritual ie. the NHST procedure |
| | **Neyman-Pearson** |
| ptII_quan_classicstats_N-P_powerfunc.r | calculate and plot power vs. effect sizes in accordance to different hypotheses |
| ptII_quan_classicstats_N-P_SE-N-dep.r | dependence of sample size characteristics via simulated data (N, SE, t-value, Cohen's d, ...) |
| ptII_quan_classicstats_N-P_simulation.r | simulation 'difference in means' via replicate |
| ptII_quan_classicstats_N-P_stat-signif-isNOT-practsignif.r | different views on sample differences if population parameters are known |
| | **Outliers** |
| ptII_quan_classicstats_outliers-and-influentialpoints.r | impact and consequences of influential points and outliers, calculate linear model, t-test, outlier test, correlation test, leverage plots |
| ptII_quan_classicstats_outliers-and-influentialpoints_helpfuncs.r | function to plot outlier (with, without) via regression lines |
| | **p-values and p-hacking** |
| ptII_quan_classicstats_p-hacking-sim.r | simulate p-hacking with varying sample sizes for standard alpha error rates, comparison with a priori power analysis, and plot results |
| ptII_quan_classicstats_p-hacking-sim_helpfuncs.r | function to simulate linear model with sim from package arm as well as p-hacking methods |
| ptII_quan_classicstats_p-t-df-relationship.r | relationship of N, t-, and p-value, simulate with same seed, growing sample sizes, constant t-value, and post-hoc power simulation |
| ptII_quan_classicstats_pvalue-as-base.r | differences of normal vs. mixed normal distribution in accordance to plot and statistics, simulation H_0 |
| | **Randomization** |
| ptII_quan_classicstats_randomization.r | apply different randomization strategies to create samples |
| | **R-Index and z-curves** |
| ptII_quan_classicstats_R-index_z-curve.r | apply R-Index and TIVA after Schimmack and colleagues, e.g. on Bem's clairvoyance study and calculate z-curve |
| ptII_quan_classicstats_R-index_z-curve_helpfuncs.r | function to calculate R-Index, p-values for R-Index, Test of Insufficient Variance (TIVA) after Schimmack and colleagues |
| | **Simpson paradox** |
| ptII_quan_classicstats_Simpsonparadox.r | investigate Simpson Paradox with classical UCB admission data set using plots, tables, corrected base rates, and glmer model, simulate Simpson Paradox data and detect sub-groups with Simpsons from package Simpsons |
| ptII_quan_classicstats_Simpsonparadox_helpfuncs.r | function to plot two groups to detect Simpson Paradox |
| | **Variances** |
| ptII_quan_classicstats_varianceestimation.r | apply function to calculate variance estimation |
| ptII_quan_classicstats_varianceestimation_helpfuncs.r | function to calculate variance estimation if sigma is known or unknown and plot it |
| ptII_quan_classicstats_variancehomogeneity.r | determine variance homogeneity via various tests (Levene, Breusch-Pagan, non-constant error variance, Cook and Weisberg, Koenker's studentized version of test statistic) |
| | |
| **Part II** | Exploratory Data Analysis (EDA) sensu JW Tukey |
| | **Basics and robust methods** |
| ptII_quan_EDA_intro_overviewrobust.r | compare median vs. mean, show robust plots of data, apply lm vs. rlm on empirical data |
| ptII_quan_EDA_intro_overviewrobust_helpfuncs.r | function to plot residuals (lm vs. rlm), simulate and plot median/ mean from normal distribution |
| | **Empirical examples** |
| ptII_quan_EDA_case_Anderson_iris-species-in-biology.r | investigate the famous iris data with pairs, lda, and tables |
| ptII_quan_EDA_case_Chiro_heartrate-variability.r | experiment in chiropractice about heart rate variability using linear models without any significance test, learning from sample size characteristics, use histograms and interaction plots |
| ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r | function to plot interactions |
| ptII_quan_EDA_case_German-states-population.r | conversion to log values lead to straight lines using a real life example from German states population characteristics, compare to classical linear model |
| ptII_quan_EDA_case_Spain_leadership-in-education.r | analyze data on leadership in Spain (educational context) with distance methods (HCA, MDS), prototype analysis, heatmap, corrgram, levelplot, and simple descriptive plots |
| ptII_quan_EDA_case_Spain_leadership-in-education_helpfuncs.r | tweaked version of heatmap from package heatmap.plus (not available in R v.4) |
| ptII_quan_EDA_case_Suisse-fertility.r | analyzing fertility in Swuisse between Catholics (yes, no, something else) using only descriptive plots and correlations, use subgroups to understand data |
| ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r | function to plot two variables with a continuous index |
| ptII_quan_EDA_case_Titanic_death-and-dying.r | use only descriptive statistics, tables, and plots to investigate the survivors' characteristics of the Titanic and how to survive and why, answer certain questions about the conditions of surviving or dying during the Titanic catastrophe |
| ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r | function to plot densities for various subgroups |
| | **Simulating pi** |
| ptII_quan_simulate-pi.r | apply pi simulation for varying sample sizes |
| ptII_quan_simulate-pi_helpfuncs.r | function to simulate pi (highly inefficient, but works) |
| **Part II** | Bayesian Statistics |
| | **Bayes Factors** |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r | compare and simulate Bayes Factors (BFs) for various sample sizes, t-values, determine minimal BF with different approaches |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r | function to simulate BFs in relation to N, convert p-values to BFs, relate p-values and sample sizes |
| ptII_quan_Bayes_BayesFactors_further-remarks.r | apply BFs in the context of EDA plots, t-tests, Cohen's d, linear models, and a real full Bayesian approach with brms, and other criteria (Bayes R^2, loo, waic, kfold), different results if a categorial variable is made continuous (ie. converted back to its natural original state like categorial age) |
| ptII_quan_Bayes_BayesFactors_test-hypos.r | empirical example from Kruschke's remarks (BFs are not enough to interpret results), better is to use priors and posterior probs, example: huge BF but roughly same parameters of the linear model, BF only caused by different priors |
| | **Reflections on the Be studym** |
| ptII_quan_Bayes_Bem-study-aspects.r | some artificial but meaningful calculations along with a very critical discussion of the theoretical background and design of the Bem study on clairvoyanca |
| | **Beta distribution** |
| ptII_quan_Bayes_Beta-distribution.r | calculate beta posterior from prior and likelihood (conjugation, grid approximation), influence of priors, various shapes of beta |
XXXCHECK WITH FIRST SCRIPT AT THE TOP????*| ptII_quan_Bayes_find-Beta-distribution-shapeparams.r | determine beta distribution from three quantile points with/ without optimization |
| | **Empirical examples** |
| ptII_quan_Bayes_case_exp-extra-sensual-perception.r | function to calculate t-test with BF, then prepare, describe, analyze empirical data using methods for binomial data and difference in means (classical and Bayesian solution), anova with repeated measurement vs. brms |
XXX CHECK FILENAME IN BOOK| ptII_quan_Bayes_case_presidential-debates_UPDATE.r | frequentist, chi-square, power, JAGS, posterior odds, plots, MCMC brute force variants, test and plot hypotheses, grid approximation, exact tests, MAP, 2d + 3d plots of theta values over intgral, comparison of methods, using tweaked functions from appell, tolerance, bayesian2beta.r, comparison with package BayesFactor and Bretthorst approach|
| ptII_quan_Bayes_case_presidential-heights.r | prepare dataset, remove NAs, EDA plots, classical vs. Bayesian statistics with Bayesian binomial test, MCMC diagnostics, test difference in means via classical and Bayesian solution incl. HDI, ROPE, bayesboot, plot results |
| ptII_quan_Bayes_case_presidential-heights_helpfuncs.r | tweaked function of bayes.binom.test from package BayesianFirstAid, function to calculate posterior OR/RR |
| ptII_quan_Bayes_case_startagain-successrates.r | compare developments and extreme cases of successes/ failures and resulting posterior probs |
| ptII_quan_Bayes_case_startagain-successrates-longterm.r | show that long-term analysis over all years vs. sequential analysis per year (prior becomes posterior becomes prior, etc.) leads exactly to the same concluding results within the Bayesian approach |
XXX CHECK PPC TERM | ptII_quan_Bayes_case_wordcounts-PPC.r | perform analysis (e.g. with JAGS) and plots along with PPC incl. diagnostic MCMC plots, bootstrap using full cases or real values, exercise Kruschke (2014, chap 18.3) regarding heteroscedasticity (influence of un-/equal variances between groups) |
| ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r | functions to prepare and summarize MCMC chains and posteriors from Kruschke (2014) scripts to match requirements here for PPC |
| | **Entropy** |
| ptII_quan_Bayes_Entropy_KullbackLeibler.r | apply entropy functions on word counts |
| ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r | functions to calculate H (Shannon entropy) from counts and priors or Kullback-Leibler distance|
| | **Lady Bristol tea experiment** |
| ptII_quan_Bayes_Fisher_LadyBristol-Beta-disttribution.r | plot prior, likelihood, and posterior of the empirical data |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS.r | analyze Lady Bristol's tea data with classical and Bayesian methods (analytical solution and MCMC with JAGS, package BEST, BUGS) |
 ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r | function to analyze Lady Bristol experiment with Bayes Theorem, plot successes/ failures, HDI, MAP, and run BUGs model with package RBugs from R |
| | **Gamma Distribution** |
| ptII_quan_Bayes_Gamma-distribution.r | display Gamma function for different parameters |
| | **Markov Chain Monte Carlo (MCMC) examples** |
**| ptII_quan_Bayes_MC-simulation_binom-norm.r | simulate binormial and normal distributions |
**| ptII_quan_Bayes_MetropolisHastings_example-normdist.r | simulate mean of normal distribution with Metropolis-Hastings (MH) algorithm, investigate MCMC chains using package coda, apply on empirical data, investigate acceptance/ rejection rates, compare MCMC chains, posterior predictive distribution and plots |
| ptII_quan_Bayes_GibbsSampling_example-normdist.r | simulate mean posterior via Gibbs sampling, analyze posterior |
**| ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r | functions to perform MH algorithm to simulate normal distribution based on empirical data, priors, Gibbs sampler, plot MCMC and its parts, normal posterior predictive distribution |
**| ptII_quan_Bayes_JAGS_example-norm.r | simulate mean of a normal distribution using JAGS, investigate MCMC, compare with package Bolstad |
**| ptII_quan_Bayes_HMC.r | apply Hamilton Monte Carlo (HMC) algorithm, plot and investigate (bivariate normal distribution), use packages hmclearn, rethinking, and bayesplot as well as the basic HMC algorithm (Neal, 2011, chap. 5), analyze MCMC chains via plots and typical diagnostics (e.g. Gelman, Heidelberger-Welch, effective sample size, remove burn-in, ... ), compare with MH algorithm |
CONTAINS FUNCS FROM ABOVE**| ptII_quan_Bayes_HMC_helpfuncs.r | function to simulate bivariate normal distribution (rnorm, mvrnorm), calculate gradient for HMC algorithm, simulate bivariate normal distribution via HMC after McElreath (2015), describe MCMC chains: Heidelberger-Welche diagnostics, describe development of mean and covariance, adjust limits to let a comparison value appear on a posterior plot, and simulate bivariate normal distribution via MH algorithm |
| | **Highest Density Interval (HDI)** |
| ptII_quan_Bayes_HDI.r | plot HDI and CI for various data sets to demonstrate the qualitative difference of those interval types |
| ptII_quan_Bayes_HDI_helpfuncs.r | function to plot HDI vs. symmetric CI |
| | **Information criteria** |
| ptII_quan_Bayes_information-criteria.r | apply function to get information criteria from a linear model (empirical example) |
| ptII_quan_Bayes_information-criteria_helpfuncs.r | function to calculate and extract information criteria from linear models |
| | **Bayes Theorem (general)** |
| ptII_quan_Bayes_intro-BayesTheorem_covid19-test.r | calculate probability of a covid-19 test in relation to population characteristics |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r | apply Bayes Theorem on medical tests using prevalence (of a disease in population), sensitivity (1-false_positives), and specifity (1-false_negatives) |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r | function to apply Bayes Theorem for medical tests using prevalence p(A), sensitivity p(B|A), and specifitiy p(not-B|not-A)|
| ptII_quan_Bayes_intro-BayesTheorem_tea.r | simple application of Bayes Theorem, using discrete values, and shift from prior knowledge to posterior to prior, etc. ie. update Bayes Theorem with new information |
| | **Loss functions** |
| ptII_quan_Bayes_lossfun_startagain.r | show loss function in the context of an empirical example (success rates in drug therapy) to draw conclusions for actual work |
| | **Maximum Entropy** |
| ptII_quan_Bayes_MaximumEntropy.r | calculate and plot Boltzmann/ Shannon entropy using a (not-so-fair/ fair) coin and dice |
| ptII_quan_Bayes_MaximumEntropy_helpfuncs.r | functions to reproduce Jaynes' (1962) analyses, entropy simulation after McElreath (2015, p.277) |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice.r | perform and plot Jaynes' (1962) analysis on a (fair?) dice |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r | functions to calculate entropy of a fair or not-so-fair coin and simulate a dice |
| | **Over- and underfitting** |
| ptII_quan_Bayes_over-and-underfitting.r | examples to show under- and overfitting incl. polynomials, use Kullback-Leibler (KL) divergence between probs |
| | **posterior** |
| ptII_quan_Bayes_posterior.r | analyse posterior and MCMC chains (convergence), apply on empirical data set |
| | **Posterior Predictive Checks** |
PPC TERM!!! | ptII_quan_Bayes_PPC_model-check.r | simulate PPC using JAGS using successes/ failures and plot it |
| ptII_quan_Bayes_PPC_model-check-graph.r | demonstrate PPC for homogenous variances (yes/no), plot and test specific hypotheses (classical, Bayesian statistics with brms), treatment/ control group design, compare linear models |
| ptII_quan_Bayes_PPC_model-check_helpfuncs.r | function to calulate one-/two-sided test (PPC) |
| | **Relationship of prior, likelihood, and posterior** |
| ptII_quan_Bayes_Prior-Likeli-Post_relationship.r | display how prior and likelihood shape the posterior, see different influences on posterior |
| | **Local minima** |
| ptII_quan_Bayes_problem-local-minima.r | local minima using Himmelblau's function, plot in 3d |
| | **Random Walk** |
| ptII_quan_Bayes_RandomWalk.r |  calculate and plot a random walk as 2d + 3d |
| ptII_quan_Bayes_RandomWalk_helpfuncs.r | function to calculate a simple random walk |
| | **Regularization** |
| ptII_quan_Bayes_regularization.r | different priors (normal with different sigmas, Cauchy, uniform) |
| | **Region of practical equivalence (ROPE)** |
| ptII_quan_Bayes_ROPE-BayesFactor.r | show importance of ROPE vs. simple clear-cut for decision-making (difference in means). plot with HDI, Bayesian binomial test for various values, priors, and hypotheses, priors and their influence on BFs, apply on empirical data, apply and plot with package BEST, analyze MCMC and posterior probs |
| ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r | function to demonstrate change in BF due to prior believes |
| | **Estimation mean** |
| ptII_quan_Bayes_simple-estimation-mean-post.r | posterior distribution of a mean with different informed priors using package Bolstad |
| ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r | function to plot mean posterior with prior and likelihood, tweaked normgcp from package Bolstadt (bug fix) |
| | **Simulating pi** |
| ptII_quan_Bayes_simulate-pi.r | simulate pi and plot it |
CHECK FILENAME IN BOOK PLUS ABOVE SOMEWHERE| ptII_quan_Bayes_simulate-pi_helpfuncs.r | simple function to calculate pi (inefficient, but works) |
| **Part III** | Qualitative Data Analysis |
| | **Text Analysis and coding paradigm** |
| ptIII_qual_code-paradigm_table-analysis.r | example for usage of expand.grid |
| ptIII_qual_quan-textanalysis.r | prepare (e.g. removal of redundant parts, special characters, white space, punctuations, stop words and convert to lower cases, split text, word cloud, plot frequencies, KWIC, collocation, ...) and analyze text using packages stringi, SnowbalC, tm, magrittr, quanteda, corps2, etc., use wordstems, corpus inspection, ... |
| **Part IV** | Mixed Methods |
| | **Boolean algebra and Qualitative Comparative Analysis (QCA)** |
| ptIV_qual_Boole_basics.r | simple true/ false statements |
| ptIV_qual_Boole_case_Krook_women-in-parliament.r | analyse data set from Krook (2010) using QCA, extract prime implicants, check for consistencies, prepare positive and negative outcome for general discussion|
| ptIV_qual_Boole_case_school-success.r | artificial data set to demonstrate QCA for negative and positive outcome |
| ptIV_qual_Boole_case_Titanic_death-and-dying.r | apply QCA on Titanic data set to investigate a minimal set for survival (yes/ no) |
| ptIV_qual_Boole_fuzzy-logic.r | simple plot to demonstrate what fuzzy logic is |
| ptIV_qual_Boole_logical-minimization.r | single case demonstration how Boolean minimization actually works |
| | **Something different** |
| ptV_mixed_prime-numbers.r | one-liner to get prime numbers using Euclidean mod division |

</details>

## Links in the R code

It is impossible to write any R code without external role models and code taken, borrowed, learned, etc. from other people. To give respect to that fact and to allow to deepen this or that understanding of R or any other topic of the sript, at some selected points the scripts contain URLs to external webpages that may be interesting for the reader or for the practice of R. We cannot guarantee that those links still exist, because the internet changes too fast. They did when the scripts were written.

## Data sets

Due to legal and licence issues not all data sets discussed in the book can be published here as well. Data sets should be located in the same folder as the E script to load them. Quite some data sets are taken from R and therefor included in R. The following data sets are added in the following folders below 'data' of this repository:

| Dataset | Description |
| --- | --- |
| `AAH` | data from a research study about collaborative learning |
| `LG` | data from a research study about the usage of word counts and a small experiment about clairvoyance to demonstrate something that does not show any kind of empirical effect |
| `school_success` | logical table about school success (fictional data created just for demonstration) |
| `Spain-edu` | data from a Spanish research study on leadership in education |
| `startagain_appl-letter-addiction` | application letter for a treatment place in drug therapy rehabilitation written from detox in psychiatry (originally sent via fax, names and places are fully anonymized) |
| `startagain_successrates` | success rates to pass through a drug rehabilitation program in the Suisse drug therapy center *start again* between 1992-2017 |
| `Titanic_survivors` | well-known data of the passangers of the Titanic along with some of their characteristics |
| `wikipedia_presidential-heights` | data from wikipedia about the relationship of body height of US presidential candidates and later winners (presidents) |

## External data sets

As mentioned above some data sets used are taken directly from R like the **bupa** data set or the one from A Dobson (1990) about **plant weight data**, the famous **iris** data set, etc. (see book for further references). Other set like **crime data** are from external sources (e.g. UCLA) and others are not published due to a missing license required for public access (e.g. data about the **chiropractice research study** or about **women in parliament** by ML Krook 2010).

## External R code

Some R code was not taken from R packages but various locations on the net. From that selection some scripts were also tweaked to fit to our needs here. Such incidents are noted in the R scripts at each place where an external script was used or tweaked. Mostly, those scripts originate from:

- `bayesian2beta.r` (Sverdlov O, Ryeznik Y & Wu S. (2015). Exact Bayesian Inference Comparing Binomial Proportions, With Application to Proof-of-Concept Clinical Trials. _Therapeutic Innovation & Regulatory Science_, _49_(1), p.163-174.)
- `DBDA2E-utilities.R` and `Jags-Ymet-XmetMulti-Mrobust.R` (Kruschke, J (2014). _Doing Bayesian Data Analysis. 2nd ed._ Academic Press.)
- package `BayesianFirstAid` ([Bååth, R](https://github.com/rasmusab/bayesian_first_aid))

The corresponding `*.r` file contains from the URL where to download the R script if it is not conainted here. Download and just place them in the main folder. Do not change the filename to avoid any error while loading it. External R code is used in the following way as (parts of) R functions:

- R packages and R functions that are modified to meet our needs here (e.g. some functions went missing from one R version to the next one like code from [heatmap.plus](https://github.com/cran/heatmap.plus), [sjstats](https://github.com/strengejacke/sjstats), [rhmc](https://github.com/cran/rhmc), ...).
- Some R code (e.g. from [Bolstad](https://github.com/cran/Bolstad)) was slightly changed due to bugs in the original code at that time. That may be different now ie. the bug is fixed, but it was not at the time when the R scripts were written.
- Some other code just follows papers and can be seen as an implementation of forumla and algorithms (e.g. about p-hacking, z-curves, etc.). This is referenced en detail in the book itself.

From time to time some code is put in sections like

```
### not run
...
### end of not run
```

Such R code is optional or sometimes not fully related to the book or just gives another (maybe even redundant) perspective. This happens e.g. while investigating the Titanic survival characteristics with a lot of plots and tables. Too many perspectives, so one has to focus on a few spots.

## R version

All R scripts were written and tested under R v3.4/ v3.6. "In theory" they should run with later versions of R as well. However, sometimes packages are not maintained anymore and then they are dropped from the official R repository or something changes so heavily in a package that previous functions either do not exist anymore or at least not in the way they should and are used here. Therefor, one can create a virtual machine or a docker engine, install R v3.4/ v3.6 and everything should run fine independent from using Linux or Windows. All scripts were developed under Linux, but also tested under win7.

## R libraries

Many scripts require external R packages and external libraries installed on the operation system (Linux, ...), espicially if it comes to compilation in the context of Bayesian linear models with package brms. Sometimes they require especially under Linux the compilation of libraries. Such compilations under Linux should be done directly by running R from the commandline and **not** (**never!**) via using some GUI like RStudio (esp. this one!), because a lot of experiences showed that the compilation tends to break and fails out of unknown reasons if such a GUI was used. Compilation directly via R on the commandline works pretty well as long as the necessary (development) libraries are installed on the system. Afterwards the GUI can be used again without any hazzle. Under windows, most libraries do not require any compilation. If compiled packages or packages installed from the commandline and not via the OS package system (using debs/ rpms/ etc.) should be made available for all local users, start R as root and install then packages via the commandline.

If future time permits the scripts will be updated to work with R v4.

## Errors in the R code

If one finds a bug please contact us with a short and clear example so we can try to reproduce it and fix the error.

## Usage of the R code

Important is to understand that sometimes due to demonstration goals the code is not perfect and is revised a few lines later or it is done differently. Not every break of the script is a bug, sometimes it is a feature of the book. Those incidents are therefor no (serious) bugs but intentional for educational purposes to show that things evolve and do not happen accidentially. Statistical work means (ideally!) slow and steady progress towards a goal that sometimes even changes. In general, the R code is adjusted to the tasks (e.g. data sets) and the specific topics of the book and its chapters. It's not an R package and does not try to be so. The R code does not fulfill the requirements of traditional R packages like being useful as a universal library. Nevertheless, some of the functions may be helpful in various changing contexts and can be used independently from the book, the datasets, etc.

If you find something useful, change it according to your needs. We do the same.

## AQUAD 7 files

One data set (application letter for a treatment place in a drug addiction rehabilitation center) is taken from qualitative data analysis along with the windows binary from [AQUAD 7](https://www.aquad.de), a free and open source QDA software. The folder AQUAD7 contains all working files along with the older AQUAD7 binary version that was originally used for the analysis. AQUAD is now on v8.

## Licenses and Credits

The R code is licensed under [GNU GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html). Please feel free to use, modify or share the code if it is helpful for you.
The functions taken from other packages have their own license. Details can be found in the license file of cited and references packages and scripts.
The AQUAD7 binary `aquad7_170117.exe` has a (C) by GL Huber (2017). The more recent version of AQUAD8 can be found freely on the [AQUAD 8](https://www.aqua.de) webpage.

## References

- Bretthorst, GL (1993). On the difference in means. In: WT Grandy and PW Milonni (Eds.), _Physics & Probability Essays in honor of ET Jaynes_. Cambridge University Press, England.
- Dobson, A (1990). _An Introduction to Generalized Linear Models._ Chapman & Hall/ CRC Texts in Sstatistical Science.
- Krook, ML (2010). Women's Representation in Parliament: A Qualitative Comparative Analysis. _Political Studies_, _58_, p.886-908.
- Kruschke, J (2014). _Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd ed._ Academic Press.
- McElreath, R (2015). _Statistical Rethinking. A Bayesian Course with Examples in R and Stan. 1st ed._ Chapman & Hall/ CRC Texts in Sstatistical Science.
- Neal, RM (2011). Handbook of Markov Chain Monte Carlo._ Edited by S Brooks, A Gelman, G Jones, and X-L Meng. Chapman & Hall/ CRC Press.

## Citation

If you ever refer to any part of the R code, please cite it as:

Gürtler, Leo (2023). R code supplement for Gürtler & Huber (2023). *Subjektive Betrachtungen und objektive Ansichten. Qual, Quan und Logik.* R code published on [Github](https://github.com/abcnorio/mixedmethod-rcode) and [OSDN](https://osdn.net/projects/mixedmethod-rcode).

## Disclaimer

Although all R scripts were tested heavily under varying conditions, we cannot rule out any possible errors. So we do not guarantee anything but to advice that users should use their common sense along with their intelligence and experience whether a result makes sense and is done properly or not. Some R code and examples make only sense in combination with the book, because the few notes in the code are not sufficient. However, it is provided "as is".
Thus - use common sense to compare results with expectations. NO WARRANTY of any kind is involved here. There is no guarantee that the software is free of error or consistent with any standards or even meets your requirements. Do not use the software or rely on it to solve problems if incorrect results may lead to hurting or injurying living beings of any kind or if it can lead to loss of property or any other possible damage to the world, living beings, non-living material or society as such. If you use the software in such a manner, you are on your own and it is your own risk.



