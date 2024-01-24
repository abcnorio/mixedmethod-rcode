# Suplemento en código R al libro "Combinación de Métodos - CUAL, CUAN Y LÓGICA. Puntos de vista objetivos y observaciones subjetivas". Escrito por Guertler & Huber (2023).

## Recursos e idiomas

La versión española del libro se titula "Combinación de Métodos - CUAL, CUAN Y LÓGICA. Puntos de vista objetivos y observaciones subjetivas". Ambos libros pueden obtenerse gratuitamente en OSF o AQUAD.

## Visión general

El tema principal del libro es la metodología mixta, especialmente en lo que se refiere al análisis de datos. Aunque el libro está escrito en alemán (idioma original) y traducido al español, el código R se puede utilizar sin conocimientos de alemán. Las notas y comentarios en los guiones están escritos en inglés. El código en sí no está organizado como un paquete R y esa no es la intención aquí. Sin embargo, algunas funciones pueden ser útiles para tal o cual propósito. Los comentarios aquí y allá en los scripts deberían ayudar a entender el objetivo principal si el libro no está disponible o no se puede entender.

## Antecedentes

El libro trata de metodología mixta (cuantitativa, cualitativa y lógica booleana) y se publica en 2023 libremente en varias plataformas de bibliotecas abiertas como OSF o AQUAD. La parte estadística abarca la estadística clásica (Fisher, Neyman-Pearson), el análisis exploratorio de datos sensu J.W. Tukey, así como la estadística bayesiana. La lógica booleana se utiliza para el análisis implícito del análisis comparativo cualitativo. La parte cualitativa abarca tanto el análisis cuantitativo como el cualitativo de textos. Este último utiliza el paradigma de codificación y análisis secuencial procedente de la Hermenéutica Objetiva sensu Oevermann y colegas y es el único análisis para el que el código R realmente no tiene ningún sentido. Sin embargo, el análisis secuencial asistido por ordenador está disponible en el software gratuito QDA AQUAD.

## Ejecutar el código R

Utilice una GUI como RStudio, R-Commander, JGR, Emacs con ESS, Deducer, Eclipse StatET, RKWard, Rattle o Tinn-R. Utilice lo que más le convenga. Normalmente usamos RStudio (escritorio, servidor) pero nunca instalamos librerías de R a través de la GUI (ver comentarios más abajo).

## Nombres de archivos

Los archivos funcionan de la siguiente manera, de modo que la mayoría de los archivos están directamente relacionados con un capítulo del libro y una tarea o conjunto de datos específicos (véase la tabla siguiente).

- `ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r`
- `ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r`

Esto significa que tomamos los nombres de archivo de ejemplo anteriores y los descomponemos en sus partes, por ejemplo

| Filename part | Description |
| --- | --- |
| `pt II` | parte II [valores posibles: parte todo, I, II, III, IV, V] |
| `quan` | cuantitativo [valores posibles: generalfuncs, scientifictheory, quantitative, qualitative] |
| `Bayes` | Estadística bayesiana [valores posibles: classicstats, EDA, code-paradigm, quan-textanalysis, Boole, mixed] |
| `BayesFactors` | subtema [varios temas posibles...] |
| `dependence-on-N-sim` | tema discutido, aquí la dependencia del Factor de Bayes del tamaño de la muestra N usando simulación para demostrar tal relación |
| `helpfuncs` [optional] | término general utilizado para las funciones utilizadas en los scripts (en la mayoría de los casos los archivos no-helpfuncs.r no contienen funciones R) |
| `ptall_generalfuncs` | colección de funciones de uso general que no son (sólo) específicas del conjunto de datos |

Algunos archivos raros no siguen ese patrón de nombre de archivo:

| Scriptname | Descripción |
| --- | --- |
| `DiM_Bretthorst_PG.r` | Implementación en R del artículo "Difference in means" de G.L. Bretthorst 1993 (una solución bayesiana analítica al problema Behrens-Fisher) modelada según Gregory, 2005 |
| `DiM_Bretthorst_UMS.r` | Igual que el anterior pero una implementación de UM Studer (1998) |
| `model.txt` | Modelo Lady Bristol BUGS para reproducir la prueba exacta de Fisher basada en los datos originales del "experimento Lady tea" (con totales de márgenes fijos para filas y columnas) |

Para el enfoque Bretthorst hay un repositorio dedicado en diffinmeans con código R actualizado y mejorado.

Para facilitar la relación entre el archivo R script y el capítulo del libro, la siguiente tabla contiene la correspondencia entre el capítulo y el R script. Está ordenada de acuerdo con los capítulos del libro.

<details>

<summary>Haga clic aquí para ver la tabla con la correspondencia entre el nombre del archivo de script R, el capítulo del libro y el título del capítulo (en alemán).</summary>

| Scriptname | Chap | SubChap | Chapter title (German) |
| --- | --- | --- | --- |
| DiM_Bretthorst_PG.r | - | - | general functions |
| DiM_Bretthorst_PG_calls.r | - | - | example calls |
| DiM_Bretthorst_UMS.r | - | - | general functions |
| DiM_Bretthorst_UMS_calls.r | - | - | example calls |
| **External functions** |  |  |  |
| EXT_bayesian2beta.r | - | - | general functions |
| EXT_DBDA2E-utilities.R | - | - | general functions |
| EXT_Jags-Ymet-XmetMulti-Mrobust.R | - | - | general functions |
| **All parts** |  |  | |
| ptall_generalfuncs_Bayes_Beta_determine.r | - | - | general functions |
| ptall_generalfuncs_Bayes_binomial-prop-test.r | - | - | general functions |
| ptall_generalfuncs_Bayes_binomial.r | - | - | general functions |
| ptall_generalfuncs_brob-integral.r | - | - | general functions |
| ptall_generalfuncs.r | - | - | general functions |
| **Part I - Scientific Theory** |  |  | |
| ptI_sciencetheory_logic.r | 2 | 2.2 | **Der deduktive Schluss** |
| **Part II - Classical Statistics** |  |  | |
| ptII_quan_classicstats_Fisher_ladyteataste.r | 4 | 4.5.1.1 | Vom Tee trinken und Milch erkennen — ein Beispielexperiment nach Fisher |
| ptII_quan_classicstats_N-P_powerfunc.r | 4 | 4.5.2.1 | Praktische Bedeutsamkeit im Kontext von statistischer Bedeutsamkeit |
| ptII_quan_classicstats_N-P_stat-signif-isNOT-practsignif.r | 4 | 4.5.2.1 | Praktische Bedeutsamkeit im Kontext von statistischer Bedeutsamkeit |
| ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r | 4 | 4.5.2.2 | Richtung und Größe — zwei unterschätzte Fehlertypen |
| ptII_quan_classicstats_GandC_type-S-M-error.r | 4 | 4.5.2.2 | Richtung und Größe — zwei unterschätzte Fehlertypen |
| ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r | 4 | 4.5.2.5 | Die Vollkostenrechnung |
| ptII_quan_classicstats_N-P_nulldist-hypotest.r | 4 | 4.5.2.5 | Die Vollkostenrechnung |
| ptII_quan_classicstats_N-P_confint_bayesboot.r | 4 | 4.5.2.6 | Konfidenzintervalle |
| ptII_quan_classicstats_N-P_confint_errorbars.r | 4 | 4.5.2.6 | Konfidenzintervalle |
| ptII_quan_classicstats_N-P_confint_helpfuncs.r | 4 | 4.5.2.6 | Konfidenzintervalle |
| ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r | 4 | 4.5.2.6 | Konfidenzintervalle |
| ptII_quan_classicstats_N-P_confint_p-t-value.r | 4 | 4.5.2.6 | Konfidenzintervalle |
| ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r | 4 | 4.5.2.6 | Konfidenzintervalle |
| ptII_quan_classicstats_N-P_confint.r | 4 | 4.5.2.6 | Konfidenzintervalle |
| ptII_quan_classicstats_N-P_simulation.r | 4 | 4.5.4 | Exkurs — Simulationen |
| ptII_quan_classicstats_N-P_example-soccer-sim_helpfuncs.r | 4 | 4.5.4.1 | Fallbeispiel Simulation — Fussball Sammelbilder |
| ptII_quan_classicstats_N-P_example-soccer-sim.r | 4 | 4.5.4.1 | Fallbeispiel Simulation — Fussball Sammelbilder |
| ptII_quan_classicstats_N-P_SE-N-dep.r | 4 | 4.5.5 | Stichprobengröße |
| ptII_quan_classicstats_nullritual.r | 4 | 4.5.7 | Der Ablauf eines statistischen Tests — das Nullritual |
| ptII_quan_classicstats_NHST_nulldist.r | 4 | 4.5.8 | Die Wahrscheinlichkeit von Daten als Basis von Testentscheidungen |
| ptII_quan_classicstats_pvalue-as-base.r | 4 | 4.5.8.1 | Die Berechnung des p-Wertes |
| ptII_quan_classicstats_normal-vs-t.r | 4 | 4.5.8.2 | Zum Verhältnis von Normalverteilung und t-Verteilung |
| ptII_quan_classicstats_centrallimittheorem_helpfuncs.r | 4 | 4.5.8.3 | Exkurs --- zentraler Grenzwertsatz |
| ptII_quan_classicstats_centrallimittheorem.r | 4 | 4.5.8.3 | Exkurs --- zentraler Grenzwertsatz |
| ptII_quan_classicstats_p-t-df-relationship.r | 4 | 4.5.9 | Erkenntnistheorie reloaded — klassische Statistik |
| ptII_quan_classicstats_effectsizes_helpfuncs_sjstats.r | 4 | 4.6.10 | Effektstärken — Größe, Häufigkeit und Bezug zur Originalskala |
| ptII_quan_classicstats_effectsizes_helpfuncs.r | 4 | 4.6.10 | Effektstärken — Größe, Häufigkeit und Bezug zur Originalskala |
| ptII_quan_classicstats_effectsizes.r | 4 | 4.6.10 | Effektstärken — Größe, Häufigkeit und Bezug zur Originalskala |
| ptII_quan_classicstats_Simpsonparadox_helpfuncs.r | 4 | 4.6.11.1 | Simpson Paradox |
| ptII_quan_classicstats_Simpsonparadox.r | 4 | 4.6.11.1 | Simpson Paradox |
| ptII_quan_classicstats_JeffreysLindleyparadox.r | 4 | 4.6.11.2 | Jeffreys-Lindley Paradox |
| ptII_quan_classicstats_p-hacking-sim_helpfuncs.r | 4 | 4.6.2 | Auf der Suche nach Signifikanzen — unbewusste Forschungsintentionen und p-hacking |
| ptII_quan_classicstats_p-hacking-sim.r | 4 | 4.6.2 | Auf der Suche nach Signifikanzen — unbewusste Forschungsintentionen und p-hacking |
| ptII_quan_classicstats_dealingwithpower.r | 4 | 4.6.3 | Der Umgang mit Power |
| ptII_quan_classicstats_R-index_z-curve_helpfuncs.r | 4 | 4.6.4.1 | R-Index |
| ptII_quan_classicstats_R-index_z-curve.r | 4 | 4.6.4.1 | R-Index |
| ptII_quan_classicstats_varianceestimation_helpfuncs.r | 4 | 4.6.5 | (Selbst-)Täuschungen |
| ptII_quan_classicstats_varianceestimation.r | 4 | 4.6.5 | (Selbst-)Täuschungen |
| ptII_quan_classicstats_randomization.r | 4 | 4.6.6 | Randomisierung |
| ptII_quan_classicstats_missingdata.r | 4 | 4.6.7 | Fehlende Daten |
| ptII_quan_classicstats_equivalentmethods_helpfuncs.r | 4 | 4.6.8.1 | Äquivalenz von Messverfahren und -methoden |
| ptII_quan_classicstats_equivalentmethods.r | 4 | 4.6.8.1 | Äquivalenz von Messverfahren und -methoden |
| ptII_quan_classicstats_variancehomogeneity.r | 4 | 4.6.8.2 | Varianzhomogenität (Homoskedastizität) |
| ptII_quan_classicstats_normaldist_residuals.r | 4 | 4.6.9.1 | Normal-Verteilung der Residuen |
| ptII_quan_classicstats_outliers-and-influentialpoints_helpfuncs.r | 4 | 4.6.9.3 | Ausreisser und einflussreiche Datenpunkte |
| ptII_quan_classicstats_outliers-and-influentialpoints.r | 4 | 4.6.9.3 | Ausreisser und einflussreiche Datenpunkte |
| **Part II - Exploratory Data Analysis (EDA) sensu Tukey** |  |  |  |
| ptII_quan_EDA_intro_overviewrobust_helpfuncs.r | 5 | 5.2.-5.3. | Typische Verfahren der EDA in R |
| ptII_quan_EDA_intro_overviewrobust.r | 5 | 5.2.-5.3. | Typische Verfahren der EDA in R |
| ptII_quan_EDA_case_German-states-population.r | 5 | 5.5.1 | population comparison of German states |
| ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r | 5 | 5.5.2 | Fruchtbarkeit und Fertilität |
| ptII_quan_EDA_case_Suisse-fertility.r | 5 | 5.5.2 | Fruchtbarkeit und Fertilität |
| ptII_quan_EDA_case_Anderson_iris-species-in-biology.r | 5 | 5.5.3 | Spezies unterscheiden in der Biologie |
| ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r | 5 | 5.5.4.6 | Leben und Sterben auf der Titanic |
| ptII_quan_EDA_case_Titanic_death-and-dying.r | 5 | 5.5.4.6 | Leben und Sterben auf der Titanic |
| ptII_quan_EDA_case_Spain_leadership-in-education_helpfuncs.r | 5 | 5.5.5 | 5.5.5 Führungsverhalten in Bildungskontexten |
| ptII_quan_EDA_case_Spain_leadership-in-education.r | 5 | 5.5.5 | Führungsverhalten in Bildungskontexten |
| ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r | 5 | 5.5.6 | Ein Experiment zur Herzratenvariabilität |
| ptII_quan_EDA_case_Chiro_heartrate-variability.r | 5 | 5.5.6 | Ein Experiment zur Herzratenvariabilität |
| **Part II - Bayesian Statistics** |  |  | |
| ptII_quan_Bayes_Beta-distribution.r | 6 | 6.12 | Die Wahl priorer Verteilungen |
| ptII_quan_Bayes_find-Beta-distribution-shapeparams.r | 6 | 6.12 | Die Wahl priorer Verteilungen |
| ptII_quan_Bayes_Fisher_LadyBristol-Beta-disttribution.r | 6 | 6.12 | Die Wahl priorer Verteilungen |
| ptII_quan_Bayes_Gamma-distribution.r | 6 | 6.12 | Die Wahl priorer Verteilungen |
| ptII_quan_Bayes_regularization.r | 6 | 6.12 | Die Wahl priorer Verteilungen |
| ptII_quan_Bayes_Prior-Likeli-Post_relationship.r | 6 | 6.12.1 | Verhältnis Prior — Likelihood — Posterior |
| ptII_quan_Bayes_MC-simulation_binom-norm.r | 6 | 6.13 | Marko Chain Monte Carlo Simulationen — MCMC |
| ptII_quan_Bayes_RandomWalk_helpfuncs.r | 6 | 6.13 | Marko Chain Monte Carlo Simulationen — MCMC |
| ptII_quan_Bayes_RandomWalk.r | 6 | 6.13 | Marko Chain Monte Carlo Simulationen — MCMC |
| ptII_quan_Bayes_simulate-pi_helpfuncs.r | 6 | 6.13 | Marko Chain Monte Carlo Simulationen — MCMC |
| ptII_quan_Bayes_simulate-pi.r | 6 | 6.13 | Marko Chain Monte Carlo Simulationen — MCMC |
| ptII_quan_simulate-pi_helpfuncs.r | 6 | 6.13 | Marko Chain Monte Carlo Simulationen — MCMC |
| ptII_quan_simulate-pi.r | 6 | 6.13 | Marko Chain Monte Carlo Simulationen — MCMC |
| ptII_quan_Bayes_problem-local-minima.r | 6 | 6.13.1.4 | Zusammenfassung MCMC-Algorithmus |
| ptII_quan_Bayes_HMC_helpfuncs.r | 6 | 6.13.2.3.1 | Hamilton Monte Carlo im R |
| ptII_quan_Bayes_HMC.r | 6 | 6.13.2.3.1 | Hamilton Monte Carlo im R |
| ptII_quan_Bayes_MetropolisHastings_example-normdist.r | 6 | 6.13.4.1 | Der Metropolis-Hastings Algorithmus im R |
| ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r | 6 | 6.13.4.1 | Der Metropolis-Hastings Algorithmus im R |
| ptII_quan_Bayes_GibbsSampling_example-normdist.r | 6 | 6.13.4.2 | Der Gibbs Sample im R |
| ptII_quan_Bayes_JAGS_example-norm.r | 6 | 6.13.4.3 | Gibbs-Sampling mit JAGS |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r | 6 | 6.13.5 | Fisher reloaded — mehr Tee |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS.r | 6 | 6.13.5 | Fisher reloaded — mehr Tee |
| ptII_quan_Bayes_MaximumEntropy_helpfuncs.r | 6 | 6.14 | Maximum Entropy |
| ptII_quan_Bayes_MaximumEntropy.r | 6 | 6.14 | Maximum Entropy |
| ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r | 6 | 6.14.1 | „I, we, and nation“ — präsidiale Eigenwerbung |
| ptII_quan_Bayes_Entropy_KullbackLeibler.r | 6 | 6.14.1 | „I, we, and nation“ — präsidiale Eigenwerbung |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r | 6 | 6.14.3 | Der Klassiker — ist ein Würfel fair? |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice.r | 6 | 6.14.3 | Der Klassiker — ist ein Würfel fair? |
| ptII_quan_Bayes_case_presidential-heights_helpfuncs.r | 6 | 6.15.1 | Präsidiale Höhenflüge |
| ptII_quan_Bayes_case_presidential-heights.r | 6 | 6.15.1 | Präsidiale Höhenflüge |
| ptII_quan_Bayes_case_startagain-successrates.r | 6 | 6.15.2 | Durchlaufquoten in der Drogensuchttherapie |
| ptII_quan_Bayes_case_startagain-successrates-longterm.r | 6 | 6.15.2.1 | Langzeitevaluation (Durchlaufquoten) |
| ptII_quan_Bayes_case_presidential-debates.r | 6 | 6.15.3 | „I, we, and nation“ — präsidiale Eigenwerbung Teil 2 |
| ptII_quan_Bayes_intro-BayesTheorem_tea.r | 6 | 6.2.2.4 | [6.2.2 Fallbeispiel — noch ein Tee-Experiment] |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r | 6 | 6.2.3.4 | [6.2.3 Fallbeispiel — Medizindiagnostik] |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r | 6 | 6.2.3.4 | [6.2.3 Fallbeispiel — Medizindiagnostik] |
| ptII_quan_Bayes_intro-BayesTheorem_covid19-test.r | 6 | 6.2.4 | Fallbeispiel — Zur Zuverlässigkeit eines COVID-19 Tests |
| ptII_quan_Bayes_posterior.r | 6 | 6.5.1 | Intuitives Verständnis von Wahrscheinlichkeit |
| ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r | 6 | 6.5.1 | Intuitives Verständnis von Wahrscheinlichkeit |
| ptII_quan_Bayes_simple-estimation-mean-post.r | 6 | 6.5.1 | Intuitives Verständnis von Wahrscheinlichkeit |
| ptII_quan_Bayes_BayesFactors_test-hypos.r | 6 | 6.7.1 | Bayes-Faktoren und Bayes-Hypothesentesten |
| ptII_quan_Bayes_lossfun_startagain.r | 6 | 6.7.1.2 | Verlustfunktionen |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r | 6 | 6.7.1.3 | Aktualität von Bayes-Faktoren |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r | 6 | 6.7.1.3 | Aktualität von Bayes-Faktoren |
| ptII_quan_Bayes_case_exp-extra-sensual-perception.r | 6 | 6.7.1.4 | Hellsehen — in guter Grund für Nullhypothesentesten? |
| ptII_quan_Bayes_Bem-study-aspects.r | 6 | 6.7.1.5 | Designkritik — die Studie von Bem |
| ptII_quan_Bayes_BayesFactors_further-remarks.r | 6 | 6.7.1.6 | Bayes-Faktoren — und nun? |
| ptII_quan_Bayes_information-criteria_helpfuncs.r | 6 | 6.7.2 | Informationskriterien |
| ptII_quan_Bayes_information-criteria.r | 6 | 6.7.2 | Informationskriterien |
| ptII_quan_Bayes_over-and-underfitting.r | 6 | 6.7.3 | Overfitting und Underfitting |
| ptII_quan_Bayes_HDI_helpfuncs.r | 6 | 6.7.4.1 | Intervallschätzung |
| ptII_quan_Bayes_HDI.r | 6 | 6.7.4.1 | Intervallschätzung |
| ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r | 6 | 6.7.4.2 | ROPE — region of practical equivalenceg |
| ptII_quan_Bayes_ROPE-BayesFactor.r | 6 | 6.7.4.2 | ROPE — region of practical equivalenceg |
| ptII_quan_Bayes_PPC_model-check_helpfuncs.r | 6 | 6.7.4.4 | Praxis — posterior predictive check |
| ptII_quan_Bayes_PPC_model-check.r | 6 | 6.7.4.4 | Praxis — posterior predictive check |
| ptII_quan_Bayes_PPC_model-check-graph.r | 6 | 6.7.4.5 | Graphische Begutachtung von Modellen im Dienste des Modellfittings |
| ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r | 6 | 6.7.4.6 | Forschungsbeispiel — Wortproduktion Humor |
| ptII_quan_Bayes_case_wordcounts-PPC.r | 6 | 6.7.4.6 | Forschungsbeispiel — Wortproduktion Humor |
| **Part III - Qualitative Data Analysis** |  |  | |
| ptIII_qual_code-paradigm_table-analysis.r | 9 | 9.4 | Tabellenanalysen nach Miles und Huberman |
| ptIII_qual_quan-textanalysis.r | 10 | 10.1 | Fallbeispiel quantitative Textanalyse |
| **Part IV - Qualitative Comparative Analysis** |  |  | |
| ptIV_qual_Boole_basics.r | 12 | 12.1 | Propädeutikum |
| ptIV_qual_Boole_case_Krook_women-in-parliament.r | 12 | 12.11.1 | Die Repräsentativität von Frauen in Parlamenten |
| ptIV_qual_Boole_case_Titanic_death-and-dying.r | 12 | 12.11.2 | Leben und Sterben auf der Titanic Teil II |
| ptIV_qual_Boole_logical-minimization.r | 12 | 12.3 | Typenbildung als Prinzip des Vergleichs mittels logischer Minimierung |
| ptIV_qual_Boole_case_school-success.r | 12 | 12.6 | Kriteriumsanalyse — positiver und negativer Ausgang |
| ptIV_qual_Boole_fuzzy-logic.r | 12 | 12.7 | Fuzzy Logic |
| **Part V - Mixed Methods** |  |  | |
| ptV_mixed_prime-numbers.r | 13 | 13.3.1 | QUAL und QUAN in Konversions-Designs |

</details>

La siguiente tabla contiene información condensada sobre cada script usando algunas palabras clave. Los archivos están agrupados de acuerdo con el tema y no representan necesariamente el orden de aparición en el libro.

<details>

<summary>Click here to see the table with a short description of the R scripts</summary>

| Scriptname | Content |
| --- | --- |
| DiM_Bretthorst_PG.r | Bretthorst (1993) difference in means, analytical solution, with package Brobdingnag for very large numbers, implementation after P Gregory (2005) |
| DiM_Bretthorst_PG_calls.r | example calls |
| DiM_Bretthorst_UMS.r | Bretthorst (1993) difference in means, analytical solution, with package Brobdingnag for very large numbers, implementation after UM Studer (1998) |
| DiM_Bretthorst_UMS_calls.r | example calls |
| **External functions** |  |
| EXT_bayesian2beta.r | exact Bayesian proportion test from Sverdlov, Ryeznik & Wu (2015) |
| EXT_DBDA2E-utilities.R | from Kruschke (2014) DBDA 2nd ed., general functions |
| EXT_Jags-Ymet-XmetMulti-Mrobust.R | from Kruschke (2014) DBDA 2nd ed., used for PPC and heterogenuous variances |
| **All parts** | |
| ptall_generalfuncs.r | Cohen's d, descriptive statistics of all kinds in one table, Tukey's fivenum with labels, convert Aquad style tables to truth tables and v.v., print prime implicants from QCA objects, full distance matrix, optimal cut through a proximity matrix after Oldenbürger and plot it, plot prototypes in 2d + 3d, plot eigenvalues (MDS), correlation and p-values |
| ptall_generalfuncs_Bayes_Beta_determine.r | determine and plot beta distribution values from three quantile points via optimization, role model: Coghlan (2017) |
| ptall_generalfuncs_Bayes_binomial.r | functions to apply and plot prior probability functions for successes/ failures, calculate summary statistics for posterior, HDI, formula to calculate beta posterior via conjugation, plot prior, likelihood, posterior, updated prior and update therefor posterior, summary statistics |
| ptall_generalfuncs_Bayes_binomial-prop-test.r | Bayesian proportion test (e.g. successes/ failures): calculate, summarize, and plot prior from successes/ failures, convert to beta disztribution values and v.v., update binomial prior with likelihood to beta (posterior) and plot, tweaked bayes.prop.test summary from BayesianFirstAid and plot theta_diff, MCMC plot, simulation from posterior, grid approximation via brute force, exact (binomial difference) tests (Evan Miller, Chris Stucchio, the approach by Sverdlov, Ryeznik, Wu (2015) from bayesian2beta.r in a tweaked form to work with log values or brob objects and plot results, numerical integration (Simpson rule) for brob objects, brute force comparison of rbetas vs. dbetas and plot it |
| ptall_generalfuncs_brob-integral.r | useful functions for brob objects, convert a list to a vector, calculate the scalarproduct, numerical integration (Simpson rule), more numerical integration methods for brob objects can be found at https://github.com/abcnorio/R-largenum-integration |
| **Part I - Scientific Theory** | |
| ptI_sciencetheory_logic.r | chap. 2.2 - simple true/ false statements |
| **Part II - Classical Statistics** | |
| ptII_quan_classicstats_Fisher_ladyteataste.r |  chap. 4.5.1.1 - Lady Bristol tea taste experiment by Fisher using his methods using hypergeometric distribution, fisher.test, and manual calculation using factorials |
| ptII_quan_classicstats_N-P_powerfunc.r | chap. 4.5.2.1 - calculate and plot power vs. effect sizes in accordance to different hypotheses |
| ptII_quan_classicstats_N-P_stat-signif-isNOT-practsignif.r | chap. 4.5.2.1 - different views on sample differences if population parameters are known |
| ptII_quan_classicstats_GandC_type-S-M-error.r | chap. 4.5.2.2 - reproduce Gelman & Carlin (2014), retrodesign power analysis |
| ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r | chap. 4.5.2.2 - plot effect size and power for retrodesign after Gelman & Carlin (2014), plot sample distribution under H_0 and hypothesis of true effect size |
| ptII_quan_classicstats_N-P_nulldist-hypotest.r | chap. 4.5.2.5 - plot H_0, H_1 for one- and two-sided tests |
| ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r | chap. 4.5.2.5 - functions to plot H_0, H_1 for hypothesis directions: two-sided, less, greater, and plot alpha and beta error rates for t-distributed densities |
| ptII_quan_classicstats_N-P_confint.r | chap. 4.5.2.6 - working and simulating confidence intervals (CI) |
| ptII_quan_classicstats_N-P_confint_bayesboot.r | chap. 4.5.2.6 - bootstrap CIs and other statistics via simulation, simulate difference in means, use Bayesian bootstrap |
| ptII_quan_classicstats_N-P_confint_errorbars.r | chap. 4.5.2.6 - CI evolution in relation to varying sample sizes |
| ptII_quan_classicstats_N-P_confint_helpfuncs.r | chap. 4.5.2.6 - calculate CI for mean, difference in means, and plot them |
| ptII_quan_classicstats_N-P_confint_p-t-value.r | chap. 4.5.2.6 - simulate t-test with p- and t-values |
| ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r | chap. 4.5.2.6 - simulate t-tests, compare with Cohen's d |
| ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r | chap. 4.5.2.6 - change of CIs along with error bars, calculate covered CIs |
| ptII_quan_classicstats_N-P_simulation.r | chap. 4.5.4 - simulation 'difference in means' via replicate |
| ptII_quan_classicstats_N-P_example-soccer-sim.r | chap. 4.5.4.1 - simulate an empirical real run of how to get all cards of a soccer card album |
| ptII_quan_classicstats_N-P_example-soccer-sim_helpfuncs.r | chap. 4.5.4.1 - simulate how many cards are still to go using chance and not commone sense like card exchange, etc. |
| ptII_quan_classicstats_N-P_SE-N-dep.r | chap. 4.5.5 - dependence of sample size characteristics via simulated data (N, SE, t-value, Cohen's d, ...) |
| ptII_quan_classicstats_nullritual.r | chap. 4.5.7 - all the non-sense around the Null ritual ie. the NHST procedure |
| ptII_quan_classicstats_NHST_nulldist.r | chap. 4.5.8 - plot NHST decision making using acceptance/ rejection regions, alpha and beta error rates |
| ptII_quan_classicstats_pvalue-as-base.r | chap. 4.5.8.1 - differences of normal vs. mixed normal distribution in accordance to plot and statistics, simulation H_0 |
| ptII_quan_classicstats_normal-vs-t.r | chap. 4.5.8.2 - relationship of normal and t distribution |
| ptII_quan_classicstats_centrallimittheorem.r | chap. 4.5.8.3 - simulate central limit theorem (CLT) und varying conditions |
| ptII_quan_classicstats_centrallimittheorem_helpfuncs.r | chap. 4.5.8.3 - basic functions to simulate CLT |
| ptII_quan_classicstats_p-t-df-relationship.r | chap. 4.5.9 - relationship of N, t-, and p-value, simulate with same seed, growing sample sizes, constant t-value, and post-hoc power simulation |
| ptII_quan_classicstats_effectsizes.r | chap. 4.6.10 - relate N, p-value, Cohens' d, odds ratio, and risk ratio with each other graphically |
| ptII_quan_classicstats_effectsizes_helpfuncs.r | chap. 4.6.10 - function to relate N, p-value, and Cohen's d |
| ptII_quan_classicstats_effectsizes_helpfuncs_sjstats.r | chap. 4.6.10 - taken and twekaed from package sjtats as it disappeared from the package, convert odds ratio to risk ratio |
| ptII_quan_classicstats_Simpsonparadox.r | chap. 4.6.11.1 - investigate Simpson Paradox with classical UCB admission data set using plots, tables, corrected base rates, and glmer model, simulate Simpson Paradox data and detect sub-groups with Simpsons from package Simpsons |
| ptII_quan_classicstats_Simpsonparadox_helpfuncs.r | chap. 4.6.11.1 - function to plot two groups to detect Simpson Paradox |
| ptII_quan_classicstats_JeffreysLindleyparadox.r | chap. 4.6.11.2 - Jeffrey's-Lindley's paradox with data examples |
| ptII_quan_classicstats_p-hacking-sim.r | chap. 4.6.2 - simulate p-hacking with varying sample sizes for standard alpha error rates, comparison with a priori power analysis, and plot results |
| ptII_quan_classicstats_p-hacking-sim_helpfuncs.r | chap. 4.6.2 - function to simulate linear model with sim from package arm as well as p-hacking methods |
| ptII_quan_classicstats_dealingwithpower.r | chap. 4.6.3 - calculate, simulate, and plot power under varying models |
| ptII_quan_classicstats_R-index_z-curve.r | chap. 4.6.4.1 - apply R-Index and TIVA after Schimmack and colleagues, e.g. on Bem's clairvoyance study and calculate z-curve |
| ptII_quan_classicstats_R-index_z-curve_helpfuncs.r | chap. 4.6.4.1 - function to calculate R-Index, p-values for R-Index, Test of Insufficient Variance (TIVA) after Schimmack and colleagues |
| ptII_quan_classicstats_varianceestimation.r | chap. 4.6.5 - apply function to calculate variance estimation |
| ptII_quan_classicstats_varianceestimation_helpfuncs.r | chap. 4.6.5 - function to calculate variance estimation if sigma is known or unknown and plot it |
| ptII_quan_classicstats_randomization.r | chap. 4.6.6 - apply different randomization strategies to create samples |
| ptII_quan_classicstats_missingdata.r | chap. 4.6.7 - handling missing data and associated changes with and without imputation methods, comparison of methods of imputation with each other and with brms (Bayesian linear model) |
| ptII_quan_classicstats_equivalentmethods.r | chap. 4.6.8.1 - comparison with Bland-Altman stats and plot (mean-diff plot), TOST from package TOSTER, Pitman-Morgan Test, tests from packages BayesFactor, BEST, usage of implementation of GL Bretthorst (1993) 'On the difference in mean', using e.g. original data from Bland-Altman |
| ptII_quan_classicstats_equivalentmethods_helpfuncs.r | chap. 4.6.8.1 - mean-difference plot after Tukey (equals Bland-Altman plot) |
| ptII_quan_classicstats_variancehomogeneity.r | chap. 4.6.8.2 - determine variance homogeneity via various tests (Levene, Breusch-Pagan, non-constant error variance, Cook and Weisberg, Koenker's studentized version of test statistic) |
| ptII_quan_classicstats_normaldist_residuals.r | chap. 4.6.9.1 - investigation of residuals from linear models for various characteristics (normality, skewness, kurtosis, various sample sizes |
| ptII_quan_classicstats_outliers-and-influentialpoints.r | chap. 4.6.9.3 - impact and consequences of influential points and outliers, calculate linear model, t-test, outlier test, correlation test, leverage plots |
| ptII_quan_classicstats_outliers-and-influentialpoints_helpfuncs.r | chap. 4.6.9.3 - function to plot outlier (with, without) via regression lines |
| **Part II - Exploratory Data Analysis** | |
| ptII_quan_EDA_intro_overviewrobust.r | chap. 5.2.-5.3. - compare median vs. mean, show robust plots of data, apply lm vs. rlm on empirical data |
| ptII_quan_EDA_intro_overviewrobust_helpfuncs.r | chap. 5.2.-5.3. - function to plot residuals (lm vs. rlm), simulate and plot median/ mean from normal distribution |
| ptII_quan_EDA_case_German-states-population.r | chap. 5.5.1 - conversion to log values lead to straight lines using a real life example from German states population characteristics, compare to classical linear model |
| ptII_quan_EDA_case_Suisse-fertility.r | chap. 5.5.2 - analyzing fertility in Swuisse between Catholics (yes, no, something else) using only descriptive plots and correlations, use subgroups to understand data |
| ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r | chap. 5.5.2 - function to plot two variables with a continuous index |
| ptII_quan_EDA_case_Anderson_iris-species-in-biology.r | chap. 5.5.3 - investigate the famous iris data with pairs, lda, and tables |
| ptII_quan_EDA_case_Titanic_death-and-dying.r | chap. 5.5.4.6 - use only descriptive statistics, tables, and plots to investigate the survivors' characteristics of the Titanic and how to survive and why, answer certain questions about the conditions of surviving or dying during the Titanic catastrophe |
| ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r | chap. 5.5.4.6 - function to plot densities for various subgroups |
| ptII_quan_EDA_case_Spain_leadership-in-education.r | chap. 5.5.5 - analyze data on leadership in Spain (educational context) with distance methods (HCA, MDS), prototype analysis, heatmap, corrgram, levelplot, and simple descriptive plots |
| ptII_quan_EDA_case_Spain_leadership-in-education_helpfuncs.r | chap. 5.5.5 - tweaked version of heatmap from package heatmap.plus (not available in R v.4) |
| ptII_quan_EDA_case_Chiro_heartrate-variability.r | chap. 5.5.6 - experiment in chiropractice about heart rate variability using linear models without any significance test, learning from sample size characteristics, use histograms and interaction plots |
| ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r | chap. 5.5.6 - function to plot interactions |
| **Part II - Bayesian Statistics** | |
| ptII_quan_Bayes_Beta-distribution.r | chap. 6.12 - calculate beta posterior from prior and likelihood (conjugation, grid approximation), influence of priors, various shapes of beta |
| ptII_quan_Bayes_find-Beta-distribution-shapeparams.r | chap. 6.12 - determine beta distribution from three quantile points with/ without optimization |
| ptII_quan_Bayes_Fisher_LadyBristol-Beta-disttribution.r | chap. 6.12 - plot prior, likelihood, and posterior of the empirical data |
| ptII_quan_Bayes_Gamma-distribution.r | chap. 6.12 - display Gamma function for different parameters |
| ptII_quan_Bayes_regularization.r | chap. 6.12 - different priors (normal with different sigmas, Cauchy, uniform) |
| ptII_quan_Bayes_Prior-Likeli-Post_relationship.r | chap. 6.12.1 - display how prior and likelihood shape the posterior, see different influences on posterior |
| ptII_quan_Bayes_MC-simulation_binom-norm.r | chap. 6.13 - simulate binormial and normal distributions |
| ptII_quan_Bayes_RandomWalk.r | chap. 6.13 -  calculate and plot a random walk as 2d + 3d |
| ptII_quan_Bayes_RandomWalk_helpfuncs.r | chap. 6.13 - function to calculate a simple random walk |
| ptII_quan_Bayes_simulate-pi.r | chap. 6.13 - apply pi simulation for varying sample sizes |
| ptII_quan_Bayes_simulate-pi_helpfuncs.r | chap. 6.13 - simple function to calculate pi and plot it (inefficient, but works) |
| ptII_quan_Bayes_problem-local-minima.r | chap. 6.13.1.4 - local minima using Himmelblau's function, plot in 3d |
| ptII_quan_Bayes_HMC.r | chap. 6.13.2.3.1 - apply Hamilton Monte Carlo (HMC) algorithm, plot and investigate (bivariate normal distribution), use packages hmclearn, rethinking, and bayesplot as well as the basic HMC algorithm (Neal, 2011, chap. 5), analyze MCMC chains via plots and typical diagnostics (e.g. Gelman, Heidelberger-Welch, effective sample size, remove burn-in, ... ), compare with MH algorithm |
| ptII_quan_Bayes_HMC_helpfuncs.r | chap. 6.13.2.3.1 - functions to simulate bivariate normal distribution (rnorm, mvrnorm), calculate gradient for HMC algorithm, simulate bivariate normal distribution via HMC after McElreath (2015), describe MCMC chains: Heidelberger-Welche diagnostics, describe development of mean and covariance, adjust limits to let a comparison value appear on a posterior plot, and simulate bivariate normal distribution via MH algorithm |
| ptII_quan_Bayes_MetropolisHastings_example-normdist.r | chap. 6.13.4.1 - simulate mean of normal distribution with Metropolis-Hastings (MH) algorithm, investigate MCMC chains using package coda, apply on empirical data, investigate acceptance/ rejection rates, compare MCMC chains, posterior predictive distribution and plots |
| ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r | chap. 6.13.4.1 - functions to perform MH algorithm to simulate normal distribution based on empirical data, priors, Gibbs sampler, plot MCMC and its parts, normal posterior predictive distribution |
| ptII_quan_Bayes_GibbsSampling_example-normdist.r | chap. 6.13.4.2 - simulate mean posterior via Gibbs sampling, analyze posterior |
| ptII_quan_Bayes_JAGS_example-norm.r | chap. 6.13.4.3 - simulate mean of a normal distribution using JAGS, investigate MCMC, compare with package Bolstad |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS.r | chap. 6.13.5 - analyze Lady Bristol's tea data with classical and Bayesian methods (analytical solution and MCMC with JAGS, package BEST, BUGS) |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r | chap. 6.13.5 - function to analyze Lady Bristol experiment with Bayes Theorem, plot successes/ failures, HDI, MAP, and run BUGs model with package RBugs from R |
| ptII_quan_Bayes_MaximumEntropy.r | chap. 6.14 - calculate and plot Boltzmann/ Shannon entropy using a (not-so-fair/ fair) coin and dice |
| ptII_quan_Bayes_MaximumEntropy_helpfuncs.r | chap. 6.14 - functions to reproduce Jaynes' (1962) analyses, entropy simulation after McElreath (2015, p.277) |
| ptII_quan_Bayes_Entropy_KullbackLeibler.r | chap. 6.14.1 - apply entropy functions on word counts |
| ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r | chap. 6.14.1 - functions to calculate H (Shannon entropy) from counts and priors or Kullback-Leibler distance| |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice.r | chap. 6.14.3 - perform and plot Jaynes' (1962) analysis on a (fair?) dice |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r | chap. 6.14.3 - functions to calculate entropy of a fair or not-so-fair coin and simulate a dice |
| ptII_quan_Bayes_case_presidential-heights.r | chap. 6.15.1 - prepare dataset, remove NAs, EDA plots, classical vs. Bayesian statistics with Bayesian binomial test, MCMC diagnostics, test difference in means via classical and Bayesian solution incl. HDI, ROPE, bayesboot, plot results |
| ptII_quan_Bayes_case_presidential-heights_helpfuncs.r | chap. 6.15.1 - tweaked function of bayes.binom.test from package BayesianFirstAid, function to calculate posterior OR/RR |
| ptII_quan_Bayes_case_startagain-successrates.r | chap. 6.15.2 - compare developments and extreme cases of successes/ failures and resulting posterior probs |
| ptII_quan_Bayes_case_startagain-successrates-longterm.r | chap. 6.15.2.1 - show that long-term analysis over all years vs. sequential analysis per year (prior becomes posterior becomes prior, etc.) leads exactly to the same concluding results within the Bayesian approach |
| ptII_quan_Bayes_case_presidential-debates.r | chap. 6.15.3 - frequentist, chi-square, power, JAGS, posterior odds, plots, MCMC brute force variants, test and plot hypotheses, grid approximation, exact tests, MAP, 2d + 3d plots of theta values over intgral, comparison of methods, using tweaked functions from appell, tolerance, bayesian2beta.r, comparison with package BayesFactor and Bretthorst approach| |
| ptII_quan_Bayes_intro-BayesTheorem_tea.r | chap. 6.2.2.4 - simple application of Bayes Theorem, using discrete values, and shift from prior knowledge to posterior to prior, etc. ie. update Bayes Theorem with new information |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r | chap. 6.2.3.4 - apply Bayes Theorem on medical tests using prevalence (of a disease in population), sensitivity (1-false_positives), and specifity (1-false_negatives) |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r | chap. 6.2.3.4 - function to apply Bayes Theorem for medical tests using prevalence p(A), sensitivity p(B|A), and specifitiy p(not-B|not-A)| |
| ptII_quan_Bayes_intro-BayesTheorem_covid19-test.r | chap. 6.2.4 - calculate probability of a covid-19 test in relation to population characteristics |
| ptII_quan_Bayes_posterior.r | chap. 6.5.1 - analyse posterior and MCMC chains (convergence), apply on empirical data set |
| ptII_quan_Bayes_simple-estimation-mean-post.r | chap. 6.5.1 - posterior distribution of a mean with different informed priors using package Bolstad |
| ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r | chap. 6.5.1 - function to plot mean posterior with prior and likelihood, tweaked normgcp from package Bolstadt (bug fix) |
| ptII_quan_Bayes_BayesFactors_test-hypos.r | chap. 6.7.1 - empirical example from Kruschke's remarks (BFs are not enough to interpret results), better is to use priors and posterior probs, example: huge BF but roughly same parameters of the linear model, BF only caused by different priors |
| ptII_quan_Bayes_lossfun_startagain.r | chap. 6.7.1.2 - show loss function in the context of an empirical example (success rates in drug therapy) to draw conclusions for actual work |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r | chap. 6.7.1.3 - compare and simulate Bayes Factors (BFs) for various sample sizes, t-values, determine minimal BF with different approaches |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r | chap. 6.7.1.3 - function to simulate BFs in relation to N, convert p-values to BFs, relate p-values and sample sizes |
| ptII_quan_Bayes_case_exp-extra-sensual-perception.r | chap. 6.7.1.4 - function to calculate t-test with BF, then prepare, describe, analyze empirical data using methods for binomial data and difference in means (classical and Bayesian solution), anova with repeated measurement vs. brms |
| ptII_quan_Bayes_Bem-study-aspects.r | chap. 6.7.1.5 - some artificial but meaningful calculations along with a very critical discussion of the theoretical background and design of the Bem study on clairvoyanca |
| ptII_quan_Bayes_BayesFactors_further-remarks.r | chap. 6.7.1.6 - apply BFs in the context of EDA plots, t-tests, Cohen's d, linear models, and a real full Bayesian approach with brms, and other criteria (Bayes R^2, loo, waic, kfold), different results if a categorial variable is made continuous (ie. converted back to its natural original state like categorial age) |
| ptII_quan_Bayes_information-criteria.r | chap. 6.7.2 - apply function to get information criteria from a linear model (empirical example) |
| ptII_quan_Bayes_information-criteria_helpfuncs.r | chap. 6.7.2 - function to calculate and extract information criteria from linear models |
| ptII_quan_Bayes_over-and-underfitting.r | chap. 6.7.3 - examples to show under- and overfitting incl. polynomials, use Kullback-Leibler (KL) divergence between probs |
| ptII_quan_Bayes_HDI.r | chap. 6.7.4.1 - plot HDI and CI for various data sets to demonstrate the qualitative difference of those interval types |
| ptII_quan_Bayes_HDI_helpfuncs.r | chap. 6.7.4.1 - function to plot HDI vs. symmetric CI |
| ptII_quan_Bayes_ROPE-BayesFactor.r | chap. 6.7.4.2 - show importance of ROPE vs. simple clear-cut for decision-making (difference in means). plot with HDI, Bayesian binomial test for various values, priors, and hypotheses, priors and their influence on BFs, apply on empirical data, apply and plot with package BEST, analyze MCMC and posterior probs |
| ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r | chap. 6.7.4.2 - function to demonstrate change in BF due to prior believes |
| ptII_quan_Bayes_PPC_model-check.r | chap. 6.7.4.4 - simulate PPC via JAGS using successes/ failures and plot it |
| ptII_quan_Bayes_PPC_model-check_helpfuncs.r | chap. 6.7.4.4 - function to calulate one-/two-sided test (PPC) |
| ptII_quan_Bayes_PPC_model-check-graph.r | chap. 6.7.4.5 - demonstrate PPC for homogenous variances (yes/no), plot and test specific hypotheses (classical, Bayesian statistics with brms), treatment/ control group design, compare linear models |
| ptII_quan_Bayes_case_wordcounts-PPC.r | chap. 6.7.4.6 - perform analysis (e.g. with JAGS) and plots along with posterior predictive checks (PPC) incl. diagnostic MCMC plots, bootstrap using full cases or real values, exercise Kruschke (2014, chap 18.3) regarding heteroscedasticity (influence of un-/equal variances between groups) |
| ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r | chap. 6.7.4.6 - functions to prepare and summarize MCMC chains and posteriors from Kruschke (2014) scripts to match requirements here for PPC |
| **Part III - Qualitative Data Analysis** | |
| ptIII_qual_code-paradigm_table-analysis.r | chap. 9.4 - example for usage of expand.grid |
| ptIII_qual_quan-textanalysis.r | chap. 10.1 - prepare (e.g. removal of redundant parts, special characters, white space, punctuations, stop words and convert to lower cases, split text, word cloud, plot frequencies, KWIC, collocation, ...) and analyze text using packages stringi, SnowbalC, tm, magrittr, quanteda, corps2, etc., use wordstems, corpus inspection, ... |
| **Part IV - Qualitative Comparative Analysis** | |
| ptIV_qual_Boole_basics.r | chap. 12.1 - simple true/ false statements |
| ptIV_qual_Boole_case_Krook_women-in-parliament.r | chap. 12.11.1 - analyse data set from Krook (2010) using QCA, extract prime implicants, check for consistencies, prepare positive and negative outcome for general discussion| |
| ptIV_qual_Boole_case_Titanic_death-and-dying.r | chap. 12.11.2 - apply QCA on Titanic data set to investigate a minimal set for survival (yes/ no) |
| ptIV_qual_Boole_logical-minimization.r | chap. 12.3 - single case demonstration how Boolean minimization actually works |
| ptIV_qual_Boole_case_school-success.r | chap. 12.6 - artificial data set to demonstrate QCA for negative and positive outcome |
| ptIV_qual_Boole_fuzzy-logic.r | chap. 12.7 - simple plot to demonstrate what fuzzy logic is |
| **Part V** | **Mixed Methods** |
| ptV_mixed_prime-numbers.r | chap. 13.3.1 - one-liner to get prime numbers using Euclidean mod division |

</details>

## Enlaces en el código R

Es imposible escribir cualquier código R sin modelos externos y código tomado, prestado, aprendido, etc. de otras personas. Para dar respeto a ese hecho y a los autores asociados y permitir profundizar en tal o cual comprensión de R o de cualquier otro tema del script, en algunos puntos seleccionados los scripts contienen URLs a páginas web externas que pueden ser interesantes para el lector o para la práctica de R. No podemos garantizar que esos enlaces sigan existiendo, porque el internet cambia demasiado rápido. Lo hacían cuando se escribieron los scripts.

## Conjuntos de datos

Debido a cuestiones legales y de licencia, no todos los conjuntos de datos tratados en el libro pueden publicarse también aquí. Los conjuntos de datos deben estar ubicados en la misma carpeta que los scripts de R para cargarlos correctamente. Algunos conjuntos de datos se toman directamente de R y por lo tanto ya están incluidos en R. Los siguientes conjuntos de datos se añaden externamente y se encuentran en las siguientes carpetas debajo de 'data' de este repositorio:

| Conjunto de datos | Descripción |
| --- | --- |
| `AAH` | Datos de un estudio de investigación sobre el aprendizaje colaborativo (Huber 2007). |
| `LG` | Datos de un estudio de investigación sobre el uso del recuento de palabras (Guertler 2005) y un pequeño experimento sobre la clarivi-dencia para demostrar algo que no muestra ningún tipo de efecto empírico (sin publicar). |
| `school_success` | Tabla lógica sobre el éxito escolar (datos ficticios creados sólo para demostración) |
| `Spain-edu` | Datos de una investigación española sobre liderazgo en educación (Huber, Guertler & Gento 2018)
startagain_appl-letter |
| `startagain_appl-letter-addiction` | Carta de solicitud de adicción para una plaza de tratamiento en rehabilitación farmacoterapéutica escrita desde desintoxicación en psiquiatría (originalmente enviada por fax, los nombres y lugares están totalmente anonimizados, véase Studer 1998) |
| `startagain_successrates` | Tasas de éxito para pasar a través de un programa de rehabilitación de drogas en el centro de terapia de drogas Suisse empezar de nuevo entre 1992-2017 |
| `Titanic_survivors` | Datos conocidos de los pasajeros del Titanic junto con algunas de sus características |
| `wikipedia_presidential-heights` | Datos de wikipedia sobre la relación de las alturas corporales de los candidatos a la presidencia de EE.UU. y los ganadores posteriores (presidentes) |

## Conjuntos de datos externos

Como se ha mencionado anteriormente, algunos conjuntos de datos utilizados se toman directamente de R, como el conjunto de datos de **bupa** o el de Dobson (1990) sobre datos de **peso de plantas**, el famoso conjunto de datos de **iris**, etc. (véase el libro para más referencias). Otros conjuntos, como los datos sobre **delincuencia**, proceden de fuentes externas (por ejemplo, UCLA) y otros no están publicados debido a la falta de una licencia necesaria para el acceso público (por ejemplo, los datos sobre **mujeres en el parlamento** de Krook 2010).

## Código R externo

Parte del código R no se tomó de paquetes R, sino de diversos lugares de la red. A partir de esa selección, algunos scripts se modificaron para adaptarlos a nuestras necesidades. Estos incidentes se señalan en los scripts de R en cada lugar donde se utilizó o modificó un script externo. En su mayoría, esos scripts se originan en:

- `bayesian2beta.r` (Sverdlov O, Ryeznik Y & Wu S. (2015). Exact Bayesian Inference Comparing Binomial Proportions, With Application to Proof-of-Concept Clinical Trials. _Therapeutic Innovation & Regulatory Science_, _49_(1), p.163-174.) Puede descargarse gratuitamente como [material complementario](https://link.springer.com/article/10.1177/2168479014547420#SecESM1)
- `DBDA2E-utilities.R` and `Jags-Ymet-XmetMulti-Mrobust.R` (Kruschke, J (2014). _Doing Bayesian Data Analysis. 2nd ed._ Academic Press.) Los scripts pueden descargarse gratuitamente en [la página del libro](https://sites.google.com/site/doingbayesiandataanalysis/software-installation) del autor
- El paquete `BayesianFirstAid` ([Bååth](https://github.com/rasmusab/bayesian_first_aid))

El archivo *.r correspondiente contiene la URL desde la que puede descargar los scripts R externos. Descárguelo y coloque el/los archivo(s) en la carpeta principal. No cambie el nombre del archivo para evitar cualquier error al cargarlo y si la descarga crea un nombre de archivo diferente, utilice el indicado anteriormente. El código R externo se utiliza de la siguiente manera como (partes de) funciones R:

- Paquetes de R y funciones de R que se modifican para satisfacer nuestras necesidades aquí (por ejemplo, algunas funciones desaparecieron de una versión de R a la siguiente como el código de [heatmap.plus](https://github.com/cran/heatmap.plus),  [sjstats](https://github.com/strengejacke/sjstats), [rhmc](https://github.com/cran/rhmc), ...).
- Algunos códigos de R (por ejemplo, de [Bolstad](https://github.com/cran/Bolstad)) se modificaron ligeramente debido a errores en el código original en ese momento. Esto puede ser diferente ahora, es decir, el error está corregido, pero no lo estaba en el momento en que se escribieron los scripts R.
- Algunos otros códigos de R simplemente siguen artículos y pueden considerarse como una implementación de fórmulas y algoritmos (por ejemplo, sobre p-hacking, curvas z, etc.). Esto se menciona con detalle en el propio libro.

De vez en cuando se pone algo de código R en secciones como

```
### not run
...
### end of not run
```

Este código R es opcional o a veces no está totalmente relacionado con el libro o simplemente ofrece otra perspectiva (tal vez incluso redundante). Esto ocurre, por ejemplo, al investigar las características de supervivencia del Titanic con un montón de gráficos y tablas. Demasiadas perspectivas, así que uno tiene que centrarse en unos pocos puntos.

## Versión R

Todos los scripts de R fueron escritos y probados con R v3.4/ v3.6. Deberían funcionar con versiones posteriores de R. Deberían funcionar también con versiones posteriores de R >= v4. Sin embargo, a veces los paquetes ya no se mantienen y luego son eliminados del repositorio oficial de R o algo cambia tan fuertemente en un paquete que las funciones anteriores ya no existen o al menos no en la forma en que deberían y se utilizan aquí. Esto ocurre cuando los nombres de los objetos, las estructuras internas, etc. quedan obsoletos. Por lo tanto, uno puede crear una máquina virtual o un motor docker, instalar R v3.4/ v3.6 y todo debería funcionar bien independientemente de usar Linux o Windows. Todos los scripts fueron desarrollados bajo Linux, pero también probados bajo win7 y deberían funcionar con win10 o win11. En el futuro vamos a proporcionar un archivo docker + imagen que se puede utilizar para ejecutar el código a través del servidor RStudio libre. Aquellos que estén interesados pueden enviar una nota para obtener la versión más reciente del archivo docker que todavía está en desarrollo. El libro contiene al final una lista de todas las bibliotecas de R y sus versiones. Sin embargo, la mayoría de las cosas deberían funcionar sin más, las notas anteriores se refieren a casos especiales de ciertas librerías requeridas y posteriormente eliminadas del repositorio de R porque los mantenedores de esos paquetes no actualizaron a versiones más recientes de R.

## Bibliotecas R

Muchos scripts requieren paquetes R externos y bibliotecas externas instaladas en el sistema operativo (Linux, ...), especialmente si se trata de compilación en el contexto de modelos lineales bayesianos con el paquete Bayes brms. A veces requieren, especialmente en Linux, la compilación de bibliotecas. Tales compilaciones en Linux deben hacerse directamente ejecutando R desde la línea de comandos y no (¡nunca!) mediante el uso de alguna GUI como RStudio (¡especialmente esta!), porque muchas experiencias mostraron que la compilación tiende a romperse y falla por razones desconocidas si se utiliza una GUI. La compilación directa vía R en la línea de comandos funciona bastante bien siempre y cuando las librerías (de desarrollo) necesarias estén instaladas en el sistema. Si falta una librería en el sistema operativo, R nos dice normalmente qué es lo que falta y te da una pista de cómo instalarla (por ejemplo, usando apt). Después, la GUI se puede utilizar de nuevo sin ningún problema. En Windows, la mayoría de las bibliotecas no requieren compilación. Los paquetes compilados o instalados desde la línea de comandos y no a través del sistema de paquetes del SO (usando debs/ rpms/ etc.) pueden estar disponibles para todos los usuarios locales. Para lograrlo, inicie R como root e instale todos los paquetes necesarios manualmente a través de la línea de comandos.

## Errores en el código de R

Si uno encuentra un error, por favor, contáctenos con un ejemplo corto y claro para que podamos intentar reproducirlo y corregir el error.

## Uso del código R

Es importante entender que a veces para demostrar o explorar un tema el código R no es perfecto y se revisa unas líneas más tarde o se hace de otra manera. No todas las interrupciones del script son errores, a veces son características del libro. Esos incidentes no son por tanto bugs (serios) sino hechos intencionadamente con fines educativos para mostrar que las cosas evolucionan y no ocurren accidentalmente. El trabajo estadístico significa (¡idealmente!) un progreso lento y constante hacia un objetivo que a veces incluso cambia. En general, el código R se ajusta a las tareas (por ejemplo, conjuntos de datos) y a los temas específicos del libro y sus capítulos. No es una especie de paquete general de R y no intenta serlo. El código R no cumple los requisitos de los paquetes R tradicionales, como ser útil como biblioteca universal. No obstante, algunas de las funciones pueden ser útiles en diversos contextos cambiantes y pueden utilizarse independientemente del libro, los conjuntos de datos, etc.

Si encuentra algo útil, modifíquelo según sus necesidades. Nosotros hacemos lo mismo.

## Archivos AQUAD 7

Un conjunto de datos (carta de solicitud de una plaza de tratamiento en un centro de rehabilitación de drogodependientes) se toma del análisis cualitativo de datos junto con el binario de Windows de [AQUAD 7](https://www.aquad.de), un software QDA gratuito y de código abierto escrito y mantenido por G.L. Huber. La carpeta AQUAD7 contiene todos los archivos de trabajo junto con la versión binaria AQUAD7 más antigua que se utilizó originalmente para el análisis. AQUAD está ahora en la v8.

## Licencias y créditos

El código R está licenciado bajo [GNU GPL v3](https://www.gnu.org/licenses/gpl-3.0.en.html). Por favor, siéntase libre de usar, modificar o compartir el código si le resulta útil.
Las funciones tomadas de otros paquetes tienen sus propias licencias. Los detalles pueden encontrarse en el archivo de licencia de los paquetes y scripts citados y referenciados.
El binario AQUAD7 aquad7_170117.exe tiene una (C) de G.L. Huber (2017). La versión más reciente de [AQUAD 8](https://www.aqua.de) se puede encontrar también libremente en la página web de AQUAD.

## Referencias

- Bretthorst, GL (1993). [On the difference in means](https://bayes.wustl.edu/glb/diff.pdf). In: WT Grandy and PW Milonni (Eds.), _Physics & Probability Essays in honor of ET Jaynes_. Cambridge University Press, England.
- Coghlan, A (2017-11-07). [A little book of R for Bayesian Statistics. Release 0.1](https://a-little-book-of-r-for-bayesian-statistics.readthedocs.io/en/latest/src/bayesianstats.html)
- Dobson, A (1990). _An Introduction to Generalized Linear Models._ Chapman & Hall/ CRC Texts in Sstatistical Science.
- Gregory, P. (2005). _Bayesian logical data analysis for the physical sciences. A comparative approach with Mathematica support._ Cambridge University Press. [Mathematica code](https://www.cambridge.org/nl/academic/subjects/statistics-probability/statistics-physical-sciences-and-engineering/bayesian-logical-data-analysis-physical-sciences-comparative-approach-mathematica-support)
- Guertler, L (2005). _Die Rekonstruktion von Innensicht und Aussensicht humorvollen Handelns in Schule und Erwachsenenbildung. Die Bewältigung der Katastrophe —Vipassanā-Meditation und Humor_. Berlin: Logos. [Book page](https://www.logos-verlag.de/cgi-bin/engbuchmid?isbn=1094&lng=deu&id=)
- Huber, AA (2007). _Wechselseitiges Lehren und Lernen (WELL) als spezielle Formen Kooperativen Lernens._ Berlin: Logos. [Book page](https://www.logos-verlag.de/cgi-bin/engbuchmid?isbn=1502&lng=deu&id=)
- Huber, GL, Guertler, L & Gento, S (2018). _La aportación de la estadística exploratoria al análisis de datos cualitativos._ In: _Perspectiva Educacional. Formación de Profesores_, 57(1), S. 50–69.
- Krook, ML (2010). [Women's Representation in Parliament: A Qualitative Comparative Analysis.](http://mlkrook.org/pdf/Krook_PS_2010.pdf) _Political Studies_, _58_, p.886-908.
- Kruschke, J (2014). _Doing Bayesian Data Analysis: A Tutorial with R, JAGS, and Stan. 2nd ed._ Academic Press. [Book page](https://sites.google.com/site/doingbayesiandataanalysis/)
- McElreath, R (2015). _Statistical Rethinking. A Bayesian Course with Examples in R and Stan. 1st ed._ Chapman & Hall/ CRC Texts in Sstatistical Science. [Book page](https://xcelab.net/rm/statistical-rethinking/)
- Neal, RM (2011). [Handbook of Markov Chain Monte Carlo.](https://arxiv.org/pdf/1206.1901.pdf)_ Edited by S Brooks, A Gelman, G Jones, and X-L Meng. Chapman & Hall/ CRC Press.
- Studer, U.M. (1998). _[Verlangen, Süchtigkeit und Tiefensystemik. Fallstudie des Suchttherapiezentrums für Drogensüchtige start again in Männedorf und Zürich von 1992 bis 1998. Bericht an das Bundesamt für Justiz (BAJ)](https://www.bj.admin.ch/dam/data/bj/sicherheit/smv/modellversuche/evaluationsberichte/37.pdf)._ Zürich.

## Cita

Si alguna vez hace referencia a cualquier parte del código R, por favor cítelo como:

Guertler, L (2023). R code supplement to Guertler & Huber (2023). Mixed Methods --- quantitativ, qualitativ, explorativ und logisch in Theorie und Anwendung. Objektive Ansichten und subjektive Analysen (Combinación de Métodos - CUAL, CUAN Y LÓGICA. Puntos de vista objetivos y observaciones subjetivas). Código R publicado en [Github](https://github.com/abcnorio/mixedmethod-rcode) y [OSDN](https://osdn.net/projects/mixedmethod-rcode).

## Descargo de responsabilidad

Aunque todos los scripts de R se probaron exhaustivamente en diversas condiciones, no podemos descartar posibles errores. Por lo tanto, no garantizamos nada, sino que aconsejamos a los usuarios que utilicen su sentido común junto con su inteligencia y experiencia para saber si un resultado tiene sentido y está bien hecho o no. Algunos códigos y ejemplos de R sólo tienen sentido en combinación con el libro, porque las pocas notas del código no son suficientes. Sin embargo, se proporciona "tal cual".
Por lo tanto, utilice el sentido común para comparar los resultados con las expectativas. Aquí no hay GARANTÍA de ningún tipo. No hay garantía de que el software esté libre de errores o sea coherente con cualquier norma o incluso cumpla sus requisitos. No utilice el software ni confíe en él para resolver problemas si los resultados incorrectos pueden llevar a herir o dañar a seres vivos de cualquier tipo o si pueden llevar a la pérdida de bienes o a cualquier otro posible daño al mundo, a los seres vivos, a la materia no viva o a la sociedad como tal. Si utiliza el software de tal manera, lo hará por su cuenta y riesgo.


