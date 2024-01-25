# Suplemento en código R al libro "Combinación de Métodos - CUAL, CUAN Y LÓGICA. Puntos de vista objetivos y observaciones subjetivas". Escrito por Guertler & Huber (2023).

## Recursos e idiomas

La versión española del libro se titula "Combinación de Métodos - CUAL, CUAN Y LÓGICA. Puntos de vista objetivos y observaciones subjetivas". Ambos libros pueden obtenerse gratuitamente en [OSF](https://osf.io/69gfz) o [AQUAD](https://www.aquad.de.

## Visión general

El tema principal del libro es la metodología mixta, especialmente en lo que se refiere al análisis de datos. Aunque el libro está escrito en alemán (idioma original) y traducido al español, el código R se puede utilizar sin conocimientos de alemán. Las notas y comentarios en los guiones están escritos en inglés. El código en sí no está organizado como un paquete R y esa no es la intención aquí. Sin embargo, algunas funciones pueden ser útiles para tal o cual propósito. Los comentarios aquí y allá en los scripts deberían ayudar a entender el objetivo principal si el libro no está disponible o no se puede entender.

## Antecedentes

El libro trata de metodología mixta (cuantitativa, cualitativa y lógica booleana) y se publica en 2023 libremente en varias plataformas de bibliotecas abiertas como [OSF](https://osf.io/69gfz) o [AQUAD](https://www.aquad.de. La parte estadística abarca la estadística clásica (Fisher, Neyman-Pearson), el análisis exploratorio de datos sensu J.W. Tukey, así como la estadística bayesiana. La lógica booleana se utiliza para el análisis implícito del análisis comparativo cualitativo. La parte cualitativa abarca tanto el análisis cuantitativo como el cualitativo de textos. Este último utiliza el paradigma de codificación y análisis secuencial procedente de la Hermenéutica Objetiva sensu Oevermann y colegas y es el único análisis para el que el código R realmente no tiene ningún sentido. Sin embargo, el análisis secuencial asistido por ordenador está disponible en el software gratuito QDA [AQUAD](https://www.aquad.de.

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

| Nombre del script | Cáp. | SubCáp. | Título del Cápitulo |
| --- | --- | --- | --- |
| DiM_Bretthorst_PG.r | - | - | funciones generales |
| DiM_Bretthorst_PG_calls.r | - | - | ejemplos de llamadas |
| DiM_Bretthorst_UMS.r | - | - | funciones generales |
| DiM_Bretthorst_UMS_calls.r | - | - | ejemplos de llamadas |
| **Funciones externales** |  |  |  |
| EXT_bayesian2beta.r | - | - | funciones generales |
| EXT_DBDA2E-utilities.R | - | - | funciones generales |
| EXT_Jags-Ymet-XmetMulti-Mrobust.R | - | - | funciones generales |
| **Todas partes** |  |  | |
| ptall_generalfuncs_Bayes_Beta_determine.r | - | - | funciones generales |
| ptall_generalfuncs_Bayes_binomial-prop-test.r | - | - | funciones generales |
| ptall_generalfuncs_Bayes_binomial.r | - | - | funciones generales |
| ptall_generalfuncs_brob-integral.r | - | - | funciones generales |
| ptall_generalfuncs.r | - | - | funciones generales |
| **Parte I - Teoría científica** |  |  | |
| ptI_sciencetheory_logic.r | 2 | 2.2 | **La conclusión deductiva** |
| **Parte II - Estadística clásica** |  |  | |
| ptII_quan_classicstats_Fisher_ladyteataste.r | 4 | 4.3.2.1 | Beber té y reconocer la leche — un ejemplo de experimento según Fisher |
| ptII_quan_classicstats_N-P_powerfunc.r | 4 | 4.3.3.1 | Relevancia práctica en el contexto de la relevancia estadística |
| ptII_quan_classicstats_N-P_stat-signif-isNOT-practsignif.r | 4 | 4.3.3.1 | Relevancia práctica en el contexto de la relevancia estadística |
| ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r | 4 | 4.3.3.2 | Dirección y tamaño: dos tipos de error subestimados |
| ptII_quan_classicstats_GandC_type-S-M-error.r | 4 | 4.3.2.2 | Dirección y tamaño: dos tipos de error subestimados |
| ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r | 4 | 4.3.3.5 | Contabilidad de costes totales |
| ptII_quan_classicstats_N-P_nulldist-hypotest.r | 4 | 4.3.3.5 | Contabilidad de costes totales |
| ptII_quan_classicstats_N-P_confint_bayesboot.r | 4 | 4.3.3.6 | Intervalos de confianza |
| ptII_quan_classicstats_N-P_confint_errorbars.r | 4 | 4.3.3.6 | Intervalos de confianza |
| ptII_quan_classicstats_N-P_confint_helpfuncs.r | 4 | 4.3.3.6 | Intervalos de confianza |
| ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r | 4 | 4.3.3.6 | Intervalos de confianza |
| ptII_quan_classicstats_N-P_confint_p-t-value.r | 4 | 4.3.3.6 | Intervalos de confianza |
| ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r | 4 | 4.3.3.6 | Intervalos de confianza |
| ptII_quan_classicstats_N-P_confint.r | 4 | 4.3.3.6 | Intervalos de confianza |
| ptII_quan_classicstats_N-P_simulation.r | 4 | 4.3.5 | Excursus - Simulaciones |
| ptII_quan_classicstats_N-P_example-soccer-sim_helpfuncs.r | 4 | 4.3.5.1 | Simulación de caso - Coleccionables de fútbol |
| ptII_quan_classicstats_N-P_example-soccer-sim.r | 4 | 4.3.5.1 | Simulación de caso - Coleccionables de fútbol |
| ptII_quan_classicstats_N-P_SE-N-dep.r | 4 | 4.3.6 | Tamaño de la muestra |
| ptII_quan_classicstats_nullritual.r | 4 | 4.3.8 | El procedimiento de una prueba estadística: el ritual nulo |
| ptII_quan_classicstats_NHST_nulldist.r | 4 | 4.3.9 | La probabilidad de los datos como base de las decisiones sobre las pruebas |
| ptII_quan_classicstats_pvalue-as-base.r | 4 | 4.3.9.1 | Cálculo del valor p |
| ptII_quan_classicstats_normal-vs-t.r | 4 | 4.3.9.2 | Sobre la relación entre la distribución normal y la distribución t |
| ptII_quan_classicstats_centrallimittheorem_helpfuncs.r | 4 | 4.3.9.3 |  Excursus --- Teorema del límite central |
| ptII_quan_classicstats_centrallimittheorem.r | 4 | 4.3.9.3 | Excursus --- Teorema del límite central |
| ptII_quan_classicstats_p-t-df-relationship.r | 4 | 4.3.10 | Epistemología recargada: la estadística clásica |
| ptII_quan_classicstats_effectsizes_helpfuncs_sjstats.r | 4 | 4.4.13 | Tamaño del efecto, frecuencia y relación con la escala original |
| ptII_quan_classicstats_effectsizes_helpfuncs.r | 4 | 4.4.13 | Tamaño del efecto, frecuencia y relación con la escala original |
| ptII_quan_classicstats_effectsizes.r | 4 | 4.4.13 | Tamaño del efecto, frecuencia y relación con la escala original |
| ptII_quan_classicstats_Simpsonparadox_helpfuncs.r | 4 | 4.4.14.1 | La paradoja de Simpson |
| ptII_quan_classicstats_Simpsonparadox.r | 4 | 4.4.14.1 | La paradoja de Simpson |
| ptII_quan_classicstats_JeffreysLindleyparadox.r | 4 | 4.4.14.2 | La paradoja de Jeffreys-Lindley |
| ptII_quan_classicstats_p-hacking-sim_helpfuncs.r | 4 | 4.4.2 | En busca de significados - intenciones de investigación inconscientes y p-hacking |
| ptII_quan_classicstats_p-hacking-sim.r | 4 | 4.4.2 | En busca de significados - intenciones de investigación inconscientes y p-hacking |
| ptII_quan_classicstats_dealingwithpower.r | 4 | 4.4.3 | La gestión del poder |
| ptII_quan_classicstats_R-index_z-curve_helpfuncs.r | 4 | 4.4.3.1 | R-Índice |
| ptII_quan_classicstats_R-index_z-curve.r | 4 | 4.4.3.1 | R-Índice |
| ptII_quan_classicstats_varianceestimation_helpfuncs.r | 4 | 4.4.5 | (auto-)engaños |
| ptII_quan_classicstats_varianceestimation.r | 4 | 4.4.5 | (auto-)engaños |
| ptII_quan_classicstats_randomization.r | 4 | 4.4.7 | Aleatorización |
| ptII_quan_classicstats_missingdata.r | 4 | 4.4.8 | Datos faltantes |
| ptII_quan_classicstats_equivalentmethods_helpfuncs.r | 4 | 4.4.9.1 | Equivalencia de procedimientos y métodos de medición |
| ptII_quan_classicstats_equivalentmethods.r | 4 | 4.4.9.1 | Equivalencia de procedimientos y métodos de medición |
| ptII_quan_classicstats_normaldist_residuals.r | 4 | 4.4.10 | Distribución normal de los residuos |
| ptII_quan_classicstats_variancehomogeneity.r | 4 | 4.4.11 | Homogeneidad de la varianza (homocedasticidad) |
| ptII_quan_classicstats_outliers-and-influentialpoints_helpfuncs.r | 4 | 4.4.12 | Valores atípicos y datos influyentes |
| ptII_quan_classicstats_outliers-and-influentialpoints.r | 4 | 4.4.12 | Valores atípicos y datos influyentes |
| **Parte II - Análisis Exploratorio de Datos(AED) sensu Tukey** |  |  |  |
| ptII_quan_EDA_intro_overviewrobust_helpfuncs.r | 5 | 5.2 | Procedimientos AED típicos en R |
| ptII_quan_EDA_intro_overviewrobust.r | 5 | 5.2 | Procedimientos AED típicos en R |
| ptII_quan_EDA_case_German-states-population.r | 5 | 5.5.1 | La población en una comparación de los estados federales de Alemania |
| ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r | 5 | 5.5.2 | Fecundidad y fertilidad |
| ptII_quan_EDA_case_Suisse-fertility.r | 5 | 5.5.2 | Fecundidad y fertilidad |
| ptII_quan_EDA_case_Anderson_iris-species-in-biology.r | 5 | 5.5.3 |  Distinción de especies en biología |
| ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r | 5 | 5.5.4 | Vivir y morir a bordo del Titanic |
| ptII_quan_EDA_case_Titanic_death-and-dying.r | 5 | 5.5.4 | Vivir y morir a bordo del Titanic |
| ptII_quan_EDA_case_Spain_leadership-in-education_helpfuncs.r | 5 | 5.5.5 | 5.5.5 Liderazgo en contextos educativos |
| ptII_quan_EDA_case_Spain_leadership-in-education.r | 5 | 5.5.5 | Liderazgo en contextos educativos |
| ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r | 5 | 5.5.6 | Un experimento sobre la variabilidad del ritmo cardíaco |
| ptII_quan_EDA_case_Chiro_heartrate-variability.r | 5 | 5.5.6 | Un experimento sobre la variabilidad del ritmo cardíaco |
| **Parte II - Estadística bayesiana** |  |  | |
| ptII_quan_Bayes_Beta-distribution.r | 6 | 6.12 | Die Wahl priorer Verteilungen |
| ptII_quan_Bayes_find-Beta-distribution-shapeparams.r | 6 | 6.12 | Elección de distribuciones a priori |
| ptII_quan_Bayes_Fisher_LadyBristol-Beta-disttribution.r | 6 | 6.12 | Elección de distribuciones a priori |
| ptII_quan_Bayes_Gamma-distribution.r | 6 | 6.12 | Elección de distribuciones a priori |
| ptII_quan_Bayes_regularization.r | 6 | 6.12 | Elección de distribuciones a priori |
| ptII_quan_Bayes_Prior-Likeli-Post_relationship.r | 6 | 6.12.2 |  Relación Prior-Likelihood-Posterior |
| ptII_quan_Bayes_MC-simulation_binom-norm.r | 6 | 6.13 | Simulaciones Markov Chain Monte Carlo – MCMC |
| ptII_quan_Bayes_RandomWalk_helpfuncs.r | 6 | 6.13 | Simulaciones Markov Chain Monte Carlo – MCMC |
| ptII_quan_Bayes_RandomWalk.r | 6 | 6.13 | Simulaciones Markov Chain Monte Carlo – MCMC |
| ptII_quan_Bayes_simulate-pi_helpfuncs.r | 6 | 6.13 | Simulaciones Markov Chain Monte Carlo – MCMC |
| ptII_quan_Bayes_simulate-pi.r | 6 | 6.13 | Simulaciones Markov Chain Monte Carlo – MCMC |
| ptII_quan_simulate-pi_helpfuncs.r | 6 | 6.13 | Simulaciones Markov Chain Monte Carlo – MCMC |
| ptII_quan_simulate-pi.r | 6 | 6.13 | Simulaciones Markov Chain Monte Carlo – MCMC |
| ptII_quan_Bayes_problem-local-minima.r | 6 | 6.13.1.4 | Resumen de los algoritmos MCMC |
| ptII_quan_Bayes_GibbsSampling_example-normdist.r | 6 | 6.13.2.2 | Muestreo de Gibbs en R |
| ptII_quan_Bayes_JAGS_example-norm.r | 6 | 6.13.2.2.1 | Muestreo de Gibbs en JAGS |
| ptII_quan_Bayes_HMC_helpfuncs.r | 6 | 6.13.2.3.1 | Hamilton Monte Carlo en R |
| ptII_quan_Bayes_HMC.r | 6 | 6.13.2.3.1 | Hamilton Monte Carlo en R |
| ptII_quan_Bayes_MetropolisHastings_example-normdist.r | 6 | 6.13.4.1 | El algoritmo Metropolis-Hastings en R |
| ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r | 6 | 6.13.4.1 | El algoritmo Metropolis-Hastings en R |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r | 6 | 6.13.5 | Fisher reloaded — más té |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS.r | 6 | 6.13.5 | Fisher reloaded — más té |
| ptII_quan_Bayes_MaximumEntropy_helpfuncs.r | 6 | 6.14 | Entropía máxima |
| ptII_quan_Bayes_MaximumEntropy.r | 6 | 6.14 | Entropía máxima |
| ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r | 6 | 6.14.3 | Estudio de caso: "Yo, nosotros y la nación" – autoanuncio presidencial |
| ptII_quan_Bayes_Entropy_KullbackLeibler.r | 6 | 6.14.3 | Estudio de caso: "Yo, nosotros y la nación" – autoanuncio presidencial |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r | 6 | 6.14.5 | El clásico: ¿es justo un dado? |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice.r | 6 | 6.14.5 | El clásico: ¿es justo un dado? |
| ptII_quan_Bayes_case_presidential-heights_helpfuncs.r | 6 | 6.15.1 | Alturas de presidentes |
| ptII_quan_Bayes_case_presidential-heights.r | 6 | 6.15.1 | Alturas de presidentes |
| ptII_quan_Bayes_case_startagain-successrates.r | 6 | 6.15.2 | Índices de aprobados en el tratamiento de drogodepencia |
| ptII_quan_Bayes_case_startagain-successrates-longterm.r | 6 | 6.15.2.1 | Evaluación a largo plazo (Índices de aprobados) |
| ptII_quan_Bayes_case_presidential-debates.r | 6 | 6.15.3 | "Yo, nosotros y la nación" - autoanuncio presidencial parte 2 |
| ptII_quan_Bayes_intro-BayesTheorem_tea.r | 6 | 6.2.2.4 | [6.2.2 Ejejmplo de caso — un experimento más de té] |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r | 6 | 6.2.3.4 | [6.2.3 Ejejmplo de caso — diagnóstico médico] |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r | 6 | 6.2.3.4 | [6.2.3 Ejejmplo de caso — diagnóstico médico] |
| ptII_quan_Bayes_intro-BayesTheorem_covid19-test.r | 6 | 6.2.4 | Ejejmplo de caso — Fiabilidad de una prueba COVID-19 |
| ptII_quan_Bayes_posterior.r | 6 | 6.6.1 | Comprensión intuitiva de la probabilidad |
| ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r | 6 | 6.6.1 | Comprensión intuitiva de la probabilidad |
| ptII_quan_Bayes_simple-estimation-mean-post.r | 6 | 6.6.1 | Comprensión intuitiva de la probabilidad |
| ptII_quan_Bayes_BayesFactors_test-hypos.r | 6 | 6.8.1 | Factores de Bayes y prueba de hipótesis de Bayes |
| ptII_quan_Bayes_lossfun_startagain.r | 6 | 6.8.1.2 | Funciones de pérdida |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r | 6 | 6.8.1.4 | Actualidad de los factores de Bayes |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r | 6 | 6.8.1.4 | Actualidad de los factores de Bayes |
| ptII_quan_Bayes_case_exp-extra-sensual-perception.r | 6 | 6.8.1.5 | Ejemplo de investigación: ¿la clarividencia? |
| ptII_quan_Bayes_Bem-study-aspects.r | 6 | 6.8.1.6 | Crítica al diseño: el estudio de Bem |
| ptII_quan_Bayes_BayesFactors_further-remarks.r | 6 | 6.8.1.7 | Factores de Bayes, ¿y ahora? |
| ptII_quan_Bayes_information-criteria_helpfuncs.r | 6 | 6.8.2 | Criterios de información |
| ptII_quan_Bayes_information-criteria.r | 6 | 6.8.2 | Criterios de información |
| ptII_quan_Bayes_over-and-underfitting.r | 6 | 6.8.3 | Sobreajuste y subajuste |
| ptII_quan_Bayes_HDI_helpfuncs.r | 6 | 6.8.4.1 | Estimación bayesiana por intervalos |
| ptII_quan_Bayes_HDI.r | 6 | 6.8.4.1 | Estimación bayesiana por intervalos |
| ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r | 6 | 6.8.4.2 | ROPE — region of practical equivalence |
| ptII_quan_Bayes_ROPE-BayesFactor.r | 6 | 6.8.4.2 | ROPE — region of practical equivalence |
| ptII_quan_Bayes_PPC_model-check_helpfuncs.r | 6 | 6.8.4.4 | Comprobaciones predictivas posteriores |
| ptII_quan_Bayes_PPC_model-check.r | 6 | 6.8.4.4 | Comprobaciones predictivas posteriores |
| ptII_quan_Bayes_PPC_model-check-graph.r | 6 | 6.8.4.5 | Evaluación gráfica de modelos |
| ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r | 6 | 6.8.4.6 | Estudio de caso – humor y la producción de palabras |
| ptII_quan_Bayes_case_wordcounts-PPC.r | 6 | 6.8.4.6 | Estudio de caso – humor y la producción de palabras |
| **Parte III - Análisis de Datos Cualitativos** |  |  | |
| ptIII_qual_code-paradigm_table-analysis.r | 9 | 9.4 | Análisis de tablas según Miles y Huberman |
| ptIII_qual_quan-textanalysis.r | 10 | 10.1 | Estudio de caso – análisis cuantitativo de textos |
| **Parte IV - Análisis cualitativo comparativo** |  |  | |
| ptIV_qual_Boole_basics.r | 12 | 12.1 | Propedéutica |
| ptIV_qual_Boole_case_Krook_women-in-parliament.r | 12 | 12.11.1 | Sobre la representatividad de las mujeres en los parlamentos |
| ptIV_qual_Boole_case_Titanic_death-and-dying.r | 12 | 12.11.2 | Vivir y morir en el Titanic — Parte II |
| ptIV_qual_Boole_logical-minimization.r | 12 | 12.3 | La formación de tipos como principio de comparación via la minimización lógica |
| ptIV_qual_Boole_case_school-success.r | 12 | 12.6 | Análisis de criterios - resultado positivo y negativo |
| ptIV_qual_Boole_fuzzy-logic.r | 12 | 12.7 | Fuzzy Logic/Lógica difusa |
| **Parte V - Métodos mixtos** |  |  | |
| ptV_mixed_prime-numbers.r | 13 | 13.3.1 | CUAL y CUAN en los diseños de conversión |

</details>

La siguiente tabla contiene información condensada sobre cada script usando algunas palabras clave. Los archivos están agrupados de acuerdo con el tema y no representan necesariamente el orden de aparición en el libro.

<details>

<summary>Click here to see the table with a short description of the R scripts</summary>

| Nombre de script | Contenido |
| --- | --- |
| DiM_Bretthorst_PG.r | Bretthorst (1993) diferencia de medias, solución analítica, con el paquete Brobdingnag para números muy grandes, implementación según P Gregory (2005) |
| DiM_Bretthorst_PG_calls.r | Ejemplos de llamada |
| DiM_Bretthorst_UMS.r | Bretthorst (1993) diferencia de medias, solución analítica, con el paquete Brobdingnag para números muy grandes, implementación según UM Studer (1998) |
| DiM_Bretthorst_UMS_calls.r | Ejemplos de llamada |
| **Funciones externales** |  |
| EXT_bayesian2beta.r | prueba de proporción bayesiana exacta de Sverdlov, Ryeznik & Wu (2015) |
| EXT_DBDA2E-utilities.R | de Kruschke (2014) DBDA 2. ed., funciones generales |
| EXT_Jags-Ymet-XmetMulti-Mrobust.R | de Kruschke (2014) DBDA 2. ed., aplicado para PPC y varianzas heterogeneas |
| **Todas partes** | |
| ptall_generalfuncs.r | d de Cohen, estadísticas descriptivas de todo tipo en una tabla, fivenum de Tukey con etiquetas, convertir tablas de estilo Aquad en tablas de verdad y v.v., imprimir implicantes primos de objetos QCA, matriz de distancia completa, corte óptimo a través de una matriz de proximidad según Oldenbürger y trazarla, trazar prototipos en 2d + 3d, trazar valores propios (MDS), correlación y valores p. |
| ptall_generalfuncs_Bayes_Beta_determine.r | determinar y trazar los valores de la distribución beta a partir de tres puntos cuantílicos mediante optimización, modelo de roles: Coghlan (2017) |
| ptall_generalfuncs_Bayes_binomial.r | funciones para aplicar y trazar funciones de probabilidad a priori para éxitos/fracasos, calcular estadísticas resumidas para la probabilidad a posteriori, IDH, fórmula para calcular la probabilidad a posteriori beta mediante conjugación, trazar probabilidad a priori, Likelihood, probabilidad a posteriori, probabilidad a priori actualizada y actualización de la probabilidad a posteriori, estadísticas resumidas |
| ptall_generalfuncs_Bayes_binomial-prop-test.r | Prueba de proporción bayesiana (por ejemplo, éxitos/fracasos): calcular, resumir y trazar la prioridad de éxitos/fracasos, convertir a valores de distribución beta y v.v., actualizar la prioridad binomial con probabilidad a beta (posterior) y trazar, resumen bayes.prop.test ajustado de BayesianFirstAid y trazar theta_diff, trazar MCMC, simulación desde posterior, aproximación de cuadrícula mediante fuerza bruta, pruebas exactas (diferencia binomial) (Evan Miller, Chris Stucchio, el enfoque de Sverdlov, Ryeznik, Wu (2015) de bayesian2beta.r en una forma ajustada para trabajar con valores logarítmicos u objetos brob y trazar los resultados, integración numérica (regla de Simpson) para objetos brob, comparación por fuerza bruta de rbetas frente a dbetas y trazarla. |
| ptall_generalfuncs_brob-integral.r | funciones útiles para objetos brob, convertir una lista en un vector, calcular el producto escalar, integración numérica (regla de Simpson), se pueden encontrar más métodos de integración numérica para objetos brob en https://github.com/abcnorio/R-largenum-integration |
| **Parte I - Teoría científica** | |
| ptI_sciencetheory_logic.r | cáp. 2.2 - afirmaciones simples verdadero/falso |
| **Parte II - Estadística clásica** | |
| ptII_quan_classicstats_Fisher_ladyteataste.r |  cáp. 4.5.1.1 - Experimento del sabor del té Lady Bristol de Fisher utilizando sus métodos de distribución hipergeométrica, fisher.test, y cálculo manual mediante factoriales. |
| ptII_quan_classicstats_N-P_powerfunc.r | cáp. 4.5.2.1 - calcular y representar gráficamente la potencia frente al tamaño de los efectos en función de diferentes hipótesis |
| ptII_quan_classicstats_N-P_stat-signif-isNOT-practsignif.r | cáp. 4.5.2.1 - diferentes puntos de vista sobre las diferencias de las muestras si se conocen los parámetros de la población |
| ptII_quan_classicstats_GandC_type-S-M-error.r | cáp. 4.5.2.2 - reproducir Gelman y Carlin (2014), análisis de potencia de retrodiseño |
| ptII_quan_classicstats_GandC_type-S-M-error_helpfuncs.r | cáp. 4.5.2.2 - trazar el tamaño del efecto y la potencia para el retrodiseño según Gelman y Carlin (2014), trazar la distribución de la muestra bajo H_0 y la hipótesis del verdadero tamaño del efecto. |
| ptII_quan_classicstats_N-P_nulldist-hypotest.r | cáp. 4.3.3.5 - trazar H_0, H_1 para pruebas unilaterales y bilaterales |
| ptII_quan_classicstats_N-P_nulldist-hypotest_helpfuncs.r | cáp. 4.3.3.5 - funciones para trazar H_0, H_1 para direcciones de hipótesis: bilateral, menor, mayor, y trazar tasas de error alfa y beta para densidades distribuidas t |
| ptII_quan_classicstats_N-P_confint.r | cáp. 4.5.2.6 - trabajar y simular intervalos de confianza (IC) |
| ptII_quan_classicstats_N-P_confint_bayesboot.r | cáp. 4.5.2.6 - bootstrap CIs y otros estadísticos mediante simulación, simular diferencia de medias, utilizar bootstrap bayesiano |
| ptII_quan_classicstats_N-P_confint_errorbars.r | cáp. 4.5.2.6 - Evolución del IC en función del tamaño de las muestras |
| ptII_quan_classicstats_N-P_confint_helpfuncs.r | cáp. 4.5.2.6 - calcular el IC de la media, la diferencia de medias y representarlos gráficamente |
| ptII_quan_classicstats_N-P_confint_p-t-value.r | cáp. 4.5.2.6 - simular la prueba t con valores p y t |
| ptII_quan_classicstats_N-P_confint_p-t-value_helpfuncs.r | cáp. 4.5.2.6 - simular pruebas t, comparar con la d de Cohen |
| ptII_quan_classicstats_N-P_confint-errorbars_helpfuncs.r | cáp. 4.5.2.6 - cambio de los IC junto con las barras de error, calcular los IC cubiertos |
| ptII_quan_classicstats_N-P_simulation.r | cáp. 4.5.4 - simulación "diferencia de medias" mediante réplica |
| ptII_quan_classicstats_N-P_example-soccer-sim.r | cáp. 4.5.4.1 - simular una ejecución real empírica de cómo conseguir todas las cartas de un álbum de cromos de fútbol |
| ptII_quan_classicstats_N-P_example-soccer-sim_helpfuncs.r | cáp. 4.5.4.1 - simular cuantas cartas faltan por salir utilizando el azar y no el sentido común como el intercambio de cartas, etc. |
| ptII_quan_classicstats_N-P_SE-N-dep.r | cáp. 4.5.5 - dependencia de las características del tamaño de la muestra mediante datos simulados (N, SE, valor t, d de Cohen, ...) |
| ptII_quan_classicstats_nullritual.r | cáp. 4.5.7 - todo el sinsentido en torno al ritual Nulo, es decir, el procedimiento NHST |
| ptII_quan_classicstats_NHST_nulldist.r | cáp. 4.5.8 - trazar la toma de decisiones NHST utilizando regiones de aceptación/rechazo, tasas de error alfa y beta |
| ptII_quan_classicstats_pvalue-as-base.r | cáp. 4.5.8.1 - diferencias de la distribución normal frente a la normal mixta según el gráfico y los estadísticos, simulación H_0 |
| ptII_quan_classicstats_normal-vs-t.r | cáp. 4.5.8.2 - relación de las distribuciones normal y t |
| ptII_quan_classicstats_centrallimittheorem.r | cáp. 4.5.8.3 - simular el teorema central del límite (CLT) en condiciones variables |
| ptII_quan_classicstats_centrallimittheorem_helpfuncs.r | cáp. 4.5.8.3 - funciones básicas para simular CLT |
| ptII_quan_classicstats_p-t-df-relationship.r | cáp. 4.5.9 - relación de N, t- y p-valor, simular con la misma semilla, tamaños de muestra crecientes, t-valor constante y simulación de potencia post-hoc. |
| ptII_quan_classicstats_effectsizes.r | cáp. 4.6.10 - relacionar gráficamente entre sí N, p-valor, d de Cohen, odds ratio y risk ratio |
| ptII_quan_classicstats_effectsizes_helpfuncs.r | cáp. 4.6.10 - para relacionar N, p-valor y d de Cohen |
| ptII_quan_classicstats_effectsizes_helpfuncs_sjstats.r | cáp. 4.6.10 - tomado y retocado del paquete sjtats ya que desapareció del paquete, convertir odds ratio a risk ratio |
| ptII_quan_classicstats_Simpsonparadox.r | cáp. 4.6.11.1 - investigar la paradoja de Simpson con el conjunto de datos de admisión clásico de UCB utilizando gráficos, tablas, tasas base corregidas y el modelo glmer, simular los datos de la paradoja de Simpson y detectar subgrupos con Simpsons del paquete Simpsons |
| ptII_quan_classicstats_Simpsonparadox_helpfuncs.r | cáp. 4.6.11.1 - función para trazar dos grupos y detectar la paradoja de Simpson |
| ptII_quan_classicstats_JeffreysLindleyparadox.r | cáp. 4.6.11.2 - Paradoja de Jeffrey-Lindley con ejemplos de datos |
| ptII_quan_classicstats_p-hacking-sim.r | cáp. 4.6.2 - simulación de p-hacking con tamaños de muestra variables para tasas de error alfa estándar, comparación con análisis de potencia a priori y representación gráfica de los resultados |
| ptII_quan_classicstats_p-hacking-sim_helpfuncs.r | cáp. 4.6.2 - función para simular el modelo lineal con sim del paquete arm así como métodos p-hacking |
| ptII_quan_classicstats_dealingwithpower.r | cáp. 4.6.3 - calcular, simular y representar gráficamente la potencia en distintos modelos |
| ptII_quan_classicstats_R-index_z-curve.r | cáp. 4.6.4.1 - aplicar R-Index y TIVA según Schimmack y colegas, por ejemplo, en el estudio de clarividencia de Bem y calcular la curva z |
| ptII_quan_classicstats_R-index_z-curve_helpfuncs.r | cáp. 4.6.4.1 - función para calcular el índice R, valores p para el índice R, prueba de varianza insuficiente (TIVA) según Schimmack y colegas |
| ptII_quan_classicstats_varianceestimation.r | cáp. 4.6.5 - aplicar la función para calcular la estimación de la varianza |
| ptII_quan_classicstats_varianceestimation_helpfuncs.r | cáp. 4.6.5 - función para calcular la estimación de la varianza si sigma es conocida o desconocida y representarla gráficamente |
| ptII_quan_classicstats_randomization.r | cáp. 4.6.6 - aplicar diferentes estrategias de aleatorización para crear muestras |
| ptII_quan_classicstats_missingdata.r | cáp. 4.6.7 - tratamiento de datos ausentes y cambios asociados con y sin métodos de imputación, comparación de métodos de imputación entre sí y con brms (modelo lineal bayesiano) |
| ptII_quan_classicstats_equivalentmethods.r | cáp. 4.6.8.1 - comparación con las estadísticas y el gráfico de Bland-Altman (gráfico de diferencia de medias), TOST del paquete TOSTER, Pitman-Morgan Test, pruebas de los paquetes BayesFactor, BEST, uso de la implementación de GL Bretthorst (1993) "On the difference in mean", utilizando, por ejemplo, los datos originales de Bland-Altman |
| ptII_quan_classicstats_equivalentmethods_helpfuncs.r | cáp. 4.6.8.1 - gráfico de diferencia de medias según Tukey (equivalente al gráfico de Bland-Altman) |
| ptII_quan_classicstats_variancehomogeneity.r | cáp. 4.6.8.2 - determinar la homogeneidad de la varianza mediante diversas pruebas (Levene, Breusch-Pagan, varianza de error no constante, Cook y Weisberg, versión de la prueba estadística de Koenker) |
| ptII_quan_classicstats_normaldist_residuals.r | cáp. 4.6.9.1 - investigación de residuos de modelos lineales para diversas características (normalidad, asimetría, curtosis, diversos tamaños de muestra |
| ptII_quan_classicstats_outliers-and-influentialpoints.r | cáp. 4.6.9.3 - impacto y consecuencias de los puntos influyentes y los valores atípicos, cálculo del modelo lineal, prueba t, prueba de valores atípicos, prueba de correlación, gráficos de apalancamiento |
| ptII_quan_classicstats_outliers-and-influentialpoints_helpfuncs.r | cáp. 4.6.9.3 - función para trazar valores atípicos (con, sin) mediante líneas de regresión |
| **Parte II - Análisis Exploratorio de Datos** | |
| ptII_quan_EDA_intro_overviewrobust.r | cáp. 5.2.-5.3. - comparar mediana vs. media, mostrar gráficos robustos de datos, aplicar lm vs. rlm a datos empíricos |
| ptII_quan_EDA_intro_overviewrobust_helpfuncs.r | cáp. 5.2.-5.3. - función para trazar los residuos (lm frente a rlm), simular y trazar la mediana/media de una distribución normal |
| ptII_quan_EDA_case_German-states-population.r | cáp. 5.5.1 - la conversión a valores logarítmicos conduce a líneas rectas utilizando un ejemplo de la vida real a partir de las características de la población de los estados alemanes, en comparación con el modelo lineal clásico |
| ptII_quan_EDA_case_Suisse-fertility.r | cáp. 5.5.2 - analizar la fecundidad en Suiza entre católicos (sí, no, algo más) utilizando sólo gráficos descriptivos y correlaciones, utilizar subgrupos para comprender los datos |
| ptII_quan_EDA_case_Suisse-fertility_helpfuncs.r | cáp. 5.5.2 - función para trazar dos variables con un índice continuo |
| ptII_quan_EDA_case_Anderson_iris-species-in-biology.r | cáp. 5.5.3 - investigar los famosos datos del iris con pares, lda y tablas |
| ptII_quan_EDA_case_Titanic_death-and-dying.r | cáp. 5.5.4.6 - utilizar sólo estadísticas descriptivas, tablas y gráficos para investigar las características de los supervivientes del Titanic y cómo sobrevivir y por qué, responder a ciertas preguntas sobre las condiciones de sobrevivir o morir durante la catástrofe del Titanic |
| ptII_quan_EDA_case_Titanic_death-and-dying_helpfuncs.r | cáp. 5.5.4.6 - función para trazar las densidades de varios subgrupos |
| ptII_quan_EDA_case_Spain_leadership-in-education.r | cáp. 5.5.5 - analizar datos sobre liderazgo en España (contexto educativo) con métodos a distancia (HCA, MDS), análisis de prototipos, heatmap, corrgram, levelplot, y gráficos descriptivos simples |
| ptII_quan_EDA_case_Spain_leadership-in-education_helpfuncs.r | cáp. 5.5.5 - versión modificada de heatmap del paquete heatmap.plus (no disponible en R v.4) |
| ptII_quan_EDA_case_Chiro_heartrate-variability.r | cáp. 5.5.6 - experimento en quiropráctica sobre la variabilidad de la frecuencia cardiaca utilizando modelos lineales sin ninguna prueba de significación, aprendiendo de las características del tamaño de la muestra, utilizando histogramas y gráficos de interacción |
| ptII_quan_EDA_case_Chiro_heartrate-variability_helpfuncs.r | cáp. 5.5.6 - función para trazar interacciones |
| **Parte II - Estadísticas bayesianas** | |
| ptII_quan_Bayes_Beta-distribution.r | cáp. 6.12 - cálculo de la beta posterior a partir de la prior y la Likelihood (conjugación, aproximación reticular), influencia de las priors, diferentes formas de beta |
| ptII_quan_Bayes_find-Beta-distribution-shapeparams.r | cáp. 6.12 - determinar la distribución beta a partir de tres puntos cuantílicos con/sin optimización |
| ptII_quan_Bayes_Fisher_LadyBristol-Beta-disttribution.r | cáp. 6.12 - trazar la prior, la Likelihood y la posterior de los datos empíricos |
| ptII_quan_Bayes_Gamma-distribution.r | cáp. 6.12 - visualizar la función Gamma para diferentes parámetros |
| ptII_quan_Bayes_regularization.r | cáp. 6.12 - diferentes priors (normal con diferentes sigmas, Cauchy, uniforme) |
| ptII_quan_Bayes_Prior-Likeli-Post_relationship.r | cáp. 6.12.1 - mostrar cómo la priori y la Likelihood dan forma a la posterior, ver las diferentes influencias en la probabilidad a posteriori |
| ptII_quan_Bayes_MC-simulation_binom-norm.r | cáp. 6.13 - simular distribuciones binormales y normales |
| ptII_quan_Bayes_RandomWalk.r | cáp. 6.13 -  calcular y trazar un paseo aleatorio como 2d + 3d |
| ptII_quan_Bayes_RandomWalk_helpfuncs.r | cáp. 6.13 - una función para calcular un paseo aleatorio simple |
| ptII_quan_Bayes_simulate-pi.r | cáp. 6.13 - aplicar la simulación pi para tamaños de muestra variables |
| ptII_quan_Bayes_simulate-pi_helpfuncs.r | cáp. 6.13 - función simple para calcular pi y trazarlo (ineficiente, pero funciona) |
| ptII_quan_Bayes_problem-local-minima.r | cáp. 6.13.1.4 - mínimos locales mediante la función de Himmelblau, trazado en 3d |
| ptII_quan_Bayes_HMC.r | cáp. 6.13.2.3.1 - aplicar el algoritmo Hamilton Monte Carlo (HMC), trazar e investigar (distribución normal bivariante), utilizar los paquetes hmclearn, rethinking y bayesplot, así como el algoritmo básico HMC (Neal, 2011, cáp. 5), analizar las cadenas MCMC mediante trazados y diagnósticos típicos (por ejemplo, Gelman, Heidelberger-Welch, tamaño efectivo de la muestra, eliminar burn-in, .... ), comparar con el algoritmo MH |
| ptII_quan_Bayes_HMC_helpfuncs.r | cáp. 6.13.2.3.1 - funciones para simular la distribución normal bivariante (rnorm, mvrnorm), calcular el gradiente para el algoritmo HMC, simular la distribución normal bivariante mediante HMC según McElreath (2015), describir las cadenas MCMC: Diagnóstico de Heidelberger-Welche, describir el desarrollo de la media y la covarianza, ajustar los límites para que aparezca un valor de comparación en un gráfico posterior y simular la distribución normal bivariante mediante el algoritmo MH. |
| ptII_quan_Bayes_MetropolisHastings_example-normdist.r | cáp. 6.13.4.1 - simular la media de la distribución normal con el algoritmo Metropolis-Hastings (MH), investigar las cadenas MCMC utilizando el paquete coda, aplicar sobre datos empíricos, investigar las tasas de aceptación/rechazo, comparar las cadenas MCMC, la distribución predictiva posterior y los gráficos |
| ptII_quan_Bayes_MH-Gibbs_example_helpfuncs.r | cáp. 6.13.4.1 - funciones para realizar el algoritmo MH para simular la distribución normal basada en datos empíricos, priors, Gibbs sampler, plot MCMC y sus partes, distribución predictiva posterior normal |
| ptII_quan_Bayes_GibbsSampling_example-normdist.r | cáp. 6.13.4.2 - simular la media posterior mediante muestreo de Gibbs, analizar la posterior |
| ptII_quan_Bayes_JAGS_example-norm.r | cáp. 6.13.4.3 - simular la media de una distribución normal utilizando JAGS, investigar MCMC, comparar con el paquete Bolstad |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS.r | cáp. 6.13.5 - analizar los datos del té de Lady Bristol con métodos clásicos y bayesianos (solución analítica y MCMC con JAGS, paquete BEST, BUGS) |
| ptII_quan_Bayes_Fisher_LadyBristol-BUGS_helpfuncs.r | cáp. 6.13.5 - función para analizar el experimento Lady Bristol con el Teorema de Bayes, trazar éxitos/fracasos, IDH, MAP, y ejecutar el modelo BUGs con el paquete RBugs de R |
| ptII_quan_Bayes_MaximumEntropy.r | cáp. 6.14 - calcular y trazar la entropía de Boltzmann/Shannon utilizando una moneda y un dado (no tan justos/justos) |
| ptII_quan_Bayes_MaximumEntropy_helpfuncs.r | cáp. 6.14 - funciones para reproducir los análisis de Jaynes (1962), simulación de entropía según McElreath (2015, p.277) |
| ptII_quan_Bayes_Entropy_KullbackLeibler.r | cáp. 6.14.1 - aplicar funciones de entropía al recuento de palabras |
| ptII_quan_Bayes_Entropy_KullbackLeibler_helpfuncs.r | cáp. 6.14.1 - funciones para calcular H (entropía de Shannon) a partir de recuentos y priors o distancia de Kullback-Leibler |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice.r | cáp. 6.14.3 - realizar y trazar el análisis de Jaynes (1962) en un dado (¿justo?) |
| ptII_quan_Bayes_MaximumEntropy_Jaynes-fair-dice_helpfuncs.r | cáp. 6.14.3 - funciones para calcular la entropía de una moneda justa o no tan justa y simular un dado |
| ptII_quan_Bayes_case_presidential-heights.r | cáp. 6.15.1 - preparación del conjunto de datos, eliminación de NA, gráficos EDA, estadística clásica frente a bayesiana con prueba binomial bayesiana, diagnósticos MCMC, prueba de diferencia de medias mediante solución clásica y bayesiana incl. HDI, ROPE, bayesboot, trazado de resultados |
| ptII_quan_Bayes_case_presidential-heights_helpfuncs.r | cáp. 6.15.1 - función retocada de bayes.binom.test del paquete BayesianFirstAid, función para calcular OR/RR posterior | 
| ptII_quan_Bayes_case_startagain-successrates.r | cáp. 6.15.2 - comparar desarrollos y casos extremos de éxitos/fracasos y probs posteriores resultantes | 
| ptII_quan_Bayes_case_startagain-successrates-longterm.r | cáp. 6.15.2.1 - demuestre que el análisis a largo plazo de todos los años frente al análisis secuencial por año (lo anterior se convierte en lo posterior se convierte en lo anterior, etc.) conduce exactamente a los mismos resultados concluyentes dentro del enfoque bayesiano | 
| ptII_quan_Bayes_case_presidential-debates.r | cáp. 6.15.3 - frequentist, chi-cuadrado, potencia, JAGS, probabilidades posteriores, gráficos, variantes de fuerza bruta MCMC, prueba y gráfico de hipótesis, aproximación de cuadrícula, pruebas exactas, MAP, gráficos 2d + 3d de valores theta sobre intgral, comparación de métodos, uso de funciones ajustadas de appell, tolerancia, bayesian2beta.r, comparación con el paquete BayesFactor y la aproximación de Bretthorst |
| ptII_quan_Bayes_intro-BayesTheorem_tea.r | cáp. 6.2.2.4 - aplicación sencilla del Teorema de Bayes, utilizando valores discretos, y paso del conocimiento previo al posterior al previo, etc. es decir, actualización del Teorema de Bayes con nueva información |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis.r | cáp. 6.2.3.4 - aplicar el Teorema de Bayes a las pruebas médicas utilizando la prevalencia (de una enfermedad en la población), la sensibilidad (1-falso_positivo) y la especificidad (1-falso_negativo) |
| ptII_quan_Bayes_intro-BayesTheorem_medicaldiagnosis_helpfuncs.r | cáp. 6.2.3.4 - función para aplicar el Teorema de Bayes a las pruebas médicas utilizando la prevalencia p(A), la sensibilidad p(B|A) y la especificidad p(no-B|no-A). |
| ptII_quan_Bayes_intro-BayesTheorem_covid19-test.r | cáp. 6.2.4 - calcular la probabilidad de una prueba covid-19 en relación con las características de la población |
| ptII_quan_Bayes_posterior.r | cáp. 6.5.1 - análisis de las cadenas posteriores y MCMC (convergencia), aplicación a un conjunto de datos empíricos |
| ptII_quan_Bayes_simple-estimation-mean-post.r | cáp. 6.5.1 - distribución posterior de una media con diferentes priores informados utilizando el paquete Bolstad |
| ptII_quan_Bayes_simple-estimation-mean-post_helpfuncs.r | cáp. 6.5.1 - una función para trazar la media posterior con prior y likelihood, ajustado normgcp del paquete Bolstadt (corrección de errores) |
| ptII_quan_Bayes_BayesFactors_test-hypos.r | cáp. 6.7.1 - ejemplo empírico de las observaciones de Kruschke (los BF no bastan para interpretar los resultados), es mejor utilizar las probabilidades a priori y a posteriori, ejemplo: BF enorme pero aproximadamente los mismos parámetros del modelo lineal, BF sólo causado por diferentes priores. |
| ptII_quan_Bayes_lossfun_startagain.r | cáp. 6.7.1.2 - mostrar la función de pérdida en el contexto de un ejemplo empírico (tasas de éxito en terapia antidrogas) para extraer conclusiones para el trabajo real |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim.r | cáp. 6.7.1.3 - comparar y simular los Factores de Bayes (FB) para distintos tamaños de muestra, valores t, determinar el FB mínimo con distintos enfoques |
| ptII_quan_Bayes_BayesFactors_dependence-on-N-sim_helpfuncs.r | cáp. 6.7.1.3 - una función para simular BF en relación con N, convertir valores p en BF, relacionar valores p y tamaños de muestra |
| ptII_quan_Bayes_case_exp-extra-sensual-perception.r | cáp. 6.7.1.4 - función para calcular la prueba t con BF, luego preparar, describir, analizar datos empíricos utilizando métodos para datos binomiales y diferencia de medias (solución clásica y bayesiana), anova con medida repetida vs. brms |
| ptII_quan_Bayes_Bem-study-aspects.r | cáp. 6.7.1.5 - algunos cálculos artificiales pero significativos junto con una discusión muy crítica de los antecedentes teóricos y el diseño del estudio de Bem sobre la clarividencia. |
| ptII_quan_Bayes_BayesFactors_further-remarks.r | cáp. 6.7.1.6 - aplicar FBs en el contexto de gráficos EDA, pruebas t, d de Cohen, modelos lineales, y un enfoque bayesiano completo real con brms, y otros criterios (Bayes R^2, loo, waic, kfold), resultados diferentes si una variable categorial se hace continua (es decir, convertida de nuevo a su estado original natural como la edad categorial). |
| ptII_quan_Bayes_information-criteria.r | cáp. 6.7.2 - aplicar función para obtener criterios de información de un modelo lineal (ejemplo empírico) |
| ptII_quan_Bayes_information-criteria_helpfuncs.r | cáp. 6.7.2 - función para calcular y extraer criterios de información de modelos lineales |
| ptII_quan_Bayes_over-and-underfitting.r | cáp. 6.7.3 - ejemplos de ajuste insuficiente y excesivo, incluidos polinomios, uso de la divergencia de Kullback-Leibler (KL) entre probs |
| ptII_quan_Bayes_HDI.r | cáp. 6.7.4.1 - representar gráficamente el IDH y el IC de varios conjuntos de datos para demostrar la diferencia cualitativa de esos tipos de intervalo |
| ptII_quan_Bayes_HDI_helpfuncs.r | cáp. 6.7.4.1 - función para representar el IDH frente al IC simétrico |
| ptII_quan_Bayes_ROPE-BayesFactor.r | cáp. 6.7.4.2 - mostrar la importancia de ROPE vs. simple claro para la toma de decisiones (diferencia de medias). graficar con IDH, prueba binomial bayesiana para varios valores, priors, e hipótesis, priors y su influencia en BFs, aplicar en datos empíricos, aplicar y graficar con paquete BEST, analizar MCMC y probs posteriores |
| ptII_quan_Bayes_ROPE-BayesFactor_helpfuncs.r | cáp. 6.7.4.2 - función para demostrar el cambio en FB debido a las creencias previas |
| ptII_quan_Bayes_PPC_model-check.r | cáp. 6.7.4.4 - simular la PPC mediante JAGS utilizando éxitos/ fracasos y representarla gráficamente |
| ptII_quan_Bayes_PPC_model-check_helpfuncs.r | cáp. 6.7.4.4 - función para calcular la prueba uni-/bilateral (PPC) |
| ptII_quan_Bayes_PPC_model-check-graph.r | cáp. 6.7.4.5 - demostrar PPC para varianzas homogéneas (sí/no), trazar y probar hipótesis específicas (estadística clásica, bayesiana con brms), diseño de grupos de tratamiento/control, comparar modelos lineales |
| ptII_quan_Bayes_case_wordcounts-PPC.r | cáp. 6.7.4.6 - realizar análisis (por ejemplo, con JAGS) y gráficos junto con comprobaciones predictivas posteriores (PPC), incluidos gráficos MCMC de diagnóstico, bootstrap utilizando casos completos o valores reales, ejercicio Kruschke (2014, cap. 18.3) en relación con la heteroscedasticidad (influencia de varianzas desiguales entre grupos) |
| ptII_quan_Bayes_case_wordcounts-PPC_helpfuncs.r | cáp. 6.7.4.6 - funciones para preparar y resumir cadenas MCMC y posteriors de guiones de Kruschke (2014) para que coincidan con los requisitos aquí para PPC. |
| **Parte III - Análisis de Datos Cualitativos** | |
| ptIII_qual_code-paradigm_table-analysis.r | cáp. 9.4 - ejemplo de uso de expand.grid |
| ptIII_qual_quan-textanalysis.r | cáp. 10.1 - preparar (por ejemplo, eliminación de partes redundantes, caracteres especiales, espacios en blanco, puntuaciones, stop words y conversión a minúsculas, división de texto, nube de palabras, trazado de frecuencias, KWIC, colocación, ...) y analizar el texto utilizando los paquetes stringi, SnowbalC, tm, magrittr, quanteda, corps2, etc., utilizar wordstems, inspección de corpus, ... |
| **Parte IV - Análisis Cualitativo Comparativo (QCA)** | |
| ptIV_qual_Boole_basics.r | cáp. 12.1 - afirmaciones simples verdadero/falso |
| ptIV_qual_Boole_case_Krook_women-in-parliament.r | cáp. 12.11.1 - analizar el conjunto de datos de Krook (2010) mediante QCA, extraer los principales implicantes, comprobar la coherencia, preparar los resultados positivos y negativos para la discusión general. |
| ptIV_qual_Boole_case_Titanic_death-and-dying.r | cáp. 12.11.2 - aplicar el QCA al conjunto de datos del Titanic para investigar un conjunto mínimo de supervivencia (sí/no) |
| ptIV_qual_Boole_logical-minimization.r | cáp. 12.3 - demostración de caso único de cómo funciona realmente la minimización booleana |
| ptIV_qual_Boole_case_school-success.r | cáp. 12.6 - conjunto de datos artificiales para demostrar el QCA para resultados negativos y positivos |
| ptIV_qual_Boole_fuzzy-logic.r | cáp. 12.7 - gráfico sencillo para demostrar qué es la lógica difusa |
| **Parte V** | **Métodos Mixtos** |
| ptV_mixed_prime-numbers.r | cáp. 13.3.1 - una sola línea para obtener números primos usando la división mod euclidiana |

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


