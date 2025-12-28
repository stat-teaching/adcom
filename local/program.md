# Introduzione al corso

- logistica, esame, ricevimento
- come organizzo lezioni, teoria + hands on
- chiarisci il ruolo di R
- piccola lezione su GPT e come usarlo al meglio nell'analisi dati
- distinguere chiaramente esame vs approfondimento
- introdurre laboratorio
- introdurre didattica integrativa
- dire che ripeto MA quello che non mi chiedete do per scontato che sia chiaro

> far vedere qualche articolo, le analisi, tabelle, risultati, grafici con obiettivo di capire le analisi

# Introduzione analisi dati

- disegni di ricerca, causalità, randomizzazione
- misurazione, errore e validità delle misure
    - https://learningstatisticswithr.com/lsr-0.6.pdf
    - https://notebooklm.google.com/notebook/2199d2b8-4db4-4c1d-8450-b79031d65216
- tipi di variabili
    - https://www.personality-project.org/r/book/Chapter3.pdf
- dataset: cosa sono, come sono organizzati, cautele, etc.

> mostrare esempi di dati e analisi misleading. tipo simpson paradox, regression to the mean (pp. 88 ROS)

# Exploring data

- statistiche descrittive
- concetto di varianza, devianza e covarianza
- rappresentazioni grafiche, quando e come usarle
- suggerimenti per un buon grafico e come leggerli1
- tipi di distribuzioni, con focus nella normale
- standardizzazione

> far vedere come usare le statistiche descrittive scorrette può creare situazioni misleading
> concetto di distribuzione e molto largamente di likelihood. Far capire che tutte i modelli fanno delle assunzioni e queste si basano su distribuzioni di probabilità

# Inferenza Statistica

- campionamento
- statistiche, parametri
- standard error, sampling distribution (*4.2 Estimates, standard errors, and confidence intervals, ROS*)
	- qui metti la lezione del master che era molto chiara, derivare intuitivamente lo standard error come misura di incertezza di una stima
- potenza, type-1 error, sovrastima effetto
	- big effect, low sample, very likely that the result is correct
	- minimum detectable effect, critical effect size
- importanza di effect size e confidence interval
- teoria del limite centrale
- small samples
- significatività statistica e significatività clinica

# Regressione lineare

- regressione lineare come modello generale (t-test, anova, etc.)
    - https://www.personality-project.org/r/book/chapter5.pdf
- assunzioni e implicazioni (pp. 153 ROS)
- concetto di varianza spiegata
- introduzione partendo dal grafico, concetto di minimi quadrati (pp. 103 ROS), residui e massima verosimiglianza
- dalla covarianza alla regressione
- interpretazione parametri, wald test, etc.
- semplice, multipla, etc.
	- concetto di slope, derivata b = dy/dx
- concetto di confounder (pp. 385 ROS), mediatiore e moderatore (interazione)
    - https://mentalhealth.bmj.com/content/12/3/68#ref-3
- il concetto di adjusting/controlling for come aspetto fondamentale
- predittori categoriali e numerici
- outliers e punti influenti
- cenno a modelli non normali (glm)
- causal inference (pp. 155 ROS)
- discretizing continous predictors, cosa succede, quando serve, etc.
- model comparison

# Regressione lineare multilivello

- concetto di struttura nested
- predittori a vari livelli
- vantaggi e problematiche dei modelli multilivello
- [se riesco] between, within e contextual effects

# Regressione multivariata

- solo cenno, dire che c'è e magari dare qualche riferimento

# Metanalisi

- crisi di replicabilità, publication bias, sovrastima effetti
- dal singolo studio al meta-analytic thinking
- esempio di metanalisi fixed-effects e random effects
- forest e funnel plot
