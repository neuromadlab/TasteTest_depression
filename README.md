# Blunted anticipation but not consummation of food rewards in depression

This project investigates 
(1) food reward ratings moving gradually from anticipation to consummation in depression and anhedonia, using a taste test paradigm. 
(2) the role of metabolic hormones in depression, anhedonia, and food reward ratings

This project code accompanies [this paper], [PrePrint](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository](https://www.medrxiv.org/content/10.1101/2024.03.26.24304849v1 )

Please cite as following: 

## Table of Contents

- [Abstract](#abstract)
- [Getting Started](#getting_started)
- [Installing](#installing)
- [Abbreviations](#abbreviations)
- [Data_Shared](#data_shared)
- [Code_Shared](#Code_Shared)

## Abstract: 

Background. Anhedonia is a core symptom of major depressive disorder (MDD).
While its narrow definition as a hedonic or consummatory deficit evolved to encompass
anticipatory and motivational reward facets, it remains unclear where reward deficits
manifest. As evidence accumulates for metabolic hormones affecting reward
processing, studying their role in mitigating reward deficits could yield crucial insights.

Methods. We conducted a study with 103 participants, including 52 patients with MDD
and 51 healthy control participants (HCPs). After overnight fasting, blood samples were
collected to determine levels of ghrelin, glucose, insulin, and triglycerides. Participants
completed a taste test, providing repeated ratings of wanting and liking, gradually
moving from reward anticipation to consummation.

Findings. Patients with MDD showed decreased wanting (p = .046) but not liking for
food rewards during visual anticipation. However, once food was inspected and tasted,
patients increased wanting relative to HCPs (p = .004), providing strong evidence
against a consummatory deficit (Bayes Factors > 9). In contrast to a narrow definition
of anhedonia, higher scores on the Snaith-Hamilton Pleasure Scale were associated
with reduced anticipatory food wanting (p = .010) and more pronounced increases in
wanting with reward proximity (p = .037). Acyl ghrelin was associated with higher
wanting and liking ratings, while poor glycemic control was linked to anhedonia.

Interpretation. Our study demonstrates that MDD and anhedonia are associated with
reduced anticipation of rewards rather than consummatory pleasure deficits. Notably,
ghrelin's association with elevated reward ratings implicates the gut-brain axis as a
potential target for treating reward deficits in MDD
 
 ![Graphical Abstract](/figures/GraphicalAbstract.jpg)

## Getting_Started

### Clone the project using SSH from github 

`$ git clone git@github.com:neuromadlab/TasteTest_depression.git`

Read more about cloning a repository [here](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository) 

### Installing 

Code is written in `R V.4.3.0.`, within `VSCode` using Radian. 
To set up R in VSCode follow these [instructions] (https://code.visualstudio.com/docs/languages/r). [Better R in VSCode](https://schiff.co.nz/blog/r-and-vscode/). 
If using Radian from windows, set the R home env in the terminal.integrated.env

`RStudio` should work equally fine. 

All required libraries are installed and loaded at the beginning of the main script `Main.R` using [librarian] (https://cran.r-project.org/web/packages/librarian/vignettes/intro-to-librarian.html) 

`if (!require("librarian")) install.packages("librarian")
librarian::shelf(ggplot2, readxl, cowplot, ggforce, ggside,
plyr, MASS, dplyr, tidybayes, table1, broom, modelr, distributional, ggpubr, 
lme4, lmerTest, performance, see, patchwork, effects,emmeans, pbkrtest, sjPlot, httpgd, languageserver,
magrittr, dabestr, ggpmisc, ggridges, ggbeeswarm, zeallot, viridis,showtext, RColorBrewer, 
lmeresampler, gridExtra,readr, grDevices, gtsummary)
`

## Code_Structure 

### Abbreviations: 
* TUE008/Ghrelin Phenotpying (Phen): lab internal study identifier/ study name 
* AG: Acyl Ghrelin 
* DAG/DG: Desacyl Ghrelin 
* TT: Taste Test
* FCR: Food Cue reactivity task 
### Input Folder
* Note: to ethically share clinical data and preserve anonymity we avoided to share data with multiple identifiers (e.g., the combination of age and sex) [Hrynaszkiewicz et al. 2010] (https://doi.org/10.1136/bmj.c181) and used [synthpop](https://www.synthpop.org.uk/get-started.html) to 
generate synthetic data for age and sex and removed all uniquely identifying data from the shared dataset (e.g., specific diagnoses with comorbities) 

### Data_Shared 
* The following data is publically available: 

##### Data from the behavioral tasks:
* ![TUE008_data_TasteTest.xlsx](/input/TUE008_data_TasteTest.xlsx) (all ratings from all phases)
* ![dliking_joint.xlsx](/input/dliking_joint.xlsx) (liking ratings,merged with questionnares and covariates)
* ![dwanting_joint.xlsx](/input/dwanting_joint.xlsx) (wanting ratings,merged with questionnares and covariates)

##### Hormonal data:
* ![TUE008_data_TasteTest.xlsx](/input/TUE008_data_TasteTest.xlsx) `TUE008_Participant_data_sharable.xlsx` incl. covariates (age and sex synthetic) and preprocessed hormonal data 

##### Questionnaire data:
* ![SHAPS_D.xlsx](/input/SHAPS_D.xlsx)
* ![SighADS.xlsx](/input/SighADS.xlsx)
* ![BDI-II.xlsx](/input/BDI-II.xlsx)


### Code_Shared: 

* ![TasteTest_main.r](/code/TasteTest_main.r) Main Skript for primary analysis and figures reported in the paper. 

Supporting Scripts used to create the dataframes that were made available: 
* ![TasteTest_main.r](/code/TasteTest_main.r) Script that preprocesses ghrelin value tables, log transforms, residualises etc. 
Creates file: TUE008_ghrelin_summary_preprocessed.csv
* ![preprocessing_metabolism.r](/code/preprocessing_metabolism.r) Script that merges ghrelin values with questionnaire data and other blood data that is also preprocessed, Creates file: TUE008_ghrelin_quests_preprocessed.csv
Plots metabolic paramters and questionnaire scores (SHAPS, BDI,..) 
* ![preprocessing_task.r](/input/preprocessing_task.r) Script that prepared dataframe from behavioral data 



