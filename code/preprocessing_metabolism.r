##############################################################################
# Analysis Script for the TUE008 Ghrelin Depression Study
# Author: Corinna Schulz, corinna.schulz96@googlemail.com 
# Created: March 2023

# This script analyses anhedonia and its association with metabolic parametes 
# Plots SHAPS against metabolic paramters 
# Linear Models to test for association 
# Atypical Balance Score 

# Input: Ghrelin_summary_preprocessed as created by script ghrelin_blood.R 
# Saves xlsx file ghrelin_quests_preprocessed with a comprehensive file with all 
# blood parameters, covariates and questionnaire data for further behavioral analysis 
##############################################################################
# (1) SET UP 

# Install and load all required libraries
if (!require("librarian")) install.packages("librarian")
librarian::shelf(ggplot2, readxl, cowplot, ggforce, ggside,
plyr, MASS, dplyr, readr, tidybayes, broom, modelr, distributional, ggpubr, 
lme4, lmerTest, performance, see, patchwork, effects,emmeans, pbkrtest, sjPlot, httpgd, languageserver,
magrittr, dabestr, ggpmisc, misty, zeallot, viridis,showtext, RColorBrewer, gridExtra, reshape2)

# Set whether Original data should be used or synthetic data (openly shared dataset with age and sex synthetic)
original_data <- FALSE 

# Set all themes
theme_set(theme_cowplot(font_size = 12))

# Set Colors 

color_MDD <- "cornflowerblue"
color_HCP <-"darkgoldenrod4"
color_Anhedonia <- hcl.colors(4, "Purp")

# Colors for Anhedonia/SHAPS Quantiles 
cQ1 =  'darkgoldenrod4'
cQ2 =  '#c2a074'
cQ3 =  '#8d7d9e'
cQ4 =  'darkslateblue'

color_atypical = '#006164'
color_lowatypical = '#DB4325'

# Set all Paths
setwd(getwd())
path_in <- "./input_original/" 
path_out <- "./output/" 

# Create Subfolders to structure the output files 
sub_metparam <-  "4.metabolicParam/"
sub_prepro <-  "0.preprocessingQC/"

if (file.exists(paste(path_out, sub_metparam, sep = "")) == FALSE){
  dir.create(paste(path_out, sub_metparam, sep = ""))}

####################################################################################################################-###
# (2) LOAD DATA ####

# Load ITS at time of datafreeze (only those that were not excluded, completed both sessions...)
d_sample <- read_excel(paste(path_in,"Recruitment_Overview.xlsx", sep = ""), sheet = "TUE008_Export_Group")
d_sample$fID <- factor(d_sample$ID)
length(unique(d_sample$fID)) # Time of Datafreeze for TT Paper: 103 participants that completed & not excluded

##################
## Either create Participant File with original data & create synthetic one 
if (file.exists(paste(path_in, "TUE008_Participant_data_sharable.xlsx", sep = "")) == FALSE){ 
      print("TUE008_Participant_data_sharable does not exists and is created from original data")
    
    # Load Covariates 
    d_conf <- read_excel("./input_original/Ghrelin_Probanden_Daten.xlsx")

    # Load Questionnaire Scores 
    d_quest <- read_excel(paste(path_in,"total_scores.xlsx", sep = ""))
    d_quest$fID <- factor(d_quest$ID)
    # Anhedonia Score 
    d_quest$SHAPS_sum <- d_quest$SHAPS_sum - 14   #remove 14 from all such that SHAPS starts at 0 not 14! 

    # Load sep SHAPS items 
    d_SHAPS_items <- read_excel(path=paste(path_in, "SHAPS_D.xlsx", sep = ""), sheet = "recoded")
    d_SIGH_items <- read_excel(path=paste(path_in, "SighADS.xlsx", sep = ""), sheet = "recoded_acute")
    d_BDI_items <- read_excel(path=paste(path_in, "BDI-II.xlsx", sep = ""), sheet = "recoded")
    d_SHAPS_items <- d_SHAPS_items[ (d_SHAPS_items$ID) %in% (d_sample$ID), ]
    d_SIGH_items <- d_SIGH_items[ (d_SIGH_items$ID) %in% (d_sample$ID), ]
    d_BDI_items <- d_BDI_items[ (d_BDI_items$ID) %in% (d_sample$ID), ]

    # Reduce quests to needed list
    drops <- c("BMI", "Age")
    d_quest <- d_quest[ , !(names(d_quest) %in% drops)]

    # Load Ghrelin Values
    d_ghrelin <- read_csv(paste("./input_original/","TUE008_ghrelin_summary_preprocessed.csv", sep = ""), show_col_types = FALSE)
    d_ghrelin_clean <- select(d_ghrelin, ID, fPlate, sample, F_AG, F_DG, logF_AG, logF_DG, res_logF_AG, res_logF_DG)

    # Load Medication Data after cleaning and coding (manually performed)
    d_MedIntake <- read_excel(paste(path_in,"Meds_Intake.xlsx", sep = ""))

    # Get and Clean other blood data 
    d_blood <- d_conf
    ## Center Covariates for model interpretation later ####
    d_blood$BMI <- as.numeric(d_blood$BMI) 
    d_blood$Sex <- as.numeric(d_blood$Female)

    d_blood$cBMI <- as.numeric(d_blood$BMI) - mean(as.numeric(d_blood$BMI), na.rm=TRUE) #  TO DO CHECK TABLE CORRECTNESS, ASK EBRU 
    d_blood$cAge <- d_blood$Age - mean(d_blood$Age, na.rm=TRUE)
    d_blood$cSex <- as.numeric(d_blood$Female) - mean(as.numeric(d_blood$Female), na.rm=TRUE)

    d_blood_clean <- select(d_blood, MDD, ID, TG, CRP, Ins_p, Glk, HOMA_IR, TyG, Age, Female, BMI, cBMI, cAge, cSex)
    d_blood_clean$fMDD <- factor(d_blood_clean$MDD, levels = c(0,1), labels = c("HCP", "MDD"))
    d_blood_clean$fID <- factor(d_blood_clean$ID)

    # Merge all blood data 
    d_ghrelin_clean$fID <- factor(d_ghrelin_clean$ID)

    d_metPar <- merge(d_blood_clean, d_ghrelin_clean, by = "ID", all  = TRUE)
    head(d_metPar)
    head(d_ghrelin_clean)

    length(unique(d_metPar$ID))

    ## For Revisions: 
    # Get Ghrelin specific sex differences before residualising 
    d_metPar$F_AG <- log(d_metPar$F_AG)
    lm_FAG_Sex <- lm(logF_AG ~ cBMI + fMDD + cAge  + cSex, data = d_metPar)
    summary(lm_FAG_Sex)
    lm_FAG_Sex <- lm(F_AG ~ cBMI + fMDD*cSex + cAge  , data = d_metPar)
    summary(lm_FAG_Sex)


    #####################################################################
    ## Correct the blood values for BMI and Age 

    # Remove IDs that do not have blood data 
    d_metPar <- subset(d_metPar, !is.na(HOMA_IR))
    d_metPar <- subset(d_metPar, !is.na(TyG))

    d_metPar$HOMA_IR <- as.numeric(d_metPar$HOMA_IR)
    d_metPar$TyG <- as.numeric(d_metPar$TyG)
    d_metPar$Glk <- as.numeric(d_metPar$Glk)
    d_metPar$Ins_p <- as.numeric(d_metPar$Ins_p)

    # log transform and get residuals to correct for bmi and age and Sex
    d_metPar$logHOMA <- log(as.numeric(d_metPar$HOMA_IR))
    lm_logHOMA <- lm(logHOMA ~ cBMI + cAge  + cSex, data = d_metPar)
    summary(lm_logHOMA)

# Do not transform log TyG as log is already included in TyG calculation
    lm_TyG <- lm(TyG ~ cBMI + cAge + cSex, data = d_metPar)
    summary(lm_TyG)

    d_metPar$logGlk <- log(as.numeric(d_metPar$Glk))
    lm_logGlk <- lm(logGlk ~ cBMI + cAge + cSex , data = d_metPar)
    summary(lm_logGlk)

    d_metPar$logIns <- log(as.numeric(d_metPar$Ins_p))
    lm_logIns <- lm(logIns ~ cBMI + cAge + cSex, data = d_metPar)
    summary(lm_logIns)

    d_metPar$res_logHOMA <- residuals(lm_logHOMA)
    d_metPar$res_TyG <- residuals(lm_TyG)
    d_metPar$res_logGlk <- residuals(lm_logGlk)
    d_metPar$res_logIns <- residuals(lm_logIns)

    d_metPar$CRP <- as.numeric(d_metPar$CRP)    

    length(unique(d_quest$fID))


    # Check Normality after  log transformations 
    shapiro.test(d_metPar$logHOMA)
    shapiro.test(d_metPar$TyG)
    shapiro.test(d_metPar$logGlk)
    shapiro.test(d_metPar$logIns)

    # Test whether TyG looks skewed too, additional test
    ggqqplot(d_metPar$TyG)
    ggdensity(d_metPar$TyG, fill = "lightgray")
    ks.test(d_metPar$TyG, "pnorm", mean=mean(d_metPar$TyG), sd=sd(d_metPar$TyG))

    # Check Visually for normal distribution and QQ-Plots 
    hist1 <- ggplot(d_metPar, aes(x = logHOMA)) +
      geom_histogram(binwidth = 0.15, fill = "#bbbbf3") +  
      xlab(label = 'HOMA-IR  [log]') +
      ggtitle("HOMA-IR (log)")
    hist2 <- ggplot(d_metPar, aes(x = TyG)) +
      geom_histogram(binwidth = 0.08, fill = "#bbbbf3") +  
      xlab(label = 'TyG') +
      ggtitle("TyG")
    hist3 <- ggplot(d_metPar, aes(x = logGlk)) +
      geom_histogram(binwidth = 0.025, fill = "#bbbbf3") +  
      xlab(label = 'Blood levels of glucose [log]') +
      ggtitle("Glucose (log)")
    hist4 <- ggplot(d_metPar, aes(x = logIns)) +
      geom_histogram(binwidth = 0.15, fill = "#bbbbf3") +  
      xlab(label = 'Blood levels of insulin [loog]') +
      ggtitle("Insulin (log)")

    # Save Grid Plot
    G <- grid.arrange(hist1, hist2, hist3, hist4, 
                    ggqqplot(d_metPar$logHOMA), ggqqplot(d_metPar$TyG),
                    ggqqplot(d_metPar$logGlk), ggqqplot(d_metPar$logIns), nrow=2,  ncol=4, 
                      layout_matrix = rbind(c(1,2,3,4),c(5,6,7,8)))

    ggsave(paste(path_out, sub_prepro, "0.Blood_histograms_reslog.png", sep=""), 
            plot = G,  height = 5, width = 12, units = "in", dpi = 600, bg = "white")

    # Merge Blood Data and Quest Data 
    d_metPar_quest <- merge(d_metPar, d_quest, by = "ID", all  = TRUE)
    d_metPar_quest <- subset(d_metPar_quest, !is.na(fMDD)) 
    length(unique(d_metPar_quest$fID)) # CHECK Here, TasteTest Paper: 103 participants

    # Change NA values to no atypical Balance Score (HCP did not do SIGH-ADS interview, set 0 to have a continous measure)
    d_metPar_quest$AtypicalBalance_acute[is.na(d_metPar_quest$AtypicalBalance_acute) & d_metPar_quest$fMDD == "HCP"] <- 0


    ### Atypical MDD/ Atypical Balance Grouping ####
    # Define another grouping Variable differentiating MDD in atypical and typical 
    # using Balance Score (i.e. taking into account overall severity) 
    d_metPar_quest$cAtypicalBalance_acute <- center(d_metPar_quest$AtypicalBalance_acute, type = c("CWC"), cluster = d_metPar_quest$fMDD, value = NULL, name = ".c", as.na = NULL, check = TRUE)

    #Akin to SAD Paper calculation (Thuile paper, older version instrument)
    d_metPar_quest$cAtypicalBalance21_acute <- center(d_metPar_quest$AtypicalBalance21_acute, type = c("CWC"), cluster = d_metPar_quest$fMDD, value = NULL, name = ".c",
          as.na = NULL, check = TRUE)

    # Also build groups for visualization later 
    d_metPar_quest$MedSp_AtyBalance <- as.numeric(d_metPar_quest$AtypicalBalance_acute > ceiling(median(na.omit(d_metPar_quest$AtypicalBalance_acute[d_metPar_quest$fMDD == "MDD"]))))

    d_metPar_quest <- d_metPar_quest %>% 
      mutate(AtypicalGroup = case_when(
        MedSp_AtyBalance == 0 & fMDD == "MDD" ~ "low atypical MDD" ,
        MedSp_AtyBalance == 0 & fMDD == "HCP" ~ "HCP",
        MedSp_AtyBalance == 1 ~ "high atypical MDD")
      ) 
      
    d_metPar_quest$AtypicalGroup <- factor(d_metPar_quest$AtypicalGroup, levels = c("HCP","low atypical MDD", "high atypical MDD"))
    length((d_metPar_quest$AtypicalGroup[d_metPar_quest$AtypicalGroup == "low atypical MDD"]))

    d_metPar_quest <- d_metPar_quest[ (d_metPar_quest$ID) %in% (d_sample$ID), ]

    ######################################################################################################################
    # Write ghrelin and quest data csv file for further analysis
    # write.csv(d_metPar_quest, paste("./input_original/","TUE008_ghrelin_quests_preprocessed.csv", sep = ""), row.names=FALSE)
    ######################################################################################################################
    d_MedIntake <- d_MedIntake %>%  select(c("ID", "Meds_AD_Coding")) 
    d_metPar_quest <- merge(d_metPar_quest, d_MedIntake, by = "ID", all = TRUE)
    # The Coding includes only those who indicated anything (e.g. baby pill) in SoSci, the rest said "no meds"
    d_metPar_quest$Meds_AD_Coding <- tidyr::replace_na(d_metPar_quest$Meds_AD_Coding, "None")

    ## For Data Sharing synthetically produce AGE and SEX columns to be able to ethically share data set 
    ## without too many potentially identifying characteristics, select only relevant questionnaires - no uniquely identifying diagnoses
    d_metPar_quest_sharable <- d_metPar_quest  %>%  select(c("ID", "MDD", "TG", "Ins_p", "CRP", "Glk", "HOMA_IR", "TyG", 
                                  "logHOMA" , "TyG" , "logGlk" ,"logIns" ,"res_logHOMA" ,"res_TyG","res_logGlk" ,"res_logIns",
                                "Age", "cAge", "Female", "cSex", "fMDD", "F_AG", "F_DG", "logF_AG", "logF_DG", "res_logF_AG", 
                                  "res_logF_DG", "SHAPS_sum", "BDI_sum", "MedSp_AtyBalance", "AtypicalGroup", 
                                  "AtypicalBalance_acute", "cAtypicalBalance_acute", "BMI", "cBMI", "Meds_AD_Coding"))


    # install.packages("synthpop")
    library(synthpop)

    d_synth <- d_metPar_quest_sharable[,c("Age", "Female")]
    d_synth$Female <- factor(d_metPar_quest_sharable$Female)

    mysyn <- synthpop::syn(d_synth) 

    d_metPar_quest_sharable$Age <- mysyn$syn[,"Age"]
    d_metPar_quest_sharable$Female <- mysyn$syn[,"Female"]
    d_metPar_quest_sharable$cAge <- as.numeric(d_metPar_quest_sharable$Age) - mean(as.numeric(d_metPar_quest_sharable$Age), na.rm=TRUE) 
    d_metPar_quest_sharable$cSex <- as.numeric(d_metPar_quest_sharable$Female) - mean(as.numeric(d_metPar_quest_sharable$Female), na.rm=TRUE)

    write.xlsx(d_metPar_quest_sharable, paste("./input/","TUE008_ghrelin_quests_preprocessed_sharable.xlsx", sep = ""))

        } else {
          
          if (original_data == TRUE){
              print("Original Data is used for further analyses") 
          } else {
              print("Synthetic Data is used for further analyses ")
              # Load Openly shared data file 
              d_metPar_quest <- read_excel(paste("./input/", "TUE008_ghrelin_quests_preprocessed_sharable.xlsx", sep = "")) 
              }
        }

             
          
