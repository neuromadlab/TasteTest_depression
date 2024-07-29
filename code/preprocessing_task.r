# Analysis Script for the taste_test TUE008 Depression Study
# Author: Corinna Schulz, corinna.schulz96@googlemail.com 
# Created: March 2023
# Edited: July 2024

# Code content:
# (1) SET UP
# (2) LOAD DATA
# (3) PREPARE VARIABLESf
# (4) DESCRIPTIVES TABLE

###############################################################
# (1) SET UP 

# Install and load all required libraries
if (!require("librarian")) install.packages("librarian")
librarian::shelf(ggplot2, readxl, cowplot, ggforce, ggside,
plyr, MASS, dplyr, tidybayes, table1, broom, modelr, distributional, ggpubr, 
lme4, lmerTest, performance, see, patchwork, effects,emmeans, pbkrtest, sjPlot, httpgd, languageserver,
magrittr, dabestr, ggpmisc, ggridges, ggbeeswarm, zeallot, viridis,showtext, RColorBrewer, 
lmeresampler, gridExtra,readr, grDevices, gtsummary, misty)


# Set whether Original data should be used or synthetic data (openly shared dataset with age and sex synthetic)
original_data <- FALSE 


# Set all themes
theme_set(theme_cowplot(font_size = 12))
systemfonts::font_info("Helvetica", bold = TRUE)

# Set Colors 
color_MDD <- "cornflowerblue"
color_HCP <-"darkgoldenrod4"
color_Anhedonia <- hcl.colors(4, "Purp")
color_atypical = '#006164'
color_lowatypical = '#DB4325'
c(color_ant1, color_ant2, color_cons) %<-%  hcl.colors(5, "Warm")[1:3]
color_lowGhrelin = '#bc5090'
color_highGhrelin = '#ffa600'

# Colors for Anhedonia/SHAPS Quantiles 
cQ1 =  'darkgoldenrod4'
cQ2 =  '#c2a074'
cQ3 =  '#8d7d9e'
cQ4 =  'darkslateblue'

# Set all Paths
setwd(getwd())
path_in <- "./input_original/" 
path_out <- "./output/" 
pathTUE <- "/mnt/TUE_general/" 

# Create Subfolders to structure the output files
sub_prepro <-  "0.preprocessingQC/"
sub_phases <- "1.Across_Phases/"
sub_groups <- "2.Group_Differences/"
sub_anhedonia <- "3.Anhedonia/"
sub_metparam <-  "4.metabolicParam/"

if (file.exists(paste(path_out, sub_prepro, sep = "")) == FALSE){
  "Please run preprocessing [1. preprocessing_ghrelin_blood.R, 2. preprocessing_metabolism.R] first"}
if (file.exists(paste(path_out, sub_phases, sep = "")) == FALSE){
  dir.create(paste(path_out, sub_phases, sep = ""))}
if (file.exists(paste(path_out, sub_groups, sep = "")) == FALSE){
  dir.create(paste(path_out, sub_groups, sep = ""))}
if (file.exists(paste(path_out, sub_anhedonia, sep = "")) == FALSE){
  dir.create(paste(path_out, sub_anhedonia, sep = ""))}
if (file.exists(paste(path_out, sub_metparam, sep = "")) == FALSE){
  dir.create(paste(path_out, sub_metparam, sep = ""))}

####################################################################################################################-###
# (2) LOAD DATA 

# Load ITS at time of datafreeze (only those that were not excluded, completed both sessions...)
d_sample <- read_excel(paste(path_in,"Recruitment_Overview.xlsx", sep = ""), sheet = "TUE008_Export_Group")
d_sample$fID <- factor(d_sample$ID)
length(unique(d_sample$fID))


  # Load Longformat (Wrap data) with taste_test Data 
  d <- read_excel(path=paste(path_in, "TT_TUE008_longformat.xlsx", sep = ""))
  # Load Longformat (Wrap data) with FCR Data (here we take the first 2 anticipatory ratings)
  d_FCR <- read_excel(path=paste(path_in, "FCR_TUE008_21-May-2023.xlsx", sep = ""))

  d_vas <- read_excel(path=paste(path_in, "VASstate_TUE008_all_output.xlsx", sep = ""))

  # Load all questionnaire items as well as blood parameter values 
  # This data file was created using anhedonia_metabolism 
  # Includes: SHAPS

# Load sep anhedonia items (primary SHAPS, others check)
d_SHAPS_items <- read_excel(path=paste(path_in, "SHAPS_D.xlsx", sep = ""), sheet = "recoded")
d_SIGH_items <- read_excel(path=paste(path_in, "SighADS.xlsx", sep = ""), sheet = "recoded_acute")
d_BDI_items <- read_excel(path=paste(path_in, "BDI-II.xlsx", sep = ""), sheet = "recoded")
d_diagnoses <- read_excel(path=paste(path_in, "SCID_Diagnoses.xlsx", sep = "")) ## NOT SHARED IN PUBLIC, too sensitive information, table is provided in manuscript 
# Filter data for participants that completed session at time of datafreeze not just home questionnaires 
d_SHAPS_items <- d_SHAPS_items[ (d_SHAPS_items$ID) %in% (d_sample$ID), ]
d_SIGH_items <- d_SIGH_items[ (d_SIGH_items$ID) %in% (d_sample$ID), ]
d_BDI_items <- d_BDI_items[ (d_BDI_items$ID) %in% (d_sample$ID), ]


# ANHEDONIA SUM SCORE ALTERNATIVES 
d_BDI_Anh <- d_BDI_items %>% 
            mutate(BDI_Anhedonia = rowMeans(select(d_BDI_items, c(BDI_04, BDI_12, BDI_15, BDI_21)), na.rm = TRUE))%>% 
            select(ID, BDI_Anhedonia)

d_SIGH_Anh <- d_SIGH_items %>% 
            mutate(SIGH_Anhedonia = rowMeans(select(d_SIGH_items, c(H2, A1, H3)), na.rm = TRUE))%>% 
            select(ID, SIGH_Anhedonia)

# Load Participant information 
# REAL ONE 
d_quest <- read_csv(paste("./input_original/","TUE008_ghrelin_quests_preprocessed.csv", sep = ""), show_col_types = FALSE)
d_quest$fID <- factor(d_quest$ID)

# SYNTHESISED (AGE, SEX) one for open data sharign 
d_quest_share <- read_excel(paste("./input/", "TUE008_ghrelin_quests_preprocessed_sharable.xlsx", sep = ""))
d_quest_share$fID <- factor(d_quest_share$ID)

# Merge pp info with questionnaire single items
d_quest <- merge(d_quest,d_BDI_Anh, by = "ID" )
d_quest <- merge(d_quest,d_SIGH_Anh, by = "ID" )

d_quest_share <- merge(d_quest_share,d_BDI_Anh, by = "ID" )
d_quest_share <- merge(d_quest_share,d_SIGH_Anh, by = "ID" )

d_quest$fSex <- factor(d_quest$Female, labels = c("male", "female")) 
d_quest_share$fSex <- factor(d_quest_share$Female, labels = c("male", "female")) 

######################################################################################################################
# Write ghrelin and quest data csv file for further analysis
write.csv(d_quest, paste("./input_original/TUE008_Participant_data_NONEsharable.xlsx"), row.names=FALSE)
write.csv(d_quest_share, paste("./input/TUE008_Participant_data_sharable.xlsx"), row.names=FALSE)

# From here one work with either or 
# SET Participant Information & Metabolic values (preprocessed) 
# Depending on whether AGE & SEX was synthesised (for ethical data sharing) load corresponding file 

if (original_data == TRUE) {
    d_quest <- d_quest
}else{
    d_quest <- d_quest_share
}

####################################################################################################################-###
# (3) PREPARE DATA ####

# Prepare FCR Data (subset of snacks) for Merging with TT Data & Rename Variables for clarity
# Snacks from FCR: 26 (cookies),40(gummy), 89 (raisins),184 (pretzels),217 (rice cracker),286 (nicnacs),373 (breadchips)
d$Snack[d$Snack == 'gummy bears'] <- 'gummy strawberry'

# We need to create Factors for all non-continous variables
# I.e., Make a factor for ID, MDD, Snack, RatingType, Phase 

d$fID <- factor(d$ID)
d$fMDD <- factor(d$MDD, levels = c(0,1), labels = c("HCP", "MDD"))
d$fSnack <- factor(d$Snack)
d$fRatingType <- factor(d$RatingType)

d_FCR_TT <-  subset(d_FCR, Image == 26 | Image == 40 | Image == 89 | Image == 184 | Image == 217 | Image == 286 | Image == 373)   

d_FCR_TT <- d_FCR_TT %>% 
  mutate(Snack = case_when( Image == 26 ~ "cookies",
                            Image == 40 ~ "gummy strawberry",
                            Image == 89 ~ "raisins",
                            Image == 184 ~ "pretzels",
                            Image == 217 ~ "rice cracker",
                            Image == 286 ~ "nic nocs",
                            Image == 373 ~ "bread rings",)) 

# Rename/Add Column Names analogoues to taste_test
# Phase: 0 (Anticipation, Baseline, 1. Rating FCR), 1 (Anticipation, 2. Rating FCR), 2 (Anticipation, 1. Rating TT), 3-5 (Consummatory from the TT)
names(d_FCR_TT)[names(d_FCR_TT) == "Repetition"] <- "Phase" # rename repetition into Phases to correspond with TasteTest 
d_FCR_TT$Phase <- 1 # Add Column indicating Phase 

d_FCR_TT$RatingType[d_FCR_TT$RatingType==0] <- "Liking"
d_FCR_TT$RatingType[d_FCR_TT$RatingType==1] <- "Wanting"

d_FCR_TT$fID <- factor(d_FCR_TT$ID)
d_FCR_TT$fSnack <- factor(d_FCR_TT$Snack)
d_FCR_TT$fRatingType <- factor(d_FCR_TT$RatingType)
d_FCR_TT$fMDD <- factor(d_FCR_TT$MDD, levels = c(0,1), labels = c("HCP", "MDD"))


# VAS MOOD Data preparation 

# Positive and Negative Affect Scales 
d_PA <- d_vas %>%
  filter((Item %in% c(5,7,8,10,14,15,17,19,21,22))) %>% 
  group_by(ID, Group, Timepoint)  %>% 
  summarize(overall_PA  = mean(Rating)) 
d_NA <- d_vas %>%
  filter((Item %in% c(6,9,11,12,13,16,18,20,23,24))) %>% 
  group_by(ID, Group, Timepoint)  %>% 
  summarize(overall_NA = mean(Rating)) 


# Note: Model preparations 
# Covariates (Age, Sex, BMI have already been centered) 

# Copy for descriptives table 
descriptives <- d_quest

# Remove double columns 
drops <- c("MDD", "fMDD", "fID")
d_quest <- d_quest[ , !(names(d_quest) %in% drops)]
d_quest_share <- d_quest_share[ , !(names(d_quest_share) %in% drops)]


dall <- merge(d,d_quest, by = "ID") 
dall_share <- merge(d,d_quest_share, by = "ID") 

d_SHAPS_items <- na.omit(select(d_SHAPS_items, ID, shaps_01, shaps_02, shaps_03, shaps_04, shaps_05, shaps_06, 
                            shaps_07, shaps_08, shaps_09, shaps_10, shaps_11, shaps_12, shaps_13, shaps_14))
                            
d_SHAPS_items <- d_SHAPS_items %>% dplyr::rename(
    "entertainment" = shaps_01,
    "familyFriends"= shaps_02,
    "hobbies"=shaps_03,
    "favDood"=shaps_04,
    "bathShower"=shaps_05,
    "smellFeeling"=shaps_06,
    "othersSmiling"=shaps_07,
    "art"=shaps_08,
    "reading"=shaps_09,
    "favDrink"=shaps_10,
    "smallThings"=shaps_11,
    "views"=shaps_12,
    "helping"=shaps_13,
     "praise"=shaps_14,
    )                  


# Separate Liking and Wanting & Add FCR data for Liking and Wanting 
dliking = dall[dall$fRatingType == "Liking",]
dwanting = dall[dall$fRatingType == "Wanting",]

### Add the Anticipatory Phase from FCR Data ####
dliking_joint <- subset(dliking, select=c("ID", "fID", "fMDD", "Phase", "fRatingType", "fSnack","RatingValue"))
dliking_FCR <- subset(d_FCR_TT[d_FCR_TT$RatingType == "Liking",], select=c("ID", "fID", "fMDD", "Phase", "fRatingType", "fSnack","RatingValue"))

dliking_joint <- rbind(dliking_joint,dliking_FCR)

dwanting_joint <- subset(dwanting, select=c("ID", "fID", "fMDD", "Phase", "fRatingType", "fSnack","RatingValue"))
dwanting_FCR <- subset(d_FCR_TT[d_FCR_TT$RatingType == "Wanting",], select=c("ID", "fID", "fMDD", "Phase", "fRatingType", "fSnack","RatingValue"))

dwanting_joint <- rbind(dwanting_joint,dwanting_FCR)

# Add Shaps and Confounds again

dwanting_joint <- merge(dwanting_joint,d_quest, by = "ID")
dliking_joint <- merge(dliking_joint,d_quest, by = "ID")

# Add Columns indicating 3 and 2 Phases to summarize the 5 Phases later more easily 
dwanting_joint <- dwanting_joint %>% 
  mutate(fPhase_trio = case_when( Phase == 1 ~ "anticipatory1",
                                  Phase == 2 ~ "anticipatory2",
                                  Phase != 1 &  Phase != 2 ~ "consummatory" )) %>% 
  mutate(fPhase_dicho = case_when( Phase == 1 | Phase == 2  ~ "anticipatory", 
                                  Phase != 1 &  Phase != 2 ~ "consummatory" )) %>% 
  mutate(fPhase_dicho_FCR_TT = case_when( Phase == 1  ~ "1st_ant", 
                                  Phase != 1 ~ "taste_test" ))

dliking_joint <- dliking_joint %>% 
  mutate(fPhase_trio = case_when( Phase == 1 ~ "anticipatory1",
                                  Phase == 2 ~ "anticipatory2",
                                  Phase != 1 &  Phase != 2 ~ "consummatory" )) %>% 
  mutate(fPhase_dicho = case_when( Phase == 1 | Phase == 2  ~ "anticipatory", 
                                  Phase != 1 &  Phase != 2 ~ "consummatory" ))%>% 
  mutate(fPhase_dicho_FCR_TT = case_when( Phase == 1  ~ "1st_ant", 
                                  Phase != 1 ~ "taste_test" ))
  
# Now factor Phase (only now because this enables sorting alphab. in the join df)
d$fPhase <- factor(d$Phase, levels = c(2,3,4,5), labels = c("anticipatory2", "consummatory1", "consummatory2", "consummatory3"))
d_FCR_TT$fPhase <- factor(d_FCR_TT$Phase, levels = c(1), labels = c("anticipatory1"))
dwanting_joint$fPhase <- factor(dwanting_joint$Phase, levels = c(1,2,3,4,5), labels = c("anticipatory1", "anticipatory2", "consummatory1", "consummatory2", "consummatory3"))
dliking_joint$fPhase <- factor(dliking_joint$Phase, levels = c(1,2,3,4,5), labels = c("anticipatory1", "anticipatory2", "consummatory1", "consummatory2", "consummatory3"))
dall$fPhase <- factor(dall$Phase, levels = c(1,2,3,4,5), labels = c("anticipatory1", "anticipatory2", "consummatory1", "consummatory2", "consummatory3"))

dwanting_joint$fPhase_dicho <- factor(dwanting_joint$fPhase_dicho, labels = c("anticipatory", "consummatory"))
dliking_joint$fPhase_dicho <- factor(dliking_joint$fPhase_dicho, labels = c("anticipatory", "consummatory"))

dwanting_joint$fPhase_trio <- factor(dwanting_joint$fPhase_trio, labels = c("anticipatory1", "anticipatory2", "consummatory"))
dliking_joint$fPhase_trio <- factor(dliking_joint$fPhase_trio, labels = c("anticipatory1", "anticipatory2", "consummatory"))

dwanting_joint$fPhase_dicho_FCR_TT <- factor(dwanting_joint$fPhase_dicho_FCR_TT, levels = c("1st_ant", "taste_test"))
dliking_joint$fPhase_dicho_FCR_TT <- factor(dliking_joint$fPhase_dicho_FCR_TT, levels = c("1st_ant", "taste_test"))

# Order according to ID and Phase 
dliking_joint <- dliking_joint[order(dliking_joint$ID, dliking_joint$Phase),]
dwanting_joint <- dwanting_joint[order(dwanting_joint$ID, dwanting_joint$Phase),]

# Merge Shaps Items with prepared wanting DF 
dwanting_joint <- merge(dwanting_joint, d_SHAPS_items, by = "ID", all  = TRUE)

# Do the same for other Ratings (here no FCR data available) 
d <- d %>% 
  mutate(fPhase_dicho = case_when(  Phase == 2  ~ "anticipatory", 
                                  Phase != 2 ~ "consummatory" )) 

d$fPhase_dicho <- factor(d$fPhase_dicho, levels = c("anticipatory", "consummatory"))
d_TT <- merge(d,d_quest, by = "ID") 

dsalty = d_TT[d_TT$fRatingType == 'Salzigkeit',]
dsweet = d_TT[d_TT$fRatingType == 'Süße',]
dumami = d_TT[d_TT$fRatingType == 'Umami',]
dintensity = d_TT[d_TT$fRatingType == 'Intensität',]

### Delta values for taste_test Rounds ####
# calculate Delta Change of RatingValues 
# Subtract 1. Rating from all other Ratings (i.e. 1. Anticipatory will become Baseline (0) 

dliking_joint <-  dliking_joint %>%
  group_by(fID, fSnack)%>%
  mutate(RatingValue_Delta = RatingValue - RatingValue[fPhase == "anticipatory1"])

dwanting_joint <-  dwanting_joint %>%
  group_by(fID, fSnack)%>%
  mutate(RatingValue_Delta = RatingValue - RatingValue[fPhase == "anticipatory1"])


## SAVE FOR DATA SHARING 
if (original_data == TRUE){
    write.xlsx(d_TT, paste("./input_original/","TUE008_data_TasteTest.xlsx", sep = "")) # Complete TT: with Other ratings 
    write.xlsx(dliking_joint, paste("./input_original/","dliking_joint.xlsx", sep = "")) 
    write.xlsx(dwanting_joint, paste("./input_original/","dwanting_joint", sep = "")) 
    save(d_TT,dliking_joint, dwanting_joint, file = "./input_original/TUE008_TaskData.RData")

}else{
    write.xlsx(d_TT, paste("./input/","TUE008_data_TasteTest.xlsx", sep = "")) # Complete TT: with Other ratings 
    write.xlsx(dliking_joint, paste("./input/","dliking_joint.xlsx", sep = "")) 
    write.xlsx(dwanting_joint, paste("./input/","dwanting_joint", sep = "")) 
    save(d_TT,dliking_joint, dwanting_joint,  file = "./input/TUE008_Data_TT_sharable.RData")
}

#####################################################################
### (4) DESCRIPTIVES TABLE ####
#####################################################################

if (original_data == TRUE){
    print("With Original Data: Create Descriptives for Manuscript")
         
# Create Table with basic sample information for Paper 

# Remove IDs without TT Data (i.e. submitted only quest Items, didnt come (yet) for study etc. ) 
descriptives <- descriptives[descriptives$ID %in% dall$fID,]
length(unique(descriptives$ID))

    # Write descriptives (to always use same dataset for analysis)
    # write.csv(descriptives, paste(path_out, sub_prepro, "TUE008_Descriptives.csv", sep = ""), row.names=FALSE)

    descriptives$Sex<- factor(descriptives$Sex)

    # Now create Table labels 
    table1::label(descriptives$BMI) <- "BMI [kg/m2]"
    table1::label(descriptives$Sex) <- "Sex"
    table1::label(descriptives$Age) <- "Age [years]"
    table1::label(descriptives$F_AG) <- "Acyl ghrelin [pg/mol])"
    table1::label(descriptives$F_DG) <- "Des-acyl ghrelin [pg/mol]"
    table1::label(descriptives$res_logHOMA) <- "Homa Index"
    table1::label(descriptives$Glk) <- "Glucose [mg/dl]"
    table1::label(descriptives$Ins_p) <- "Insulin [mg/dl]"
    table1::label(descriptives$HOMA_IR) <- "HOMA IR"
    table1::label(descriptives$TG) <- "Triglycerides"
    table1::label(descriptives$TyG) <- "Triglyceride Index"
    table1::label(descriptives$Weight) <- "Weight [kg]"
    table1::label(descriptives$Waist) <- "Waist [cm]"
    table1::label(descriptives$Height) <- "Height [cm]"
    table1::label(descriptives$Hip) <- "Hip [cm]"

    table1::label(descriptives$BDI_sum) <- "BDI"
    table1::label(descriptives$SHAPS_sum) <- "SHAPS"
    table1::label(descriptives$StAIT_sum) <- "STAIT"
    table1::label(descriptives$CRP) <- "C-reactive Protein"
    table1::label(descriptives$Meds_AD_Type) <- "Antidepressive (AD) medication"

    head(descriptives)
    # Set render such that 2 places after comma 
    my.render.cont <- function(x) {
        with(stats.apply.rounding(stats.default(x), digits=4), 
        {MEAN <- as.numeric(MEAN)
        SD <- as.numeric(SD) 
        c("","Mean (SD)"=sprintf("%0.2f (&plusmn; %0.2f)", MEAN, SD))})
    }


   # Now create the Table, specify Rows, Split by MDD Group status 
    descrp_table <- table1::table1(~ Age  + Sex + Weight + Waist  +  BMI + F_AG + F_DG  
                + Glk + Ins_p + HOMA_IR + TG + TyG + Height  + Hip  + StAIT_sum+ BDI_sum + SHAPS_sum + Meds_AD_Type  | fMDD , data = descriptives, 
                render.continuous = my.render.cont)

    # Add stats to descriptives tabl 
    reset_gtsummary_theme()
    theme_gtsummary_mean_sd()
    theme_gtsummary_journal(journal = "jama")
    #> Setting theme `JAMA`

    table_stats <-
    descriptives %>%
        select(fSex, Age, BMI, F_AG, F_DG, Glk, Ins_p, HOMA_IR, TG,TyG,   BDI_sum, SHAPS_sum, fMDD, StAIT_sum, Meds_AD_Type) %>%
    tbl_summary(by = fMDD,  statistic = list(all_continuous() ~ "{mean} (±{sd})"), digits = all_continuous() ~ 1) %>%
    add_p(test = c(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test.no.correct")) %>% 
    add_overall() %>% 
    bold_labels() %>%
    add_stat_label() %>%
    # add a header to the statistic column, which is hidden by default
    # adding the header will also unhide the column
    modify_header(statistic ~ "**Test Statistic**") %>%
    modify_fmt_fun(statistic ~ style_sigfig) 


    # Beeswarm plot in ggplot2 to visualize BDI, SHAPS, and MEDS Intake
    descriptives_quest <- descriptives %>%
    select(fID, fMDD, SHAPS_sum, BDI_sum, Meds_AD_Type) 

    descriptives_quest_long <- tidyr::gather(descriptives_quest, Questionnaire, Score, SHAPS_sum:BDI_sum, factor_key=TRUE)

    BeeQuest <- ggplot(descriptives_quest_long, aes(x = fMDD, y = Score, color = fMDD)) +
    geom_quasirandom(method = "quasirandom", cex = 3) +
    facet_wrap(~Questionnaire, labeller = labeller(Questionnaire = 
        c("SHAPS_sum" = "SHAPS",
        "BDI_sum" = "BDI"))) +
    scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
    theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
            axis.text = element_text(face = 'plain',size = 12.0),
            axis.title=element_text(size=16), axis.text.x = element_text(size = 12.0)) +
    xlab(label = 'Group') +
    ylab(label = 'Questionnaire Score')

    ggsave(paste(path_out, "SampleDescriptives_BDI_SHAPS.png", sep=""), 
            plot = BeeQuest,  height = 6, width = 6, units = "in", dpi = 600, bg = "white")

    # Add Medication information

    BeeQuest <- ggplot(descriptives_quest_long, aes(x = fMDD, y = Score, color = Meds_AD_Type)) +
    geom_quasirandom(method = "quasirandom", cex = 3) +
    facet_wrap(~Questionnaire, labeller = labeller(Questionnaire = 
        c("SHAPS_sum" = "SHAPS",
        "BDI_sum" = "BDI"))) +
    scale_color_manual(guide = guide_legend(title="Medication"),values = c("gray", "lightblue", "darkblue")) +
    theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
            axis.text = element_text(face = 'plain',size = 12.0),
            axis.title=element_text(size=16), axis.text.x = element_text(size = 12.0)) +
    xlab(label = 'Group') +
    ylab(label = 'Questionnaire Score')

    ggsave(paste(path_out, "SampleDescriptives_BDI_SHAPS_MEDS.png", sep=""), 
            plot = BeeQuest,  height = 6, width = 6, units = "in", dpi = 600, bg = "white")

    ## Comorbidities 
    descriptives <- merge(descriptives, d_diagnoses, by = "ID")
    descriptives[descriptives$ADHD == "Na",]$ADHD <- "healthy" # Checked ID 87, no ADHS according to protocol, missing in digital protocol 
    descriptives[descriptives$Alcohol_Abuse_Time == "CurrentAndLifetime",]$Alcohol_Abuse_Time <- "Lifetime" 
    descriptives[descriptives$Other_Substance_Abuse_Time == "CurrentAndLifetime",]$Other_Substance_Abuse_Time <- "Lifetime"  
    descriptives[descriptives$Social_Anxiety == "Na",]$Social_Anxiety <- "0" # All Social Anxiety was excluded before study, so not existing not NA! 
    descriptives[descriptives$GAS == "Na",]$GAS <- "0" # All GAD was excluded before study, so not existing not NA! 
    descriptives[descriptives$Panic_Disorder == "Na",]$Panic_Disorder <- "0" # All Panic_Disorder was excluded before study, so not existing not NA! 

    descriptives$Alcohol_Abuse_TimeS <-  descriptives$Alcohol_Abuse_Time
    descriptives[descriptives$Alcohol_Abuse_TimeS == "Current" | descriptives$Alcohol_Abuse_TimeS == "Lifetime",]$Alcohol_Abuse_TimeS <- "Lifetime&Current"  
    descriptives$Other_Abuse_TimeS <-  descriptives$Other_Substance_Abuse_Time
    descriptives[descriptives$Other_Abuse_TimeS == "Current" | descriptives$Other_Abuse_TimeS == "Lifetime",]$Other_Abuse_TimeS <- "Lifetime&Current"  

    comorbidies_table <- table1::table1(~  Social_Anxiety + GAS + Panic_Disorder  + OCD_Lifetime + 
                    Alcohol_Abuse_TimeS + Alcohol_Substance_Abuse+ Alcohol_Substance_Abuse + PTSD  + 
                     ADHD + Binge_Lifetime  + Other_Substance_Abuse_Time + Other_Abuse_TimeS + Other_Substance_Abuse   | fMDD , data = descriptives, 
                render.continuous = my.render.cont)

    }
    else { print("No table with synth data, see manuscript")} 



