# Analysis Script for the taste_test TUE008 Depression Study
# Author: Corinna Schulz, corinna.schulz96@googlemail.com 
# Created: March 2023
# Edited: July 2024

# Code content:
# (1) SET UP
# (2) LOAD DATA
# (3) VISUALIZE WANTING LIKING CHANGES OVER TIME  
# (4) LINEAR MIXED EFFECTS MODELING & DATA VIZ for MDD. VS. HCP 
# (5) LINEAR MIXED EFFECTS MODELING & DATA VIZ for ANHEDONIA 
# (6) METABOLIC PARAMTERS & SYMPTOMS (MDD, ANHEDONIA)
# (7) METABOLIC PARAMTERS & BEHAVIOR 
# (8) LIKING WANTING TOGETHER 

###############################################################
# (1) SET UP 

# Install and load all required libraries
if (!require("librarian")) install.packages("librarian")
librarian::shelf(ggplot2, readxl, cowplot, ggforce, ggside,
plyr, MASS, dplyr, tidybayes, table1, broom, modelr, distributional, ggpubr, 
lme4, lmerTest, performance, see, patchwork, effects,emmeans, pbkrtest, sjPlot, httpgd, languageserver,
magrittr, dabestr, ggpmisc, ggridges, ggbeeswarm, zeallot, viridis,showtext, RColorBrewer, 
lmeresampler, gridExtra,readr, grDevices, gtsummary, car)


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
path_in <- "./input/" 
path_out <- "./output/" 
pathTUE <- "/mnt/TUE_general/" 

sub_prepro <-  "0.preprocessingQC/"
sub_phases <- "1.Across_Phases/"
sub_groups <- "2.Group_Differences/"
sub_anhedonia <- "3.Anhedonia/"
sub_metparam <-  "4.metabolicParam/"

####################################################################################################################-###
# (2) LOAD DATA 

# Load ITS at time of datafreeze (only those that were not excluded, completed both sessions...)
d_sample <- read_excel(paste(path_in,"Recruitment_Overview.xlsx", sep = ""), sheet = "TUE008_Export_Group")
d_sample$fID <- factor(d_sample$ID)
length(unique(d_sample$fID))

# Load Participant Information & Metabolic values (preprocessed) & Task Data
# Depending on whether AGE & SEX was synthesised (for ethical data sharing) load corresponding file 

if (original_data == TRUE) {
    d_quest <- read_csv(paste("./input_original/","TUE008_Participant_data_NONEsharable.xlsx", sep = ""), show_col_types = FALSE)
    d_quest$fID <- factor(d_quest$ID)
}else{
    d_quest <- read_excel(paste(path_in,"TUE008_Participant_data_sharable.xlsx", sep = ""))
    d_quest$fID <- factor(d_quest$ID)
}

if (original_data == TRUE){
  base::load("./input_original/TUE008_TaskData.RData")
} else {
  base::load("./input/TUE008_Data_TT_sharable.RData")
}

###############################################################################################
# (3). VISUALIZE CHANGES OVER TIME 
# GGRidges Plot to visualize initial (abs) values and changes across phases/time (i.e. delta values) 
###############################################################################################

# Ridgeplot: All absolute values (does not show "intra-individual changes)
  ggplot(dwanting_joint, aes(y = fPhase)) +
  geom_density_ridges(scale = 3, bandwidth = 4,
    aes(x = RatingValue, fill = fMDD), 
    alpha = .6, color = "white", from = 0, to = 100 ) +
  labs(
    x = "Wanting",
    y = "Reward Phase",
    title = "Changes in wanting of food rewards over time",
    subtitle = "Wanting decreases in HCPs, whereas it increases in MDD",
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c(color_HCP, color_MDD),
    name = "Group", guide = "legend"
  ) +
  coord_cartesian(clip = "off") +
  theme_ridges(font_size = 13, grid = FALSE) 

ggsave(paste(path_out, sub_phases, "GGRidge_Wanting.png", sep=""), height = 6, 
width = 8, units = "in", dpi = 600, bg = "transparent")

# Ridgeplot: Initial absolute values 

dwanting_jointGG1 <-  dwanting_joint %>%
  filter(fPhase == "anticipatory1")

ggplot(dwanting_jointGG1, aes(y = fPhase)) +
  geom_density_ridges(aes(x = RatingValue, fill = fMDD), 
    alpha = .5, color = "white", from = 0, to = 100, bandwidth = 4) +
  labs(
    x = "Wanting",
    y = "",
    #title = "Changes in wanting of food rewards over time",
    #subtitle = "Wanting decreases in HCPs, whereas it increases in MDD",
  ) +
  scale_y_discrete(expand = c(0, 0), labels=c("1nd anticipation\n(distal)"))+ 
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c(color_HCP, color_MDD),
    name = "Group", guide = "legend"
  ) +
  coord_cartesian(clip = "on") +
  theme_ridges(font_size = 13, grid = FALSE) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
        axis.text = element_text(family="sans", face = 'plain',size = 12.0),
        axis.text.x = element_text(family="sans", size = 12.0))

ggsave(paste(path_out, sub_phases, "GGRidge_WantingAbs.png", sep=""), 
height = 3, width = 8, units = "in", dpi = 600, bg = "transparent")


# Ridgeplot: Changes over phases/time (i.e. Delta Values)
dwanting_jointGG <-  dwanting_joint %>%
  filter(fPhase != "anticipatory1")

# GGridge with Delta values to account for individual changes instead of group
ggplot(dwanting_jointGG, aes(y = fPhase)) +
  geom_density_ridges(aes(x = RatingValue_Delta, fill = fMDD), 
    alpha = .5, color = "white", from = -100, to = 100, scale=3, bandwidth = 4 ) +
  labs(
    x = expression(bold(Delta~"Wanting")),
    y = "",
    #title = "Changes in wanting of food rewards over time",
    #subtitle = "Wanting decreases in HCPs, whereas it increases in MDD",
  ) +
  #scale_y_discrete(expand = c(0, 0), labels=c("2nd anticipation\n(proximal)",
  #"1st consummation","2nd consummation", "3rd consummation")) +
  scale_y_discrete(expand = c(0, 0), labels=c("",
  "","", "")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c(color_HCP, color_MDD),
    name = "Group", guide = "legend"
  ) +
  coord_cartesian(clip = "on") +
  theme_ridges(font_size = 13, grid = FALSE) +
  theme(legend.position = "none", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 18.0),
        axis.text = element_text(family="sans", face = 'plain',size = 14.0),
        axis.text.x = element_text(family="sans", size = 14.0))

ggsave(paste(path_out, sub_phases, "GGRidge_WantingDelta.png", sep=""), height =6, 
width = 8, units = "in", dpi = 600, bg = "transparent")

## Note: These plots are combined in biorender to build one panel together 

# Same for liking 
# Ridgeplot: Initial absolute values 

dliking_jointGG1 <-  dliking_joint %>%
  filter(fPhase == "anticipatory1") 

ggplot(dliking_jointGG1, aes(y = fPhase)) +
  geom_density_ridges(aes(x = RatingValue, fill = fMDD), 
    alpha = .5, color = "white", from = -100, to = 100 ) +
  labs(
    x = "Liking",
    y = "",
    #title = "Changes in wanting of food rewards over time",
    #subtitle = "Wanting decreases in HCPs, whereas it increases in MDD",
  ) +
  scale_y_discrete(expand = c(0, 0), labels=c(""))+ 
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c(color_HCP, color_MDD),
    name = "Group", guide = "legend"
  ) +
  coord_cartesian(clip = "on") +
  theme_ridges(font_size = 13, grid = FALSE) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 18.0),
        axis.text = element_text(family="sans", face = 'plain',size = 15.5),
        axis.text.x = element_text(family="sans", size = 15.5))

ggsave(paste(path_out, sub_phases, "GGRidge_LikingAbs.png", sep=""), 
height = 3.5, width = 8, units = "in", dpi = 600, bg = "transparent")

# Ridgeplot: Changes over phases/time (i.e. Delta Values)

dliking_jointGG <-  dliking_joint %>%
  filter(fPhase != "anticipatory1")

# GGridge with Delta values to account for individual changes instead of group
ggplot(dliking_jointGG, aes(y = fPhase)) +
  geom_density_ridges(aes(x = RatingValue_Delta, fill = fMDD), 
    alpha = .5, color = "white", from = -100, to = 100, scale=3 ) +
  labs(
    x = expression(bold(Delta~"Liking")),
    y = "",
    #title = "Changes in wanting of food rewards over time",
    #subtitle = "Wanting decreases in HCPs, whereas it increases in MDD",
  ) +
  #scale_y_discrete(expand = c(0, 0), labels=c("2nd anticipation\n(proximal)",
  #"1st consummation","2nd consummation", "3rd consummation")) +
  scale_y_discrete(expand = c(0, 0), labels=c("",
  "","", "")) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c(color_HCP, color_MDD),
    name = "Group", guide = "legend"
  ) +
  coord_cartesian(clip = "on") +
  theme_ridges(font_size = 13, grid = FALSE) +
  theme(legend.position = "none", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 18.0),
        axis.text = element_text(family="sans", face = 'plain',size = 14.0),
        axis.text.x = element_text(family="sans", size = 14.0))

ggsave(paste(path_out, sub_phases, "GGRidge_LikingDelta.png", sep=""), height =6, 
width = 8, units = "in", dpi = 600, bg = "transparent")

############################################################################################################################
# (4). Linear Mixed Effects Modelling ####
###############################################################################################

## Categorical Variables require Coding (Dummy, Effect Coding,.), this will be set per Model separately 

# Treatment Coding: Dummy Coding contr.treatment
# This is useful for directly interpreting locations of cell means, simple effects. 
# contrasts(dliking_joint$fPhase_dicho)  <- contr.treatment(levels(dliking_joint$fPhase_dicho))

# Effect Coding: Sum Coding  contr.sum
# TLTR: Sum Coding compares the mean of the dep. varibale for a givel level (e.g. one level of Phase)
# to the "mean of means" of dep. variables at each level (e.g. each level of Phase)
# This is useful for interpreting interactions of categorical variables, if gives "deviations between conditions and the mean"

# Treatment Coding: Dummy Coding - because this way it is easier to see effect of Con Vs Ant
contrasts(dliking_joint$fPhase_dicho)  <- contr.treatment(levels(dliking_joint$fPhase_dicho))
contrasts(dliking_joint$fPhase_trio)  <- contr.treatment(levels(dliking_joint$fPhase_trio))
contrasts(dliking_joint$fSnack) <-  contr.sum(levels(dliking_joint$fSnack))
contrasts(dliking_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dliking_joint$fPhase_dicho_FCR_TT), base = 1)

# Treatment Coding: Dummy Coding - because this way it is easier to see effect of Con Vs Ant
contrasts(dwanting_joint$fPhase_dicho)  <- contr.treatment(levels(dwanting_joint$fPhase_dicho))
contrasts(dwanting_joint$fPhase_trio)  <- contr.treatment(levels(dwanting_joint$fPhase_trio))
contrasts(dwanting_joint$fSnack) <-  contr.sum(levels(dwanting_joint$fSnack))
contrasts(dwanting_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dwanting_joint$fPhase_dicho_FCR_TT), base = 1)

dwanting_joint$cBDI_sum <- dwanting_joint$BDI_sum - mean(dwanting_joint$BDI_sum) 
dliking_joint$cBDI_sum <- dliking_joint$BDI_sum - mean(dliking_joint$BDI_sum) 

########################################################################################
## BASE CHECK:  Simple Model for Liking & Wanting independent of Group Effects ####
########################################################################################

liking_1a <- lmer(RatingValue ~ fPhase_dicho  + fSnack + cBMI + cAge + cSex + (1 + fSnack + fPhase_trio|ID), dliking_joint)
summary(liking_1a)

liking_1b <- lmer(RatingValue ~ fPhase_trio  + fSnack + cBMI + cAge + cSex + (1 + fSnack + fPhase_trio|ID), dliking_joint)
summary(liking_1b)

wanting_1a <- lmer(RatingValue ~ fPhase_dicho + fSnack  + cBMI + cAge + cSex + (1 + fSnack + fPhase_trio|ID), dwanting_joint)
summary(wanting_1a)

wanting_1b <- lmer(RatingValue ~ fPhase_trio + fSnack  + cBMI + cAge + cSex + (1 + fSnack + fPhase_trio|ID), dwanting_joint)
summary(wanting_1b)

# After inspecting random effects correlation and AIC & BIC  decided to comfbine ANT2 and Cons 
liking_1c <- lmer(RatingValue ~ fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1 + fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_1c)

wanting_1c <- lmer(RatingValue ~ fPhase_dicho_FCR_TT + fSnack  + cBMI + cAge + cSex + (1 + fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_1c)

#Formel Test of Model Fit 
AIC(wanting_1a, wanting_1b, wanting_1c)
BIC(wanting_1a, wanting_1b, wanting_1c)

#######################################################################################
## 4.1. Hypothesis I: Group Differences (HCP vs. MDD) ####
########################################################################################

contrasts(dwanting_joint$fPhase_dicho)  <- contr.treatment(levels(dwanting_joint$fPhase_dicho))
contrasts(dwanting_joint$fPhase_trio)  <- contr.treatment(levels(dwanting_joint$fPhase_trio))
contrasts(dwanting_joint$fSnack) <-  contr.sum(levels(dwanting_joint$fSnack))
contrasts(dwanting_joint$fMDD) <-  contr.treatment(levels(dwanting_joint$fMDD))
contrasts(dwanting_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dwanting_joint$fPhase_dicho_FCR_TT))

contrasts(dliking_joint$fPhase_dicho)  <- contr.treatment(levels(dliking_joint$fPhase_dicho))
contrasts(dliking_joint$fPhase_trio)  <- contr.treatment(levels(dliking_joint$fPhase_trio))
contrasts(dliking_joint$fSnack) <-  contr.sum(levels(dliking_joint$fSnack))
contrasts(dliking_joint$fMDD) <-  contr.treatment(levels(dliking_joint$fMDD), base =1 )
contrasts(dliking_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dliking_joint$fPhase_dicho_FCR_TT))
contrasts(factor(dliking_joint$Meds_AD_Type))
## LIKING 
contrasts(dliking_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dliking_joint$fPhase_dicho_FCR_TT), base = 1) # Change here if you want to get effects for other phase 
liking_2c <- lmer(RatingValue ~ fMDD * fPhase_dicho_FCR_TT +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_2c) 
confint(liking_2c, method="Wald")

# Test boostrapped to make sure results are not dependent on normality assumption 
# lmeresampler package 

# Use Wild Bootstrapping Method 
liking_2c_lmer_par_boot <- lmeresampler::bootstrap(liking_2c, .f = fixef, type = "wild" , hccme = "hc2", aux.dist = "mammen", B = 1000)
summary(liking_2c_lmer_par_boot)

plot(liking_2c_lmer_par_boot)
print(confint(liking_2c_lmer_par_boot), n = 30)

## Sensitivity analyses 
liking_2c_sex <- lmer(RatingValue ~ fMDD * fPhase_dicho_FCR_TT *cSex +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_2c_sex) 

liking_2c_sever <- lmer(RatingValue ~ fMDD * fPhase_dicho_FCR_TT + cBDI_sum +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_2c_sever) 

liking_2d_MEDS <- lmer(RatingValue ~ fMDD * fPhase_dicho_FCR_TT + Meds_AD_Type*fPhase_dicho_FCR_TT + cBDI_sum +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_2d_MEDS) 

# Check Influence of Phase codings
liking_2a <- lmer(RatingValue ~ fMDD * fPhase_dicho +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_trio|ID), dliking_joint)
summary(liking_2a) 

liking_2b <- lmer(RatingValue ~ fMDD * fPhase_trio +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_trio|ID), dliking_joint)
summary(liking_2b) 

########################################################################################
## Same for Wanting  
contrasts(dwanting_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dwanting_joint$fPhase_dicho_FCR_TT), base = 1) # Change here if you want to get effects for other phase 
contrasts(dwanting_joint$fMDD) <-  contr.treatment(levels(dwanting_joint$fMDD), base = 1) # Change here if you want to get effects for other group

wanting_2c <- lmer(RatingValue ~ fMDD * fPhase_dicho_FCR_TT +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_2c)

#Formel Test of Model Fit 
AIC <- AIC(wanting_2a, wanting_2b, wanting_2c)
BIC <- BIC(wanting_2a, wanting_2b, wanting_2c)

# Give Column with Model IDs 
data1 <- cbind(Model = rownames(AIC), AIC)
rownames(data1) <- 1:nrow(data1)

data2 <- cbind(Model = rownames(BIC), BIC)
rownames(data2) <- 1:nrow(data2)

AIC_comp <- ggplot(data1, aes(x = Model,
                  y = AIC,
                  fill = AIC))+geom_tile() + 
                  theme(legend.position = "off",text = element_text(face = 'bold',size = 16.0),
                      axis.text = element_text(face = 'plain',size = 12.0),
                      axis.text.x = element_text(size = 12.0, angle = 65, vjust = 1, hjust=1)) +
                  scale_fill_gradient(low="gray", high="dark gray") +
                  scale_x_discrete(labels = c('Ant - Cons', 'Ant1 - Ant2 - Cons', 'Ant1 - Ant2/Cons'))

                  
BIC_comp <- ggplot(data2, aes(x = Model,
                  y = BIC,
                  fill = BIC))+geom_tile() +
                  theme(legend.position = "off",text = element_text(face = 'bold',size = 16.0),
                      axis.text = element_text(face = 'plain',size = 12.0),
                      axis.text.x = element_text(size = 12.0, angle = 65, vjust = 1, hjust=1)) +
                  scale_fill_gradient(low="gray", high="dark gray") +
                  scale_x_discrete(labels = c('Ant - Cons', 'Ant1 - Ant2 - Cons', 'Ant1 - Ant2/Cons'))

# Check Assumptions 
confint(wanting_2c, method="Wald")
qqnorm(residuals(wanting_3c))
plot(wanting_2c, resid(., type="pearson") ~ fitted(.), abline=0)
hist(resid(wanting_2c))

# Test boostrapped to make sure results are not dependent on normality assumption 
# lmeresampler package 

# Use Wild Bootstrapping Method 
wanting_2c_lmer_par_boot <- lmeresampler::bootstrap(wanting_2c, .f = fixef, type = "wild" , hccme = "hc2", aux.dist = "mammen", B = 1000)
summary(wanting_2c_lmer_par_boot)

print(confint(wanting_2c_lmer_par_boot), n = 30)

# Sensitivtiy Analyses
wanting_2c_sex <- lmer(RatingValue ~ fMDD * fPhase_dicho_FCR_TT *cSex +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_2c_sex)

wanting_2c_sever <- lmer(RatingValue ~ fPhase_dicho_FCR_TT*BDI_sum +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_2c_sever)

wanting_2d_MEDS <- lmer(RatingValue ~ fMDD*fPhase_dicho_FCR_TT + Meds_AD_Type*fPhase_dicho_FCR_TT  +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_2d_MEDS)

# Check whether phase coding changes resutls qualitatively! 
wanting_2a <- lmer(RatingValue ~ fMDD * fPhase_dicho +  fSnack + cBMI + cAge + cSex + (1+ fSnack +fPhase_dicho  |ID), dwanting_joint)
summary(wanting_2a)

wanting_2b <- lmer(RatingValue ~ fMDD * fPhase_trio +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_trio |ID), dwanting_joint)
summary(wanting_2b)

# Exploratory Analyses: Depression Subtypes 
dwanting_joint$AtypicalGroup <- factor(dwanting_joint$AtypicalGroup, levels = c("HCP","low atypical MDD", "high atypical MDD"))

contrasts(dwanting_joint$AtypicalGroup) <-  contr.treatment(levels(dwanting_joint$AtypicalGroup), base = 1)
contrasts(dwanting_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dwanting_joint$fPhase_dicho_FCR_TT), base = 2) # Change here if you want to get effects for other phase 

wanting_2e <- lmer(RatingValue ~ AtypicalGroup * fPhase_dicho_FCR_TT +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_2e)
confint(wanting_2e, method="Wald")

wanting_2f <- lmer(RatingValue ~ cAtypicalBalance_acute * fPhase_dicho_FCR_TT + cBDI_sum  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_2f)

# Super interesting, for liking nothing at all still! 
contrasts(dliking_joint$fPhase_dicho_FCR_TT) <-  contr.treatment(levels(dliking_joint$fPhase_dicho_FCR_TT), base = 1)
dliking_joint$AtypicalGroup <- factor(dliking_joint$AtypicalGroup, levels = c("HCP","low atypical MDD", "high atypical MDD"))
contrasts(dliking_joint$AtypicalGroup) <-  contr.treatment(levels(dliking_joint$AtypicalGroup), base = 3)

liking_2e <- lmer(RatingValue ~ AtypicalGroup * fPhase_dicho_FCR_TT +  fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_2e)
confint(liking_2e, method="Wald")

p_AT = c(0.62,0.55,0.34) # liking 
p.adjust(p_AT, method = "BH", n = length(p_AT))

liking_2f<- lmer(RatingValue ~ cAtypicalBalance_acute  * fPhase_dicho_FCR_TT + fMDD* fPhase_dicho_FCR_TT + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_2f)

## Save Main Results ## 
# Create Html Output to Word Document with Results 
sjPlot:: tab_model(liking_2c, wanting_2c,
                    p.val = "satterthwaite",
                    show.re.var=TRUE,
                    show.ci =FALSE,
                    show.se =TRUE,
                    dv.labels = c("Liking depression", "Wanting depression"), 
                    file= paste(path_out, sub_groups, "/","SI_LW_MDD_ModelResults", ".doc", sep = ""))


########################################################################################
#  MECHANISTIC UNDERSTANDING: Difference Ant1-Ant2-Cons 
########################################################################################
# Get Random Slopes and correlate them to see 

## Get the random effects 

# ranef:extract the conditional modes of the 
# random effect from the fitted model 
# condval is the value of the conditional mean

# Grouping Factor is here our ID (i.e., single Person)
re <- as.data.frame(ranef(wanting_2b, drop = TRUE))
re_phase_ant2 <- filter(re, term=="fPhase_trioanticipatory2") # Use this for Treatmet Coding 
re_phase_cons <- filter(re, term=="fPhase_trioconsummatory") # Use this for Treatmet Coding 

# Merge Random effects with anhedonia df
dwanting_joint_re_Phase_MDD <- merge(dwanting_joint, re_phase_ant2, by.x=("ID"), by.y=("grp"), all = TRUE)
dwanting_joint_re_Phase_MDD <- merge(dwanting_joint_re_Phase_MDD, re_phase_cons, by.x=("ID"), by.y=("grp"), all = FALSE)

# Get Fixed effect of Phase / Intercept
Coeff_Phase_ant2 <- mean(coef(wanting_2b)$ID[,3])
Coeff_Phase_cons <- mean(coef(wanting_2b)$ID[,4])

# Add FE and RE together 
re_phase_ant2$condval + Coeff_Phase_ant2
re_phase_cons$condval + Coeff_Phase_cons

# Check that this corresponds with output: 
fe = fixef(wanting_2b)

# Add fixed effect to conditional mode for unbiased slope 
dwanting_joint_re_Phase_MDD$EC_phase_ant2 <- dwanting_joint_re_Phase_MDD$condval.x + Coeff_Phase_ant2
dwanting_joint_re_Phase_MDD$EC_phase_cons <- dwanting_joint_re_Phase_MDD$condval.y + Coeff_Phase_cons

# 5.4 Visualize the Random Slopes Correlation
plot_PhasesTrio <- 
  ggplot(aes(x = EC_phase_ant2 ,y = EC_phase_cons, color = fMDD),data = dwanting_joint_re_Phase_MDD) +
  geom_point(size = 2.5) +
  geom_smooth(aes(colour=fMDD), size = 1.5, method = 'rlm', alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "bottom", legend.justification = c("center"), text = element_text(face = 'bold',size = 12.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0))+
  xlab(label = 'Random slopes (ant2)') +
  ylab(label = 'Random slopes (cons)') +
  geom_abline(linewidth = 1, linetype = "dashed", color = "gray") + 
  ggtitle("Model with 3 levels for Phase: \nhigh correlation of random slopes")

ggsave(paste(path_out, sub_groups, "Wanting_PhasesTrio_RandSlopesCorr.png", sep=""), 
              plot = plot_PhasesTrio,  height =4, width = 6, units = "in", dpi = 600, bg = "transparent")

# Now plot BIC, AIC, and random effects structure together (Supplement Material) 
G <-ggarrange(GaAltPlot_5P_AbsWanting, ggarrange(BIC_comp, AIC_comp, plot_PhasesTrio, ncol = 3,
      labels = c("b", "c","d"), widths = c(0.25, 0.25,0.5)), nrow = 2, labels = c("a"), common.legend = FALSE)

ggsave(paste(path_out, sub_groups, "SI1_Model_Comparison_Phases.png", sep=""), 
        plot = G,  height = 10, width = 16, units = "in", dpi = 600, bg = "white")

########################################################################################
# VISUALIZING MAIN RESULT OF WANTING AND MDD 
########################################################################################
## Get the random effects 

# ranef:extract the conditional modes of the 
# random effect from the fitted model 
# condval is the value of the conditional mean

# Grouping Factor is here our ID (i.e., single Person)
re <- as.data.frame(ranef(wanting_2c, drop = TRUE))
re_phase_MDD <- filter(re, term=="fPhase_dicho_FCR_TTtaste_test") # Use this for Treatmet Coding 

re_intercept_MDD <- filter(re, term=="(Intercept)") #this now has condval column we call later 

# Merge Random effects with anhedonia df
dwanting_joint_re_Phase_MDD <- merge(dwanting_joint, re_phase_MDD, by.x=("ID"), by.y=("grp"), all = TRUE)
dwanting_joint_re_Phase_MDD <- merge(dwanting_joint_re_Phase_MDD, re_intercept_MDD, by.x=("ID"), by.y=("grp"), all = FALSE)

# Get Fixed effect of Phase / Intercept
Coeff_Phase <- mean(coef(wanting_2c)$ID[,3])
Coeff_Intercept <- mean(coef(wanting_2c)$ID[,1])

# Add FE and RE together 
re_phase_MDD$condval + Coeff_Phase
re_intercept_MDD$condval + Coeff_Intercept

# Check that this corresponds with output: 
fe = fixef(wanting_2c)

# Add fixed effect to conditional mode for unbiased slope 
dwanting_joint_re_Phase_MDD$EC_phase <- dwanting_joint_re_Phase_MDD$condval.x + Coeff_Phase
dwanting_joint_re_Phase_MDD$EC_intercept <- dwanting_joint_re_Phase_MDD$condval.y + Coeff_Intercept

########################################################################################
# VISUALIZING NO EFFECT OF LIKING AND MDD 
########################################################################################
## Get the random effects 

# ranef:extract the conditional modes of the 
# random effect from the fitted model 
# condval is the value of the conditional mean

# Grouping Factor is here our ID (i.e., single Person)
re <- as.data.frame(ranef(liking_2c, drop = TRUE))
re_phase_MDD <- filter(re, term=="fPhase_dicho_FCR_TTtaste_test") # Use this for Treatmet Coding 

re_intercept_MDD <- filter(re, term=="(Intercept)") #this now has condval column we call later 

# Merge Random effects with anhedonia df
dliking_joint_re_Phase_MDD <- merge(dliking_joint, re_phase_MDD, by.x=("ID"), by.y=("grp"), all = TRUE)
dliking_joint_re_Phase_MDD <- merge(dliking_joint_re_Phase_MDD, re_intercept_MDD, by.x=("ID"), by.y=("grp"), all = FALSE)

# Get Fixed effect of Phase / Intercept
Coeff_Phase <- mean(coef(liking_2c)$ID[,3])
Coeff_Intercept <- mean(coef(liking_2c)$ID[,1])

# Add FE and RE together 
re_phase_MDD$condval + Coeff_Phase
re_intercept_MDD$condval + Coeff_Intercept

# Check that this corresponds with output: 
fe = fixef(liking_2c)

# Add fixed effect to conditional mode for unbiased slope 
dliking_joint_re_Phase_MDD$EC_phase <- dliking_joint_re_Phase_MDD$condval.x + Coeff_Phase
dliking_joint_re_Phase_MDD$EC_intercept <- dliking_joint_re_Phase_MDD$condval.y + Coeff_Intercept


##################################################################################
# Other Ratings: SENSITIVITY 
# Check that results are not due to differences in taste ratings per se 
#### Test MDD Differences in other Ratings (not that that is influencing results!)
########################################################################################

# WITH MD GROUP 
MDD_sweet <- lmer(RatingValue ~ fMDD * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1 + fSnack + fPhase_dicho|fID), dsweet)
MDD_sweetAT <- lmer(RatingValue ~ cAtypicalBalance_acute * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1 + fSnack + fPhase_dicho|fID), dsweet)
MDD_salty <- lmer(RatingValue ~ fMDD * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1+  fSnack + fPhase_dicho|fID), dsalty)
MDD_saltyAT <- lmer(RatingValue ~ cAtypicalBalance_acute * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1+  fSnack + fPhase_dicho|fID), dsalty)
MDD_umami <- lmer(RatingValue ~ fMDD * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1+  fSnack + fPhase_dicho|fID), dumami)
MDD_umamiAT <- lmer(RatingValue ~ cAtypicalBalance_acute * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1+  fSnack + fPhase_dicho|fID), dumami)
MDD_intensity <-lmer(RatingValue ~ fMDD * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1+  fSnack + fPhase_dicho|fID), dintensity)
MDD_intensityAT <-lmer(RatingValue ~ cAtypicalBalance_acute * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1+  fSnack + fPhase_dicho|fID), dintensity)

# WITH SHAPS 
dsweet$cSHAPS_sum <- dsweet$SHAPS_sum - mean(dsweet$SHAPS_sum) 
SHAPS_sweet <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1 +fSnack + fPhase_dicho|fID), dsweet)
dsalty$cSHAPS_sum <- dsalty$SHAPS_sum - mean(dsalty$SHAPS_sum) 
SHAPS_salty <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1 +fSnack + fPhase_dicho|fID), dsalty)
dumami$cSHAPS_sum <- dumami$SHAPS_sum - mean(dumami$SHAPS_sum) 
SHAPS_dumami <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1 +fSnack + fPhase_dicho|fID), dumami)
dintensity$cSHAPS_sum <- dintensity$SHAPS_sum - mean(dintensity$SHAPS_sum) 
SHAPS_intensity<- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho +  fSnack + cBMI + cSex + cAge + (1 +fSnack + fPhase_dicho|fID), dintensity)

# Create Html Output to Word Document with Results 
sjPlot:: tab_model(MDD_sweet, MDD_salty, MDD_umami, MDD_intensity,
                    p.val = "satterthwaite",
                    show.re.var=TRUE,
                    dv.labels = c("Sweetness","Saltiness", "Umami", "Intensity"), 
                    file= paste(path_out, sub_groups, "/","SI_OtherRatings_ModelResults_MDD", ".doc", sep = ""))

sjPlot:: tab_model(SHAPS_sweet, SHAPS_salty, SHAPS_dumami, SHAPS_intensity,
                    p.val = "satterthwaite",
                    show.re.var=TRUE,
                    dv.labels = c("Sweetness","Saltiness", "Umami", "Intensity"), 
                    file= paste(path_out, sub_groups, "/","SI_OtherRatings_ModelResults_Shaps", ".doc", sep = ""))

## Wanting and Liking for Different Flavours? 
# Merge Wanting/Liking with Other Ratings.. 
dOther <- dall[(dall$fRatingType != "Wanting") & (dall$fRatingType != "Liking"),]
head(dOther)
dOther <- dOther %>% 
  mutate(fRatingType = case_when( fRatingType == "Süße" ~ "Sweet",
                            fRatingType == "Salzigkeit" ~ "Salty",
                            fRatingType == "Intensität" ~ "Intensity",
                            fRatingType == "Umami" ~ "Umami"))

dOther <- dOther %>%  as_tibble() %>% 
   select(ID, RatingValue, fRatingType, cAge, cSex, cBMI, fSnack) %>%
   tidyr::pivot_wider(names_from = fRatingType, values_from = RatingValue, values_fn = mean)

dOther$cSweet <- dOther$Sweet - mean(dOther$Sweet)
dOther$cSalty <- dOther$Salty - mean(dOther$Salty)
dOther$cIntensity <- dOther$Intensity - mean(dOther$Intensity)
dOther$cUmami <- dOther$Umami - mean(dOther$Umami)

# Merge Main Outcomes with other Ratings 
dWanting_Other <- merge(dwanting_joint, dOther, by.x=("ID"), by.y=("ID"), all = FALSE)
dLiking_Other <- merge(dliking_joint , dOther, by.x=("ID"), by.y=("ID"), all = FALSE)

# Check whether differences in MDD subtype are explained by Intensity 
dWanting_Other$AtypicalGroup <- factor(dWanting_Other$AtypicalGroup, levels = c("HCP","low atypical MDD", "high atypical MDD"))
contrasts(dWanting_Other$AtypicalGroup) <-  contr.treatment(levels(dWanting_Other$AtypicalGroup), base =1)

wanting_2e_Int <- lmer(RatingValue ~ AtypicalGroup * fPhase_dicho_FCR_TT + AtypicalGroup * cIntensity + fSnack.x + cBMI.x + cAge.x + cSex.x + (1+ fSnack.x + fPhase_dicho_FCR_TT|ID), dWanting_Other)
summary(wanting_2e_Int)

wanting_2e_Int2 <- lmer(RatingValue ~ fMDD * fPhase_dicho_FCR_TT + fMDD * cIntensity + fSnack.x + cBMI.x + cAge.x + cSex.x + (1+ fSnack.x + fPhase_dicho_FCR_TT|ID), dWanting_Other)
summary(wanting_2e_Int2)

liking_2e_Int <- lmer(RatingValue ~ AtypicalGroup * fPhase_dicho_FCR_TT + AtypicalGroup* cIntensity + fSnack.x + cBMI.x + cAge.x + cSex.x + (1+ fSnack.x + fPhase_dicho_FCR_TT|ID), dLiking_Other)
summary(liking_2e_Int)

#############################################################################################
## (5). Hypothesis II: Anhedonia ####
#############################################################################################

contrasts(dwanting_joint$fPhase_trio)  <- contr.treatment(levels(dwanting_joint$fPhase_trio))
contrasts(dliking_joint$fPhase_trio)  <- contr.treatment(levels(dliking_joint$fPhase_trio))
contrasts(dwanting_joint$fPhase_dicho_FCR_TT)  <- contr.treatment(levels(dwanting_joint$fPhase_dicho_FCR_TT))
contrasts(dliking_joint$fPhase_dicho_FCR_TT)  <- contr.treatment(levels(dliking_joint$fPhase_dicho_FCR_TT))

# Center Variable 
dliking_joint$cSHAPS_sum <- dliking_joint$SHAPS_sum - mean(dliking_joint$SHAPS_sum) 
dwanting_joint$cSHAPS_sum <- dwanting_joint$SHAPS_sum - mean(dwanting_joint$SHAPS_sum) 
dwanting_joint$cBDI_Anh <- dwanting_joint$BDI_Anhedonia - mean(dwanting_joint$BDI_Anhedonia) 
dliking_joint$cBDI_Anh <- dliking_joint$BDI_Anhedonia - mean(dliking_joint$BDI_Anhedonia) 

liking_3c <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho_FCR_TT + fSnack + cBMI + cAge + cSex + (1 + fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_3c)

# Check Assumptions 
confint(liking_3c, method="Wald")
plot(liking_3c)

qqnorm(residuals(liking_3c))
qqnorm(residuals(liking_3c))
plot(liking_3c, resid(., type="pearson") ~ fitted(.), abline=0)
hist(resid(liking_3c))

# Test boostrapped to make sure results are not dependent on normality assumption 
# lmeresampler package 

# Use Wild Bootstrapping Method 
liking_3c_lmer_par_boot <- lmeresampler::bootstrap(liking_3c, .f = fixef, type = "wild" , hccme = "hc2", aux.dist = "mammen", B = 1000)
summary(liking_3c_lmer_par_boot)

l3c_boot <- plot(liking_3c_lmer_par_boot) + geom_vline(xintercept = 0, color = "gray", linewidth = 2, linetype = "dashed") 

ggsave(paste(path_out, "checks/l3c_boot.png", sep=""), 
        plot = l3c_boot,  height = 10, width = 7, units = "in", dpi = 600, bg = "white")

print(confint(liking_3c_lmer_par_boot), n = 30)

# Sensitivtiy Analyses

liking_3c_sex <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho_FCR_TT* cSex + fSnack + cBMI + cAge + cSex + (1 + fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_3c_sex)

liking_3c_MEDS <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho_FCR_TT* Meds_AD_Type + fSnack + cBMI + cAge + cSex + (1 + fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_3c_MEDS)

liking_3c_BDIcheck <- lmer(RatingValue ~ cBDI_Anh * fPhase_dicho_FCR_TT + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_3c_BDIcheck)
confint(liking_3c_BDIcheck, method="Wald")

# Check phase coding influence 
liking_3b <- lmer(RatingValue ~ cSHAPS_sum * fPhase_trio + fSnack + cBMI + cAge + cSex + (1 + fSnack + fPhase_trio|ID), dliking_joint)
summary(liking_3b)

## Anehdonia and WANTING 
wanting_3c <- lmerTest::lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho_FCR_TT + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_3c)

# Check Assumptions 
confint(wanting_3c, method="Wald")
plot(wanting_3c)

qqnorm(residuals(wanting_3c))
qqnorm(residuals(wanting_3c))
plot(wanting_3c, resid(., type="pearson") ~ fitted(.), abline=0)
hist(resid(wanting_3c))

# Test boostrapped to make sure results are not dependent on normality assumption 
# lmeresampler package 

# Use Wild Bootstrapping Method 
wanting_3c_lmer_par_boot <- lmeresampler::bootstrap(wanting_3c, .f = fixef, type = "wild" , hccme = "hc2", aux.dist = "mammen", B = 1000)
summary(wanting_3c_lmer_par_boot)

w3c_boot <- plot(wanting_3c_lmer_par_boot) + geom_vline(xintercept = 0, color = "gray", linewidth = 2, linetype = "dashed") 

ggsave(paste(path_out, "checks/w3c_boot.png", sep=""), 
        plot = w3c_boot,  height = 10, width = 7, units = "in", dpi = 600, bg = "white")

print(confint(wanting_3c_lmer_par_boot), n = 30)

# SENSITIVITY ANALYSES

wanting_3c_sex <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho_FCR_TT*cSex + fSnack + cBMI + cAge +cSex  + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_3c_sex)

wanting_3c_MEDS <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho_FCR_TT*Med_AD_Type + fSnack + cBMI + cAge +cSex  + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_3c_MEDS)

wanting_3c_BDISEVER <- lmer(RatingValue ~ cBDI_sum * fPhase_dicho_FCR_TT + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_3c_BDISEVER)
confint(wanting_3c_BDISEVER, method="Wald")

wanting_3c_BDIcheck <- lmer(RatingValue ~ cBDI_Anh * fPhase_dicho_FCR_TT + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_3c_BDIcheck)
confint(wanting_3c_BDIcheck, method="Wald")

# Check phase coding influence 
wanting_3a <- lmer(RatingValue ~ cSHAPS_sum * fPhase_dicho  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho|ID), dwanting_joint)
summary(wanting_3a)

wanting_3b <- lmer(RatingValue ~ cSHAPS_sum * fPhase_trio  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_trio|ID), dwanting_joint)
summary(wanting_3b)

## Save Main Results ## 
# Create Html Output to Word Document with Results 
sjPlot:: tab_model(liking_3c, wanting_3c,
                    p.val = "satterthwaite",
                    show.re.var=TRUE,
                    show.ci =FALSE,
                    show.se =TRUE,
                    dv.labels = c("Liking anhedonia", "Wanting anhedonia"), 
                    file= paste(path_out, sub_groups, "/","SI_LW_anhedonia_ModelResults", ".doc", sep = ""))

#########################################################################################
# PREPARE FOR BAYESIAN ANALYSES 
######################################################################################### 

# Write Wanting Joint Table for easy JASP Analysis for Bayes Factors 
dwanting_joint_JASP <- dwanting_joint %>%
  dplyr::group_by(ID, fMDD, fPhase_dicho_FCR_TT, res_logF_AG, cSHAPS_sum ) %>% 
  dplyr::summarize(RatingValue_Delta_Mean  = mean(RatingValue_Delta))%>% 
  filter(fPhase_dicho_FCR_TT == "taste_test")

write.csv(dwanting_joint_JASP, paste(path_out, sub_prepro, "TUE008_WantingJoint_Delta.csv", sep = ""), row.names=FALSE)

dwanting_joint_JASP_Type <- dwanting_joint %>%
  group_by(ID, AtypicalGroup, fPhase_dicho_FCR_TT, res_logF_AG, cSHAPS_sum ) %>% 
  summarize(RatingValue_Delta_Mean  = mean(RatingValue_Delta))%>% 
  filter(fPhase_dicho_FCR_TT == "taste_test")

write.csv(dwanting_joint_JASP_Type, paste(path_out, sub_prepro, "TUE008_WantingJoint_Delta_MDDType.csv", sep = ""), row.names=FALSE)


dwanting_joint_JASP <- dwanting_joint %>%
  group_by(ID, fMDD, fPhase_dicho_FCR_TT, res_logF_AG, cSHAPS_sum ) %>% 
  summarize(RatingValue_Mean  = mean(RatingValue))

write.csv(dwanting_joint_JASP, paste(path_out, sub_prepro, "TUE008_WantingJoint.csv", sep = ""), row.names=FALSE)

dwanting_joint_JASP_type <- dwanting_joint %>%
  group_by(ID, AtypicalGroup, fPhase_dicho_FCR_TT, res_logF_AG, cSHAPS_sum ) %>% 
  summarize(RatingValue_Mean  = mean(RatingValue))

write.csv(dwanting_joint_JASP_type, paste(path_out, sub_prepro, "TUE008_WantingJoint_MDDType.csv", sep = ""), row.names=FALSE)

dwanting_joint_JASP_W <- dwanting_joint_JASP %>%
  group_by(ID, fMDD, res_logF_AG, cSHAPS_sum ) %>% 
  summarize(Wanting_Overall  = mean(RatingValue_Mean))

dliking_joint_JASP_L <- dliking_joint %>%
  group_by(ID, fMDD, fPhase_dicho_FCR_TT, res_logF_AG, cSHAPS_sum ) %>% 
  summarize(RatingValue_Mean  = mean(RatingValue)) %>%
  group_by(ID, fMDD, res_logF_AG, cSHAPS_sum ) %>% 
  summarize(Liking_Overall  = mean(RatingValue_Mean))

dliking_joint_JASP <- dliking_joint %>%
  dplyr::group_by(ID, fMDD, fPhase_dicho_FCR_TT, res_logF_AG, cSHAPS_sum ) %>% 
  dplyr::summarize(RatingValue_Mean  = mean(RatingValue))

write.csv(dliking_joint_JASP, paste(path_out, sub_prepro, "TUE008_LikingJoint.csv", sep = ""), row.names=FALSE)


install.packages("scales")                              # Install & load scales
library("scales")
dwanting_joint_JASP_W$Wanting_Overall <-  scales::rescale(dwanting_joint_JASP_W$Wanting_Overall, to = c(-100, 100))     

d_WL_JASP  <- left_join(dwanting_joint_JASP_W, dliking_joint_JASP_L)
WL_MV_AG_v2 <- lm(cbind(Wanting_Overall, Liking_Overall) ~ res_logF_AG + fMDD, data = d_WL_JASP)
summary(WL_MV_AG_v2)

d_WL_JASP <- d_WL_JASP %>% 
   tidyr::pivot_longer(-c("ID","fMDD","res_logF_AG","cSHAPS_sum"), names_to = "RatingType", values_to = "Rating")

write.csv(d_WL_JASP, paste(path_out, sub_prepro, "TUE008_WantingLikingLong.csv", sep = ""), row.names=FALSE)



#############################################################################################
# PLOTTING THE MAIN RESULT OF WANTING AND ANHEDONIA - Including anhedonia 
#############################################################################################

## Get the random effects 

# ranef:extract the conditional modes of the 
# random effect Liiking from the fitted model 
# condval is the value of the conditional mean

# Grouping Factor is here our ID (i.e., single Person)
re <- as.data.frame(ranef(wanting_3c, drop = TRUE))
re_phase <- filter(re, term=="fPhase_dicho_FCR_TTtaste_test") #this now has condval column we call later 
#re_phase <- filter(re, term=="fPhase.L") #this now has condval column we call later 

re_intercept <- filter(re, term=="(Intercept)") #this now has condval column we call later 

# Merge Random effects with anhedonia df
dwanting_joint_re_Phase <- merge(dwanting_joint, re_phase, by.x=("ID"), by.y=("grp"), all = TRUE)
dwanting_joint_re_Phase <- merge(dwanting_joint_re_Phase, re_intercept, by.x=("ID"), by.y=("grp"), all = FALSE)

# Get Fixed effect of Phase / Intercept
Coeff_Phase <- mean(coef(wanting_3c)$ID[,3]) # Phase fPhase_dicho_FCR_TTtaste
Coeff_Intercept <- mean(coef(wanting_3c)$ID[,1])
# Add FE and RE together 
re_phase$condval + Coeff_Phase
re_intercept$condval + Coeff_Intercept

# Check that this corresponds with output: 
fe = fixef(wanting_3c)

# Add fixed effect to conditional mode for unbiased slope 
dwanting_joint_re_Phase$EC_phase <- dwanting_joint_re_Phase$condval.x + Coeff_Phase
dwanting_joint_re_Phase$EC_intercept <- dwanting_joint_re_Phase$condval.y + Coeff_Intercept

#############################################################################################
# 5.4 Visualize the Random Slopes and Anhedonia 
#############################################################################################

## This figure visualizes Model wanting_1a, individual slopes for overall effect of phase-wanting 
## in the left pane, and right pane Model wanting_3a effect of anhedonia on phase-wanting , split by group

# Median Split SHAPS 
dwanting_joint_re_Phase$MedSp_anhedonia <- as.numeric(dwanting_joint_re_Phase$cSHAPS_sum > median(na.omit(dwanting_joint_re_Phase$cSHAPS_sum)))
dwanting_joint_re_Phase$MedSp_anhedonia <- factor(dwanting_joint_re_Phase$MedSp_anhedonia, labels = c("low SHAPS","high SHAPS"))

#############################################################################################
# 5.4 Visualize the Random Slopes and MDD 
#############################################################################################

plot_phase_SHAPS <- 
  ggplot(aes(x = fPhase_dicho_FCR_TT,y = RatingValue, color = MedSp_anhedonia),data = dwanting_joint_re_Phase) +
  geom_smooth(aes(group=ID), size = 1.5, method = 'rlm', alpha = 0) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(cQ1, cQ4)) +
  coord_cartesian(ylim = c(0,100)) +
  facet_wrap(~MedSp_anhedonia) +
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0)) +
  xlab(label = 'Phase') +
  ylab(label = 'Wanting')  

plot_SHAPS <- 
  ggplot(aes(x = EC_intercept ,y = EC_phase, color = MedSp_anhedonia),data = dwanting_joint_re_Phase) +
  geom_point(size = 2.5) +
  geom_smooth(aes(colour=MedSp_anhedonia), size = 1.5, method = 'rlm', alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(cQ1, cQ4))  +
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Random intercepts') +
  ylab(label = 'Random slopes (phase)')  

# Save Grid Plot
G <- grid.arrange(plot_phase_SHAPS, plot_SHAPS, ncol=2, nrow=2, 
                  layout_matrix = rbind(c(1,2), c(3,3)),
                  widths=c(7, 5), heights=c(6, 0.2))
ggsave(paste(path_out, sub_anhedonia, "Final_Wanting_SHAPS_Facet.png", sep=""), 
        plot = G,  height = 6, width = 10, units = "in", dpi = 600, bg = "transparent")

#############################################################################################
## Now Plot SHAPS in relation to Random effects from MODEL WITHOUT SHAPS 
#############################################################################################

#############################################################################################
# WANTING
#############################################################################################

# ranef:extract the conditional modes of the 
# random effect Liiking from the fitted model 
# condval is the value of the conditional mean

# Grouping Factor is here our ID (i.e., single Person)
re_NULL <- as.data.frame(ranef(wanting_1c, drop = TRUE))
re_NULL_phase <- filter(re_NULL, term=="fPhase_dicho_FCR_TTtaste_test") #this now has condval column we call later 
#re_phase <- filter(re, term=="fPhase.L") #this now has condval column we call later 

re_NULL_intercept <- filter(re_NULL, term=="(Intercept)") #this now has condval column we call later 

# Merge Random effects with anhedonia df
dwanting_joint_re_NULL_Phase <- merge(dwanting_joint, re_NULL_phase, by.x=("ID"), by.y=("grp"), all = TRUE)
dwanting_joint_re_NULL_Phase <- merge(dwanting_joint_re_NULL_Phase, re_NULL_intercept, by.x=("ID"), by.y=("grp"), all = FALSE)

# Get Fixed effect of Phase / Intercept
Coeff_NULL_Phase <- mean(coef(wanting_1c)$ID[,2]) # Phase fPhase_dicho_FCR_TTtaste
Coeff_NULL_Intercept <- mean(coef(wanting_1c)$ID[,1])

# Add FE and RE together 
re_NULL_phase$condval + Coeff_NULL_Phase
re_NULL_intercept$condval + Coeff_NULL_Intercept

# Check that this corresponds with output: 
fe_NULL = fixef(wanting_1c)

# Add fixed effect to conditional mode for unbiased slope 
dwanting_joint_re_NULL_Phase$EC_NULL_phase <- dwanting_joint_re_NULL_Phase$condval.x + Coeff_NULL_Phase
dwanting_joint_re_NULL_Phase$EC_NULL_intercept <- dwanting_joint_re_NULL_Phase$condval.y + Coeff_NULL_Intercept

#############################################################################################
## LIKING 
#############################################################################################

# ranef:extract the conditional modes of the 
# random effect Liiking from the fitted model 
# condval is the value of the conditional mean

# Grouping Factor is here our ID (i.e., single Person)
re_NULL <- as.data.frame(ranef(liking_1c, drop = TRUE))
re_NULL_phase <- filter(re_NULL, term=="fPhase_dicho_FCR_TTtaste_test") #this now has condval column we call later 
#re_phase <- filter(re, term=="fPhase.L") #this now has condval column we call later 

re_NULL_intercept <- filter(re_NULL, term=="(Intercept)") #this now has condval column we call later 

# Merge Random effects with anhedonia df
dliking_joint_re_NULL_Phase <- merge(dliking_joint, re_NULL_phase, by.x=("ID"), by.y=("grp"), all = TRUE)
dliking_joint_re_NULL_Phase <- merge(dliking_joint_re_NULL_Phase, re_NULL_intercept, by.x=("ID"), by.y=("grp"), all = FALSE)

# Get Fixed effect of Phase / Intercept
Coeff_NULL_Phase <- mean(coef(liking_1c)$ID[,2]) # Phase fPhase_dicho_FCR_TTtaste
Coeff_NULL_Intercept <- mean(coef(liking_1c)$ID[,1])

# Add FE and RE together 
re_NULL_phase$condval + Coeff_NULL_Phase
re_NULL_intercept$condval + Coeff_NULL_Intercept

# Check that this corresponds with output: 
fe_NULL = fixef(liking_1c)

# Add fixed effect to conditional mode for unbiased slope 
dliking_joint_re_NULL_Phase$EC_NULL_phase <- dliking_joint_re_NULL_Phase$condval.x + Coeff_NULL_Phase
dliking_joint_re_NULL_Phase$EC_NULL_intercept <- dliking_joint_re_NULL_Phase$condval.y + Coeff_NULL_Intercept


#############################################################################################
# 5.4 Visualize the Random Slopes and Anhedonia 
#############################################################################################
# Median Split Met Parameters
dwanting_joint_re_NULL_Phase$MedSp_FAG <- as.numeric(dwanting_joint_re_NULL_Phase$res_logF_AG > median(na.omit(dwanting_joint_re_NULL_Phase$res_logF_AG)))
dwanting_joint_re_NULL_Phase$MedSp_FAG <- factor(dwanting_joint_re_NULL_Phase$MedSp_FAG, labels = c("low F AG","high F AG"))
dwanting_joint_re_NULL_Phase$MedSp_anhedonia <- as.numeric(dwanting_joint_re_NULL_Phase$cSHAPS_sum > median(na.omit(dwanting_joint_re_NULL_Phase$cSHAPS_sum)))
dwanting_joint_re_NULL_Phase$MedSp_anhedonia <- factor(dwanting_joint_re_NULL_Phase$MedSp_anhedonia, labels = c("low SHAPS","high SHAPS"))

### Visualize Random Slopes and Intercepts using only 1 value per subject, so that CI are correct
dwanting_joint_re_NULL_Phase_Collapsed <- dwanting_joint_re_NULL_Phase %>%
  group_by(ID, fMDD, BMI, SHAPS_sum, res_logF_AG, MedSp_FAG,MedSp_anhedonia, AtypicalGroup, EC_NULL_phase, EC_NULL_intercept) %>%
  summarise(EC_NULL_phase = mean(EC_NULL_phase)) %>%
  summarise(EC_NULL_intercept = mean(EC_NULL_intercept))

dliking_joint_re_NULL_Phase_Collapsed <- dliking_joint_re_NULL_Phase %>%
  group_by(ID, fMDD, BMI, SHAPS_sum,  res_logF_AG, AtypicalGroup, EC_NULL_phase, EC_NULL_intercept) %>%
  summarise(EC_NULL_phase = mean(EC_NULL_phase)) %>%
  summarise(EC_NULL_intercept = mean(EC_NULL_intercept))


plot_anhedonia1 <- 
  ggplot(aes(x = SHAPS_sum ,y = EC_NULL_intercept),data = dwanting_joint_re_NULL_Phase_Collapsed) +
  geom_point(aes(color = fMDD, size = BMI)) +
  #stat_cor(label.y = 80, label.x = 10,  size = 5) + 
  geom_smooth(size = 1.5, method = 'lm', alpha = 0.5, color = 'black') +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "none",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=16), axis.text.x = element_text(size = 12.0))+
  xlab(label = 'SHAPS') +
  ylab(label = 'Intercept\nwanting')+
  scale_y_continuous(breaks=seq(0,90,20)) 

plot_anhedonia2 <- 
  ggplot(aes(x = SHAPS_sum ,y = EC_NULL_phase),data = dwanting_joint_re_NULL_Phase_Collapsed) +
  geom_point(aes(color = fMDD, size = BMI)) +
  #stat_cor(label.y = 40, label.x = 10,  size = 5) + 
  geom_smooth(size = 1.5, method = 'lm', alpha = 0.5, color = 'black') +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "bottom",legend.justification = "center",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=16), axis.text.x = element_text(size = 12.0))+
  xlab(label = 'SHAPS') +
  ylab(label = 'Consummatory phase\nslope, wanting') +
  ylim(-40,30)
  #ggtitle("Higher SHAPS Scores are associated \nwith stronger wanting increases", )

plot_anhedonia3 <- 
  ggplot(aes(x = SHAPS_sum ,y = EC_NULL_intercept),data = dliking_joint_re_NULL_Phase_Collapsed) +
  geom_point(aes(color = fMDD, size = BMI)) +
  #stat_cor(label.y = 80, label.x = 10,  size = 5) + 
  geom_smooth(size = 1.5, method = 'lm', alpha = 0.5, color = 'black') +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "none",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=16), axis.text.x = element_text(size = 12.0))+
  xlab(label = 'SHAPS') +
  ylab(label = 'Intercept\nliking') +
  scale_y_continuous(breaks=seq(-30, 60, 20)) 


plot_anhedonia4 <- 
  ggplot(aes(x = SHAPS_sum ,y = EC_NULL_phase),data = dliking_joint_re_NULL_Phase_Collapsed) +
  geom_point(aes(color = fMDD, size = BMI)) +
  #stat_cor(label.y = 40, label.x = 10,  size = 5) + 
  geom_smooth(size = 1.5, method = 'lm', alpha = 0.5, color = 'black') +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "bottom",legend.justification = "center",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=16), axis.text.x = element_text(size = 12.0))+
  xlab(label = 'SHAPS') +
  ylab(label = 'Consummatory phase\nslope, liking') +
  ylim(-40,30)

#All Anhedonia Plots together 
# Figure 2 

# Save Grid Plot
legend <- get_legend(plot_anhedonia2) # get common legend 

G_TT_Shaps <-ggarrange(plot_anhedonia3, plot_anhedonia4, plot_anhedonia1, plot_anhedonia2, ncol = 2, nrow = 2, 
      labels = c('A', 'B','C','D'), common.legend = TRUE, legend = "bottom", legend.grob = legend)

ggsave(paste(path_out, sub_anhedonia, "Fig2_Revision_Wanting_SHAPS.png", sep=""), 
        plot = G_TT_Shaps,  height = 10, width = 11, units = "in", dpi = 600, bg = "white")


#####################################################################################
# Same for MDD, Plot the Slope distribution from a Model without the Predictor MDD (unbiased)
#####################################################################################

# Plot random slopes distribution to understand better whether MDD decrease wanting less or actually increase

dwanting_joint_re_Phase_MDD_Collapsed <- dwanting_joint_re_Phase_MDD %>%
  group_by(ID, fMDD, BMI, SHAPS_sum, fSnack, fPhase_dicho_FCR_TT, AtypicalGroup, RatingValue, EC_phase, EC_intercept) %>%
  summarise(EC_phase = mean(EC_phase)) %>%
  summarise(EC_intercept = mean(EC_intercept))

dliking_joint_re_Phase_MDD_Collapsed <- dliking_joint_re_Phase_MDD %>%
  group_by(ID, fMDD, BMI, SHAPS_sum, fPhase_dicho_FCR_TT, AtypicalGroup, RatingValue, EC_phase, EC_intercept) %>%
  summarise(EC_phase = mean(EC_phase)) %>%
  summarise(EC_intercept = mean(EC_intercept))

plot_MDD2 <-
  ggplot(aes(x = EC_NULL_phase, group = fMDD, fill = fMDD),data = dwanting_joint_re_NULL_Phase_Collapsed) +
  geom_density(alpha=.5, adjust = 1, ,color="white") +
  geom_vline(xintercept = 0, color = "white", linewidth = 2, linetype = "dashed") +  #white #5a5656
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  xlab(label = 'Unbiased random \nslopes (phase)') +
  ylab(label = 'Density') 
  #ggtitle("Patients with MDD decrease \nwanting ratings less than HCPs")  

ggsave(paste(path_out, sub_groups, "MDD_slopes.png", sep=""),
       plot = plot_MDD2,  height = 6, width = 6, units = "in", dpi = 600, bg = "white")

plot_MDD2_intercepts <-
  ggplot(aes(x = EC_NULL_intercept, group = fMDD, fill = fMDD),data = dwanting_joint_re_NULL_Phase_Collapsed) +
  geom_density(alpha=.5, adjust = 1,color= "white") +
  geom_vline(xintercept = mean(na.omit(dwanting_joint_re_NULL_Phase$EC_NULL_intercept)), 
            color = "white", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  xlab(label = 'Unbiased random \nintercepts') +
  ylab(label = 'Density')  
  #ggtitle("Patients with MDD decrease \nwanting ratings less than HCPs")  

ggsave(paste(path_out, sub_groups, "MDD_intercepts.png", sep=""),
       plot = plot_MDD2_intercepts,  height = 6, width = 6, units = "in", dpi = 600, bg = "white")

## Alternative Design to Fit with GGrdiges 
dwanting_joint_re_NULL_Phase_Collapsed$fMDD <- factor(dwanting_joint_re_NULL_Phase_Collapsed$fMDD, levels = c("HCP", "MDD"))

dwanting_joint_re_NULL_Phase_Collapsed$BETA <- 'BETA'

plot_MDD2GG <- ggplot(dwanting_joint_re_NULL_Phase_Collapsed, aes(y = BETA)) +
  geom_density_ridges(aes(x = EC_NULL_phase, fill = fMDD), 
    alpha = .5, color = "white", bandwidth = 4, scale = 6) +
  geom_vline(xintercept = 0, 
            color = "white", linewidth = 1, linetype = "dashed") +
  labs(
    x = "",
    y = "",
    #title = "Changes in wanting of food rewards over time",
    #subtitle = "Wanting decreases in HCPs, whereas it increases in MDD",
  ) +
  scale_y_discrete(expand = c(0, 0), labels=c("",""))+ 
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c(color_HCP, color_MDD  ),
    name = "Group", guide = "legend"
  ) +
  coord_cartesian(clip = "on") +
  theme_ridges(font_size = 13, grid = FALSE) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
        axis.text = element_text(family="sans", face = 'plain',size = 12.0),
        axis.text.x = element_text(family="sans", size = 12.0))

ggsave(paste(path_out, sub_phases, "GGRidge_Slopes.png", sep=""), 
height = 3, width = 8, units = "in", dpi = 600, bg = "white")


plot_MDD2_interceptsGG <- ggplot(dwanting_joint_re_NULL_Phase_Collapsed, aes(y = BETA)) +
  geom_density_ridges(aes(x = EC_NULL_intercept, fill = fMDD), 
    alpha = .5, color = "white", bandwidth = 4, scale = 6) +
  geom_vline(xintercept = mean(na.omit(dwanting_joint_re_NULL_Phase$EC_NULL_intercept)), 
            color = "white", linewidth = 1, linetype = "dashed") +
  labs(
    x = "",
    y = "",
    #title = "Changes in wanting of food rewards over time",
    #subtitle = "Wanting decreases in HCPs, whereas it increases in MDD",
  ) +
  scale_y_discrete(expand = c(0, 0), labels=c("",""))+ 
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_cyclical(
    values = c(color_HCP, color_MDD  ),
    name = "Group", guide = "legend"
  ) +
  coord_cartesian(clip = "on") +
  theme_ridges(font_size = 13, grid = FALSE) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
        axis.text = element_text(family="sans", face = 'plain',size = 12.0),
        axis.text.x = element_text(family="sans", size = 12.0))

ggsave(paste(path_out, sub_phases, "GGRidge_Intercepts.png", sep=""), 
height = 3, width = 8, units = "in", dpi = 600, bg = "transparent")


# Save PLOT Alltogethr  plot_phase_MDD
plot_phase_MDD <- 
  ggplot(aes(x = factor(fPhase_dicho_FCR_TT),y = RatingValue, color = fMDD),data = dwanting_joint_re_Phase_MDD_Collapsed) +
  geom_smooth(aes(group=ID), size = 1, position = position_dodge(0.2) ,method = 'rlm', alpha = 0) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  coord_cartesian(ylim = c(0,100)) +
  facet_grid(~fMDD) + 
  theme(legend.position = "none",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.text.x = element_text(size = 12.0, margin = margin(r=5)),
        panel.spacing = unit(-5,'lines')) +
  xlab(label = 'Phase') +
  scale_x_discrete(labels = c('1. ant','2.ant\n+cons')) + 
  ylab(label = 'Wanting') 

# Save PLOT Alltogethr  
plot_phase_MDD_liking <- 
  ggplot(aes(x = fPhase_dicho_FCR_TT,y = RatingValue, color = fMDD),data = dliking_joint_re_Phase_MDD_Collapsed) +
  geom_smooth(aes(group=ID), size = 1,position = position_dodge(0.2), method = 'rlm', alpha = 0) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  coord_cartesian(ylim = c(-35,75)) +
  facet_grid(~fMDD) + 
  theme(legend.position = "none",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.text.x = element_text(size = 12.0),
        panel.spacing = unit(-5,'lines')) +
  xlab(label = 'Phase') +
  scale_x_discrete(labels = c('1. ant','2.ant\n+cons')) + 
  ylab(label = 'Liking') 

#####################################################################################
# Mechanistic Understanding: INTERCEPTS & SLOPES 
#####################################################################################

## For Wanting zoom into underlying "process" 
## Pot Intercepts and Slopes Correlation 
plot_IntSlopes <- 
  ggplot(aes(x = EC_NULL_intercept ,y = EC_NULL_phase  , color = fMDD),data = dwanting_joint_re_NULL_Phase_Collapsed) +
  geom_point(size = 3.5) +
  #geom_vline(xintercept = mean(dwanting_joint_re_NULL_Phase_Collapsed$EC_NULL_intercept), color = "#5a5656", linewidth = 2, linetype = "dashed") +
  #geom_hline(yintercept = 0, color = "#5a5656", linewidth = 2, linetype = "dashed") +
  geom_smooth(aes(colour=fMDD), size = 1.5, method = 'rlm', alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "none",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  xlab(label = 'Intercepts wanting') +
  ylab(label = 'Slopes wanting') #+  
  #ggtitle("Changes depending on initial levels") +
  #theme(plot.title = element_text(size = 17, face = "bold"))

### SCATTER AND DENSITY TOGETHER 
plot_IntSlopes +
border()   

# Cleaning the plots
pmain <- plot_IntSlopes + rremove("legend")
#yplot <- yplot + clean_theme() + rremove("legend") 
xdens <- plot_MDD2_interceptsGG  + clean_theme() + rremove("legend")

ydens <- plot_MDD2GG + rotate() + clean_theme() + rremove("legend")

p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.3, "null"), position = "top")
P_Int_Slopes_Density <- insert_yaxis_grob(p1, ydens, grid::unit(.3, "null"), position = "right")
ggdraw(P_Int_Slopes_Density)
ggsave(paste(path_out, sub_groups, "Wanting_IntSlopes_Scatter_VF.png", sep=""), 
       plot = P_Int_Slopes_Density,  height = 5, width = 6, units = "in", dpi = 600, bg = "transparent")


######################################################################################
# Now Testing whether SHAPS explains also something without MDD status 
# For that we regress SHAPS onto MDD and work with the residuals 
####################################################################################

# Make sure to center MDD for that! 
contrasts(dwanting_joint$fMDD)  <- contr.sum(levels(dwanting_joint$fMDD))

lm_Shaps <-lm(SHAPS_sum ~ fMDD, data = dwanting_joint)
dwanting_joint$uni_SHAPS <- residuals(lm_Shaps)

wanting_3c_resShaps <- lmer(RatingValue ~ uni_SHAPS * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_3c_resShaps)


####################################################################################
# Atypical MDD
####################################################################################

dwanting_joint_re_NULL_Phase_Collapsed_AT <- dwanting_joint_re_NULL_Phase_Collapsed[complete.cases(dwanting_joint_re_NULL_Phase_Collapsed[,c("AtypicalGroup")]),]
dwanting_joint_re_NULL_Phase_Collapsed_AT$AtypicalGroup <- factor(dwanting_joint_re_NULL_Phase_Collapsed_AT$AtypicalGroup,
                        levels = c("HCP", "low atypical MDD", "high atypical MDD"),
                        ordered = TRUE)

plot_MDD_AT <-
  ggplot(aes(x = EC_NULL_phase, group = AtypicalGroup, fill = AtypicalGroup),data = dwanting_joint_re_NULL_Phase_Collapsed_AT) +
  geom_density(alpha=.5, adjust = 1.5) +
  geom_vline(xintercept = 0, color = "#5a5656", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_lowatypical, color_atypical)) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
        axis.text = element_text(family="sans", face = 'plain',size = 12.0),axis.text.x = element_text(family="sans", size = 12.0)) +
  xlab(label = 'Unbiased random \nslopes (phase)') +
  ylab(label = 'density') 
  #ggtitle("Patients with MDD decrease \nwanting ratings less than HCPs")  

plot_MDD_AT_intercepts <-
  ggplot(aes(x = EC_NULL_intercept, group = AtypicalGroup, fill = AtypicalGroup),data = dwanting_joint_re_NULL_Phase_Collapsed_AT) +
  geom_density(alpha=.5, adjust = 1) +
  geom_vline(xintercept = mean(na.omit(dwanting_joint_re_NULL_Phase$EC_NULL_intercept)), 
            color = "#5a5656", linewidth = 2, linetype = "dashed") +
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_lowatypical, color_atypical)) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
        axis.text = element_text(family="sans", face = 'plain',size = 12.0),axis.text.x = element_text(family="sans", size = 12.0)) +
  xlab(label = 'Unbiased random \nintercepts') +
  ylab(label = 'density') 
  #ggtitle("Patients with MDD decrease \nwanting ratings less than HCPs")  

# White spcae for methods to add
legend <- get_legend(plot_MDD_AT) # get common legend 

G_AT_TT <-ggarrange(plot_MDD_AT_intercepts, plot_MDD_AT, ncol = 2, nrow = 1, 
            labels = c("d","e"), common.legend = TRUE, legend = "bottom", legend.grob = legend)
ggsave(paste(path_out, sub_groups, "MDD_intercepts_slopes_plot_ATypical.png", sep=""), 
        plot = G_AT_TT,  height = 4, width = 10, units = "in", dpi = 600, bg = "white")

# Plot Scatter with marginal density plots 

## Intercepts and Slopes from NULL Model 
plot_IntSlopes_AT <- 
  ggplot(aes(x = EC_NULL_intercept ,y = EC_NULL_phase , color = AtypicalGroup),data = dwanting_joint_re_NULL_Phase_Collapsed_AT) +
  geom_point(size = 3.5) +
    geom_vline(xintercept = mean(dwanting_joint_re_NULL_Phase_Collapsed_AT$EC_NULL_intercept), color = "#5a5656", linewidth = 2, linetype = "dashed") +
  geom_hline(yintercept = 0, color = "#5a5656", linewidth = 2, linetype = "dashed") +
  geom_smooth(aes(colour=AtypicalGroup), size = 1.5, method = 'rlm', alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_lowatypical, color_atypical)) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
        axis.text = element_text(family="sans", face = 'plain',size = 12.0),axis.text.x = element_text(family="sans", size = 12.0)) +
  xlab(label = 'Intercepts wanting') +
  ylab(label = 'Slopes wanting') +  ggtitle("")

# Correlation (also per Group) 
cor.test(formula = ~ EC_NULL_phase + EC_NULL_intercept,
         data = dwanting_joint_re_NULL_Phase,
         subset = AtypicalGroup == "low atypical MDD")

## Prettify Plots, remove gaps between Marginal Plots
pmain <- plot_IntSlopes_AT + rremove("legend")
xdens <- plot_MDD_AT_intercepts  + clean_theme() + rremove("legend")
ydens <- plot_MDD_AT + rotate() + clean_theme() + rremove("legend")
p1 <- insert_xaxis_grob(pmain, xdens, grid::unit(.3, "null"), position = "top")
p2 <- insert_yaxis_grob(p1, ydens, grid::unit(.3, "null"), position = "right")
ggdraw(p2)
ggsave(paste(path_out, "Wanting_IntSlopes_Scatter_ATB_VF.png", sep=""), 
       plot = p2,  height = 6, width = 8, units = "in", dpi = 600, bg = "white")

###### MDD & ATypical Stuff together for simplicity 
## Revision of Figure 1: Box for TT (Null Placeholder), Wanting, Liking Spaghetti Plot 
title0 <- ggdraw() + draw_label("Design: Disentangling food reward anticipation and consummation", size = 18, fontface = 'bold',hjust = 0, x =0) + theme(plot.margin = margin(0, 0, 0, 45))

title1 <- ggdraw() + draw_label("No differences in liking\nbetween HCP and MDD", size = 18, fontface = 'bold',hjust = 0, x =0) + theme(plot.margin = margin(0, 0, 0, 45))

title2 <- ggdraw() + draw_label("Difference in anticipatory wanting\nbetween HCP and MDD", size = 18, fontface = 'bold',hjust = 0, x =0) + theme(plot.margin = margin(0, 0, 0, 45))

title3 <- ggdraw() + draw_label("Lower wanting during initial anticipation but attenuated decreases\nin wanting over time in MDD", size = 18, fontface = 'bold',hjust = 0, x =0) + theme(plot.margin = margin(0, 0, 0, 45))

#title4 <- ggdraw() + draw_label("Difference in wanting according to MDD subtypes\n", size = 18, fontface = 'bold',x = 0,hjust = 0) +
#  theme(plot.margin = margin(0, 0, 0, 45))

BC <- cowplot::plot_grid(title1, title2, NULL, NULL,
                  ncol = 2, nrow = 2, rel_heights =  c(0.1,1),
                  labels = c("B","C"," "," "), label_size = 18)

DE <- cowplot::plot_grid(title3, NULL, plot_phase_MDD, P_Int_Slopes_Density,
                  ncol = 2, nrow = 2, rel_heights =  c(0.1,1),
                  labels = c("D"," "," "," "), label_size = 18)

Fig1_Revision <- cowplot::plot_grid(title0, 
                  NULL, 
                  BC, 
                  DE, 
                  ncol = 1, nrow = 4, rel_heights =  c(0.1,0.7,1,1),
                  labels = c("A","","","","",""), label_size = 18)

ggsave(paste(path_out, sub_groups, "TT_Fig1_RevisionV3.png", sep=""), 
        plot = Fig1_Revision,  height = 18, width = 12,
        units = "in", dpi = 600, bg = "white")


#############################################################################################################
# (6). Blood parameters and SYMPTOMS 
#############################################################################################################

# Visalize Atypical 
d_quest <- d_quest[complete.cases(d_quest[,c("MedSp_AtyBalance")]),]
d_quest_MDD <- d_quest %>% 
                    filter(fMDD == "MDD") 

# Inspect Distribution of Atypical Balance Scores 
ABS_median <- median(na.omit(d_quest$AtypicalBalance_acute[d_quest$fMDD == "MDD"]))
ABS_mean <-mean(na.omit(d_quest$AtypicalBalance_acute[d_quest$fMDD == "MDD"]))
ABS_std <-sd(na.omit(d_quest$AtypicalBalance_acute[d_quest$fMDD == "MDD"]))

# Visualize Atypical Balance Score Histogram 
ABS_hist <- ggplot(d_quest_MDD, aes(x=AtypicalBalance_acute, fill = AtypicalGroup)) + 
 geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  geom_vline(aes(xintercept= ABS_mean, color = "mean"), linewidth = 2, linetype = "dashed")+                 
  geom_vline(aes(xintercept= ABS_median, color = "median"), linewidth = 2, linetype = "dashed") + 
  scale_fill_manual(guide = guide_legend(title="MDD Group"),values = c(color_lowatypical, color_atypical)) +
  xlab(label = 'Atypical Balance Score') +
  scale_color_manual(name = "statistics", values = c(median = "#7a3023", mean = "#613535"))

ggsave(paste(path_out, sub_prepro, "1.atypical_hist.png", sep=""), 
        plot = ABS_hist,  height = 6, width = 8, units = "in", dpi = 600, bg = "white")

# Visualize Atypical Balance Score & ghrelin F AG 
# Scatter Only MDD 
# Scatter plot colored by groups 
ATB_AG_Scatter_MDD <- ggscatter(d_quest_MDD, x = "AtypicalBalance_acute", y = "res_logF_AG",
            color = "AtypicalGroup",
            size = 3, alpha = 0.6)+
  #gradient_color(c(color_HCP, color_lowatypical, color_atypical)) + 
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_lowatypical, color_atypical)) +
  theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0))+
  xlab(label = 'Atypical Balance Score') +
  ylab(label = "Fasting acyl ghrelin (res log)") +
border()   

# Correlation (also per Group) 
cor.test(formula = ~ cAtypicalBalance_acute + res_logF_AG,
         data = d_quest)

# Scatter plot colored by groups 
ATB_AG_Scatter <- ggscatter(d_quest, x = "cAtypicalBalance_acute", y = "res_logF_AG",
            color = "AtypicalGroup",
            size = 3, alpha = 0.6)+
  #gradient_color(c(color_HCP, color_lowatypical, color_atypical)) + 
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_lowatypical, color_atypical)) +
  theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0))+
  xlab(label = 'Atypical Balance Score') +
  ylab(label = "Fasting acyl ghrelin (res log)") +
border()   

# Marginal density plot of x (top panel) and y (right panel)
# Next to Scatterplot 
dummy <- ddply(d_quest, c("AtypicalGroup"), summarise, 
          res_logF_AG.mean = mean(na.omit(res_logF_AG)))

ATB_AG_density <- ggplot(d_quest, aes(res_logF_AG, after_stat(count),  fill = AtypicalGroup)) +
  geom_density(alpha = 0.5, adjust = 0.8) +
  #geom_vline(xintercept= 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed")+                 
  scale_fill_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_lowatypical, color_atypical)) +
  theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  geom_vline(data = dummy, aes(xintercept = res_logF_AG.mean),  
                    linewidth = 2, linetype = "dashed", colour = c(color_HCP, color_lowatypical, color_atypical)) +
rotate()

# Cleaning the plots
ATB_AG_Scatter <- ATB_AG_Scatter + rremove("legend")
ATB_AG_density <- ATB_AG_density + clean_theme() + rremove("legend")

# Arranging the plot using cowplot
P_Ghrelin_Atypical <- cowplot::plot_grid(ATB_AG_Scatter, ATB_AG_density, ncol = 2, align = "hv", 
      rel_widths = c(2, 1))


ggsave(paste(path_out, sub_metparam, "1.Ghrelin_atypical_V4.png", sep=""), 
        plot = P_Ghrelin_Atypical,  height = 6, width = 8, units = "in", dpi = 600, bg = "white")

## Prepare for Statistical Tests 

# prepare SHAPS - center 
d_quest$cSHAPS_sum <- d_quest$SHAPS_sum - mean(!is.na(d_quest$SHAPS_sum))

# Add Atypical Distinction 
d_quest_MDD <- d_quest %>% 
                    filter(fMDD == "MDD") 

# Center Variables for Models 
d_quest_MDD$cSHAPS_sum <- d_quest_MDD$SHAPS_sum - mean(!is.na(d_quest_MDD$SHAPS_sum))


## Plot Metabolic Paramters and SHAPS (anhedonia measure)

# HOMA-IR and SHAPS 
p_SHAPS_Homa <- ggplot(data = d_quest, aes(x = res_logHOMA, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(color = 'black', method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0.5)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) + 
  theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0))+
  xlab(label = 'HOMA-IR (res log)') +
  ylab(label = "SHAPS") #+
  #labs(title =  "Higher Insulin Resistance is associated with  \nhigher anhedonia")

ggsave(paste(path_out, sub_metparam, "1.SHAPS_HOMA.png", sep=""), 
        plot = p_SHAPS_Homa,  height = 5, width = 8, units = "in", dpi = 600, bg = "white")

## Test whether metabolic subtype related to atypical MDD
d_quest_MDD2 <- d_quest_MDD[complete.cases(d_quest_MDD[,c("MedSp_AtyBalance")]),]
length(unique(d_quest_MDD$ID))

## Generally SHAPS and Atypical 
p_SHAPS_Atypical <- ggplot(data = d_quest_MDD2, aes(x = AtypicalBalance_acute, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(color = 'black', method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  stat_cor(label.y = 35, label.x = 20)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) + 
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Atypical Balance Score (SIGH-ADS)') +
  ylab(label = "SHAPS") 
  #labs(title =  "Higher atypical balance is \nnot associated with lower anhedonia")

###################################################################################

# LM for SHAPS and Metabolic parameters 

# ADD BDI to see whether it holds up no diff when adding severity
d_quest$cBDI_sum <- d_quest$BDI_sum - mean(d_quest$BDI_sum)
d_quest$cSHAPS_sum <- d_quest$SHAPS_sum - mean(!is.na(d_quest$SHAPS_sum))

## HOMA IR 
Shaps_Homa1 <- lm(cSHAPS_sum  ~ res_logHOMA  + cBMI + cAge+ cSex , d_quest)
summary(Shaps_Homa1)
confint(Shaps_Homa1)

# Check whether still holds true when taking into account MDD 
Shaps_Homa2 <- lm(cSHAPS_sum  ~ res_logHOMA  +  fMDD  + cBMI + cAge+ cSex , d_quest)
summary(Shaps_Homa2)
confint(Shaps_Homa2)

Shaps_Homa3 <- lm(cSHAPS_sum  ~ res_logHOMA  +  fMDD + cBDI_sum + cBMI + cAge+ cSex , d_quest)
summary(Shaps_Homa3)
Metaboli
## Triglyceride Index

Shaps_TyG <- lm(cSHAPS_sum  ~ res_TyG  + cBMI + cAge+ cSex , d_quest)
summary(Shaps_TyG)
confint(Shaps_TyG)
# Check whether still holds true when taking into account MDD 
Shaps_TyG2 <- lm(cSHAPS_sum  ~ res_TyG +  fMDD  + cBMI + cAge+ cSex , d_quest)
summary(Shaps_TyG2)
confint(Shaps_TyG2)

Shaps_TyG3 <- lm(cSHAPS_sum  ~ res_TyG +  fMDD   + cBDI_sum + cBMI + cAge+ cSex , d_quest)
summary(Shaps_TyG3)
confint(Shaps_TyG3)

# As both indeces rely on Glucose, check!
ShapsGlk <- lm(cSHAPS_sum ~ res_logGlk + cBMI + cAge + cSex , d_quest)
summary(ShapsGlk)
confint(ShapsGlk)

ShapsGlk2 <- lm(cSHAPS_sum ~ res_logGlk+ fMDD + cBMI + cAge + cSex , d_quest)
summary(ShapsGlk2)
confint(ShapsGlk2)

ShapsGlk3<- lm(cSHAPS_sum ~ res_logGlk+ fMDD + cBDI_sum + cBMI + cAge + cSex , d_quest)
summary(ShapsGlk3)

# Check group differences MDD 
MDD_HOMA <- lm(res_logHOMA  ~ fMDD  + cBMI + cAge+ cSex , d_quest)
MDD_HOMA2 <- lm(res_logHOMA  ~ fMDD  + cBDI_sum + cBMI + cAge+ cSex , d_quest)
summary(MDD_HOMA)
confint(MDD_HOMA)
summary(MDD_HOMA2)

MDD_TyG <- lm(res_TyG  ~ fMDD    + cBMI + cAge+ cSex , d_quest)
MDD_TyG2 <- lm(res_TyG  ~ fMDD  + cBDI_sum  + cBMI + cAge+ cSex , d_quest)
summary(MDD_TyG2)
confint(MDD_TyG2)

MDD_AG<- lm(res_logF_AG  ~ fMDD  + cBMI + cAge+ cSex , d_quest)
MDD_AG2 <- lm(res_logF_AG  ~ fMDD + cBDI_sum + cBMI + cAge+ cSex , d_quest)
summary(MDD_AG2)
confint(MDD_AG2)

MDD_Glk<- lm(res_logGlk  ~ fMDD  + cBMI + cAge+ cSex , d_quest)
MDD_Glk2 <- lm(res_logGlk  ~ fMDD  + cBDI_sum  + cBMI + cAge+ cSex , d_quest)
summary(MDD_Glk)
confint(MDD_Glk)

# Split MDD in atypical and typical balanc  
d_quest$AtypicalGroup <- factor(d_quest$AtypicalGroup, levels = c("HCP","low atypical MDD", "high atypical MDD"))
contrasts(d_quest$AtypicalGroup) <-  contr.treatment(levels(d_quest$AtypicalGroup), base = 1)

MDD_AG<- lm(res_logF_AG  ~ AtypicalGroup  + cBMI + cAge+ cSex , d_quest)
summary(MDD_AG)
confint(MDD_AG)

# Check whether artefact of Median Split or direction replicable with continous measure 
# Correlation (also per Group) 
cor.test(formula = ~ cAtypicalBalance_acute + res_logF_AG,
         data = d_quest, subset = fMDD == "MDD")

MDD_AG_AtyC <- lm(res_logF_AG  ~ cAtypicalBalance_acute + fMDD  + cBMI + cAge+ cSex , d_quest)
summary(MDD_AG_AtyC)

MDD_DG<- lm(res_logF_DG  ~ AtypicalGroup  + cBMI + cAge+ cSex , d_quest)
summary(MDD_DG)
confint(MDD_DG)

MDD_Glk_AT <- lm(res_logGlk  ~ AtypicalGroup  + cBMI + cAge+ cSex , d_quest)
summary(MDD_Glk_AT)

MDD_HOMA_AT <- lm(res_logHOMA  ~ AtypicalGroup   + cBMI + cAge+ cSex , d_quest)
summary(MDD_HOMA_AT)

MDD_TyG_AT <- lm(res_TyG  ~ AtypicalGroup   + cBMI + cAge+ cSex , d_quest)
summary(MDD_TyG_AT)


# SHAPS and With Atypical Balance Score 
contrasts(d_quest$AtypicalGroup) <-  contr.treatment(levels(d_quest$AtypicalGroup), base = 1)

Shaps_AT <- lm(cSHAPS_sum  ~ AtypicalGroup  + cBMI + cAge+ cSex , d_quest)
summary(Shaps_AT)

Shaps_Homa_AT <- lm(cSHAPS_sum ~ res_logHOMA  * AtypicalGroup  + cBMI + cAge+ cSex , d_quest)
summary(Shaps_Homa_AT)
confint(Shaps_Homa_AT)

## Glucose IR 
Shaps_Glk_AT <- lm(cSHAPS_sum ~ res_logGlk  * AtypicalGroup + cBMI + cAge+ cSex , d_quest)
summary(Shaps_Glk_AT)

Shaps_TyG_AT <- lm(cSHAPS_sum ~ res_TyG  * AtypicalGroup + cBMI + cAge+ cSex , d_quest)
summary(Shaps_TyG_AT)

Shaps_Glk <- lm(cSHAPS_sum ~ res_logGlk + cBMI + cAge + cSex , d_quest)
summary(Shaps_Glk)

Shaps_Glk <- lm(cSHAPS_sum ~ res_logGlk + fMDD +  cBDI_sum + cBMI + cAge + cSex , d_quest)
summary(Shaps_Glk)

## Fasting Acyl Ghrelin 
Shaps <- lm(cSHAPS_sum ~  res_logF_AG * AtypicalGroup + cBMI + cAge + cSex  , d_quest)
summary(Shaps)
confint(Shaps)

# Result: HOMA-IR, GLk associated with SHAPS, ghrelin only marginally

## Fasting Des-Acyl Ghrelin 
Shaps <- lm(cSHAPS_sum ~  res_logF_DG* AtypicalGroup + cBMI + cAge + cSex , d_quest)
summary(Shaps)

############  MC CORRECTION  #########
p_SHAPS = c(0.04,0.005, 0.0007, 0.11, 0.59 )
p.adjust(p_SHAPS, method = "BH", n = length(p_SHAPS))
p_MDD = c(0.13, 0.094, 0.94, 0.33, 0.046)
p.adjust(p_MDD, method = "BH", n = length(p_MDD))
p_AT = c(0.039,0.71,0.13,0.65,0.061)
p.adjust(p_AT, method = "BH", n = length(p_AT))
p_SHAPS_AT = c(0.086, 0.53, 0.008, 0.55, 0.204 )
p.adjust(p_SHAPS_AT, method = "BH", n = length(p_SHAPS_AT))

# plot Triglyceride Index and SHAPS 
p_SHAPS_TyG <- ggplot(data = d_quest, aes(x = res_TyG, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(color = 'black', method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0.5)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) + 
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Triglyceride Index (residualized)') +
  ylab(label = "SHAPS") 
ggsave(paste(path_out, sub_metparam, "1.SHAPS_TyG.png", sep=""), 
        plot = p_SHAPS_TyG,  height = 8, width = 8, units = "in", dpi = 600, bg = "white")

# Plot SHAPS and GLUCOSE (underlying TyG and HOMA IR ;))
p_SHAPS_Glk <- ggplot(data = d_quest, aes(x = res_logGlk, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(color = 'black', method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0.1)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) + 
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Fasting glucose (res log)') +
  ylab(label = "SHAPS") 
ggsave(paste(path_out, sub_metparam, "1.SHAPS_Glk.png", sep=""), 
        plot = p_SHAPS_Glk,  height = 8, width = 8, units = "in", dpi = 600, bg = "white")

# Slit for Atypical MDD 
p_SHAPS_Glk_AT <- ggplot(data = d_quest, aes(x = res_logGlk, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(aes(color = factor(fMDD)), method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0.1)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_lowatypical, color_atypical)) + 
  theme(legend.position = "right",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0))+
  xlab(label = 'Fasting glucose (res log)') +
  ylab(label = "SHAPS")
ggsave(paste(path_out, sub_metparam, "1.SHAPS_Glk_Atypical.png", sep=""), 
        plot = p_SHAPS_Glk_AT,  height = 8, width = 8, units = "in", dpi = 600, bg = "white")

# Insulin
p_SHAPS_Ins <- ggplot(data = d_quest, aes(x = res_logIns, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(color = 'black', method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0.1)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) + 
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Fasting insulin (res log)') +
  ylab(label = "SHAPS")
ggsave(paste(path_out, sub_metparam, "1.SHAPS_Insulin.png", sep=""), 
        plot = p_SHAPS_Ins,  height = 8, width = 8, units = "in", dpi = 600, bg = "white")

# Shaps and Ghrelin 
p_SHAPS_AG <- ggplot(data = d_quest, aes(x = res_logF_AG, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(color = 'black', method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0.5)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) + 
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Fasting acyl ghrelin (res log)') +
  ylab(label = "SHAPS") 
ggsave(paste(path_out, sub_metparam, "1.SHAPS_AG.png", sep=""), 
        plot = p_SHAPS_AG,  height = 8, width = 8, units = "in", dpi = 600, bg = "white")


p_SHAPS_DG <- ggplot(data = d_quest, aes(x = res_logF_DG, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(fMDD)), alpha =1)+
  geom_smooth(color = 'black', method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) + 
  theme(legend.position = "bottom",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Fasting des-acyl ghrelin (log res)') +
  ylab(label = "SHAPS")
ggsave(paste(path_out, sub_metparam, "1.SHAPS_DG.png", sep=""), 
        plot = p_SHAPS_DG,  height = 8, width = 8, units = "in", dpi = 600, bg = "white")

# Ghrelin and SHAPS - SPLIT ATYPICAL MDD
p_SHAPS_AG_atyp <- ggplot(data = d_quest, aes(x = res_logF_AG, y = SHAPS_sum) ) +
  geom_point(aes(size = BMI, color = factor(AtypicalGroup)), alpha =1)+
  geom_smooth(aes(color = AtypicalGroup), method = 'rlm', alpha = 0.5, linewidth = 1) +
  #stat_regline_equation(label.y = 30, label.x = 0) + #this means at 30th unit regresion line equation will be shown  scale_color_manual(guide = guide_legend(title="Group"),values = c('darkgoldenrod4','cornflowerblue')) +
  #stat_cor(label.y = 30, label.x = 0.5)+ #this means at 30th unit in the y axis, the r squared and p value will be shown
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_lowatypical, color_atypical)) + 
  theme(legend.position = "right",text = element_text(face = 'bold',size = 18.0),
        axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0))+
  xlab(label = 'Acyl ghrelin (log transformed, residualized)') +
  ylab(label = "SHAPS") 

## Ghrelin and atypical 
d_quest2 <- d_quest[complete.cases(d_quest[,c("AtypicalGroup")]),]

plot_AG_AT <-
  ggplot(aes(x = res_logF_AG, group = AtypicalGroup, fill = AtypicalGroup),data = (d_quest2)) +
  geom_density(alpha=.5, adjust = 2) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""), 
                    labels=c('HCP', 'low atypical \nMDD', 'high atypical \nMDD'), 
                    values = c(color_HCP, color_lowatypical, color_atypical )) + 
  theme(legend.position = "top", legend.justification = c("center"), text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0),
        legend.spacing.x = unit(.5, 'cm')) +
  xlab(label = 'Fasting acyl ghrelin (res log)') +
  ylab(label = 'density')

ggsave(paste(path_out, sub_metparam , "2.plot_AG_AT.png", sep=""), 
              plot = plot_AG_AT,  height =6, width = 8, units = "in", dpi = 600, bg = "white")

plot_DG_AT <-
  ggplot(aes(x = res_logF_DG, group = AtypicalGroup, fill = AtypicalGroup),data = d_quest2) +
  geom_density(alpha=.5, adjust = 2) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""), values = c(color_HCP, color_lowatypical, color_atypical )) + 
  theme(legend.position = "top",text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  xlab(label = 'Fasting des-acyl ghrelin (res log)') +
  ylab(label = 'density')

# Save Grid Plot
legend <- get_legend(plot_AG_AT) # get common legend 

G <-ggarrange(plot_AG_AT, plot_DG_AT, ncol = 2, nrow = 1, 
      labels = c("a","b"), common.legend = TRUE, legend = "bottom", legend.grob = legend)

ggsave(paste(path_out, sub_metparam, "Ghrelin_atypical_density.png", sep=""), 
        plot = G,  height = 5, width = 10, units = "in", dpi = 300, bg = "white")


## Ghrelin and MDD Generally 
##################################################################################################
# Plot Group differences in Ghrelin for MDD 

plot_AG1 <-
  ggplot(aes(x = res_logF_AG, group = fMDD, fill = fMDD),data = d_quest) +
  geom_density(alpha=.5, adjust = 2) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""), values = c(color_HCP, color_MDD)) +
theme(legend.position = "top", legend.justification = c("center"), text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  xlab(label = 'Fasting acyl ghrelin (res log)') +
  ylab(label = 'density')

plot_DG1 <-
  ggplot(aes(x = res_logF_DG, group = fMDD, fill = fMDD),data = d_quest) +
  geom_density(alpha=.5, adjust = 2) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_MDD)) +
theme(legend.position = "top", legend.justification = c("right"), text = element_text(face = 'bold',size = 16.0),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
  xlab(label = 'Fasting des-acyl ghrelin (res log)') +
  ylab(label = 'density')


# Save Grid Plot
legend <- get_legend(plot_AG1) # get common legend 

G <-ggarrange(plot_AG1, p_SHAPS_Homa,  ncol = 2, nrow = 1, 
      labels = c("a","b"), common.legend = TRUE, legend = "top", legend.grob = legend)

ggsave(paste(path_out, sub_metparam, "Ghrelin_MDD_density_HOMAAdd.png", sep=""), 
        plot = G,  height = 5, width = 12, units = "in", dpi = 300, bg = "white")


# Zoom into AG for Atypical and Appetite 
G <-ggarrange(plot_AG_AT, ncol = 2, nrow = 1, 
      labels = c("c"), common.legend = TRUE, legend = "bottom")

ggsave(paste(path_out, sub_metparam, "Ghrelin_atypical_AG_density.png", sep=""), 
        plot = G,  height = 5, width = 12, units = "in", dpi = 300, bg = "white")


#### Group Differences

# Prep Stats and Plot
d_quest_prep <- d_quest %>% 
        select(fID, fMDD, res_logGlk, res_logF_AG, res_logF_DG, res_logHOMA, res_TyG) %>% 
        melt(id.vars=c("fID", "fMDD"))


d_quest_prep <- d_quest_prep %>% 
  mutate(GroupingVar = case_when( fMDD == "MDD" & variable == "res_logHOMA" ~ "MDD-HOMA",
                            fMDD == "MDD" & variable == "res_logGlk" ~ "MDD-Glk",
                            fMDD == "MDD" & variable == "res_logF_AG" & value  != "NA"  ~ "MDD-AG",
                            fMDD == "MDD" & variable == "res_logF_DG" &  value  != "NA"  ~ "MDD-DG",
                            fMDD == "MDD" & variable == "res_TyG" ~ "MDD-TyG",
                            fMDD == "HCP" & variable == "res_logHOMA" ~ "HCP-HOMA",
                            fMDD == "HCP" & variable == "res_logGlk" ~ "HCP-Glk",
                            fMDD == "HCP" & variable == "res_logF_AG" & value  != "NA"~ "HCP-AG",
                            fMDD == "HCP" & variable == "res_logF_DG" & value  !="NA" ~ "HCP-DG",
                            fMDD == "HCP" & variable == "res_logGlk" ~ "HCP-Glk",
                            fMDD == "HCP" & variable == "res_TyG" ~ "HCP-TyG"))  %>% 
  filter(is.na(GroupingVar) == FALSE)

# Prep Stats and Plot
d_quest_prepA <- 
  d_quest_prep %>%
  dabest(GroupingVar, value, 
         idx = list(c("HCP-Glk", "MDD-Glk")),
                 paired = FALSE)
#  c("MDD-DG","HCP-DG")


# Compute the mean difference
d_quest_prepA.cohens_d <- cohens_d(d_quest_prepA, ci = 95, reps = 5000, seed = 12345)

# Plot Multi-paired Liking Split by Phases and Group 
GA_MetParam_Glk <- plot(d_quest_prepA.cohens_d  , 
      color.column = fMDD, 
      palette = c(color_HCP, color_MDD), 
      group.summaries = "mean_sd",
      rawplot.type = c("swarmplot", "sinaplot"),
      rawplot.ylabel = "Metabolic index",
      rawplot.markersize = 3,
      rawplot.groupwidth = 0.2,
      effsize.ylabel = NULL,
      effsize.markersize = 8,
      tick.fontsize = 18,
      axes.title.fontsize = 18,
      theme = ggplot2::theme(text = element_text(face = 'bold',size = 20.0),
            axis.text = element_text(face = 'plain',size = 22.0),axis.text.x = element_text(size = 18.0)), 
      show.legend = FALSE)

ggsave(paste(path_out, sub_metparam, "GaAltPlot_MetParamGLK.png", sep=""), height = 7, width = 6, units = "in", dpi = 600, bg = "white")

## GA ALT PLOT, SUPPLEMENT MATERIAL WITH ALL 
# Prep Stats and Plot
d_quest_prepB <- 
  d_quest_prep %>%
  dabest(GroupingVar, value, 
         idx = list(c("HCP-HOMA", "MDD-HOMA"), 
                    c("HCP-TyG", "MDD-TyG"), 
                    c("HCP-AG", "MDD-AG"), 
                    c("HCP-DG","MDD-DG")),
         paired = FALSE)

# Compute the mean difference
d_quest_prepB.cohens_d <- cohens_d(d_quest_prepB, ci = 95, reps = 5000, seed = 12345)

# Plot Multi-paired Liking Split by Phases and Group 
GA_MetParam <- plot(d_quest_prepB.cohens_d  , 
      color.column = fMDD, 
      palette = c(color_HCP, color_MDD), 
      group.summaries = "mean_sd",
      rawplot.type = c("swarmplot", "sinaplot"),
      rawplot.ylabel = "Metabolic index",
      rawplot.markersize = 2,
      rawplot.groupwidth = 0.4,
      effsize.ylabel = NULL,
      effsize.markersize = 5,
      tick.fontsize = 12,
      axes.title.fontsize = 16,
      theme = ggplot2::theme(text = element_text(face = 'bold',size = 16.0),
            axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 14.0)), 
      show.legend = FALSE)

ggsave(paste(path_out, sub_metparam, "GaAltPlot_MetParamAllAll.png", sep=""), height = 6, width = 13, units = "in", dpi = 600, bg = "white")

## Split Atypical 

## Atypical and Ghrelin
# Prep Stats and Plot
d_quest_prep <- d_quest %>% 
        select(fID, fMDD, AtypicalGroup, res_logF_AG) %>% 
        melt(id.vars=c("fID", "fMDD", "AtypicalGroup"))

d_quest_prep2 <- d_quest_prep[complete.cases(d_quest_prep[,c("value")]),]

# Prep Stats and Plot
d_quest_prep2 <- 
  d_quest_prep2 %>%
  mutate(AtypicalGroup = recode(AtypicalGroup, "HCP" = "HCP", "low atypical MDD" = "melancholic MDD", "high atypical MDD" =  "atypical MDD" ))%>%  
  dabest(AtypicalGroup, value, 
         idx = list(c("HCP", "melancholic MDD", "atypical MDD")),
         paired = FALSE)

# Compute the mean difference
d_quest_prep2.cohens_d <- dabestr::cohens_d(d_quest_prep2, ci = 95, reps = 5000, seed = 12345)
d_quest_prep2.meandiff <- dabestr::mean_diff(d_quest_prep2, ci = 95, reps = 10000, seed = 12345)

# Plot Multi-paired Liking Split by Phases and Group 
GA_Ghrelin_AT <- plot(d_quest_prep2.meandiff , 
      color.column = AtypicalGroup, 
      palette = c(color_HCP, color_lowatypical, color_atypical), 
      group.summaries = "mean_sd",
      rawplot.type = c("swarmplot", "sinaplot"),
      rawplot.ylabel = "acyl ghrelin \n(res, log)",
      rawplot.markersize = 2,
      rawplot.groupwidth = 0.3,
      effsize.ylabel = NULL,
      effsize.markersize = 8,
      tick.fontsize = 12,
      axes.title.fontsize = 16,
      theme = ggplot2::theme(text = element_text(face = 'bold',size = 18.0),
      axis.text = element_text(face = 'plain',size = 16.0),axis.text.x = element_text(size = 16.0)), 
      show.legend = FALSE)

ggsave(paste(path_out, sub_metparam, "GaAltPlot_Ghrelin_Atypical.png", sep=""), height = 5, width = 8, units = "in", dpi = 600, bg = "white")

# Save Figure for Acyl Ghrelin Continous & Categorical Atypical Balance Score association 
titleA <- ggdraw() + draw_label("A  Lower acyl ghrelin in melancholic MDD \n    (categorical split)", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

titleB <- ggdraw() + draw_label("B  AtypMetaboliical balance score is only weakly \n    associated with acyl ghrelin (continous)", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

Plot_ABCD <-ggarrange(titleA, titleB, GA_Ghrelin_AT, P_Ghrelin_Atypical, ncol=2, nrow=2, 
      heights = c(0.15,1.1), common.legend = FALSE)

ggsave(paste(path_out, sub_metparam, "S_GhrelinAtypical.png", sep=""), 
        plot = Plot_ABCD ,  height = 6, width = 16, units = "in", dpi = 300, bg = "white")




# White spcae for methods to add
blankPlot <- ggplot()+geom_blank(aes(1,1)) + 
  cowplot::theme_nothing()

p_SHAPS_TYGF <- p_SHAPS_TyG + theme(text = element_text(face = 'bold',size = 15.0),
        legend.text = element_text(size=10) , legend.title = element_text(size=14),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
        guides(color = guide_legend(override.aes = list(size = 4) ),fill=guide_legend(title="Group")) +
        scale_alpha_manual(name = "Group")  +
        xlab(label = 'TyG') +
        ylab(label = "") #+

p_SHAPS_GlkF <- p_SHAPS_Glk + theme(text = element_text(face = 'bold',size = 15.0),
        legend.text = element_text(size=10) , legend.title = element_text(size=14),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
        guides(color = guide_legend(override.aes = list(size = 4) ),fill=guide_legend(title="Group")) +
        scale_alpha_manual(name = "Group") +
        xlab(label = 'Glucose') +
        ylab(label = "") #+

p_SHAPS_HomaF <- p_SHAPS_Homa + theme(text = element_text(face = 'bold',size = 15.0),
        legend.text = element_text(size=10) , legend.title = element_text(size=14),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) +
        guides(color = guide_legend(override.aes = list(size = 4) ),fill=guide_legend(title="Group")) +
        scale_alpha_manual(name = "Group") +
        xlab(label = 'HOMA-IR') 

        
p_SHAPS_AGF <-p_SHAPS_AG + theme(text = element_text(face = 'bold',size = 15.0),
        legend.text = element_text(size=10) , legend.title = element_text(size=14),
        axis.text = element_text(face = 'plain',size = 12.0),axis.text.x = element_text(size = 12.0)) + 
        guides(color = guide_legend(override.aes = list(size = 4) ),fill=guide_legend(title="Group")) +
        ylab(label = '')  + labs(fill = "Group  ")

# Final Figure 3: Cowplot, Titel, shared legends 
titleA <- ggdraw() + draw_label("A", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

titleB <- ggdraw() + draw_label("B   Altered glycemic control is associated with anhedonia", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

titleC <- ggdraw() + draw_label("C   MDD is not characterized by different peripheral levels of metabolic hormones", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

PlotA <- ggarrange(titleA, blankPlot, ncol = 1, nrow = 2, heights = c(0.15,1.1))

PlotBC <- ggarrange(titleB, NULL, NULL, p_SHAPS_HomaF, p_SHAPS_TYGF, p_SHAPS_GlkF, ncol = 3, nrow = 2, heights = c(0.15,1.1),
      common.legend = TRUE, legend="right", labels = c('','','','',''))

Plot_ABC <-ggarrange(ggarrange(PlotA, PlotBC, ncol = 2, nrow = 1,
      labels = c('',''),common.legend = FALSE, widths = c(1,2.2)))

Plot_DE <-ggarrange(ggarrange(GA_MetParam, NULL, ncol = 2, nrow = 1,
      labels = c('',''),common.legend = FALSE, widths = c(1,0.3)))

Plot_ABCDE <-ggarrange(Plot_ABC, titleC, Plot_DE, ncol=1, nrow=3, 
      heights = c(1.1,0.15,1), common.legend = FALSE)

ggsave(paste(path_out, sub_metparam, "Fig3_Revision.png", sep=""), 
        plot = Plot_ABCDE ,  height = 10, width = 13, units = "in", dpi = 300, bg = "transparent")


##############################################################################################################
# (7) Blood paramters and the taste_test 
##############################################################################################################
## Plot blood paramters and anhedonia questionnaire association 
## --> This is being done in the script anhedonia_metabolism 

#Single SHAPS Items 
dwanting_joint$cfavFood <- dwanting_joint$favDood - mean(dwanting_joint$favDood)
dwanting_joint$cbathShower <- dwanting_joint$bathShower - mean(dwanting_joint$bathShower)
dwanting_joint$csmellFeeling <- dwanting_joint$smellFeeling - mean(dwanting_joint$smellFeeling)
dwanting_joint$cothersSmiling <- dwanting_joint$othersSmiling - mean(dwanting_joint$othersSmiling)
dwanting_joint$cart <- dwanting_joint$art - mean(dwanting_joint$art)
dwanting_joint$creading <- dwanting_joint$reading - mean(dwanting_joint$reading)
dwanting_joint$cfavDrink <- dwanting_joint$favDrink - mean(dwanting_joint$favDrink)
dwanting_joint$csmallThings <- dwanting_joint$smallThings - mean(dwanting_joint$smallThings)
dwanting_joint$cviews <- dwanting_joint$views - mean(dwanting_joint$views)
dwanting_joint$chelping <- dwanting_joint$helping - mean(dwanting_joint$helping)
dwanting_joint$cpraise <- dwanting_joint$praise - mean(dwanting_joint$praise)
dwanting_joint$centertainment <- dwanting_joint$entertainment - mean(dwanting_joint$entertainment)
dwanting_joint$cfamilyFriends <- dwanting_joint$familyFriends - mean(dwanting_joint$familyFriends)
dwanting_joint$chobbies <- dwanting_joint$hobbies - mean(dwanting_joint$hobbies)

wanting_SHAPS_cfavFood <- lmer(RatingValue ~ cfavFood * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_bathShower <- lmer(RatingValue ~ cbathShower * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_smellFeeling <- lmer(RatingValue ~ csmellFeeling * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_othersSmiling <- lmer(RatingValue ~ cothersSmiling * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_art <- lmer(RatingValue ~ cart * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_reading <- lmer(RatingValue ~ creading * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_favDrink <- lmer(RatingValue ~ cfavDrink * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_smallThings <- lmer(RatingValue ~ csmallThings * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_views <- lmer(RatingValue ~ cviews * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_helping <- lmer(RatingValue ~ chelping * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_praise <- lmer(RatingValue ~ cpraise * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_entertainment <- lmer(RatingValue ~ centertainment * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_familyFriends <- lmer(RatingValue ~ cfamilyFriends * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
wanting_SHAPS_hobbies <- lmer(RatingValue ~ chobbies * fPhase_dicho_FCR_TT  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)

# Get Fixed effect of respective SHAPS ITEM 
wanting_SHAPS_Items_Coeff = data.frame(matrix(nrow = 1, ncol = 0))  
wanting_SHAPS_Items_Coeff$favFood <- coef(summary(wanting_SHAPS_cfavFood))["cfavFood","t value"]
wanting_SHAPS_Items_Coeff$bathShower <- coef(summary(wanting_SHAPS_bathShower))["cbathShower","t value"]
wanting_SHAPS_Items_Coeff$smellFeeling <- coef(summary(wanting_SHAPS_smellFeeling))["csmellFeeling","t value"]
wanting_SHAPS_Items_Coeff$othersSmiling <- coef(summary(wanting_SHAPS_othersSmiling))["cothersSmiling","t value"]
wanting_SHAPS_Items_Coeff$art <- coef(summary(wanting_SHAPS_art))["cart","t value"]
wanting_SHAPS_Items_Coeff$reading <- coef(summary(wanting_SHAPS_reading))["creading","t value"]
wanting_SHAPS_Items_Coeff$favDrink <- coef(summary(wanting_SHAPS_favDrink))["cfavDrink","t value"]
wanting_SHAPS_Items_Coeff$smallThings <- coef(summary(wanting_SHAPS_smallThings))["csmallThings","t value"] 
wanting_SHAPS_Items_Coeff$views <- coef(summary(wanting_SHAPS_views))["cviews","t value"]
wanting_SHAPS_Items_Coeff$helping <- coef(summary(wanting_SHAPS_helping))["chelping","t value"]
wanting_SHAPS_Items_Coeff$praise <-coef(summary(wanting_SHAPS_praise))["cpraise","t value"]
wanting_SHAPS_Items_Coeff$entertainment <-coef(summary(wanting_SHAPS_entertainment))["centertainment","t value"]
wanting_SHAPS_Items_Coeff$familyFriends <-coef(summary(wanting_SHAPS_familyFriends))["cfamilyFriends","t value"]
wanting_SHAPS_Items_Coeff$hobbies <-coef(summary(wanting_SHAPS_hobbies))["chobbies","t value"]

wanting_SHAPS_Items_Coeff_prep <- wanting_SHAPS_Items_Coeff %>%
     tidyr::gather(colname, value)

wanting_SHAPS_Items_CoeffInt = data.frame(matrix(nrow = 1, ncol = 0))  
wanting_SHAPS_Items_CoeffInt$favFood <- coef(summary(wanting_SHAPS_cfavFood))["cfavFood:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$bathShower <- coef(summary(wanting_SHAPS_bathShower))["cbathShower:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$smellFeeling <- coef(summary(wanting_SHAPS_smellFeeling))["csmellFeeling:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$othersSmiling <- coef(summary(wanting_SHAPS_othersSmiling))["cothersSmiling:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$art <- coef(summary(wanting_SHAPS_art))["cart:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$reading <- coef(summary(wanting_SHAPS_reading))["creading:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$favDrink <- coef(summary(wanting_SHAPS_favDrink))["cfavDrink:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$smallThings <- coef(summary(wanting_SHAPS_smallThings))["csmallThings:fPhase_dicho_FCR_TTtaste_test","t value"] 
wanting_SHAPS_Items_CoeffInt$views <- coef(summary(wanting_SHAPS_views))["cviews:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$helping <- coef(summary(wanting_SHAPS_helping))["chelping:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$praise <-coef(summary(wanting_SHAPS_praise))["cpraise:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$entertainment <-coef(summary(wanting_SHAPS_entertainment))["centertainment:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$familyFriends <-coef(summary(wanting_SHAPS_familyFriends))["cfamilyFriends:fPhase_dicho_FCR_TTtaste_test","t value"]
wanting_SHAPS_Items_CoeffInt$hobbies <-coef(summary(wanting_SHAPS_hobbies))["chobbies:fPhase_dicho_FCR_TTtaste_test","t value"]

wanting_SHAPS_Items_CoeffInt_prep <- wanting_SHAPS_Items_CoeffInt %>%
     tidyr::gather(colname, value)

wanting_SHAPS_Items_CoeffTog <- merge(wanting_SHAPS_Items_Coeff_prep, wanting_SHAPS_Items_CoeffInt_prep, by = "colname")

wanting_SHAPS_Items_CoeffTog <- arrange(wanting_SHAPS_Items_CoeffTog, -value.x)
wanting_SHAPS_Items_CoeffTog$colname <- factor(wanting_SHAPS_Items_CoeffTog$colname, levels = wanting_SHAPS_Items_CoeffTog$colname)
#wanting_SHAPS_Items_CoeffTog$colname <- wanting_SHAPS_Items_CoeffTog$colname

# Plotting t-values for separate models 
# Main effect t values 
Shaps_Items_MainEffect <- ggplot(wanting_SHAPS_Items_CoeffTog,aes(x = value.x,y = colname)) + 
    geom_bar(aes(fill = value.x),stat = "identity") +
    theme(legend.position = "none", legend.justification = c("center"),
          text = element_text(family="sans", face = 'bold',size = 16.0),
          axis.title.y = element_text(family="sans", face = 'bold',size = 12.0),
          axis.text.y = element_text(family="sans", face = 'plain',size = 12.0),
          axis.text.x = element_text(family="sans", face = 'plain',size = 12.0)) +
  xlab(label = 't-value \nanticipatory wanting') +
  ylab(label = "SHAPS item: 'Lack of \nenjoymemt/pleasure from...'") 

# Interaction t values 
Shaps_Items_Interaction <- ggplot(wanting_SHAPS_Items_CoeffTog,aes(x = value.y,y = colname)) + 
  geom_bar(aes(fill = value.y),stat = "identity") +
  theme(legend.position = "none", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
  axis.text = element_text(family="sans", face = 'plain',size = 12.0),
  axis.text.x = element_text(family="sans", size = 12.0), 
  axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
  xlab(label = 't-value \nincrease wanting') +
  ylab(label='') +
  xlim(0,3.5)

ShapsItems <- cowplot::plot_grid(Shaps_Items_MainEffect, Shaps_Items_Interaction, 
                  ncol = 2, labels = c('E',''), label_size = 18, rel_widths = c(1.2,1))

# Figure 2 Revision 
titleA <- ggdraw() + draw_label("SHAPS is weakly associated with liking ratings", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

titleC <- ggdraw() + draw_label("SHAPS is associated with reduced anticipatory wanting but later increases in wanting", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

titleE <- ggdraw() + draw_label("Associations with single SHAPS items", size = 16, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

legend <- get_legend(plot_anhedonia2) # get common legend 
plot_anhedonia2 <- plot_anhedonia2 + theme(legend.position = "none")
plot_anhedonia4 <- plot_anhedonia4 + theme(legend.position = "none")

G_TT_Shaps01 <- cowplot::plot_grid(titleA, NULL)
G_TT_Shaps1 <-cowplot::plot_grid(plot_anhedonia3, plot_anhedonia4, ncol = 2, nrow = 1, labels = c('A', 'B'))
G_TT_Shaps02 <- cowplot::plot_grid(titleC, NULL)
G_TT_Shaps12 <- cowplot::plot_grid(legend)
G_TT_Shaps2 <-cowplot::plot_grid(plot_anhedonia1, plot_anhedonia2, ncol = 2, nrow = 1, labels = c('C','D'))
G_TT_Shaps03 <- cowplot::plot_grid(titlG_TT_Shaps01eE, NULL)

Fig2_Revision <- cowplot::plot_grid(G_TT_Shaps01, #Titel A
                  G_TT_Shaps1, #Data
                  G_TT_Shaps12, # Legend
                  G_TT_Shaps02, # Titel C
                  G_TT_Shaps2,  # Data
                   #G_TT_Shaps03, # Titel E
                   #ShapsItems, # Data 
                  ncol = 1, nrow = 7, rel_heights =  c(0.12, 1.4,0.2,0.12,1.4, 0.12, 1.1)) 

ggsave(paste(path_out, sub_anhedonia, "Fig2_Revision.png", sep=""), 
        plot = Fig2_Revision,  height = 14, width = 12, 
        units = "in", dpi = 600, bg = "transparent")


#################################################################################
## (7). Metabolic paramters and Behavior
#################################################################################


##############################################################################################################
## MULTIVARIATE TESTING 
################################################

# https://library.virginia.edu/data/articles/getting-started-with-multivariate-multiple-regression#:~:text=Multivariate%20Multiple%20Regression%20is%20a,parent%20income%2C%20and%20so%20forth.

# For MV Testing simplify model, average for phases, and snacks 
dwanting_agg2 <- aggregate(dwanting_joint_ghr,
                by = list(dwanting_joint_ghr$fID),
                FUN = mean)

dliking_agg2 <- aggregate(dliking_joint_ghr,
                by = list(dliking_joint_ghr$fID),
                FUN = mean)

# Merge Liking and Wanting Aggregate Dataframes 
d_WL_MV_agg2 <- merge(dwanting_agg2, dliking_agg2, by = "ID")
head(d_WL_MV_agg2)
#  From aggregation re-introduce MDD
d_WL_MV_agg2$fMDD <- factor(d_WL_MV_agg2$Group_MDD.x, levels = c(0,1), labels = c("HCP", "MDD"))

# Multivariate Model
WL_MV_AG_v2 <- lm(cbind(RatingValue.x, RatingValue.y) ~ res_logF_AG.x  +   fMDD  + cBMI.x + cAge.x + cSex.x, data = d_WL_MV_agg2)
# Test statistic for Multivariate regression, Pillai test 
Anova(WL_MV_AG_v2)
summary(WL_MV_AG_v2)

# Test Coupling of wanting and liking 
d_WL_MV_agg2$cRatingLiking <- d_WL_MV_agg2$RatingValue.y - mean(d_WL_MV_agg2$RatingValue.y)
d_WL_MV_agg2$cRatingWanting <- d_WL_MV_agg2$RatingValue.x - mean(d_WL_MV_agg2$RatingValue.x)

fit =lm(RatingValue.x~cRatingLiking*res_logF_AG.x ,data=d_WL_MV_agg2)
fit_check=lm(RatingValue.x~cRatingLiking*res_logF_AG.x +cBMI.x + fMDD + cSex.x + cAge.x   ,data=d_WL_MV_agg2)
summary(fit_check)
confint(fit_check, method = "Wald")
p.adjust(c(0.006, 0.022, 0.08, 0.047, 0.047, 0.051), method = "BH", n = 6)

# LINEAR MIXED EFFECTS MODELS 
# HOMA And taste_test 
# Switch the Base Coding to get Statistics for Consummatory Phase (Main effect) 
contrasts(dwanting_joint$fPhase_dicho_FCR_TT)  <- contr.treatment(levels(dwanting_joint$fPhase_dicho_FCR_TT), base = 1)
wanting_6b <- lmer(RatingValue ~  fMDD + fPhase_dicho_FCR_TT * res_logHOMA + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint)
summary(wanting_6b)

# For liking
contrasts(dliking_joint$fPhase_dicho_FCR_TT)  <- contr.treatment(levels(dliking_joint$fPhase_dicho_FCR_TT), base = 1)
liking_6b <- lmer(RatingValue ~  fPhase_dicho_FCR_TT * res_logHOMA + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint)
summary(liking_6b)

# For ghrelin remove NA values 
dwanting_joint_ghr <- dwanting_joint[complete.cases(dwanting_joint[,c("res_logF_AG")]),]
length(unique(dwanting_joint_ghr$fID))

dliking_joint_ghr <- dliking_joint[complete.cases(dliking_joint[,c("res_logF_AG")]),]
length(unique(dliking_joint_ghr$fID))

dwanting_joint_ghr$cBDI_sum <- dwanting_joint_ghr$BDI_sum - mean(dwanting_joint_ghr$BDI_sum) 
dwanting_joint_ghr$cBDI_sum <- dwanting_joint_ghr$BDI_sum - mean(dwanting_joint_ghr$BDI_sum) 

# Ghrelin and Wanting Models 

# Base Coding to get Statistics for Anticipatory Phase (Main effect) 
contrasts(dwanting_joint_ghr$fPhase_dicho_FCR_TT)  <- contr.treatment(levels(dwanting_joint_ghr$fPhase_dicho_FCR_TT), base = 1)
wanting_6c <- lmer(RatingValue ~  fMDD  + res_logF_AG * fPhase_dicho_FCR_TT + fSnack + cBMI + cAge + cSex + (1+ fSnack +fPhase_dicho_FCR_TT |ID), dwanting_joint_ghr)
summary(wanting_6c)
confint(wanting_6c, method="Wald")

# Sum coded to get aerage phase main effect
contrasts(dwanting_joint_ghr$fPhase_dicho_FCR_TT)  <- contr.sum(levels(dwanting_joint_ghr$fPhase_dicho_FCR_TT))
wanting_06c <- lmer(RatingValue ~   fMDD + res_logF_AG  * fPhase_dicho_FCR_TT +res_logF_AG* cSex *cAge   + fSnack + cBMI + cAge + cSex + (1+ fSnack +fPhase_dicho_FCR_TT |ID), dwanting_joint_ghr)
summary(wanting_06c)
confint(wanting_06c, method="Wald")

# https://strengejacke.github.io/ggeffects/reference/plot.html#ref-examples
dat <- ggeffects::ggpredict(wanting_06c, terms = c("cAge", "res_logF_AG[-2,0,2]"))
yy <- plot(dat, colors = "us", use_theme = FALSE,jitter = 0.1 )  + 
labs(x = "Age (centered)", y = "Expected outcome wanting") + 
theme(legend.position = "right", legend.justification = c("right"), text = element_text(face = 'bold',size = 12.0),
            axis.text = element_text(face = 'plain',size = 12.0),
            axis.title=element_text(size=12), axis.text.x = element_text(size = 12.0)) 
  ggsave(paste(path_out, sub_metparam, "GHrelin_Age_wanting.png", sep=""), width = 8, height = 4, dpi=300, bg = "transparent")
          

wanting_6c_checkBDI <- lmer(RatingValue ~   res_logF_AG * fPhase_dicho_FCR_TT + res_logF_AG *cBDI_sum + fSnack + cBMI + cAge + cSex + (1+ fSnack +fPhase_dicho_FCR_TT |ID), dwanting_joint_ghr)
summary(wanting_6c_checkBDI)

#Same for DG 
wanting_6c_DG <- lmer(RatingValue ~  fPhase_dicho_FCR_TT * res_logF_DG + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dwanting_joint_ghr)
summary(wanting_6c_DG)

# Sum coded to get aerage phase main effect
contrasts(dliking_joint_ghr$fPhase_dicho_FCR_TT)  <- contr.sum(levels(dliking_joint_ghr$fPhase_dicho_FCR_TT))

liking_06c <- lmer(RatingValue ~  fMDD + res_logF_AG  * fPhase_dicho_FCR_TT  + fSnack + cBMI + cSex + (1+ fSnack +fPhase_dicho_FCR_TT |ID), dliking_joint_ghr)
summary(liking_06c)
confint(liking_06c, method="Wald")

contrasts(dliking_joint_ghr$fPhase_dicho_FCR_TT)  <- contr.treatment(levels(dliking_joint_ghr$fPhase_dicho_FCR_TT), base = 1)
liking_6c <- lmer(RatingValue ~   fPhase_dicho_FCR_TT * res_logF_AG + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint_ghr)
summary(liking_6c)
confint(liking_6c, method="Wald")

liking_6c_DG <- lmer(RatingValue ~  fPhase_dicho_FCR_TT * res_logF_DG  + fSnack + cBMI + cAge + cSex + (1+ fSnack + fPhase_dicho_FCR_TT|ID), dliking_joint_ghr)
summary(liking_6c_DG)

# Create Html Output to Word Document with Results 
sjPlot:: tab_model(liking_06c, wanting_06c,
                    p.val = "satterthwaite",
                    show.re.var=TRUE,
                    show.ci =FALSE,
                    show.se =TRUE,
                    dv.labels = c("Liking Ghrelin", "Wanting Ghrelin"), 
                    file= paste(path_out, sub_groups, "/","SI_LW_Ghrelin_ModelResults", ".doc", sep = ""))

sjPlot:: tab_model(liking_06b, wanting_06b,
                    p.val = "satterthwaite",
                    show.re.var=TRUE,
                    show.ci =FALSE,
                    show.se =TRUE,
                    dv.labels = c("Liking HOMA-IR", "Wanting HOMA-IR"), 
                    file= paste(path_out, sub_groups, "/","SI_LW_HOMA_ModelResults", ".doc", sep = ""))

## PLOT MULTIVARRIATE ###

# install.packages("devtools")
# devtools::install_github("cardiomoon/ggiraphExtra")
require(ggiraph)
require(ggiraphExtra)
require(plyr)

library("RColorBrewer")
coloursP = colorRampPalette(brewer.pal(8, "Greens"))(10)
MV_plot_AcrossPhase <- ggPredict(fit, colorn = 12, digits = 3, show.summary = TRUE, se = TRUE ) + 
    xlab("Liking (centered)")+  ylab("Prediction wanting") + 
    theme(legend.position = "right", legend.justification = c("right"), text = element_text(face = 'bold',size = 12.0),
            axis.text = element_text(face = 'plain',size = 12.0),
            axis.title=element_text(size=12), axis.text.x = element_text(size = 12.0))  

ggsave(paste(path_out, sub_metparam, "Model_Predict_Want_Ghrelin_Liiking_across_Phases_MDD.png", sep=""), width = 7, height = 4, dpi=300, bg = "transparent")

titleA <- ggdraw() + draw_label("A     Acyl ghrelin is associated with stronger\n       liking and wanting across phases", size = 14, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(7, 0, 0, 7))

titleB <- ggdraw() + draw_label("B     With higher acyl ghrelin less liked\n       food rewards are wanted more", size = 14, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

Fig4_Revision2 <- cowplot::plot_grid(titleA, titleB, 
                  ncol = 2, nrow = 1, rel_heights =  c(0.12))
ggsave(paste(path_out, sub_metparam, "cheatTitle.png", sep=""), plot = Fig4_Revision2, width = 12, height = 2, dpi=300, bg = "white")


WL_MV_HOMA_v2 <- lm(cbind(RatingValue.x, RatingValue.y) ~ res_logHOMA.x + fMDD + cBMI.x + cAge.x + cSex.x, data = d_WL_MV_agg2)
summary(WL_MV_HOMA_v2)

##########################################################################################
# Visulaizing taste_test and Metabolic Parameter 
##########################################################################################

# Change Ghrelin into Categories for Plotting
(mylist <- list(res_logF_AG=seq(-2.5,2,by=0.5),fPhase_dicho_FCR_TT=c("1st_ant","taste_test")))

# Create Color palette with enough levels
mycolors <- colorRampPalette(brewer.pal(8, "PuOr"))(2) # 28 level of Anhedonia Scores ggf. anpassen

# Ghrelin Solo effect
p2W <- emmip(wanting_6c, fPhase_dicho_FCR_TT ~res_logF_AG , at=mylist,CIs=TRUE,
      mult.name = "variety",
      cov.reduce = FALSE, 
      nuisance = c("cSex", "cAge", "cBMI"),
      xlab = "fasting acyl ghrelin (res log)", ylab = "Prediction wanting", 
      plotit = TRUE ) 

p2W <- p2W + scale_color_manual(values=mycolors) + scale_fill_viridis(option="magma") +
  theme(legend.position = "none", legend.justification = c("center"), text = element_text(face = 'bold',size = 12.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=12), axis.text.x = element_text(size = 12.0)) + 
        scale_color_manual(guide = guide_legend(title="Phase"), values = mycolors) 

ggsave(paste(path_out, sub_metparam, "Model_EMM_Ghrelin_Phases.png", sep=""), width = 5, height = 5, dpi=300, bg = "white")

(mylist <- list(res_logF_AG=seq(-2.5,2,by=0.5),fPhase_dicho_FCR_TT=c("1st_ant","taste_test")))
p2L <- emmip(liking_6c, fPhase_dicho_FCR_TT ~res_logF_AG , at=mylist,CIs=TRUE,
      mult.name = "variety",
      cov.reduce = FALSE, 
      nuisance = c("cSex", "cAge", "cBMI"),
      xlab = "fasting acyl ghrelin (res log)", ylab = "Prediction liking", 
      plotit = TRUE ) 

p2L <- p2L + scale_color_manual(values=mycolors) + scale_fill_viridis(option="magma") +
  theme(legend.position = "none", legend.justification = c("center"), text = element_text(face = 'bold',size = 12.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=12), axis.text.x = element_text(size = 12.0)) + 
        scale_color_manual(guide = guide_legend(title="Phase"), values = mycolors) 

ggsave(paste(path_out, sub_metparam, "Model_EMM_Ghrelin_Phases_Liking.png", sep=""), width = 5, height = 5, dpi=300, bg = "white")

# HOMA IR Model 
(mylist <- list(res_logHOMA=seq(-1,1.5,by=0.25),fPhase_dicho_FCR_TT=c("1st_ant","taste_test")))
mycolors <- colorRampPalette(brewer.pal(8, "PuOr"))(2) # 28 level of Anhedonia Scores ggf. anpassen

p3_want <- emmip(wanting_6b, fPhase_dicho_FCR_TT ~res_logHOMA , at=mylist,CIs=TRUE,
      mult.name = "variety",
      cov.reduce = FALSE, 
      nuisance = c("cSex", "cAge", "cBMI"),
      xlab = "HOMA-IR (res log)", ylab = "Prediction wanting", 
      plotit = TRUE ) 

p3_want <- p3_want + scale_color_manual(values=mycolors) + scale_fill_viridis(option="magma") +
  theme(legend.position = "none", legend.justification = c("center"), text = element_text(face = 'bold',size = 12.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=12), axis.text.x = element_text(size = 12.0)) + 
        scale_color_manual(guide = guide_legend(title="Phase"), values = mycolors) 

ggsave(paste(path_out, sub_metparam, "Model_EMM_HOMA_Phases.png", sep=""), width = 5, height = 5, dpi=300, bg = "white")

# HOMA LIKING PLOT 
p3_like <- emmip(liking_6b, fPhase_dicho_FCR_TT ~res_logHOMA , at=mylist,CIs=TRUE,
      mult.name = "variety",
      cov.reduce = FALSE, 
      nuisance = c("cSex", "cAge", "cBMI"),
      xlab = "HOMA-IR (res log)", ylab = "Prediction liking", 
      plotit = TRUE ) 

p3_like <- p3_like + scale_color_manual(values=mycolors) + scale_fill_viridis(option="magma") +
  theme(legend.position = "none", legend.justification = c("center"), text = element_text(face = 'bold',size = 12.0),
        axis.text = element_text(face = 'plain',size = 12.0),
        axis.title=element_text(size=12), axis.text.x = element_text(size = 12.0)) + 
        scale_color_manual(guide = guide_legend(title="Phase"), values = mycolors) 

ggsave(paste(path_out, sub_metparam, "Model_EMM_HOMA_Phases_LIKING.png", sep=""), width = 5, height = 5, dpi=300, bg = "white")


# Figure 4: Emmip Plots Wanting and Liking for Ghrelin And Homa next to each other 
titleA <- ggdraw() + draw_label("A     Acyl ghrelin is associated with stronger liking and wanting", size = 14, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

titleC <- ggdraw() + draw_label("A     Insulin sensitivity is not associated with liking or wanting", size = 14, fontface = 'bold',x = 0,hjust = 0) +
  theme(plot.margin = margin(0, 0, 0, 7))

legend <- get_legend(p2L + theme(legend.position = "bottom", legend.justification = c("center"), text = element_text(face = 'bold',size = 14.0)))

Fig4_Revision <- cowplot::plot_grid(titleA, NULL, 
                  p2L, p2W, 
                  legend, NULL,
                  ncol = 2, nrow = 3, rel_heights =  c(0.12, 1,0.12))

Fig4_RevisionSI <- cowplot::plot_grid(
                  titleC, NULL, 
                  p3_like , p3_want, 
                  legend, NULL,
                  ncol = 2, nrow = 3, rel_heights =  c(0.12, 1,0.12))

ggsave(paste(path_out, sub_metparam, "TT_Fig4_Revision.png", sep=""), 
        plot = Fig4_Revision,  height = 4, width = 10, units = "in", dpi = 600, bg = "white")

ggsave(paste(path_out, sub_metparam, "TT_Fig4_RevisionSI.png", sep=""), 
        plot = Fig4_RevisionSI,  height = 4, width = 10, units = "in", dpi = 600, bg = "white")


###########################################################################################

# 8.1. Wanting and Liking Not independent 
#####################################
# Merge Liking and Wanting Dataframes 
d_WL_coupling <- merge(dwanting_joint, dliking_joint, by = c("fID", "fPhase_dicho_FCR_TT", "fSnack"))
head(d_WL_coupling)

# Rescale to same Range    # Wanting 0-100
d_WL_coupling$RatingValue.y <-  scales::rescale(d_WL_coupling$RatingValue.y, to = c(0, 100))     # Liking -100 100
d_WL_coupling$cRatingValue.y <-  d_WL_coupling$RatingValue.y - mean(d_WL_coupling$RatingValue.y)
d_WL_coupling$cRatingValue.x <-  d_WL_coupling$RatingValue.x - mean(d_WL_coupling$RatingValue.x)
d_WL_coupling$cSHAPS_sum <-  d_WL_coupling$SHAPS_sum.x - mean(d_WL_coupling$SHAPS_sum.x)


## Re-Test Main Models with Liking & Wanting 

LW_1 <- lmerTest::lmer(RatingValue.x ~ fMDD.x*fPhase_dicho_FCR_TT*cRatingValue.y + fSnack + cBMI.x + cAge.x + cSex.x + (1+  cRatingValue.y + fSnack + fPhase_dicho_FCR_TT|fID), d_WL_coupling)
summary(LW_1)

LW_2 <- lmerTest::lmer(RatingValue.x ~ cSHAPS_sum * fPhase_dicho_FCR_TT * cRatingValue.y + fSnack + cBMI.x + cAge.x + cSex.x + (1+  cRatingValue.y + fSnack + fPhase_dicho_FCR_TT|fID), d_WL_coupling)
summary(LW_2)

confint(wanting_3c_LW1, method="Wald")

# Correlation 
d_WL_coupling_Agg <- d_WL_coupling %>%
  dplyr::group_by(ID.x, fMDD.x, fPhase_dicho_FCR_TT)  %>% 
  dplyr::summarize(MEANRatingValue.y  = mean(RatingValue.y), MEANRatingValue.x = mean(RatingValue.x)) %>%  
  dplyr::group_by(ID.x, fMDD.x)  %>% 
  dplyr::summarize(MEANRatingValue.y  = mean(MEANRatingValue.y), MEANRatingValue.x = mean(MEANRatingValue.x)) 

cor.test(formula = ~ MEANRatingValue.x + MEANRatingValue.y,
         data = d_WL_coupling_Agg)



m_WL_coupling <- lmerTest::lmer(RatingValue.x ~ fMDD.x * fPhase_dicho_FCR_TT * cRatingValue.y + fSnack + cBMI.x + cAge.x + cSex.x + (1+ fSnack + cRatingValue.y + fPhase_dicho_FCR_TT|fID), d_WL_coupling)
summary(m_WL_coupling)

m_WL_coupling2 <- lmerTest::lmer(RatingValue.y ~ fMDD.x * fPhase_dicho_FCR_TT * cRatingValue.x + fSnack + cBMI.x + cAge.x + cSex.x + (1+ fSnack + cRatingValue.x + fPhase_dicho_FCR_TT|fID), d_WL_coupling)
summary(m_WL_coupling2)

## plot Wanting and Liking Together 

# Aggregate for Plotting 
d_WL_coupling_agg <- d_WL_coupling %>%
  group_by(fID, fMDD.x, fPhase_dicho_FCR_TT)  %>% 
  summarize(RatingValue.y  = mean(RatingValue.y), RatingValue.x = mean(RatingValue.x)) 


Plot_WL <- 
  ggplot(aes(x = RatingValue.y ,y = RatingValue.x , color = fMDD.x),data = d_WL_coupling_agg) +
  geom_point(size = 3.5) +
  geom_smooth(aes(colour=fMDD.x), size = 1.5, method = 'rlm', alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "bottom", legend.justification = c("center"),text = element_text(family="sans", face = 'bold',size = 16.0),
        axis.text = element_text(family="sans", face = 'plain',size = 12.0),axis.text.x = element_text(family="sans", size = 12.0)) +
  xlab(label = 'Liking') +
  ylab(label = 'Wanting') +  ggtitle("") +
  facet_wrap(~fPhase_dicho_FCR_TT)

ggsave(paste(path_out, "checks/WL_Phases.png", sep=""), 
        plot = Plot_WL,  height = 10, width = 16, units = "in", dpi = 600, bg = "white")



