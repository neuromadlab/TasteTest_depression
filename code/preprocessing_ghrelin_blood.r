# TUE008 Phenotyping Study
# Corinna Schulz 
# 16.06.2023 
######################################################################
# Read in Ghrelin Blood data from Lab Files
# Make first Plots to inspect acyl and des-acyl data 
# save final csv with all blood data and preprocessed ghrelin values 

######################################################################
# (1) SET UP

# Install and load all required libraries
if (!require("librarian")) install.packages("librarian")
librarian::shelf(readxl, lme4, lmerTest, foreign, MASS, readr,
ggplot2, ggpubr, cowplot, gridExtra, viridis, tidybayes, dplyr, httpgd, 
languageserver, dabestr)


# Set all themes
theme_set(theme_cowplot(font_size = 12))

# Set Colors 

color_MDD <- "cornflowerblue"
color_HCP <-"darkgoldenrod4"

color_lowGhrelin = '#bc5090'
color_highGhrelin = '#ffa600'
# Set all Paths
setwd(getwd())
path_in <- "./input/" 
path_out <- "./output/" 
pathTUE <- "/mnt/TUE_general/" 

sub_prepro <-  "0.preprocessingQC/"
if (file.exists(paste(path_out, sub_prepro, sep = "")) == FALSE){
  dir.create(paste(path_out, sub_prepro, sep = ""))}

######################################################################
# (2) LOAD DATA and PREP DATA

# 2.1. Load Ghrelin Values

# Ghrelin Values ("raw results" and group membership)
d_acylG <- read_excel(path=paste(path_in, "TUE008_Acyl_fin.xlsx", sep = ""))
d_desaG <- read_excel(path=paste(path_in, "TUE008_Desacyl_fin.xlsx", sep = ""))

d_acylG$fID  <- factor(d_acylG$ID)
d_desaG$fID  <- factor(d_desaG$ID)

d_acylG$fSample  <- factor(d_acylG$sample)
d_desaG$fSample  <- factor(d_desaG$sample)

d_acylG$fPlate  <- factor(d_acylG$Plate)
d_desaG$fPlate  <- factor(d_desaG$Plate)

# Remove No Calc values
d_acylG$RESULT[d_acylG$RESULT ==  "NoCalc"] <- NA
d_desaG$RESULT[d_desaG$RESULT ==  "NoCalc"] <- NA

d_acylG <- d_acylG[complete.cases(d_acylG[,c("RESULT")]),]
d_desaG <- d_desaG[complete.cases(d_desaG[,c("RESULT")]),]

d_acylG$RESULT <- as.double(d_acylG$RESULT)
d_desaG$RESULT <- as.double(d_desaG$RESULT)

# Add MDD Info 
# Load TT data where Group is specified 
d <- read_excel(path=paste(path_in, "TT_TUE008_longformat.xlsx", sep = ""))
d$fID <- factor(d$ID)
d$fMDD <- factor(d$MDD, levels = c(0,1), labels = c("HCP", "MDD"))

fGroup <- select(d, ID, fMDD)

# Aggregate Group Information
fGroup <- fGroup %>%
  group_by(ID, fMDD) %>%
  summarize(ID = unique(ID))

d_acylG <- merge(d_acylG, fGroup, by = "ID", all = FALSE)
d_desaG <- merge(d_desaG, fGroup, by = "ID", all = FALSE)

# Rename for clarity for merging 
d_acylG$F_AG <- (d_acylG$RESULT)
d_desaG$F_DG <- (d_desaG$RESULT)

## Correct ghrelin levels (i.e., log transformation)
d_acylG$logF_AG <- log(d_acylG$F_AG)
d_desaG$logF_DG <- log(d_desaG$F_DG)

# Merge AG and DG values, also add other blood paramters 
d_desaG_merge <- select(d_desaG, fID, F_DG, logF_DG)

d_ghr <- merge(d_acylG, d_desaG_merge, by = "fID", all = TRUE)

# Load Blood parameters and Covariates 
d_conf <- read_excel(paste(path_in,"Ghrelin_Probanden_Daten.xlsx", sep = ""))

# Center Covariates for model interpretation later 
d_conf$cBMI <- d_conf$BMI - mean(d_conf$BMI, na.rm=TRUE) 
d_conf$cAge <- d_conf$Age - mean(d_conf$Age, na.rm=TRUE)
d_conf$cSex <- d_conf$Female - mean(d_conf$Female, na.rm=TRUE)

d_ghr <- merge(d_ghr, d_conf, by = "ID", all = FALSE)

# Remove incomplete Data (during datacollection, make sure at end of project this is correct!)
d_ghr <- d_ghr[complete.cases(d_ghr[,c("F_AG")]),]
d_ghr <- d_ghr[complete.cases(d_ghr[,c("F_DG")]),]

length(unique(d_ghr$ID)) # Check! 

######################################################################
# (3) Ghrelin Plots 

hist1 <- ggplot(d_acylG, aes(x = RESULT)) +
 #facet_wrap(~fPlate) +
  geom_histogram(binwidth = 20, fill = "#76767f") +
  xlab(label = 'Blood levels of acyl ghrelin [pg/ml]') +
  ggtitle("Acyl ghrelin (raw)")

hist1log <- ggplot(d_acylG, aes(x = logF_AG)) +
  #facet_wrap(~fPlate) +
  geom_histogram(binwidth = 0.2, fill = "#a3a3c9") +
  xlab(label = 'Blood levels of acyl ghrelin [pg/ml]') +
  ggtitle("Acyl ghrelin (log)")

hist2 <- ggplot(d_desaG, aes(x = RESULT)) +
  #facet_wrap(~fPlate) +
  geom_histogram(binwidth = 20, fill = "#76767f") +  
  xlab(label = 'Blood levels of des-acyl ghrelin [pg/ml]') +
  ggtitle("Des-acyl ghrelin (raw)")

hist2log <- ggplot(d_desaG, aes(x = logF_DG)) +
  #facet_wrap(~fPlate) +
  geom_histogram(binwidth = 0.2, fill = "#a3a3c9") +
  xlab(label = 'Blood levels of des-acyl ghrelin [log(pg/ml)]') +
  ggtitle("Des-acyl ghrelin (log)")

# Save Grid Plot
G <- grid.arrange(hist1, hist1log, hist2, hist2log, ncol=2, nrow=2, 
                  layout_matrix = rbind(c(1,2), c(3,4)),
                  widths=c(6,6), heights=c(6, 6))

ggsave(paste(path_out, sub_prepro, "0.Ghrelin_histograms.png", sep=""), 
        plot = G,  height = 6, width = 10, units = "in", dpi = 600, bg = "white")

# Test for Normality 
shapiro.test(d_desaG$logF_DG)
shapiro.test(d_acylG$logF_AG)


# Correlation of Fasting Acyl and Desacyl Ghrelin levels 

p1<-
  ggplot(aes(x = F_AG,y = F_DG),data = d_ghr) +
  geom_point(aes(size = BMI, color = fMDD)) +
  geom_smooth(aes(group = fMDD, color = fMDD), method = 'rlm', alpha = 0.1) +
  geom_smooth(color = 'black', group = 1, method = 'rlm', size = 1.5, alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP,color_MDD)) +
  scale_fill_manual(guide = guide_legend(title="Group"),values = c(color_HCP,color_MDD)) +
  theme(text = element_text(face = 'bold',size = 12.0),axis.text = element_text(face = 'plain',size = 12.0),
        axis.text.x = element_text(size = 12.0), strip.text.x = element_text(margin = margin(0.15,0,0.15,0, "cm")),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Correlation of ghrelin levels") +
  xlab(label = 'Fasting levels of acyl ghrelin (raw)') +
  ylab(label = 'Fasting levels of des-acyl ghrelin (raw)')

ggsave(paste(path_out, sub_prepro , "0.QC_Ghrelin_Corr_Raw_MDD.png", sep=""), 
              plot = p1,  height =6, width = 8, units = "in", dpi = 600, bg = "white")

p2<-
  ggplot(aes(x = F_AG,y = F_DG),data = d_ghr) +
  geom_point(aes(size = BMI, color = fPlate)) +
  geom_smooth(method = 'rlm', alpha = 0.1) +
  geom_smooth(color = 'black', group = 1, method = 'rlm', size = 1.5, alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"), values = c(color_HCP,color_MDD, color_highGhrelin, color_lowGhrelin)) +
  scale_fill_manual(guide = guide_legend(title="Group"), values = c(color_HCP,color_MDD, color_highGhrelin, color_lowGhrelin)) +
  theme(text = element_text(face = 'bold',size = 12.0),axis.text = element_text(face = 'plain',size = 12.0),
        axis.text.x = element_text(size = 12.0), strip.text.x = element_text(margin = margin(0.15,0,0.15,0, "cm")),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Correlation of ghrelin levels") +
  xlab(label = 'Fasting levels of acyl ghrelin (raw)') +
  ylab(label = 'Fasting levels of des-acyl ghrelin (raw)')

ggsave(paste(path_out, sub_prepro , "0.QC_Ghrelin_Corr_Raw_Plate.png", sep=""), 
              plot = p2,  height =6, width = 8, units = "in", dpi = 600, bg = "white")

p3<-
  ggplot(aes(x = logF_AG,y = logF_DG),data = d_ghr) +
  geom_point(aes(size = BMI, color = fMDD)) +
  geom_smooth( method = 'rlm', alpha = 0.1) +
  geom_smooth(color = 'black', group = 1, method = 'rlm', size = 1.5, alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"),values = c(color_HCP,color_MDD)) +
  scale_fill_manual(guide = guide_legend(title="Group"),values = c(color_HCP,color_MDD)) +
  theme(text = element_text(face = 'bold',size = 12.0),axis.text = element_text(face = 'plain',size = 12.0),
        axis.text.x = element_text(size = 12.0), strip.text.x = element_text(margin = margin(0.15,0,0.15,0, "cm")),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Correlation of ghrelin levels") +
  xlab(label = 'Fasting levels of acyl ghrelin (log)') +
  ylab(label = 'Fasting levels of des-acyl ghrelin (log)')

ggsave(paste(path_out, sub_prepro , "0.QC_Ghrelin_Corr_Log_MDD.png", sep=""), 
              plot = p3,  height =6, width = 8, units = "in", dpi = 600, bg = "white")

# Check for Simple Effects of BMI, Plate,... 
# Effect Coding: Sum Coding  contr.sum
# TLTR: Sum Coding compares the mean of the dep. varibale for a givel level (e.g. one level of Phase)
# to the "mean of means" of dep. variables at each level (e.g. each level of Phase)
# This is useful for interpreting interactions of categorical variables, if gives "deviations between conditions and the mean"

contrasts(d_ghr$fPlate) <-  contr.sum(levels(d_ghr$fPlate))
contrasts(d_ghr$fPlate) <-  contr.sum(levels(d_ghr$fPlate))

lm_F_AG <- lm(logF_AG ~    cBMI + cSex + cAge + fPlate , data = d_ghr)
summary(lm_F_AG)

lm_F_DG <- lm(logF_DG ~   cBMI + cSex + cAge  + fPlate, data = d_ghr)
summary(lm_F_DG)

# Residualize the Ghrelin Levels
# i.e., regress out cBMI and cAge and cSex (c = centered)

# Fasting levels
lm_F_AG <- lm(logF_AG ~ cBMI + cAge + cSex   , data = d_ghr, na.rm = TRUE)
summary(lm_F_AG)
lm_F_DG <- lm(logF_DG ~ cBMI + cAge + cSex  , data = d_ghr, na.rm = TRUE)
summary(lm_F_DG)

length(unique(d_ghr$fID))
# Linear Model for Residualization
d_ghr$res_logF_AG <- lm_F_AG$residuals
d_ghr$res_logF_DG <- lm_F_DG$residuals

# Test for Normality 
shapiro.test(d_ghr$res_logF_AG)
shapiro.test(d_ghr$res_logF_DG)


hist1res <- ggplot(d_ghr, aes(x = res_logF_AG)) +
  #facet_wrap(~fPlate) +
  geom_histogram(binwidth = 0.2, fill = "#bbbbf3") +  
  xlab(label = 'Blood levels of acyl ghrelin [res]') +
  ggtitle("Acyl ghrelin (log + res)")

hist2res <- ggplot(d_ghr, aes(x =res_logF_DG)) +
  #facet_wrap(~fPlate) +
  geom_histogram(binwidth = 0.2, fill = "#bbbbf3") +
  xlab(label = 'Blood levels of des-acyl ghrelin [res]') +
  ggtitle("Des-acyl ghrelin (log + res)")

# Save Grid Plot
G <- grid.arrange(hist1, hist1log, hist1res, hist2, hist2log, hist2res, ncol=3, nrow=2, 
                  layout_matrix = rbind(c(1,2,3), c(4,5,6)),
                  widths=c(13,13,13), heights=c(6, 6))

ggsave(paste(path_out, sub_prepro, "0.Ghrelin_histograms_reslog.png", sep=""), 
        plot = G,  height = 6, width = 12, units = "in", dpi = 600, bg = "white")

# Test correlations
cor.test(d_ghr$res_logF_AG, d_ghr$res_logF_DG)
cor.test(d_ghr$logF_AG, d_ghr$logF_DG)
cor.test(d_ghr$F_AG, d_ghr$F_DG)

# Test again but residualized
lm_F_AG <- lm(res_logF_AG ~ cBMI + cAge + fMDD + cSex  , data = d_ghr)
summary(lm_F_AG)

lm_F_DG <- lm(res_logF_DG ~ cBMI + cAge + fMDD + cSex   , data = d_ghr)
summary(lm_F_DG)

## Plot Res Log 
p4<-
  ggplot(aes(x = res_logF_AG,y = res_logF_DG),data = d_ghr) +
  geom_point(aes(size = BMI, color = fPlate)) +
  #geom_smooth(method = 'rlm', alpha = 0.1) +
  geom_smooth(aes(group = fPlate, color = fPlate), method = 'rlm', size = 1.5, alpha = 0.5) +
  scale_color_manual(guide = guide_legend(title="Group"), values = c(color_HCP,color_MDD, color_highGhrelin, color_lowGhrelin)) +
  scale_fill_manual(guide = guide_legend(title="Group"), values = c(color_HCP,color_MDD, color_highGhrelin, color_lowGhrelin)) +
  theme(text = element_text(face = 'bold',size = 12.0),axis.text = element_text(face = 'plain',size = 12.0),
        axis.text.x = element_text(size = 12.0), strip.text.x = element_text(margin = margin(0.15,0,0.15,0, "cm")),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle("Correlation of ghrelin levels") +
  xlab(label = 'Fasting levels of acyl ghrelin (res log)') +
  ylab(label = 'Fasting levels of des-acyl ghrelin (res log)')

ggsave(paste(path_out, sub_prepro , "0.QC_Ghrelin_Corr_ResLog_Plate_sep.png", sep=""), 
              plot = p4,  height =6, width = 8, units = "in", dpi = 600, bg = "white")

##########################################################################################
# Density Plots 

plot_AG1 <-
  ggplot(aes(x = res_logF_AG, group = fMDD, fill = fMDD),data = d_ghr) +
  geom_density(alpha=.5, adjust = 2) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "top",text = element_text(face = 'bold',size = 20.0),
        axis.text = element_text(face = 'plain',size = 18.0),axis.text.x = element_text(size = 18.0)) +
  xlab(label = 'Fasting acyl ghrelin (res log)') +
  ylab(label = 'Density')

ggsave(paste(path_out, sub_prepro , "0.QC_Plates_AG_MDD.png", sep=""), 
              plot = plot_AG1,  height =6, width = 8, units = "in", dpi = 600, bg = "white")


plot_DG1 <-
  ggplot(aes(x = res_logF_DG, group = fMDD, fill = fMDD),data = d_ghr) +
  geom_density(alpha=.5, adjust = 2) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_MDD)) +
  theme(legend.position = "top",text = element_text(face = 'bold',size = 20.0),
        axis.text = element_text(face = 'plain',size = 18.0),axis.text.x = element_text(size = 18.0)) +
  xlab(label = 'Fasting des-acyl ghrelin (res log)') +
  ylab(label = 'Density')

ggsave(paste(path_out, sub_prepro , "0.QC_Plates_DG_MDD.png", sep=""), 
              plot = plot_DG1,  height =6, width = 8, units = "in", dpi = 600, bg = "white")


plot_DG_plate <-
  ggplot(aes(x = res_logF_DG, group = fPlate, fill = fPlate), data = d_ghr) +
  geom_density(alpha=.5, adjust = 1) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_MDD, color_highGhrelin, color_lowGhrelin)) +
  theme(legend.position = "top",text = element_text(face = 'bold',size = 20.0),
        axis.text = element_text(face = 'plain',size = 18.0),axis.text.x = element_text(size = 18.0)) +
  xlab(label = 'Fasting des-acyl ghrelin (res log)') +
  ylab(label = 'Density')

ggsave(paste(path_out, sub_prepro , "0.QC_Plates_DAG.png", sep=""), 
              plot = plot_DG_plate,  height =6, width = 8, units = "in", dpi = 600, bg = "white")

plot_AG_plate <-
  ggplot(aes(x = res_logF_AG, group = fPlate, fill = fPlate), data = d_ghr) +
  geom_density(alpha=.5, adjust = 1) +
  geom_vline(xintercept = 0, color = "#3d3b3b", linewidth = 2, linetype = "dashed") +
  #stat_slab(aes(thickness = after_stat(pdf*n)), scale = 0.7) +
  #stat_dotsinterval(side = "bottom", scale = 0.7, slab_linewidth = NA) +  
  scale_fill_manual(guide = guide_legend(title=""),values = c(color_HCP, color_MDD, color_highGhrelin, color_lowGhrelin)) +
  theme(legend.position = "top",text = element_text(face = 'bold',size = 20.0),
        axis.text = element_text(face = 'plain',size = 18.0),axis.text.x = element_text(size = 18.0)) +
  xlab(label = 'Fasting acyl ghrelin (res log)') +
  ylab(label = 'Density')

ggsave(paste(path_out, sub_prepro , "0.QC_Plates_AG.png", sep=""), 
              plot = plot_AG_plate,  height =6, width = 8, units = "in", dpi = 600, bg = "white")

######################################################################
# Clean and Save Ghrelin CSV 

d_ghr_clean <- select(d_ghr, ID, fPlate, sample, F_AG, F_DG, fMDD, logF_AG, logF_DG, res_logF_AG, res_logF_DG, TG, CRP, Ins_p, Glk, HOMA_IR, TyG, Age, Weight, Height, Hip, Waist, BMI, Female, Age, cBMI, cAge, cSex)

# Write final csv file for further analysis

# write.csv(d_ghr_clean, paste("./input_original/","TUE008_ghrelin_summary_preprocessed.csv", sep = ""), row.names=FALSE)


