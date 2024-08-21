library(dplyr)
library(lubridate)
library(anytime)
library(hms)
library(pROC)

# Merge d3 (nonatal data) with data


df$neo_comp<-factor(ifelse(df$shoulder_dystocia_at_birth=="Yes" |
                             df$brachial_plexus_injury=="Yes" | 
                             df$neonatal_fracture___2=="Checked" |
                             df$neonatal_fracture___3=="Checked" | 
                             df$lga=="Yes" | 
                             df$need_for_iv_glucose=="Yes" | 
                             df$respiratory_distress_need=="Yes" | 
                             df$liveborn=="No" | df$neonatal_death=="Yes",1,0))


# L5 starttime and M10 starttime must be numeric for pROC package ---------------------------------------------------

#df$L5_starttime <- as.POSIXct(df$L5_starttime)
df$L5_starttime <- strptime(df$L5_starttime, format="%H:%M:%S")
df$L5_starttime_num <- hour(df$L5_starttime) + minute(df$L5_starttime)/60 + second(df$L5_starttime)/3600

#df$M10_starttime <- ymd_hms(df$M10_starttime)
df$M10_starttime <- strptime(df$M10_starttime, format="%H:%M:%S")
df$M10_starttime_num <- hour(df$M10_starttime) + minute(df$M10_starttime)/60 + second(df$M10_starttime)/3600




# Check cutoff values  ==============================================================================================
library(pROC)

df_cutoff <- df[, c('MESOR', 'Amplitude', 'corrected_acrophase', 'Percent.rhythm', 'IS', 'IV', 'RA', 'L5', 'M10', 'L5_starttime_num', 'M10_starttime_num',
                    'neo_comp', 'sd_injury', 'lga', 'need_for_iv_glucose', 'respiratory_distress_need', 'fet_neo_death', 'time_high')]



# Separate predictor variables and outcome variable
predictors <- df_cutoff[, c('MESOR', 'Amplitude', 'corrected_acrophase', 'Percent.rhythm', 'IS', 'IV', 'RA', 'L5', 'M10', 'L5_starttime_num', 'M10_starttime_num',
                            'sd_injury', 'lga', 'need_for_iv_glucose', 'respiratory_distress_need', 'fet_neo_death', 'time_high')]
outcome <- df$neo_comp

# Perform ROC analysis for each predictor variable
outcome <- as.numeric(outcome)

roc_mesor <- roc(outcome, predictors$MESOR)
roc_amplitude <- roc(outcome, predictors$Amplitude)
roc_acrophase <- roc(outcome, predictors$corrected_acrophase)
roc_IS <- roc(outcome, predictors$IS)
roc_IV <- roc(outcome, predictors$IV)
roc_RA <- roc(outcome, predictors$RA)
roc_L5 <- roc(outcome, predictors$L5)
roc_M10 <- roc(outcome, predictors$M10)
roc_L5starttime <- roc(outcome, predictors$L5_starttime_num)
roc_M10starttime <- roc(outcome, predictors$M10_starttime_num)
roc_PR <- roc(outcome, predictors$Percent.rhythm)




# Get the cutoff values
cutoff_mesor <- coords(roc_mesor, "best", best.method = "youden")

plot.roc(roc_mesor, print.thres = TRUE, #print.thres.pattern="%.2f",
         print.thres.col="red",
         print.thres.pattern=ifelse(roc_mesor$percent, "%.2f (%.1f%%, %.1f%%)", "%.2f (%.2f, %.2f)"),
         max.auc.polygon.border = TRUE)

roc_mesor$call


cutoff_amplitude <- coords(roc_amplitude, 'best', best.method = 'youden')
cutoff_acrophase <- coords(roc_acrophase, 'best', best.method = 'youden')
cutoff_IS <- coords(roc_IS, 'best', best.method = 'youden')
cutoff_IV <- coords(roc_IV, 'best', best.method = 'youden')
cutoff_RA <- coords(roc_RA, 'best', best.method = 'youden')
cutoff_L5 <- coords(roc_L5, 'best', best.method = 'youden')
cutoff_M10 <- coords(roc_M10, 'best', best.method = 'youden')
cutoff_L5starttime <- coords(roc_L5starttime, 'best', best.method = 'youden')
cutoff_M10starttime <- coords(roc_M10starttime, 'best', best.method = 'youden')
cutoff_PR <- coords(roc_PR, 'best', best.method = 'youden')


#Convert the cutoffs into dataframes
cutoff_mesor <- as.data.frame(cutoff_mesor)
cutoff_amplitude <- as.data.frame(cutoff_amplitude)
cutoff_acrophase <- as.data.frame(cutoff_acrophase)
cutoff_IS <- as.data.frame(cutoff_IS)
cutoff_IV <- as.data.frame(cutoff_IV)
cutoff_RA <- as.data.frame(cutoff_RA)
cutoff_L5 <- as.data.frame(cutoff_L5)
cutoff_M10 <- as.data.frame(cutoff_M10)
cutoff_L5starttime <- as.data.frame(cutoff_L5starttime)
cutoff_M10starttime <- as.data.frame(cutoff_M10starttime)
cutoff_PR <- as.data.frame(cutoff_PR)

df_cutoffs <- rbind(cutoff_mesor, cutoff_amplitude, cutoff_acrophase, cutoff_IS,
                    cutoff_IV, cutoff_RA, cutoff_L5, cutoff_M10, cutoff_L5starttime,
                    cutoff_M10starttime, cutoff_PR)

variables <- c('MESOR', 'Amplitude', 'Acrophase', 'Interdaily Stability', 
               'Intradaily Variability', 'Relative Amplitude', 'L5', 'M10',
               'L5 start time', 'M10 start time', 'Percentage of Rhythm')

df_cutoffs$Variable <- variables



# Add column with AUC and 95% CI

mesor_auc <- ci.auc(roc_mesor)
amplitude_auc <- ci.auc(roc_amplitude)
acrophase_auc <- ci.auc(roc_acrophase)
IS_auc <- ci.auc(roc_IS)
IV_auc <- ci.auc(roc_IV)
RA_auc <- ci.auc(roc_RA)
L5_auc <- ci.auc(roc_L5)
M10_auc <- ci.auc(roc_M10)
L5start_auc <- ci.auc(roc_L5starttime)
M10start_auc <- ci.auc(roc_M10starttime)
PR_auc <- ci.auc(roc_PR)

df_auc <- rbind(mesor_auc, amplitude_auc, acrophase_auc, IS_auc, IV_auc, RA_auc, 
                L5_auc, M10_auc, L5start_auc, M10start_auc, PR_auc)

df_auc <- as.data.frame(df_auc)

auc_ColNames <- c('AUC: 95% CI low', 'AUC', 'AUC: 95% CI up')

colnames(df_auc) <- auc_ColNames


# Bind df_cutoffs & df_auc and export

df_cutoffs <- cbind(df_cutoffs, df_auc)

df_cutoffs <- df_cutoffs[, c(4,1:3,6,5,7)]

df_cutoffs %>% as_flextable() %>% set_header_labels(
  values = list(
    threshold = 'Treshold',
    specificity = 'Specificity',
    sensitivity = 'Sensitivity')) %>% colformat_double(digits = 2) #%>%
# save_as_docx(path=paste0("tables/Circadian_ROCs_",Sys.Date(),".docx"))

# Create a column to differentiate between 112,5 mg/dl MESOR oscillating glucose

df$MESOR_cutoff <- ifelse(df$MESOR < 112.5, 'MESOR < 112.5 (mg/dL)',  'MESOR >= 112.5 (mg/dL)')



#library(writexl)
#write_xlsx(df, 'patient_cleaned_data/final_data.xlsx')

