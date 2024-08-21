library(tibble)
library(flextable)
library(writexl)

rm(list=ls())

# Load environment
load('environment_LinearReg.Rdata')

# Serie 1: MESOR_cutoff + 1 covariate ==========================================



# Model 1: MESOR_cutoff + time_high
df$MESOR_cutoff2 <- as.factor(ifelse(df$MESOR_cutoff == "MESOR < 112.5 (mg/dL)", 0, 1))
df$time_high2 <- as.factor(ifelse(df$time_high == "Time above range <10%", 0, 1))
df$neo_comp2 <- as.numeric(ifelse(df$neo_comp == 1, 0, 1))
 
mod01 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + time_high2,
                         id=record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod01)
m1 <- as_tibble(t(geepack::QIC(mod01)))

# Model 2: MESOR_cutoff + age
mod02 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age,
                         id=record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")


summary(mod02)
m2 <- as_tibble(t(geepack::QIC(mod02)))

# Model 3: MESOR_cutoff + age
mod03 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + bmi,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod03)
m3 <- as_tibble(t(geepack::QIC(mod03)))

# Model 4: MESOR_cutoff + Amplitude
mod04 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + Amplitude,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod04)
m4 <- as_tibble(t(geepack::QIC(mod04)))

# Model 5: MESOR_cutoff + Acrophase
mod05 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + corrected_acrophase,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod05)
m5 <- as_tibble(t(geepack::QIC(mod05)))

# Model 6: MESOR_cutoff + IS

df_mod06 <- df[complete.cases(df[,c("neo_comp2", "MESOR_cutoff2", "IS")]), ]

mod06 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + IS,
                         id = record_id,
                         data = df_mod06,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod06)
m6 <- as_tibble(t(geepack::QIC(mod06)))

# Model 7: MESOR_cutoff + IV
mod07 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + IV,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod07)
m7 <- as_tibble(t(geepack::QIC(mod07)))

# Model 8: MESOR_cutoff + RA
mod08 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + RA,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod08)
m8 <- as_tibble(t(geepack::QIC(mod08)))

# Model 9: MESOR_cutoff + M10
mod09 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + M10,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod09)
m9 <- as_tibble(t(geepack::QIC(mod09)))

# Model 10: MESOR_cutoff + M10
mod10 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + L5,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod10)
m10 <- as_tibble(t(geepack::QIC(mod10)))

# Model 11: MESOR_cutoff + M10
df_mod11 <- df[complete.cases(df[,c("neo_comp2", "MESOR_cutoff2", "M10_starttime_num")]), ]

mod11 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + M10_starttime_num,
                         id = record_id,
                         data = df_mod11,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod11)
m11 <- as_tibble(t(geepack::QIC(mod11)))

# Model 12: MESOR_cutoff + M10

df_mod12 <- df[complete.cases(df[,c("neo_comp2", "MESOR_cutoff2", "L5_starttime_num")]), ]

mod12 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + L5_starttime_num,
                         id = record_id,
                         data = df_mod12,
                         family = binomial,
#                         na.action = na.omit,
                         corstr="exch")
summary(mod12)
m12 <- as_tibble(t(geepack::QIC(mod12)))

# Model 13: MESOR_cutoff + PR
mod13 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + Percent.rhythm,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod13)
m13 <- as_tibble(t(geepack::QIC(mod13)))

m01 <- as.data.frame(rbind(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, 
             m11, m12, m13))# %>% as_flextable()

#write_xlsx(m01, "O:/Projects/PROJECT - GLUCOSE RHYTHM GDM/tables/m01_coefs.xlsx")

# Serie 2: MESOR_cutoff + 2 covariates ==========================================
# Model 14: MESOR_cutoff + age + bmi

mod14 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + bmi,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod14)
m14 <- as_tibble(t(geepack::QIC(mod14)))

# Model 15: MESOR_cutoff + age + amplitude

mod15 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + Amplitude,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")


summary(mod15)
m15 <- as_tibble(t(geepack::QIC(mod15)))

# Model 16: MESOR_cutoff + age + acrophase

mod16 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + corrected_acrophase,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")


summary(mod16)
m16 <- as_tibble(t(geepack::QIC(mod16)))

# Model 17: MESOR_cutoff + age + IS

df_mod17 <- df[complete.cases(df[,c("neo_comp2", "MESOR_cutoff2", "age", "IS")]), ]

mod17 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + IS,
                         id = record_id,
                         data = df_mod17,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod17)
m17 <- as_tibble(t(geepack::QIC(mod17)))

# Model 18: MESOR_cutoff + age + IV

mod18 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + IV,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod18)
m18 <- as_tibble(t(geepack::QIC(mod18)))

# Model 19: MESOR_cutoff + age + RA
mod19 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + RA,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod19)
m19 <- as_tibble(t(geepack::QIC(mod19)))

# Model 20: MESOR_cutoff + age + M10
mod20 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + M10,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod20)
m20 <- as_tibble(t(geepack::QIC(mod20)))

# Model 21: MESOR_cutoff + age + L5
mod21 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + L5,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod21)
m21 <- as_tibble(t(geepack::QIC(mod21)))

# Model 22: MESOR_cutoff + age + M10 start-time

df_mod22 <- df[complete.cases(df[,c("neo_comp2", "MESOR_cutoff2", "age", "M10_starttime_num")]), ]

mod22 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + M10_starttime_num,
                         id = record_id,
                         data = df_mod22,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod22)
m22 <- as_tibble(t(geepack::QIC(mod22)))

# Model 23: MESOR_cutoff + age + L5 start-time

df_mod23 <- df[complete.cases(df[,c("neo_comp2", "MESOR_cutoff2", "age", "L5_starttime_num")]), ]

mod23 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + L5_starttime_num,
                         id = record_id,
                         data = df_mod23,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod23)
m23 <- as_tibble(t(geepack::QIC(mod23)))

# Model 24: MESOR_cutoff + age + PR

mod24 <- geepack::geeglm(neo_comp2 ~ MESOR_cutoff2 + age + Percent.rhythm,
                         id = record_id,
                         data = df,
                         family = binomial,
                         na.action = na.omit,
                         corstr="exch")

summary(mod24)
m24 <- as_tibble(t(geepack::QIC(mod24)))

m02 <- as.data.frame(rbind(m14, m15, m16, m17, m18, m19, m20,
                           m21, m22, m23, m24))

#write_xlsx(m02, "O:/Projects/PROJECT - GLUCOSE RHYTHM GDM/tables/m02_coefs.xlsx")

# MODEL 22 - PREDICTIONS for the new two models =========================================

# Predict the outcome for each observation
df$mod22_predictions <- predict(mod22, 
                               newdata = df, type = "response")






# ROCs for the new models (three-covariate model)============================================

# Check cutoff values  ==============================================================================================
library(pROC)

df_cutoff <- df[, c('MESOR', 'Amplitude', 'corrected_acrophase', 'Percent.rhythm', 'IS', 'IV', 'RA', 'L5', 'M10', 'L5_starttime_num', 'M10_starttime_num',
                    'neo_comp', 'sd_injury', 'lga', 'need_for_iv_glucose', 'respiratory_distress_need', 'fet_neo_death', 'time_high', 'mod5_predictions', 'mod22_predictions')]


# Separate predictor variables and outcome variable
predictors <- df_cutoff[, c('MESOR', 'Amplitude', 'corrected_acrophase', 'Percent.rhythm', 'IS', 'IV', 'RA', 'L5', 'M10', 'L5_starttime_num', 'M10_starttime_num',
                            'sd_injury', 'lga', 'need_for_iv_glucose', 'respiratory_distress_need', 'fet_neo_death', 'time_high', 'mod5_predictions', 'mod22_predictions')]
outcome <- df$neo_comp



# Model 5 / Model 2 - Perform ROC analysis 

roc_mod5 <- roc(outcome, predictors$mod5_predictions)

cutoff_mod5 <- coords(roc_mod5, "best", best.method = "youden")

coords(roc_mod5, "best", best.method = "youden")

auc(roc_mod5)


# Model 22 - Perform ROC analysis 
outcome <- as.numeric(outcome)

roc_mod22 <- roc(outcome, predictors$mod22_predictions)


coords(roc_mod22, "best", best.method = "youden")

auc(roc_mod22)

ci.auc(roc_mod22)


# Cutoff variable for model 22

df$mod22value <- ifelse(df$mod22_predictions > 0.4703, 
                       'mod22 value < 0.4703',  'mod22 value >= 0.4703') 



#library(writexl)
#write_xlsx(df, "O:/PROJECT - GLUCOSE RHYTHM GDM/patient_cleaned_data/data_logMod_3covs.xlsx")
