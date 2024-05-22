stroke_dat_dwi <- read.csv("C:/Users/morit/Documents/Fachhochschule/IDP/Projekte/Stroke/Data/Bern/data_bern_25_11_2020_dwi.csv", header=T, sep = ",")


# NA detection:

na_col_log <- rep(NA, dim(stroke_dat_dwi)[2])
for (col in 1:dim(stroke_dat_dwi)[2]) {
  na_col_log[col] <- any(is.na(stroke_dat_dwi[,col]))
}
na_col <- which(na_col_log == T)

na_sum_col <- rep(NA, length(na_col))
for (i in 1:length(na_col)) {
  na_sum_col[i] <- sum(is.na(stroke_dat_dwi[,na_col[i]]))
}
na_sum_col


na_summary <- data.frame(na_col, na_sum_col)
colnames(na_summary) <- c("column", "NA_count")

na_summary

apply(stroke_dat_dwi, MARGIN = 2, FUN = function(x) sum(is.na(x)))
stroke_dat_dwi$angio <- factor(stroke_dat_dwi$angio, levels = c("0", "1"))

# char columns detection
char_col_log <- rep(NA, dim(stroke_dat_dwi)[2])
for (col in 1:dim(stroke_dat_dwi)[2]) {
  char_col_log[col] <- ifelse(class(stroke_dat_dwi[,col]) == "character", TRUE, FALSE)
}
char_col <- which(char_col_log == T)
(char_col_names <- names(stroke_dat_dwi[,char_col]))


#X_tab_train = np.array([train[f].age, train[f].nihss_bl, train[f].sys_bloodpressure_bl, 
#                        train[f].rf_diabetes, train[f].rf_hypertonia, train[f].rf_smoker, 
#                        train[f].rf_tia_stroke, train[f].lyse, train[f].time_to_groin_puncture, 
#                         train[f].collateralization, train[f].S_Medm_rbf, train[f].volume_adc, train[f].volume_tar]).T

factor_ind <- stroke_dat_dwi[1,] == 0 | stroke_dat_dwi[1,] == 1
factor_ind <- which(factor_ind == T)
(factor_ind <- sort(c(factor_ind[c(-3, -10, -28)], 37, 20, 47, 55, 63, 65)))
stroke_dat_dwi$angio <- gsub(pattern = 2, replacement = 1, stroke_dat_dwi$angio)
stroke_dat_dwi$angio <- as.numeric(stroke_dat_dwi$angio)
stroke_dat_dwi$angio <- as.factor(stroke_dat_dwi$angio)

stroke_dat_dwi[factor_ind] <- lapply(stroke_dat_dwi[factor_ind], FUN=as.factor)

# imputation
library(missForest)
stroke_dat_dwi.imp <- missForest(stroke_dat_dwi[,-char_col], verbose = T, variablewise = T)
stroke_dat_dwi.imp$OOBerror
stroke_dat_dwi_imp <- stroke_dat_dwi.imp$ximp
write.csv(stroke_dat_dwi_imp, "C:/Users/morit/Documents/Fachhochschule/IDP/Projekte/Stroke/Data/Bern/data_bern_25_11_2020_dwi_imputed.csv", row.names=FALSE)
View(stroke_dat_dwi_imp)

save(stroke_dat_dwi_imp, file = "C:/Users/morit/Documents/Fachhochschule/IDP/Projekte/Stroke/Data/Bern/data_bern_25_11_2020_dwi_imputed.Rda")
stroke_dat_dwi_imp <- read.csv("C:/Users/morit/Documents/Fachhochschule/IDP/Projekte/Stroke/Data/Bern/data_bern_25_11_2020_dwi_imputed.csv", header=T, sep = ",")



# NA detection

na_col_log <- rep(NA, dim(stroke_dat_dwi_imp)[2])
for (col in 1:dim(stroke_dat_dwi_imp)[2]) {
  na_col_log[col] <- any(is.na(stroke_dat_dwi_imp[,col]))
}
sum(na_col_log) # 0 -> no more NAs
