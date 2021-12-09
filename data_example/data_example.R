library(devtools)
install_github("KatarzynaReluga/postcAIC/postcAIC", 
               auth_token = "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")

library(dplyr)
library(mosaic)
library(NHANES)
rm(list=ls())

postcAIC_nhaens0 <-
  NHANES %>%
  filter(SurveyYr == "2011_12", Age >= 20) %>%
  select(
    Poverty,
    SleepHrsNight,
    Gender,
    Age,
    Race1,
    BMI,
    BPSys2,
    DirectChol,
    Diabetes,
    PhysActive,
    SmokeNow,
    Smoke100
  ) %>%
  mutate(
    CurrentSmokingStatus = derivedFactor(
      Yes = SmokeNow == "Yes",
      No = (SmokeNow == "No" | Smoke100 == "No")
    ),
    clusterID = derivedFactor(
      Age20_f1  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Black"),
      Age20_f2  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age20_f3  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Mexican"),
      Age20_f4  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "Other"),
      Age20_f5  = (Age >= 20 & Age < 30 & Gender == "female" &
                     Race1 == "White"),

      Age20_m1  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Black"),
      Age20_m2  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age20_m3  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Mexican"),
      Age20_m4  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "Other"),
      Age20_m5  = (Age >= 20 & Age < 30 & Gender == "male" &
                     Race1 == "White"),

      Age30_f1  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Black"),
      Age30_f2  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age30_f3  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Mexican"),
      Age30_f4  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "Other"),
      Age30_f5  = (Age >= 30 & Age < 40 & Gender == "female" &
                     Race1 == "White"),

      Age30_m1  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Black"),
      Age30_m2  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age30_m3  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Mexican"),
      Age30_m4  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "Other"),
      Age30_m5  = (Age >= 30 & Age < 40 & Gender == "male" &
                     Race1 == "White"),
      
      Age40_f1  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Black"),
      Age40_f2  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age40_f3  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Mexican"),
      Age40_f4  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "Other"),
      Age40_f5  = (Age >= 40 & Age < 50 & Gender == "female" &
                     Race1 == "White"),

      Age40_m1  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Black"),
      Age40_m2  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age40_m3  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Mexican"),
      Age40_m4  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "Other"),
      Age40_m5  = (Age >= 40 & Age < 50 & Gender == "male" &
                     Race1 == "White"),

      Age50_f1  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Black"),
      Age50_f2  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age50_f3  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Mexican"),
      Age50_f4  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "Other"),
      Age50_f5  = (Age >= 50 & Age < 60 & Gender == "female" &
                     Race1 == "White"),

      Age50_m1  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Black"),
      Age50_m2  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age50_m3  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Mexican"),
      Age50_m4  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "Other"),
      Age50_m5  = (Age >= 50 & Age < 60 & Gender == "male" &
                     Race1 == "White"),
      

      Age60_f1  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Black"),
      Age60_f2  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age60_f3  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Mexican"),
      Age60_f4  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "Other"),
      Age60_f5  = (Age >= 60 & Age < 70 & Gender == "female" &
                     Race1 == "White"),
      #      : Mexican, Hispanic, White, Black, or Oth
      Age60_m1  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Black"),
      Age60_m2  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age60_m3  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Mexican"),
      Age60_m4  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "Other"),
      Age60_m5  = (Age >= 60 & Age < 70 & Gender == "male" &
                     Race1 == "White"),
      
      Age70_f1  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Black"),
      Age70_f2  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Hispanic"),
      Age70_f3  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Mexican"),
      Age70_f4  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "Other"),
      Age70_f5  = (Age >= 70 & Age <= 80 & Gender == "female" &
                     Race1 == "White"),

      Age70_m1  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Black"),
      Age70_m2  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Hispanic"),
      Age70_m3  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Mexican"),
      Age70_m4  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "Other"),
      Age70_m5  = (Age >= 70 & Age <= 80 & Gender == "male" &
                     Race1 == "White")
    ), log_BMI = log(BMI),
  ) %>% 
  select(c("PhysActive", "CurrentSmokingStatus", "SleepHrsNight", 
           "BPSys2", "DirectChol", "Poverty",
           "Diabetes", "clusterID", "log_BMI"))


postcAIC_nhaens <- na.omit(postcAIC_nhaens0) 


#######################################################
y = postcAIC_nhaens$log_BMI

X = data.frame(select(postcAIC_nhaens, -c("clusterID", "log_BMI")))

clusterID = postcAIC_nhaens$clusterID

cAIC_model_set = compute_cAIC_for_model_set(X, y, clusterID,
                                            model = "NERM",
                                            covariate_selection_matrix = NULL,
                                            modelset  = "part_subset",
                                            common = c(1:2),
                                            intercept = FALSE)

cAIC_min = cAIC_model_set$cAIC_min 
degcAIC_models = cAIC_model_set$degcAIC_models

Z = cAIC_model_set$Z
X_full = cAIC_model_set$X_full
X_cluster_full = cAIC_model_set$X_cluster_full

G_full = cAIC_model_set$G_full
R_full = cAIC_model_set$R_full
V_full = cAIC_model_set$V_full

beta_sel = cAIC_model_set$beta_sel
mu_sel = cAIC_model_set$mu_sel

modelset_matrix = cAIC_model_set$modelset_matrix
x_beta_lin_com = cAIC_model_set$X_cluster_full



# Post-cAIC CI for mixed and fixed parameters -------------------------------------
postcAIC_CI_results = postcAIC_CI(cAIC_min, degcAIC_models,
                                  Z, X_full, X_cluster_full,
                                  G_full, R_full, V_full,
                                  
                                  beta_sel, mu_sel,
                                  
                                  modelset  = "part_subset",
                                  common = c(1:2), 
                                  modelset_matrix, x_beta_lin_com,
                                  n_starting_points = 5, 
                                  scale_mvrnorm = 10)

# Naive CI for mixed and fixed parameters -------------------------------------

sig_u_sel = cAIC_model_set$sig_u_sel
sig_e_sel = cAIC_model_set$sig_e_sel
indices_sel = cAIC_model_set$indices_sel
X_cluster_sel = cAIC_model_set$X_cluster_full[, indices_sel]
C_cluster_sel = cbind(as.matrix(X_cluster_sel), diag(nlevels(clusterID)))

R_sel = cAIC_model_set$R_sel
V_sel = cAIC_model_set$V_sel
G_sel = cAIC_model_set$G_sel

naive_CI_results = naive_CI(beta_sel, mu_sel,
                            G_sel, R_sel, V_full,
                            sig_u_sel, sig_e_sel,
                            X_full, Z, C_cluster_sel = C_cluster_sel,
                            clusterID, indices_sel = indices_sel,
                            type_MSE_mixed = "corrected",
                            x_beta_lin_com)

# Post-OBSP CI for mixed parameters -------------------------------------
sig_u_full = cAIC_model_set$sig_u_full
sig_e_full = cAIC_model_set$sig_e_full

postOBSP_CI_results = postOBSP_CI(X_full, y, clusterID, Z, X_cluster_full,
                                  G_full, V_full, R_full,
                                  sig_u_full, sig_e_full,
                                  modelset_matrix,
                                  boot = 200)


