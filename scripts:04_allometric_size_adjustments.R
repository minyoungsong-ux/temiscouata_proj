# ============================================
# Temiscouata Project – Allometric Size Adjustments
# MSc Thesis
# Author: Sarah Minyoung Song
# ============================================
library(tidyverse)

# ALTERNATIVE CODE TO INCLUDE CMN STICKLEBACK WITH N/As, STILL MISSING SOME
# CONTEMPORARY FISH WITH HALF BODIES
# data.log <- stickleback_analysis |>
#  filter(sl > 0) |>
#  mutate(across(c(sl, body_depth, ap_length, ap_width, jaw_length,
#                  ppl, ppw, dorsal_1, dorsal_2, pspine_r, pspine_l),
#                ~ log10(.)))

data.log <- stickleback_analysis |>
  filter(across(c(sl, body_depth, ap_length, ap_width, jaw_length,
                  ppl, ppw, dorsal_1, dorsal_2, pspine_r, pspine_l),
                ~ . > 0)) |>
  mutate_at(vars(sl, body_depth, ap_length, ap_width, jaw_length, ppl, ppw, dorsal_1,
                 dorsal_2, pspine_r, pspine_l),
            log10)

# size.coef is a function to get coefficient for #size adjustment 
# trait - name of trait col to get #slope coefficient
# altered code to account for NA and INF values
size.coef<-function(traits){
  ancova <- lm(traits ~ sl + lake, data=data.log, na.action = na.omit)
  coef <- ancova$coefficients[2]
  coef
}

data.log <- data.log |>
  mutate(across(c(sl, body_depth, ap_length, ap_width, jaw_length,
                  ppl, ppw, dorsal_1, dorsal_2, pspine_r, pspine_l),
                ~ ifelse(is.finite(.), ., NA)))

# The following line produces slope coefficient. Repeat for each trait.
BD.coef<-size.coef(data.log$body_depth)
APL.coef<-size.coef(data.log$ap_length)
APW.coef<-size.coef(data.log$ap_width)
JL.coef<-size.coef(data.log$jaw_length)
PPL.coef<-size.coef(data.log$ppl)
PPW.coef<-size.coef(data.log$ppw)
D1.coef<-size.coef(data.log$dorsal_1)
D2.coef<-size.coef(data.log$dorsal_2)
PSR.coef<-size.coef(data.log$pspine_r)
PSL.coef<-size.coef(data.log$pspine_l)

# Following makes a motrix 1 x k matrix of slopes for each trait
Coef<-matrix(c(BD.coef, APL.coef, APW.coef,JL.coef, PPL.coef, PPW.coef, D1.coef, D2.coef, PSR.coef, PSL.coef),dimnames = list(c("body_depth", "ap_length", "ap_width", "jaw_length", "ppl", "ppw", "dorsal_1", "dorsal_2", "pspine_r", "pspine_l")))

# Adjusts trait value using formula: Xadj=Xi*(mean(SL)/SLi)^b
stickleback_analysis$body_depth.adj<-stickleback_analysis$body_depth*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["body_depth",]
stickleback_analysis$ap_length.adj<-stickleback_analysis$ap_length*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["ap_length",]
stickleback_analysis$ap_width.adj<-stickleback_analysis$ap_width*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["ap_width",]
stickleback_analysis$jaw_length.adj<-stickleback_analysis$jaw_length*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["jaw_length",]
stickleback_analysis$ppl.adj<-stickleback_analysis$ppl*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["ppl",]
stickleback_analysis$ppw.adj<-stickleback_analysis$ppw*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["ppw",]
stickleback_analysis$dorsal_1.adj<-stickleback_analysis$dorsal_1*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["dorsal_1",]
stickleback_analysis$dorsal_2.adj<-stickleback_analysis$dorsal_2*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["dorsal_2",]
stickleback_analysis$pspine_r.adj<-stickleback_analysis$pspine_r*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["pspine_r",]
stickleback_analysis$pspine_l.adj<-stickleback_analysis$pspine_l*(mean(stickleback_analysis$sl)/stickleback_analysis$sl)^Coef["pspine_l",]

# Allometric size adjustments for gill rakers?
raker.log <- gill_spatial |>
  mutate_at(vars(SL, mean_length_mm, mean_width_mm),
            log10)

size.coef<-function(traits){
  ancova=lm(data=raker.log, traits~SL+lake)
  coef=ancova$coefficients[2]
  coef
}

rakerlength.coef<-size.coef(raker.log$mean_length_mm)
rakerwidth.coef<-size.coef(raker.log$mean_width_mm)

Coef<-matrix(c(rakerlength.coef, rakerwidth.coef),dimnames = list(c("mean_length_mm", "mean_width_mm")))

gill_spatial$mean_length_mm.adj<-gill_spatial$mean_length_mm*(mean(gill_spatial$SL)/gill_spatial$SL)^Coef["mean_length_mm",]
gill_spatial$mean_width_mm.adj<-gill_spatial$mean_width_mm*(mean(gill_spatial$SL)/gill_spatial$SL)^Coef["mean_width_mm",]

# PLOTS AFTER ALLOMETRIC SIZE ADJUSTMENTS:
# PCA? (unfinished)
pca_data <- data.log[, c("lake","year", traits)]

# remove NA
pca_data <- pca_data[complete.cases(pca_data), ]

# remove Inf
pca_data <- pca_data[!apply(pca_data[,traits],1,function(x) any(is.infinite(x))), ]
