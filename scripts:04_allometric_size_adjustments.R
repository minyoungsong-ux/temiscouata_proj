# ============================================
# Temiscouata Project – Allometric Size Adjustments
# MSc Thesis
# Author: Sarah Minyoung Song
# ============================================
library(tidyverse)

data.log <- stickleback_all |>
  mutate_at(vars(sl, body_depth, ap_length, ap_width, jaw_length, ppl, ppw, dorsal_1,
                 dorsal_2, pspine_r, pspine_l),
            log10)

# size.coef is a function to get coefficient for #size adjustment 
# trait - name of trait col to get #slope coefficient
size.coef<-function(traits){
  ancova=lm(data=data.log, traits~sl+lake)
  coef=ancova$coefficients[2]
  coef
}

# The following line produces slope coefficient. Repeat for each trait.
BD.coef<-size.coef(data.log$body_depth)
APL.coef<-size.coef(data.log$ap_length)
APW.coef<-size.coef(data.log$ap_width)
JL.coef<-size.coef(data.log$jaw_length)
PPL.coef<-size.coef(data.log$ppl)
PPW.coef<-size.coef(data.log$ppw)
D1.coef<-size.coef(data.log$dorsal_1)
D2.coef<-size.coef(data.log$dorsal_2)

data.log <- stickleback_all |>
  filter(pspine_r > 0, pspine_l > 0) |>
  mutate(
    across(
      c(sl, body_depth, ap_length, ap_width, jaw_length,
        ppl, ppw, dorsal_1, dorsal_2, pspine_r, pspine_l),
      log10
    )
  )

PSR.coef<-size.coef(data.log$pspine_r)
PSL.coef<-size.coef(data.log$pspine_l)

# Following makes a motrix 1 x k matrix of slopes for each trait
Coef<-matrix(c(BD.coef, APL.coef, APW.coef,JL.coef, PPL.coef, PPW.coef, D1.coef, D2.coef, PSR.coef, PSL.coef),dimnames = list(c("body_depth", "ap_length", "ap_width", "jaw_length", "ppl", "ppw", "dorsal_1", "dorsal_2", "pspine_r", "pspine_l")))

# Adjusts trait value using formula: Xadj=Xi*(mean(SL)/SLi)^b
stickleback_all$body_depth.adj<-stickleback_all$body_depth*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["body_depth",]
stickleback_all$ap_length.adj<-stickleback_all$ap_length*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["ap_length",]
stickleback_all$ap_width.adj<-stickleback_all$ap_width*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["ap_width",]
stickleback_all$jaw_length.adj<-stickleback_all$jaw_length*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["jaw_length",]
stickleback_all$ppl.adj<-stickleback_all$ppl*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["ppl",]
stickleback_all$ppw.adj<-stickleback_all$ppw*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["ppw",]
stickleback_all$dorsal_1.adj<-stickleback_all$dorsal_1*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["dorsal_1",]
stickleback_all$dorsal_2.adj<-stickleback_all$dorsal_2*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["dorsal_2",]
stickleback_all$pspine_r.adj<-stickleback_all$pspine_r*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["pspine_r",]
stickleback_all$pspine_l.adj<-stickleback_all$pspine_l*(mean(stickleback_all$sl)/stickleback_all$sl)^Coef["pspine_l",]