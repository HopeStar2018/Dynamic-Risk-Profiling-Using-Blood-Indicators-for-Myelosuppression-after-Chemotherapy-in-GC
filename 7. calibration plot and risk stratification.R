setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output")
library(ggplot2)
library(dplyr)
library(pROC)

############################################################################################################################################
############ data preparation for calibration plot (isotonic)
data_calibration<-read.csv("smote_model_smote_testset_with_prediction_callibration_isotonic.csv")
data_calibration<-subset(data_callibration,select = c(event,posterior))

names(data_calibration)
summary(data_calibration)

data_calibration$event<-as.factor(data_calibration$event)
data_calibration$posterior_group<-cut(data_calibration$posterior,quantile(data_callibration$posterior,seq(0,1,0.1)))

d1<-subset(data_calibration,data_calibration$posterior_group == "(0.00199,0.128]")
d2<-subset(data_calibration,data_calibration$posterior_group == "(0.128,0.225]")
d3<-subset(data_calibration,data_calibration$posterior_group == "(0.225,0.344]")
d4<-subset(data_calibration,data_calibration$posterior_group == "(0.344,0.504]")
d5<-subset(data_calibration,data_calibration$posterior_group == "(0.504,0.63]")
d6<-subset(data_calibration,data_calibration$posterior_group == "(0.63,0.717]")
d7<-subset(data_calibration,data_calibration$posterior_group == "(0.717,0.809]")
d8<-subset(data_calibration,data_calibration$posterior_group == "(0.809,0.865]")
d9<-subset(data_calibration,data_calibration$posterior_group == "(0.865,0.891]")
d10<-subset(data_calibration,data_calibration$posterior_group == "(0.891,0.931]")

t.test(d1$posterior)
t.test(d2$posterior)
t.test(d3$posterior)
t.test(d4$posterior)
t.test(d5$posterior)
t.test(d6$posterior)
t.test(d7$posterior)
t.test(d8$posterior)
t.test(d9$posterior)
t.test(d10$posterior)

prop.test(2,78,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(6,80,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(9,78,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(30,80,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(39,79,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(46,78,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(54,79,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(67,81,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(71,78,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(71,78,p = NULL, alternative = "two.sided",correct = TRUE)

## fit the calibration plot (calculate the intercept and slope)
cal<-read.csv("callibration_plot_result_isotonic.csv")
cal<-subset(cal,select = c(Obs,Pred))
fit<-lm(Obs~Pred,data = cal)



############################################################################################################################################
############### calculate the 95% confidence interval of AUC for different models
setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output/isotonic")

static_4w<-read.csv("testset_static_bayes_cycle_4_prediction_result.csv")
static_8w<-read.csv("testset_static_bayes_cycle_8_prediction_result.csv")
PLT_q_4w<-read.csv("testset_PLT_q_cycle_4_model_prediction_result.csv")
PLT_q_8w<-read.csv("testset_PLT_q_cycle_8_model_prediction_result.csv")
Hb_q_4w<-read.csv("testset_Hb_q_cycle_4_model_prediction_result.csv")
Hb_q_8w<-read.csv("testset_Hb_q_cycle_8_model_prediction_result.csv")
WBC_q_4w<-read.csv("testset_WBC_q_cycle_4_model_prediction_result.csv")
WBC_q_8w<-read.csv("testset_WBC_q_cycle_8_model_prediction_result.csv")
Neut_q_4w<-read.csv("testset_Neut_q_cycle_4_model_prediction_result.csv")
Neut_q_8w<-read.csv("testset_Neut_q_cycle_8_model_prediction_result.csv")
Lymph_q_4w<-read.csv("testset_Lymph_q_cycle_4_model_prediction_result.csv")
Lymph_q_8w<-read.csv("testset_Lymph_q_cycle_8_model_prediction_result.csv")
static_dynamic_4w<-read.csv("testset_dynamic_plus_static_bayes_cycle_4_prediction_result.csv")
static_dynamic_8w<-read.csv("testset_dynamic_plus_static_bayes_cycle_8_prediction_result.csv")

roc1 <-roc(as.factor(static_4w$event),static_4w$posterior,ci=TRUE,auc = T)
roc2 <-roc(as.factor(PLT_q_4w$event),PLT_q_4w$posterior,ci=TRUE,auc = T)
roc3 <-roc(as.factor(Hb_q_4w$event),Hb_q_4w$posterior,ci=TRUE,auc = T)
roc4 <-roc(as.factor(WBC_q_4w$event),WBC_q_4w$posterior,ci=TRUE,auc = T)
roc5 <-roc(as.factor(Neut_q_4w$event),Neut_q_4w$posterior,ci=TRUE,auc = T)
roc6 <-roc(as.factor(Lymph_q_4w$event),Lymph_q_4w$posterior,ci=TRUE,auc = T)
roc7 <-roc(as.factor(static_dynamic_4w$event),static_dynamic_4w$posterior,ci=TRUE,auc = T)

roc8 <-roc(as.factor(static_8w$event),static_8w$posterior,ci=TRUE,auc = T)
roc9 <-roc(as.factor(PLT_q_8w$event),PLT_q_8w$posterior,ci=TRUE,auc = T)
roc10 <-roc(as.factor(Hb_q_8w$event),Hb_q_8w$posterior,ci=TRUE,auc = T)
roc11 <-roc(as.factor(WBC_q_8w$event),WBC_q_8w$posterior,ci=TRUE,auc = T)
roc12 <-roc(as.factor(Neut_q_8w$event),Neut_q_8w$posterior,ci=TRUE,auc = T)
roc13 <-roc(as.factor(Lymph_q_8w$event),Lymph_q_8w$posterior,ci=TRUE,auc = T)
roc14 <-roc(as.factor(static_dynamic_8w$event),static_dynamic_8w$posterior,ci=TRUE,auc = T)

roc.test(roc1,roc7)
roc.test(roc2,roc7)
roc.test(roc3,roc7)
roc.test(roc4,roc7)
roc.test(roc5,roc7)
roc.test(roc6,roc7)

roc.test(roc8,roc14)
roc.test(roc9,roc14)
roc.test(roc10,roc14)
roc.test(roc11,roc14)
roc.test(roc12,roc14)
roc.test(roc13,roc14)


#######################################################################################################################
###### incidence rate of different cycyle
prop.test(39,474,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(37,474,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(33,474,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(16,474,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(10,405,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(6,349,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(2,231,p = NULL, alternative = "two.sided",correct = TRUE)
prop.test(3,182,p = NULL, alternative = "two.sided",correct = TRUE)


#######################################################################################################################
###### confusion matrix result of different threshold
setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output/isotonic")
data_isotonic<-read.csv("testset_dynamic_plus_static_bayes_cycle_4_prediction_result.csv")

data_sub <- c()
cycle_n <- c()
threshold_low <- c()
threshold_high <- c()
n1 <- c()
n2 <- c()
n3 <- c()
n4 <- c()
n5 <- c()
n6 <- c()
p_low <- c()
p_mid <- c()
p_high <- c()
p1 <- c()
p2 <- c()

i = 1
for (x in 1:4) {
  data_sub[[x]]<-data_isotonic[which(data_isotonic$cycle == x),]
  for (thresh1 in seq(0,0.5,by=0.05)){
    for (thresh2 in seq(0.7,0.85,by=0.05)){
      cycle_n[i] <- x
      threshold_low[i] <- thresh1
      threshold_high[i] <- thresh2
      n1[i] <- nrow(filter(data_sub[[x]],data_sub[[x]]$posterior <= thresh1))
      n2[i] <- nrow(filter(data_sub[[x]],data_sub[[x]]$posterior <= thresh1,data_sub[[x]]$event==1))
      n3[i] <- nrow(filter(data_sub[[x]],data_sub[[x]]$posterior > thresh1,data_sub[[x]]$posterior <= thresh2))
      n4[i] <- nrow(filter(data_sub[[x]],data_sub[[x]]$posterior > thresh1,data_sub[[x]]$posterior <= thresh2,data_sub[[x]]$event==1))
      n5[i] <- nrow(filter(data_sub[[x]],data_sub[[x]]$posterior > thresh2))
      n6[i] <- nrow(filter(data_sub[[x]],data_sub[[x]]$posterior > thresh2,data_sub[[x]]$event==1))
      p_low[i] <- n2[i]/n1[i]   # 低风险人群中实际发生不良反应的比例
      p_mid[i] <- n4[i]/n3[i]   # 中低风险人群中实际发生不良反应的比例
      p_high[i] <- n6[i]/n5[i]  # 高低风险人群中实际发生不良反应的比例
      p1[i] <- chisq.test(matrix(c(n2[i],n1[i]-n2[i],n4[i],n3[i]-n4[i]),ncol = 2))$p.value  
      p2[i] <- chisq.test(matrix(c(n4[i],n3[i]-n4[i],n6[i],n5[i]-n6[i]),ncol = 2))$p.value  
      data_result<-cbind(cycle_n,threshold_low,threshold_high,n1,n2,n3,n4,n5,n6,p_low,p_mid,p_high,p1,p2)
      i <- i + 1 
    }
  }
}

# write.csv(data_result,file = "/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output/危险分层结果/危险分层结果(高中低危_0.05)_aft_calibration.csv")


######################补充单因素分析表格-20220603
install.packages("lme4")
library(epiDisplay)
library(Matrix)
library(lme4)

data_static <- read.csv("填补之后的_complete.csv")
data_static <- subset(data_static,select = c(patient_sn,visit_year,event,gender:PNI,PS_score_2))

factor_list <- c("event","gender", "previous_radiation","previous_chemotherapy","peptic_ulcer","diabetes",
                 "other_chronic","tumor_stage_pN","tumor_stage_pT","PS_score_2")

for (i in factor_list) {
  data_static[,i] <- as.factor(data_static[,i])
}


data_static_train <- data_static[which(data_static$visit_year <= 2018),]
data_static_test <- data_static[which(data_static$visit_year > 2018),]

des_result_static<-tableStack(vars = gender:PS_score_2,by= event,dataFrame = data_static_train)
write.csv(des_result_static,file = "univariate_static.csv")


####单因素分析
fit <- glm(event ~ PS_score_2 , data=data_static_train,family=binomial)
summary(fit)
result.fit <- cbind(exp(cbind(OR = coef(fit),confint(fit))),
                   pvalue = summary(fit)$coefficients[ ,4])
result.fit

####多因素分析(p<0.2)
fit_multi <- glm(event ~ gender + age_at_diagnosis + weight + tumor_stage_pN + tumor_stage_pT , 
                 data=data_static_train,family=binomial)
summary(fit_multi)
result.fit_multi <- cbind(exp(cbind(OR = coef(fit_multi),confint(fit_multi))),
                    pvalue = summary(fit_multi)$coefficients[ ,4])
result.fit_multi



data_dynamic <- read.csv("data_long.csv")
data_dynamic <- subset(data_dynamic,select = c(patient_sn,GFR:event,visit_year))
data_dynamic$prevention <- as.factor(data_dynamic$prevention)
data_dynamic$event <- as.factor(data_dynamic$event)

data_dynamic_train <- data_dynamic[which(data_dynamic$visit_year <= 2018),]
data_dynamic_test <- data_dynamic[which(data_dynamic$visit_year > 2018),]

des_result_dynamic<-tableStack(vars = GFR:prevention,by= event,dataFrame = data_dynamic_train)
write.csv(des_result_dynamic,file = "univariate_dynamic.csv")

####单因素分析(普通logistic)
fit <- glm(event ~ GFR , data=data_dynamic_train,family=binomial)
summary(fit)
result.fit <- cbind(exp(cbind(OR = coef(fit),confint(fit))),
                    pvalue = summary(fit)$coefficients[ ,4])
result.fit

####多因素分析(p<0.2)
fit_multi <- glm(event ~ PA + ALP + Crea + PLT + Hb + Lymph + WBC + Neut, 
                 data=data_dynamic_train,family=binomial)
summary(fit_multi)
result.fit_multi <- cbind(exp(cbind(OR = coef(fit_multi),confint(fit_multi))),
                          pvalue = summary(fit_multi)$coefficients[ ,4])
result.fit_multi

#######混合效应logistic模型(单因素)
model_mixed <- glmer(event~Neut + (1|patient_sn),
           data = data_dynamic_train, family = binomial)
summary(model_mixed)

#######混合效应logistic模型(多因素-不考虑交互作用)
model_mixed_multi <- glmer(event~PA  + Crea +  PLT + Hb + Lymph + WBC + Neut +  (1|patient_sn) 
                             ,data = data_dynamic_train, family = binomial)
summary(model_mixed_multi)

#######混合效应logistic模型(多因素-考虑交互作用)
model_mixed_multi <- glmer(event~ Crea +  Hb + PA + PLT  + Neut + (1|patient_sn) + 
                              + WBC*Lymph,data = data_dynamic_train, family = binomial)
summary(model_mixed_multi)
