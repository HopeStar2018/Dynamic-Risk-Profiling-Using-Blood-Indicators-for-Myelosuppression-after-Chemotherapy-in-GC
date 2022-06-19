setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/smote")
library(pROC)

############# calculate AUCs of bayes model (static and dynamic variables) for different cycle (smo-data)
setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output/isotonic")
data_test_smo_isotonic <- read.csv("testset_dynamic_plus_static_bayes_cycle_8_prediction_result.csv")
# data_test_smo_isotonic <- read.csv("testset_dynamic_plus_static_bayes_cycle_4_prediction_result.csv")

# auc for all cycyles
roc <-roc(as.factor(data_test_smo_isotonic$event),data_test_smo_isotonic$posterior,ci=TRUE,auc = T)

# auc for each cycyle
data_test_smo_isotonic_sub <- c()
roc <- c()
auc <- c()
ci_lower <- c()
ci_upper <- c()

i <- 1
for (i in 1:8) {
  data_test_smo_isotonic_sub <-data_test_smo_isotonic[which(data_test_smo_isotonic$cycle == i),]
  roc <-roc(as.factor(data_test_smo_isotonic_sub$event),data_test_smo_isotonic_sub$posterior,ci=TRUE,auc = T)
  auc[i] <- roc$auc
  ci_lower[i] <- roc$ci[1]
  ci_upper[i] <- roc$ci[3]
  auc_smo_aft_isotonic <- cbind(auc, ci_lower, ci_upper)
  i <- i + 1
}

write.csv(auc_smo_aft_isotonic,file = "auc_reslt_smo_by_cycle_aft_isotonic.csv")


#############  calculate AUCs of bayes model  (static and dynamic variables) for different cycle (raw data)
setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output")
data_test_raw_isotonic <- read.csv("smote_model_on_raw_testset_with_prediction_isotonic.csv")
# data_test_raw_isotonic <-data_test_raw_isotonic[which(data_test_raw_isotonic$cycle <= 4),]

# auc for all cycyles
roc <-roc(as.factor(data_test_raw_isotonic$event),data_test_raw_isotonic$posterior,ci=TRUE,auc = T)

# auc for each cycyle
data_test_raw_isotonic_sub <- c()
roc <- c()
auc <- c()
ci_lower <- c()
ci_upper <- c()

i <- 1
for (i in c(1,2,3,5,6,8)) {
  ### there is no outcome in the fourth or seventh cycle
  data_test_raw_isotonic_sub <-data_test_raw_isotonic[which(data_test_raw_isotonic$cycle == i),]
  roc <-roc(as.factor(data_test_raw_isotonic_sub$event),data_test_raw_isotonic_sub$posterior,ci=TRUE,auc = T)
  auc[i] <- roc$auc
  ci_lower[i] <- roc$ci[1]
  ci_upper[i] <- roc$ci[3]
  auc_raw_aft_isotonic <- cbind(auc, ci_lower, ci_upper)
  i <- i + 1
}

write.csv(auc_raw_aft_isotonic,file = "auc_reslt_raw_by_cycle_aft_isotonic.csv")

############################################################################################################################################
############### calculate 95% confidence interval of AUC for bayes models with selected variables
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


#############calculate AUCs of logistic regression model for different cycle (smo-data)
setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output")
data_test_smo <- read.csv("log_model_on_smo_testset_with_prediction_new.csv")
# data_test_smo <-data_test_smo[which(data_test_smo$cycle <= 4),]

# auc for all cycyles
roc <-roc(as.factor(data_test_smo$event),data_test_smo$predict,levels = c(0,1), direction = "<", ci=TRUE,auc = T)

# auc for each cycyle
data_test_smo_sub <- c()
roc <- c()
auc <- c()
ci_lower <- c()
ci_upper <- c()

i <- 1
for (i in 1:8) {
  data_test_smo_sub <-data_test_smo[which(data_test_smo$cycle == i),]
  roc <-roc(as.factor(data_test_smo_sub$event),data_test_smo_sub$predict,ci=TRUE,auc = T)
  auc[i] <- roc$auc
  ci_lower[i] <- roc$ci[1]
  ci_upper[i] <- roc$ci[3]
  auc_smo_log <- cbind(auc, ci_lower, ci_upper)
  i <- i + 1
}

write.csv(auc_smo_log,file = "log_auc_reslt_smo_by_cycle_new.csv")


##  calculate AUCs of logistic regression model for different cycle (raw data)
setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output")
data_test <- read.csv("log_model_on_raw_testset_with_prediction_new.csv")
# data_test <-data_test[which(data_test$cycle <= 4),]

# auc for all cycyles
roc <-roc(as.factor(data_test$event),data_test$predict,levels = c(0,1), direction = "<",ci=TRUE,auc = T)

# auc for each cycyle
data_test_sub <- c()
roc <- c()
auc <- c()
ci_lower <- c()
ci_upper <- c()

i <- 1
for (i in c(1,2,3,5,6,8)) {
  ### there is no outcome in the fourth or seventh cycle
  data_test_sub <-data_test[which(data_test$cycle == i),]
  roc <-roc(as.factor(data_test_sub$event),data_test_sub$predict,levels = c(0,1), direction = "<",ci=TRUE,auc = T)
  auc[i] <- roc$auc
  ci_lower[i] <- roc$ci[1]
  ci_upper[i] <- roc$ci[3]
  auc_log <- cbind(auc, ci_lower, ci_upper)
  i <- i + 1
}

write.csv(auc_log,file = "log_auc_reslt_raw_by_cycle_new.csv")


