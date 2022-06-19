## variable selection

# install.packages("epiDisplay")
# install.packages("Matrix")
# install.packages("lme4")

library(epiDisplay)
library(Matrix)
library(lme4)

setwd("/Users/weiyuna/Desktop/工作/合作项目/医大一-胃癌/模型/code_202203/output")
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


## univariate analysis (static features)
fit <- glm(event ~ PS_score_2 , data=data_static_train,family=binomial)
summary(fit)
result.fit <- cbind(exp(cbind(OR = coef(fit),confint(fit))),
                    pvalue = summary(fit)$coefficients[ ,4])
result.fit

## multivariate analysis (static features)
fit_multi <- glm(event ~ gender + age_at_diagnosis + weight + tumor_stage_pN + tumor_stage_pT , 
                 data=data_static_train,family=binomial)
summary(fit_multi)
result.fit_multi <- cbind(exp(cbind(OR = coef(fit_multi),confint(fit_multi))),
                          pvalue = summary(fit_multi)$coefficients[ ,4])
result.fit_multi


## dynamic feature selection
data_dynamic <- read.csv("data_long.csv")
data_dynamic <- subset(data_dynamic,select = c(patient_sn,GFR:event,visit_year))
data_dynamic$prevention <- as.factor(data_dynamic$prevention)
data_dynamic$event <- as.factor(data_dynamic$event)

data_dynamic_train <- data_dynamic[which(data_dynamic$visit_year <= 2018),]
data_dynamic_test <- data_dynamic[which(data_dynamic$visit_year > 2018),]

des_result_dynamic<-tableStack(vars = GFR:prevention,by= event,dataFrame = data_dynamic_train)
write.csv(des_result_dynamic,file = "univariate_dynamic.csv")

## univariate analysis (logistic regression)
fit <- glm(event ~ GFR , data=data_dynamic_train,family=binomial)
summary(fit)
result.fit <- cbind(exp(cbind(OR = coef(fit),confint(fit))),
                    pvalue = summary(fit)$coefficients[ ,4])
result.fit

## multivariate analysis (logistic regression)
fit_multi <- glm(event ~ PA + ALP + Crea + PLT + Hb + Lymph + WBC + Neut, 
                 data=data_dynamic_train,family=binomial)
summary(fit_multi)
result.fit_multi <- cbind(exp(cbind(OR = coef(fit_multi),confint(fit_multi))),
                          pvalue = summary(fit_multi)$coefficients[ ,4])
result.fit_multi


## univariate analysis (mixed-effect logistic regression)
model_mixed <- glmer(event~Neut + (1|patient_sn),
                     data = data_dynamic_train, family = binomial)
summary(model_mixed)

## multivariate analysis (mixed-effect logistic regression regardless of interaction effect)
model_mixed_multi <- glmer(event~PA  + Crea +  PLT + Hb + Lymph + WBC + Neut +  (1|patient_sn) 
                           ,data = data_dynamic_train, family = binomial)
summary(model_mixed_multi)

## multivariate analysis (mixed-effect logistic regression considering interaction effect)
model_mixed_multi <- glmer(event~ Crea +  Hb + PA + PLT  + Neut + (1|patient_sn) + 
                             + WBC*Lymph,data = data_dynamic_train, family = binomial)
summary(model_mixed_multi)
