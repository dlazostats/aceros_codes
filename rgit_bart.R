# BART  - Compelte Guide
#=======================
library(dplyr)
library(tidymodels)
library(caret)
library(dbarts)
library(ppsr)
library(bartMachine)
library(MASS)

## Working directory
script_name <- 'bart1.R'
ruta <- gsub(rstudioapi::getActiveDocumentContext()$path,pattern = script_name,replacement = '')
setwd(ruta)

#Loading data and assesing predictive power
ppsr::score_predictors(df = Boston, y = 'medv') %>% arrange(desc(pps))
dbml<-Boston %>% dplyr::select(-chas)

# Train/test set
set.seed(2134)
split<-initial_split(dbml,prop=0.8,strata=medv)
train<-training(split)
test<-testing(split)
dim(train);dim(test)

# ML Models
#-----------
## Random Forest
#-----------------
fitcontrol<-trainControl(method="repeatedcv",repeats = 3,number=3)
rf_ml<-train(medv~.,
             data=train,
             method="ranger",
             trControl=fitcontrol)
#train error
mean(rf_ml$resample$RMSE)
mean(rf_ml$resample$Rsquared)

## test error
p_rf<-predict(rf_ml,test)   
postResample(p_rf,test$medv)

## BART
## caret / bartmachine
set.seed(3123)
bartGrid <- expand.grid(num_trees = c(100,150,200),beta = c(2,3), 
                        k = c(2,3),alpha=c(0.90,0.95), nu = 3)
bart_caret<-train(medv ~.,
                  data=train,
                  method="bartMachine",
                  tuneGrid = bartGrid,
                  trControl=fitcontrol)
bart_caret$bestTune

#train error
mean(bart_caret$resample$RMSE)
mean(bart_caret$resample$Rsquared)

## test error
p_b1<-predict(bart_caret,test)   
postResample(p_b1,test$medv)

## credible intervals
Xt<-train %>% dplyr::select(-medv)
yt<-train %>% dplyr::select(medv)
bartmch<-bartMachine(Xt,yt,num_trees=20,)


## DBARTS
## falta completar esto 


