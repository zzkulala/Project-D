library(stringr)
library(tidyquant)
library(RTextTools)
library(tm)
library(SnowballC)
library(broom)
library(pdftools)
library(tau)
library(wordcloud)
library(caret)
library(xtable)
options(xtable.floating = FALSE)
options(xtable.timestamp = "")

RCV1 <- read.csv("~/Desktop/ProjectD_data.csv",header = TRUE)
RCV1 <- data.frame(RCV1)
RCV1$X <- NULL


#Part 1(a) #only tried 50000 data points
doc_matrix <- create_matrix(RCV1[1:50000,'contents'], language="english", removeNumbers=TRUE,
                            stemWords=TRUE, removeSparseTerms=.99)
# 2.Creating a container
container <- create_container(doc_matrix, as.numeric(factor(RCV1[1:50000,'h1'])), trainSize=1:40000,
                              testSize=40001:50000, virgin=FALSE)


#MAXENT
MAXENT <- train_model(container,"MAXENT")
MAXENT_CLASSIFYL_998 <- classify_model(container, MAXENT)
MAXENT_analyticsL_998 <- create_analytics(container,
                                          cbind(MAXENT_CLASSIFYL_998))
summary(MAXENT_analyticsL_998)

#GLMNET
GLMNET <- train_model(container,"GLMNET")
GLMNET_CLASSIFYL_998 <- classify_model(container, GLMNET)
GLMNET_analyticsL_998 <- create_analytics(container,
                                          cbind(GLMNET_CLASSIFYL_998))
summary(GLMNET_analyticsL_998)

#SVM
SVM <- train_model(container,"SVM")
SVM_CLASSIFYL_998 <- classify_model(container, SVM)
SVM_analyticsL_998 <- create_analytics(container,
                                       cbind(SVM_CLASSIFYL_998))
summary(SVM_analyticsL_998)
#SVM <- cross_validate(container, 4, "SVM")

#Random Forest
RF <- train_model(container,"RF")
RF_CLASSIFYL_998 <- classify_model(container, RF)
RF_analyticsL_998 <- create_analytics(container,
                                      cbind(RF_CLASSIFYL_998))
summary(RF_analyticsL_998)



analytics_98 <- create_analytics(container,
                                 cbind(SVM_CLASSIFYL_998, 
                                       RF_CLASSIFYL_998, 
                                       GLMNET_CLASSIFYL_998,
                                       MAXENT_CLASSIFYL_998))
summary(analytics_98)

b <- RF_CLASSIFYL_998$FORESTS_LABEL
levels(b) <- levels(as.numeric(factor(RCV1[23150:35000,'h1'])))
CM_0 = confusionMatrix(b, as.factor(as.numeric(RCV1[23150:35000,'h1'])))


#Part 1(a) #only tried 50000 data points
doc_matrix1 <- create_matrix(RCV1[1:50000,'contents'], language="english", removeNumbers=TRUE,
                             stemWords=TRUE, removeSparseTerms=.99)
# 2.Creating a container
container1 <- create_container(doc_matrix1, as.numeric(factor(RCV1[1:50000,'topic'])), trainSize=1:40000,
                               testSize=40001:50000, virgin=FALSE)

SVM1 <- train_model(container1,"SVM")
SVM_CLASSIFY1 <- classify_model(container1, SVM1)
SVM_analytics1 <- create_analytics(container1,
                                   cbind(SVM_CLASSIFY1))
summary(SVM_analytics1)
#topic_summary <- analytics@label_summary
#alg_summary <- analytics@algorithm_summary
#ens_summary <-analytics@ensemble_summary
#doc_summary <- analytics@document_summary
# 6.Cross-Validation
#SVM <- cross_validate(container, 4, "SVM")

#Random Forest
RF1 <- train_model(container1,"RF")
RF_CLASSIFY1 <- classify_model(container1, RF1)
RF_analytics1 <- create_analytics(container1,
                                  cbind(RF_CLASSIFY1))
summary(RF_analytics1)

#MAXENT
MAXENT1 <- train_model(container1,"MAXENT")
MAXENT_CLASSIFY1 <- classify_model(container1, MAXENT1)
MAXENT_analytics1 <- create_analytics(container1,
                                      cbind(MAXENT_CLASSIFY1))
summary(MAXENT_analytics1)

#GLMNET
GLMNET1 <- train_model(container1,"GLMNET")
GLMNET_CLASSIFY1 <- classify_model(container1, GLMNET1)
GLMNET_analytics1 <- create_analytics(container1,
                                      cbind(GLMNET_CLASSIFY1))
summary(GLMNET_analytics1)

analytics1L_80 <- create_analytics(container1,
                                   cbind(SVM_CLASSIFY1, 
                                         RF_CLASSIFY1, 
                                         GLMNET_CLASSIFY1,
                                         MAXENT_CLASSIFY1))
summary(analytics1L_80)

SVM2 <- cross_validate(container1, 4, "SVM")