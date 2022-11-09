library(ggplot2)
library(dplyr)
library(InformationValue)
library(glmnet)
library(caret)
library(ROCR)
library(pROC)
library(smotefamily)
library(randomForest)

#===================================================#
#                      DATA                         #
#===================================================#


credit_data = read.csv("credit_data.csv")

creditDataEdit = credit_data %>%
  filter(CARDHLDR == 1, AGE >= 18 & AGE <= 70) %>%
  mutate(
    DEFAULT = as.factor(DEFAULT),
    OWNRENT = as.factor(OWNRENT),
    SELFEMPL = as.factor(SELFEMPL),
    LOGINC = log(INCOME),
    MAJORDRG = as.factor(ifelse(MAJORDRG > 0, 1, 0)),
    MINORDRG = as.factor(ifelse(MINORDRG > 0, 1, 0)),
    LOGINC_DEP = log(INCPER)
  ) %>%
  select(-CARDHLDR, -INCOME, -SPENDING, -INCPER) %>%
  as_tibble()



#=================================================================================#
#       DATA PREPARATION FOR SMOTE TECHNIQUE (turning factor variables to numeric)#
#       AND DATA PARTITIONING                                                     #
#=================================================================================#


mote.data = creditDataEdit %>%
  mutate(
    DEFAULT = as.numeric(DEFAULT),
    MAJORDRG = as.numeric(MAJORDRG),
    MINORDRG = as.numeric(MINORDRG),
    OWNRENT = as.numeric(OWNRENT),
    SELFEMPL = as.numeric(SELFEMPL)
  ) 

   
set.seed(222)

Nsam = nrow(mote.data)
nsam = round(.80*Nsam)

trainingIndex = sample(Nsam, nsam)
train_credit2 = mote.data[trainingIndex, ]

#balancing the defaults thorugh SMOTE
smote_train_credit2 = SMOTE(train_credit2[,-1], train_credit2[,1])

#balanced training data set
smote_train_credit2 = smote_train_credit2$data %>%
                        as_tibble() %>%
                        rename(DEFAULT = class)

#===========================================================#
#                        THE LASSO                          #
#===========================================================#

#balanced design matrix
trainDesignMatrix = model.matrix(DEFAULT ~. , smote_train_credit2)


trainClass = as.numeric(smote_train_credit2$DEFAULT) - 1

## TESTING DATA SET
test_credit2 = mote.data[-trainingIndex, ]
testDesignMatrix = model.matrix(DEFAULT ~. , test_credit2)
testClass = as.numeric(test_credit2$DEFAULT) - 1

set.seed(222)
smote.cv.lasso = cv.glmnet(trainDesignMatrix, trainClass,
                     alpha = 1, type = "class")


plot(smote.cv.lasso)

lassoModel = glmnet(trainDesignMatrix, trainClass, alpha = 1,
           family = "binomial", lambda = smote.cv.lasso$lambda.1se,
           intercept = T)

smote.cv.lasso$lambda.1se #(THIS IS THE VALUE OF LAMBDA, THE TUNING PARAMETER)

coef(lassoModel)


################### LASSO MODEL VALIDATION ##################


test_prob = predict(lassoModel, newx = testDesignMatrix,
              s = smote.cv.lasso$lambda.1se, type = 'response')

auc = roc(testClass, test_prob)


plot(auc, ylim = c(0, 1), print.thres = TRUE)
print(auc)


lasso.pred = rep("1", nrow(test_credit2))

lasso.pred[test_prob > .488] = "2"


#crossTab = table(predicted = lasso.pred, observed = test_credit2$DEFAULT)
#crossTab
confusionMatrix(crossTab, mode = "everything", positive = '2')


#============================================================================#
#:::::                 R A N D O M       F O R E S T S                  :::::#
#============================================================================#


train_credit2 = train_credit2%>%
  mutate(DEFAULT = factor(DEFAULT, labels = c('0', '1')))


test_credit2 = test_credit2 %>%
  mutate(DEFAULT = factor(DEFAULT, labels = c('0', '1')))

set.seed(2222)
RFmodel = randomForest(DEFAULT ~ .,
             data = train_credit2, mtry = 4, importance = T)
RFmodel

RFprediction = predict(RFmodel, test_credit2)

auc(test_credit2$DEFAULT, as.numeric(RFprediction))

confusionMatrix(RFprediction, test_credit2$DEFAULT, positive = '1')


imp = importance(RFmodel)

var2 = c('Age', 'Length of stay at current address', 'No. of dependents', 'Major Derogatory',
         'Minor Derogatory', 'Home Ownership', 'Employment',
         'Expenditure to Income', 'Log Spending', 'Log Income', 'Log Income per dependent')
varimp = data.frame(var2, mean_gini = imp[,4])

ggplot(varimp) + geom_bar(aes(x=reorder(var2, - mean_gini), y = mean_gini), stat = 'identity', 
                          colour = 'Black', fill = 'red', width = .5) + labs(title = "Variable Importance",
                                                                             x = 'Variable', y = 'Mean decrease in Gini')  + theme_minimal() + coord_flip() 









