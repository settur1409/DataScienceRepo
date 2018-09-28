#Loading Neccessary libraries

library(kernlab)
library(readr)
library(caret)
library(e1071)

#Loading Data
Data <- read.csv("mnist_train.csv")
test <- read.csv("mnist_test.csv")

## Observed discripency in colname of train and test
col_train = colnames(Data)
colnames(test) = c(col_train)

summary(Data)

sapply(Data, function(x) sum(is.na(x)))

# Split the data into train and test setnd considering only 10% of data
# for training and also for crossfold validation
set.seed(1)
indices = sample(1:nrow(Data), 0.9*nrow(Data))
validationdata = Data[-indices,]

##Performing scaling by excluding the digit column
scaled_vals = validationdata[,-1]/255
scaled_vals$letters = as.factor(validationdata$X5)

##Performing scaling by excluding the digit column
test_scaled = test[-1]/255
test_scaled$X5 = test$X5
test_scaled$letters = as.factor(test$X5)

## Further reducing the data to avoid modelling time
compresseddata = scaled_vals[1:1000,]

trainControl <- trainControl(method="cv", number=5)
# Number - Number of folds 
# Method - cross validation

metric <- "Accuracy"

set.seed(100)

# making a grid of C values. 
grid <- expand.grid(.C=seq(1,10, by=2))

# Performing 5-fold cross validation
fit.svm <- train(letters~., data=compresseddata, method="svmLinear", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

print(fit.svm$results)
#C  Accuracy     Kappa AccuracySD    KappaSD
#1 1 0.8789711 0.8653214 0.02788966 0.03106136
#2 3 0.8789711 0.8653214 0.02788966 0.03106136
#3 5 0.8789711 0.8653214 0.02788966 0.03106136
#4 7 0.8789711 0.8653214 0.02788966 0.03106136
#5 9 0.8789711 0.8653214 0.02788966 0.03106136

plot(fit.svm)
evaluate_linear_test<- predict(fit.svm, test_scaled)
confusionMatrix(evaluate_linear_test, test_scaled$letters)

### Overall Statistics ###
#Accuracy : 0.8896          
#95% CI : (0.8833, 0.8957)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16    

##############################
####### RBF METHOD ###########
##############################

#fit.svm_rbf <- ksvm(letters ~ ., data = compresseddata,scale = FALSE,kernel = "rbfdot", 
#                    C=seq(1,10,by=2), epsilon=seq(0,1,by=0.25))

trainControl_rbf <- trainControl(method="cv", number=5)
metric <- "Accuracy"
## Can tune the below hyper parameters for better accuracy
grid_rbf <- expand.grid(.sigma=seq(0,1,by=0.25), .C=seq(1,10,by=2))

# Performing 5-fold cross validation
fit.svm_rbf <- train(letters~., data=compresseddata, method="svmRadial", metric=metric, 
                 tuneGrid=grid_rbf, trControl=trainControl_rbf)
print(fit.svm_rbf$results)

#sigma C  Accuracy       Kappa  AccuracySD     KappaSD
#1   0.00 1 0.1289963 0.000000000 0.001903559 0.000000000
#2   0.00 3 0.1289963 0.000000000 0.001903559 0.000000000
#3   0.00 5 0.1289963 0.000000000 0.001903559 0.000000000
#4   0.00 7 0.1289963 0.000000000 0.001903559 0.000000000
#5   0.00 9 0.1289963 0.000000000 0.001903559 0.000000000
#6   0.25 1 0.1760118 0.055902852 0.008498797 0.010848030
#7   0.25 3 0.1830118 0.064216855 0.006108816 0.007879962
#8   0.25 5 0.1830118 0.064216855 0.006108816 0.007879962
#9   0.25 7 0.1830118 0.064216855 0.006108816 0.007879962
#10  0.25 9 0.1830118 0.064216855 0.006108816 0.007879962
#11  0.50 1 0.1310013 0.002384744 0.002282107 0.003265460
#12  0.50 3 0.1360014 0.008340587 0.004210042 0.005341405
#13  0.50 5 0.1360014 0.008340587 0.004210042 0.005341405
#14  0.50 7 0.1360014 0.008340587 0.004210042 0.005341405
#15  0.50 9 0.1360014 0.008340587 0.004210042 0.005341405
#16  0.75 1 0.1289963 0.000000000 0.001903559 0.000000000
#17  0.75 3 0.1289963 0.000000000 0.001903559 0.000000000
#18  0.75 5 0.1289963 0.000000000 0.001903559 0.000000000
#19  0.75 7 0.1289963 0.000000000 0.001903559 0.000000000
#20  0.75 9 0.1289963 0.000000000 0.001903559 0.000000000
#21  1.00 1 0.1289963 0.000000000 0.001903559 0.000000000
#22  1.00 3 0.1289963 0.000000000 0.001903559 0.000000000
#23  1.00 5 0.1289963 0.000000000 0.001903559 0.000000000
#24  1.00 7 0.1289963 0.000000000 0.001903559 0.000000000
#25  1.00 9 0.1289963 0.000000000 0.001903559 0.000000000


plot(fit.svm_rbf)

evaluate_rbf_test<- predict(fit.svm_rbf, test_scaled)
confusionMatrix(evaluate_rbf_test, test_scaled$letters)

## Overall Statistics ########
#Accuracy : 0.1769          
#95% CI : (0.1695, 0.1845)
#No Information Rate : 0.1135          
#P-Value [Acc > NIR] : < 2.2e-16       
