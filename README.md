Practical-Machine-Learning_Assignment
=====================================
########TRAINING
  ##reading dataset
    df= read.csv ('pml-training.csv', header = T)

  ##removing first 7 columns(this columns don't contain prediction important values)
    df[1:7] = list(NULL)

  ##cleaning dataset
      ##putting NA value on no values places (because of analyzes)
          nd=df
          nd[ nd == '' | nd == 'NA'] = NA
      ##removing columns with NA values (because of analyzes)
          i <- sapply(nd,function(x)any(is.na(x))) 
          nd1<-nd[,!(i)] 

  ## dimensions of training dataset
    dim(nd1)
        ##result## [1] 19622    53

  ##creating training and testing set on training dataset 
    inTrain = createDataPartition( y =nd1$classe, p=0.7,list=FALSE)
    training = nd1[inTrain,]
    testing = nd1[-inTrain,]

  ##training model with random forest method (random forest is chosen because of big dataset and no 
                                              specific trend between variables )
    modFit <- train(classe~., data=training, method = "rf", tuneLength = 1, ntree = 25)
  
  ##printing train model results (shows number of samples, predictors and other training model parameters )
    print(modFit)
        ##result## Random Forest
                  13737 samples
                  52 predictor
                  5 classes: 'A', 'B', 'C', 'D', 'E' 
                  No pre-processing
                  Resampling: Bootstrapped (25 reps) 
                  Summary of sample sizes: 13737, 13737, 13737, 13737, 13737, 13737, ... 
                  Resampling results
                  Accuracy  Kappa  Accuracy SD  Kappa SD
                  0.988     0.985  0.0019       0.00241 
                  Tuning parameter 'mtry' was held constant at a value of 7
 

  ##validation of model (confusion matrix shows results of predicting on test dataset(known prediction classes) in which 
                      we can see number of true predictions, important accurancy of our prediction and confidence interval)
    confusionMatrix(predict(modFit, testing), testing$classe)
        ##result##Confusion Matrix and Statistics
                            Reference
                  Prediction    A    B    C    D    E
                            A 1674   14    0    0    0
                            B    0 1120   13    0    0
                            C    0    4 1012   12    1
                            D    0    0    1  949    5
                            E    0    1    0    3 1076

                  Overall Statistics
                             Accuracy : 0.9908
                             95% CI : (0.988, 0.9931)
                             No Information Rate : 0.2845
                             P-Value [Acc > NIR] : < 2.2e-16
                             Kappa : 0.9884
                             Mcnemar's Test P-Value : NA             

                  Statistics by Class:
                            Class: A Class: B Class: C Class: D Class: E
                            Sensitivity            1.0000   0.9833   0.9864   0.9844   0.9945
                            Specificity            0.9967   0.9973   0.9965   0.9988   0.9992
                            Pos Pred Value         0.9917   0.9885   0.9835   0.9937   0.9963
                            Neg Pred Value         1.0000   0.9960   0.9971   0.9970   0.9988
                            Prevalence             0.2845   0.1935   0.1743   0.1638   0.1839
                            Detection Rate         0.2845   0.1903   0.1720   0.1613   0.1828
                            Detection Prevalence   0.2868   0.1925   0.1749   0.1623   0.1835
                            Balanced Accuracy      0.9983   0.9903   0.9914   0.9916   0.9968

#######TESTING
  ##reading test dataset
    testingset= read.csv ('pml-testing.csv', header = T)

  ##cleaning test dataset
    ts=testingset
    ts[ ts == '' | ts == 'NA'] = NA
    i1 <- sapply(ts,function(x)any(is.na(x))) 
    ts1<-ts[,!(i1)] 
    
  ##predicting classe level (prediction for 20 observations of their classe level value )
    modPred<-predict(modFit, ts1)
    print(modPred)
        ##result##  [1] B A B A A E D B A A B C B A E E A B B B
                    Levels: A B C D E
