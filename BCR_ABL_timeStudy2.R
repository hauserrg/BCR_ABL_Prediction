#shared library
source('C:/Users/George/Desktop/BCR_ABL/Analysis/BCR_ABL_SharedLibrary.R', echo=TRUE)
source('C:/Users/George/Desktop/BCR_ABL/Analysis/SharedLibrary.R', echo=TRUE)
#library(doParallel)

di = list(
  paths = c("C:\\Users\\George\\Desktop\\BCR_ABL\\Data\\bcrabl_t0.txt","C:\\Users\\George\\Desktop\\BCR_ABL\\Data\\bcrabl_6mo.txt","C:\\Users\\George\\Desktop\\BCR_ABL\\Data\\bcrabl_1yr.txt","C:\\Users\\George\\Desktop\\BCR_ABL\\Data\\bcrabl_2yr.txt","C:\\Users\\George\\Desktop\\BCR_ABL\\Data\\bcrabl_3yr.txt","C:\\Users\\George\\Desktop\\BCR_ABL\\Data\\bcrabl_4yr.txt","C:\\Users\\George\\Desktop\\BCR_ABL\\Data\\bcrabl_5yr.txt"),
  pathsPrint = c("Time = 0","6 months","1 year","2 years","3 years","4 years","5 years"),
  missing = c(10, 31, 35, 42, 48, 52, 56), #manually determined based on review of baso_pct_maxlab1yr for missing data in each dataset
  stratifyVars = c("PatId","BaselineId","Sta3nId"),
  reps = 10,
  #dfs #original dataframes loaded from file
  dfs_missingRemoved = list(),
  #dfs_trainTest #dfs_missingRemoved with train/test split
  xgb.maxVarsPerRun = 10, #number of runs with different numbers of variables per dataset
  includeOnlyPatientsInAllTimeSeries = T
)
#di

##load data into di
di$dfs = loadAllData(di, loadData)

#remove missing cases at 'missing' thresholds specified
patientCounts.initial = rep(-1, length(di$paths))
patientCounts.afterMissingRemoved = rep(-1, length(di$paths))
variableCounts.initial = rep(-1, length(di$paths))
variableCounts.afterMissingRemoved = rep(-1, length(di$paths))
for (i in 1:length(di$paths)) {
  patientCounts.initial = dim(di$dfs[[i]])[1]
  variableCounts.initial = dim(di$dfs[[i]])[2] - 1 - length(di$stratifyVars) #remove 1 variable for $Truth and the stratifying variables
  di$dfs_missingRemoved[[i]] = sl_missing_subset(di$dfs[[i]], di$missing[i])$complete
  patientCounts.afterMissingRemoved[i] = dim(di$dfs_missingRemoved[[i]])[1]
  variableCounts.afterMissingRemoved[i] = dim(di$dfs_missingRemoved[[i]])[2] - 1 - length(di$stratifyVars) #remove 1 variable for $Truth and the stratifying variables
}

#optional: remove patients not in all datasets
patientCounts.afterRemovalOfPatientsNotInAllDatasets = rep(-1, length(di$paths))
if(di$includeOnlyPatientsInAllTimeSeries){
  patientsInEveryDataset = di$dfs_missingRemoved[[1]]$PatId
  for (i in 2:length(di$paths)) {
    patientsInEveryDataset = intersect(patientsInEveryDataset, di$dfs_missingRemoved[[i]]$PatId)
  }
  for (i in 1:length(di$paths)) {
    di$dfs_missingRemoved[[i]] = di$dfs_missingRemoved[[i]][di$dfs_missingRemoved[[i]]$PatId %in% patientsInEveryDataset, ]
    patientCounts.afterRemovalOfPatientsNotInAllDatasets = dim(di$dfs_missingRemoved[[i]])[1]
  }
}

#optional: remove variables not in all datasets
variablesTemp = c()
for (i in 1:length(di$paths)) {
  variablesTemp = c(variablesTemp, colnames(di$dfs_missingRemoved[[i]]))
}
variablesToInclude = data.frame(table(variablesTemp))
variablesToInclude = variablesToInclude[variablesToInclude$Freq == 7,]
variableCounts.afterRemovalOfPatientsNotInAllDatasets = rep(-1, length(di$paths))
for (i in 1:length(di$paths)) {
  di$dfs_missingRemoved[[i]] = di$dfs_missingRemoved[[i]][, names(di$dfs_missingRemoved[[i]]) %in% variablesToInclude$variablesTemp]
  variableCounts.afterRemovalOfPatientsNotInAllDatasets = dim(di$dfs_missingRemoved[[i]])[2] - 1 - length(di$stratifyVars) #remove 1 variable for $Truth and the stratifying variables
}

#review pos/neg distribution after missing data removed
sl_missPosNeg = function(di){
  missPosNeg = data.frame(
    di$pathsPrint,
    pos = rep(-1, length(di$paths)),
    neg = rep(-1, length(di$paths)),
    total = rep(-1, length(di$paths)),
    posPct = rep(-1, length(di$paths))
  )
  for (i in 1:length(di$paths)) {
    truth = di$dfs_missingRemoved[[i]]$Truth
    missPosNeg$pos[i] = sum(truth == "1")
    missPosNeg$neg[i] = sum(truth == "0")
    missPosNeg$total[i] = length(truth)
  }
  missPosNeg$posPct = missPosNeg$pos/missPosNeg$total*100
  return(missPosNeg)
}
di$output_missPosNeg = sl_missPosNeg(di)


##split the data
#option 1: split by patient
for(i in 1:length(di$dfs_missingRemoved)){
  di$trainTest[[i]] = sl_splitData_Truth01(di$dfs_missingRemoved[[i]], di$stratifyVars) #$train.data, $test.data
  #di$trainTest[[i]] = sl_splitData_ByBaselineId(di$dfs_missingRemoved[[i]], di$stratifyVars) #$train.data, $test.data <- sensitivity analysis
  #variableCounts.afterRemovalOfStratifyingVars = dim(di$trainTest[[i]]$train.data)[2] - 1
}

di$patientCounts = data.frame(pat.initial = patientCounts.initial,
                              pat.afterMissingRemoved = patientCounts.afterMissingRemoved,
                              pat.afterRemovalOfPatientsNotInAllDatasets = patientCounts.afterRemovalOfPatientsNotInAllDatasets,
                              pat.removedForMissing = patientCounts.initial-patientCounts.afterMissingRemoved,
                              pat.removedForNotInAllDatasets = patientCounts.afterMissingRemoved-patientCounts.afterRemovalOfPatientsNotInAllDatasets,
                              var.initial = variableCounts.initial,
                              var.afterMissingRemoved = variableCounts.afterMissingRemoved,
                              var.removedForMissing = variableCounts.initial-variableCounts.afterMissingRemoved,
                              var.afterRemovalOfVariablesNotInAllDatasets = variableCounts.afterRemovalOfPatientsNotInAllDatasets,
                              var.afterRemovalOfStratifyingVars = variableCounts.afterRemovalOfStratifyingVars)


#add models - full model
for(i in 1:length(di$trainTest)){
  modelNames = !names(di$trainTest[[i]]$train.data) %in% "Truth"
  di$models[[i]] = sl_createModelFormula("Truth", names(di$trainTest[[i]]$train.data)[modelNames])
}

#add models - CBC only
cbcNames = c()
cbc = c("RBC", "WBC", "Platelet", "Hematocrit", "Hemoglobin")
for(m in names(di$trainTest[[i]]$train.data)){
  for(n in cbc){
    if(startsWith(m, n)){
      cbcNames = c(cbcNames, m)
      break
    }
  }
}
for(i in 1:length(di$trainTest)){
  di$models[[i]] = sl_createModelFormula("Truth", cbcNames)
}

#add models - basophil only
basoNames = c()
cbc = c("Baso")
for(m in names(di$trainTest[[i]]$train.data)){
  for(n in cbc){
    if(startsWith(m, n)){
      basoNames = c(basoNames, m)
      break
    }
  }
}
for(i in 1:length(di$trainTest)){
  # di$models[[i]] = sl_createModelFormula("Truth", basoNames)
  di$models[[i]] = sl_createModelFormula("Truth", "Baso_pct_MinLab1yr")
}

#LASSO - setup train and test data
for(i in 1:length(di$trainTest)){
  di$trainTestLasso[[i]] = list()
  di$trainTestLasso[[i]]$train.data.x = model.matrix(di$models[[i]], di$trainTest[[i]]$train.data) #will drop rows with an NA
  di$trainTestLasso[[i]]$train.data.y = ifelse(di$trainTest[[i]]$train.data$Truth=="1",1,0) #convert class to numerical variable
  di$trainTestLasso[[i]]$test.data.x  = model.matrix(di$models[[i]], di$trainTest[[i]]$test.data) #will drop rows with an NA
  di$trainTestLasso[[i]]$test.data.y  = ifelse(di$trainTest[[i]]$test.data$Truth=="1",1,0) #convert class to numerical variable
}

#QC: count dfs
di$output_counts = data.frame(
  rows_fromFile = rep(-1, length(di$dfs)),
  rows_afterMissingRemoved = rep(-1, length(di$dfs)),
  rows_train = rep(-1, length(di$trainTest)),
  rows_test = rep(-1, length(di$trainTest)),
  rows_trainLasso = rep(-1, length(di$trainTest)),
  rows_testLasso = rep(-1, length(di$trainTest)),
  columns_fromFile = rep(-1, length(di$dfs)),
  columns_afterMissingRemoved = rep(-1, length(di$dfs)),
  columns_train = rep(-1, length(di$trainTest)),
  columns_test = rep(-1, length(di$trainTest)),
  columns_trainLasso = rep(-1, length(di$trainTest)),
  columns_testLasso = rep(-1, length(di$trainTest))
)
for(i in 1:length(di$dfs)){
  di$output_counts$rows_fromFile[i] = dim(di$dfs[[i]])[1]
  di$output_counts$rows_afterMissingRemoved[i] = dim(di$dfs_missingRemoved[[i]])[1]
  di$output_counts$rows_train[i] = dim(di$trainTest[[i]]$train.data)[1]
  di$output_counts$rows_test[i] = dim(di$trainTest[[i]]$test.data)[1]
  di$output_counts$rows_trainLasso[i] = dim(di$trainTestLasso[[i]]$train.data.x)[1]
  di$output_counts$rows_testLasso[i] = dim(di$trainTestLasso[[i]]$test.data.x)[1]
  
  di$output_counts$columns_fromFile[i] = dim(di$dfs[[i]])[2]
  di$output_counts$columns_afterMissingRemoved[i] = dim(di$dfs_missingRemoved[[i]])[2]
  di$output_counts$columns_train[i] = dim(di$trainTest[[i]]$train.data)[2]
  di$output_counts$columns_test[i] = dim(di$trainTest[[i]]$test.data)[2]
  di$output_counts$columns_trainLasso[i] = dim(di$trainTestLasso[[i]]$train.data.x)[2]
  di$output_counts$columns_testLasso[i] = dim(di$trainTestLasso[[i]]$test.data.x)[2]
  
}

#remove and garbage collect large variables
di = within(di, rm(dfs, dfs_missingRemoved))
gc()

################################# DUMMY CLASSIFIER
dummy.aucDf = list()
dummy.legend = c()
dummy.pathsPrint = "Dummy"
for(i in 1:length(di$paths)){
  dummy.prob = sum(as.numeric(di$trainTest[[i]]$train.data$Truth)-1 == 1)/length(di$trainTest[[i]]$train.data$Truth)  #train (not test) data +rate
  dummy.prediction = rep(dummy.prob, length(di$trainTest[[i]]$test.data$Truth))
  dummy.prediction.combined = ROCR::prediction( cbind(p1 = dummy.prediction),
                                                labels = matrix(di$trainTest[[i]]$test.data$Truth,
                                                                nrow = length(di$trainTest[[i]]$test.data$Truth),
                                                                ncol = 1))
  #for graph
  dummy.performance.combined = ROCR::performance(dummy.prediction.combined, "tpr", "fpr")
  dummy.aucDf[[i]] = data.frame(Curve=as.factor(c(rep(dummy.pathsPrint, length(dummy.performance.combined@x.values[[1]])))),
                           FalsePositive=c(dummy.performance.combined@x.values[[1]]),
                           TruePositive=c(dummy.performance.combined@y.values[[1]]))
  #for graph legend
  dummy.roc = roc(di$trainTest[[i]]$test.data$Truth, dummy.prediction)
  dummy.auc = ci.auc(dummy.roc)
  dummy.legend = c(dummy.legend, paste(di$pathsPrint[i],
                       " (",format(round(dummy.auc[2],2),nsmall=2),
                       ", ",format(round(dummy.auc[1],2),nsmall=2),
                       "-",format(round(dummy.auc[3],2),nsmall=2),")",sep=""))
}

#graph
ggplot() + 
  geom_line(data=dummy.aucDf[[1]], aes(x=FalsePositive, y=TruePositive, color='a'), size=1) +
  geom_line(data=dummy.aucDf[[2]], aes(x=FalsePositive, y=TruePositive, color='b'), size=1) +
  geom_line(data=dummy.aucDf[[3]], aes(x=FalsePositive, y=TruePositive, color='c'), size=1) +
  geom_line(data=dummy.aucDf[[4]], aes(x=FalsePositive, y=TruePositive, color='d'), size=1) +
  geom_line(data=dummy.aucDf[[5]], aes(x=FalsePositive, y=TruePositive, color='e'), size=1) +
  geom_line(data=dummy.aucDf[[6]], aes(x=FalsePositive, y=TruePositive, color='f'), size=1) +
  geom_line(data=dummy.aucDf[[7]], aes(x=FalsePositive, y=TruePositive, color='g'), size=1) +
  ylab("True positive rate (sensitivity)") + xlab("False positive rate (1-specificity)") +
  theme_minimal(base_size=18) +
  theme(legend.justification=c(1,0), legend.position=c(0.86,0.17), legend.background = element_rect()) +
  scale_color_manual(name="Dummy (AUC, 95% CI)", 
                     values = c('a'='dark gray','b'='red', 'c'="blue", 'd'="green", 'e'="orange", 'f'="yellow", 'g'="light gray"), 
                     labels = dummy.legend)

################################# LASSO
#perform CV in LASSO - about 30 minutes
for(i in 1:length(di$trainTestLasso)){
  print(i)
  di$cv.lasso[[i]] = cv.glmnet(di$trainTestLasso[[i]]$train.data.x,di$trainTestLasso[[i]]$train.data.y,
                        alpha=1, #alpha = 1 (defualt) = lasso; = 0 = ridge
                        family="binomial",nfolds=di$reps, type.measure = c("auc"),
                        parallel = T
                        #weights allows you to assign more/less meaning to data points; see paper
  )
}

di$lasso.train = data.frame(
  min.cvm = rep(-1, length(di$cv.lasso)),
  min.cvsd = rep(-1, length(di$cv.lasso)),
  min.nvars = rep(-1, length(di$cv.lasso)),
  se1.cvm = rep(-1, length(di$cv.lasso)),
  se1.cvsd = rep(-1, length(di$cv.lasso)),
  se1.nvars = rep(-1, length(di$cv.lasso))
)
di$lasso.vars.min = list()
di$lasso.vars.se1 = list()

for(i in 1:length(di$cv.lasso)){
  #min
  idx.min = match(di$cv.lasso[[i]]$lambda.min, di$cv.lasso[[i]]$lambda)
  di$lasso.train$min.cvm[i] = di$cv.lasso[[i]]$cvm[idx.min]
  di$lasso.train$min.cvsd[i] = di$cv.lasso[[i]]$cvsd[idx.min]
  
  #se1
  idx.se1 = match(di$cv.lasso[[i]]$lambda.1se, di$cv.lasso[[i]]$lambda)
  di$lasso.train$se1.cvm[i] = di$cv.lasso[[i]]$cvm[idx.se1]
  di$lasso.train$se1.cvsd[i] = di$cv.lasso[[i]]$cvsd[idx.se1]
  
  #nvars - min
  cv.lasso.vars = as.matrix(coef(di$cv.lasso[[i]],s="lambda.min"))
  cv.lasso.vars.names = names(cv.lasso.vars[cv.lasso.vars != 0,])
  di$lasso.vars.min[[i]] = cv.lasso.vars.names[!cv.lasso.vars.names %in% "(Intercept)"]
  di$lasso.train$min.nvars[[i]] = length(di$lasso.vars.min[[i]])  
  
  #nvars - se1
  cv.lasso.vars = as.matrix(coef(di$cv.lasso[[i]],s="lambda.1se"))
  cv.lasso.vars.names = names(cv.lasso.vars[cv.lasso.vars != 0,])
  di$lasso.vars.se1[[i]] = cv.lasso.vars.names[!cv.lasso.vars.names %in% "(Intercept)"]
  di$lasso.train$se1.nvars[[i]] = length(di$lasso.vars.se1[[i]])  
}

#predict
di$lasso.test = data.frame(
  min.auc = rep(-1, length(di$cv.lasso)),
  min.low = rep(-1, length(di$cv.lasso)),
  min.high = rep(-1, length(di$cv.lasso)),
  se1.auc = rep(-1, length(di$cv.lasso)),
  se1.low = rep(-1, length(di$cv.lasso)),
  se1.high = rep(-1, length(di$cv.lasso))
)
di$lasso.predict.min = list() #used for ROC plot
di$lasso.predict.se1 = list()

for(i in 1:length(di$cv.lasso)){
  #min
  di$lasso.predict.min[[i]] = predict(di$cv.lasso[[i]],newx = di$trainTestLasso[[i]]$test.data.x,type="response"
                          ,s=di$cv.lasso[[i]]$lambda.min, exact=T)
  roc.lasso = roc(di$trainTestLasso[[i]]$test.data.y, di$lasso.predict.min[[i]])
  auc.lasso = ci.auc(roc.lasso)
  di$lasso.test$min.auc[i] = auc.lasso[2]
  di$lasso.test$min.low[i] = auc.lasso[1]
  di$lasso.test$min.high[i] = auc.lasso[3]

  #1se
  di$lasso.predict.se1[[i]] = predict(di$cv.lasso[[i]],newx = di$trainTestLasso[[i]]$test.data.x,type="response"
                                      ,s=di$cv.lasso[[i]]$lambda.1se, exact=T)
  roc.lasso = roc(di$trainTestLasso[[i]]$test.data.y, di$lasso.predict.se1[[i]])
  auc.lasso = ci.auc(roc.lasso)
  di$lasso.test$se1.auc[i] = auc.lasso[2]
  di$lasso.test$se1.low[i] = auc.lasso[1]
  di$lasso.test$se1.high[i] = auc.lasso[3]
}


################################# XGBOOST
#what is the maximum number of variables available for the model to use?
numVarsToChoose = function(di){
  varsPerRun = rep(-1, length(di$trainTest))  
  for(i in 1:length(di$paths)){ 
    numVars = dim(di$trainTest[[i]]$train.data)[2]-1 #This includes Truth, so -1 var
    runMax = -1
    if(numVars < di$xgb.maxVarsPerRun){ #if the number of variables desired (maxVarsPerRun) is greater than available
      varsPerRun[i] = numVars #set the number equal to the number available
    }else{
      varsPerRun[i] = di$xgb.maxVarsPerRun #otherwise, set it equal to the number desired (maxVarsPerRun)
    }
  }
  return(varsPerRun)
}
di$xgb$varsPerRun = numVarsToChoose(di)


#how many varaibles will you include in each run of the model? (It could theoretically be a jagged array if too few variables are available.)
di$xgb$varNums = list()
for(i in 1:length(di$trainTest)){
  numVars = dim(di$trainTest[[i]]$train.data)[2]-1
  di$xgb$varNums[[i]] = rev(round(seq(1, numVars, length.out = di$xgb$varsPerRun[i]),0))
}

#shuffle vars to put Truth first
for(i in 1:length(di$trainTest)){
  #train
  truthIdx = match("Truth", colnames(di$trainTest[[i]]$train.data))
  totalVars = length(di$trainTest[[i]]$train.data)
  #if(truthIdx == 1) #do nothing
  if(truthIdx > 1 && truthIdx < totalVars){ #move Truth from the middle to the front
    di$trainTest[[i]]$train.data = di$trainTest[[i]]$train.data[,c(truthIdx, 1:(truthIdx-1), (truthIdx+1):dim(di$trainTest[[i]]$train.data)[2])]  
  }else if(truthIdx == totalVars){ #move Truth from the end to the front
    di$trainTest[[i]]$train.data = di$trainTest[[i]]$train.data[,c(truthIdx, 1:(truthIdx-1))]  
  }
  
  #test
  truthIdx = match("Truth", colnames(di$trainTest[[i]]$test.data))
  totalVars = length(di$trainTest[[i]]$test.data)
  #if(truthIdx == 1) #do nothing
  if(truthIdx > 1 && truthIdx < totalVars){ #move Truth from the middle to the front
    di$trainTest[[i]]$test.data = di$trainTest[[i]]$test.data[,c(truthIdx, 1:(truthIdx-1), (truthIdx+1):dim(di$trainTest[[i]]$test.data)[2])]  
  }else if(truthIdx == totalVars){ #move Truth from the end to the front
    di$trainTest[[i]]$test.data = di$trainTest[[i]]$test.data[,c(truthIdx, 1:(truthIdx-1))]  
  }
}

di$xgb$cv_summary = list()
di$xgb.test = data.frame(
  min.auc = rep(-1, length(di$trainTest)),
  min.low = rep(-1, length(di$trainTest)),
  min.high = rep(-1, length(di$trainTest)),
  se1.auc = rep(-1, length(di$trainTest)),
  se1.low = rep(-1, length(di$trainTest)),
  se1.high = rep(-1, length(di$trainTest))
)
di$xgb.predict.min = list() #used for ROC plot
di$xgb.predict.se1 = list()
di$xgb$modelXNames = list()
for(i in 1:length(di$trainTest)){
  di$xgb$modelXNames[[i]] = list()
  
  train.data = di$trainTest[[i]]$train.data
  test.data = di$trainTest[[i]]$test.data
  
  #dynamic
  outputAUC = c() #output
  modelId = c() #output
  models = c() #output!
  modelIter = 0 #used to carry over between runs of the for loop
  medianGainDf = 0 #""
  repModel = 0 #""
  model = 0
  
  #build model formula
  varNames = names(train.data)
  yName = varNames[1] #Truth
  
  #CV dataset
  folds <- caret::createFolds(train.data$Truth, k = di$reps, list = TRUE, returnTrain = FALSE)
  
  #?
  for(j in 1:length(di$xgb$varNums[[i]])){
    #setup model
    #print(varsPerRunIter)
    if(di$xgb$varNums[[i]][j] == di$xgb$varNums[[i]][1]){
      di$xgb$modelXNames[[i]][[j]] = list()
      xNames = varNames[2:di$xgb$varNums[[i]][j]] #use all the variables the first run
      di$xgb$modelXNames[[i]][[j]] = xNames
      model = sl_createModelFormula(yName, xNames)
    }else{
      medianGainDf = sl_xgb_getMedianGain(repModel$gainDf)
      xNames = sl_xgb_selectVarsFromGainDf(medianGainDf, di$xgb$varNums[[i]][j]) #hardcoded with factors from train.data data set
      #alternative variable reduction strategy -> xNames = names(medianGainDf)[which(medianGainDf != 0, arr.ind=T)]
      di$xgb$modelXNames[[i]][[j]] = xNames
      model = sl_createModelFormula(yName, xNames)
    }
    
    #run model
    #print(model)
    repModel = sl_xgb_repeatModelForGain(train.data, folds, model, di$reps) #$gainDf, $auc
    
    #save for output
    modelIter = modelIter + 1
    outputAUC = append(outputAUC, repModel$auc)
    modelId = append(modelId, rep(modelIter, di$reps))
    models = append(models, model)
  }
  di$xgb$models[[i]] = models
  
  #when finished...
  #review model formula -> models
  output = data.frame(modelId = as.character(modelId), outputAUC, NumberOfVars=factor(rep(di$xgb$varNums[[i]], each=di$reps)))
  output$NumberOfVars = reorder(output$NumberOfVars, new.order=as.character(di$xgb$varNums[[i]]))
  
  #plot - this plots the distribution of AUCs.  (Instead plot the CI, avg. AUC.)
  #tiff("C:/Users/George/Desktop/AUCbyVars.tiff", units="in", width=9.15, height=8.04, res=1200)
  #ggplot(output, aes(x=NumberOfVars, y=outputAUC)) + geom_boxplot() +
  #  theme_minimal(base_size=18) + theme(panel.grid.major.x = element_blank()) +
  #  ylab("AUC") + xlab("Number of Variables")
  #dev.off()
  
  #mean/sd of cross-validation -> used to select models
  di$xgb$cv_summary[[i]] = data.frame(
    nvars = di$xgb$varNums[[i]],
    mean = aggregate(output[, 2], list(output$modelId), mean)$x,
    se = aggregate(output[, 2], list(output$modelId), sd)$x/sqrt(di$reps)
  )
  di$xgb$cv_summary[[i]]$hi = di$xgb$cv_summary[[i]]$mean + di$xgb$cv_summary[[i]]$se
  di$xgb$cv_summary[[i]]$lo = di$xgb$cv_summary[[i]]$mean - di$xgb$cv_summary[[i]]$se
  
  #greatest AUC - "lambda.min"
  idx.min = match(max(di$xgb$cv_summary[[i]]$mean), di$xgb$cv_summary[[i]]$mean) #maximize auc (notation comes from minimize lambda)
  
  #fewest reasonable variables - "lambda.1se"
  idx.se1 = -1
  for(j in rev(1:dim(di$xgb$cv_summary[[i]])[1])){
    if(di$xgb$cv_summary[[i]]$mean[j] == di$xgb$cv_summary[[i]]$mean[idx.min]){ #current value is best value
      idx.se1 = idx.min
      break
    }
    if(di$xgb$cv_summary[[i]]$hi[j] >= di$xgb$cv_summary[[i]]$mean[idx.min]){
      idx.se1 = j
      break
    }
  }
  di$xgb$cv_summary[[i]]$lambda.min = rep(0,nrow(di$xgb$cv_summary[[i]]))
  di$xgb$cv_summary[[i]]$lambda.se1 = rep(0,nrow(di$xgb$cv_summary[[i]]))
  di$xgb$cv_summary[[i]]$lambda.min[idx.min] = 1
  di$xgb$cv_summary[[i]]$lambda.se1[idx.se1] = 1
  
  #"lambda.min" model - complete training set
  xgboost.min = sl_xgb_run1(train.data, test.data, models[[idx.min]]) #(train, test, model) -> $predict, $roc, $auc, $xgmodel
  di$xgb.test$min.low[i] = xgboost.min$auc[1] #low-95%
  di$xgb.test$min.auc[i] = xgboost.min$auc[2] #middle
  di$xgb.test$min.high[i] = xgboost.min$auc[3] #high-95%
  di$xgb.predict.min[[i]] = xgboost.min #save output
  
  xgboost.se1 = sl_xgb_run1(train.data, test.data, models[[idx.se1]]) #(train, test, model) -> $predict, $roc, $auc, $xgmodel
  di$xgb.test$se1.low[i] = xgboost.se1$auc[1] #low-95%
  di$xgb.test$se1.auc[i] = xgboost.se1$auc[2] #middle
  di$xgb.test$se1.high[i] = xgboost.se1$auc[3] #high-95%
  di$xgb.predict.se1[[i]] = xgboost.se1 #save output
}

#ROC - LASSO - time plot
di$lasso.aucDf.min = list()
for(i in 1:length(di$trainTestLasso)){
  prediction.combined = ROCR::prediction( cbind(p1 = di$lasso.predict.min[[i]]),
                                          labels = matrix(di$trainTestLasso[[i]]$test.data.y, 
                                                          nrow = length(di$trainTestLasso[[i]]$test.data.y), 
                                                          ncol = 1)) 
  performance.combined = ROCR::performance(prediction.combined, "tpr", "fpr")
  di$lasso.aucDf.min[[i]] = data.frame(Curve=as.factor(c(rep(di$pathsPrint[[i]], length(performance.combined@x.values[[1]])))), 
                     FalsePositive=c(performance.combined@x.values[[1]]),
                     TruePositive=c(performance.combined@y.values[[1]]))
}

di$lasso.aucDf.se1 = list()
for(i in 1:length(di$trainTestLasso)){
  prediction.combined = ROCR::prediction( cbind(p1 = di$lasso.predict.se1[[i]]),
                                          labels = matrix(di$trainTestLasso[[i]]$test.data.y, 
                                                          nrow = length(di$trainTestLasso[[i]]$test.data.y), 
                                                          ncol = 1)) 
  performance.combined = ROCR::performance(prediction.combined, "tpr", "fpr")
  di$lasso.aucDf.se1[[i]] = data.frame(Curve=as.factor(c(rep(di$pathsPrint[[i]], length(performance.combined@x.values[[1]])))), 
                                       FalsePositive=c(performance.combined@x.values[[1]]),
                                       TruePositive=c(performance.combined@y.values[[1]]))
}

di$lasso.legend.min = rep(-1, length(di$trainTestLasso))
for(i in 1:length(di$trainTestLasso)){
  di$lasso.legend.min[i] = paste(di$pathsPrint[i]," (",format(round(di$lasso.test$min.auc[i],2),nsmall=2),", ",format(round(di$lasso.test$min.low[i],2),nsmall=2),"-",format(round(di$lasso.test$min.high[i],2),nsmall=2),")",sep="")
}

di$lasso.legend.se1 = rep(-1, length(di$trainTestLasso))
for(i in 1:length(di$trainTestLasso)){
  di$lasso.legend.se1[i] = paste(di$pathsPrint[i]," (",format(round(di$lasso.test$se1.auc[i],2),nsmall=2),", ",format(round(di$lasso.test$se1.low[i],2),nsmall=2),"-",format(round(di$lasso.test$se1.high[i],2),nsmall=2),")",sep="")
}

p1 = ggplot() + 
  geom_line(data=di$lasso.aucDf.min[[1]], aes(x=FalsePositive, y=TruePositive, color='a'), size=1) +
  geom_line(data=di$lasso.aucDf.min[[2]], aes(x=FalsePositive, y=TruePositive, color='b'), size=1) +
  geom_line(data=di$lasso.aucDf.min[[3]], aes(x=FalsePositive, y=TruePositive, color='c'), size=1) +
  geom_line(data=di$lasso.aucDf.min[[4]], aes(x=FalsePositive, y=TruePositive, color='d'), size=1) +
  geom_line(data=di$lasso.aucDf.min[[5]], aes(x=FalsePositive, y=TruePositive, color='e'), size=1) +
  geom_line(data=di$lasso.aucDf.min[[6]], aes(x=FalsePositive, y=TruePositive, color='f'), size=1) +
  geom_line(data=di$lasso.aucDf.min[[7]], aes(x=FalsePositive, y=TruePositive, color='g'), size=1) +
  ylab("True positive rate (sensitivity)") + xlab("False positive rate (1-specificity)") +
  theme_minimal(base_size=18) +
  theme(legend.justification=c(1,0), legend.position=c(0.86,0.17), legend.background = element_rect()) +
  scale_color_manual(name="LASSO - Max AUC (AUC, 95% CI)", 
                     values = c('a'='dark gray','b'='red', 'c'="blue", 'd'="green", 'e'="orange", 'f'="yellow", 'g'="light gray"), 
                     labels = di$lasso.legend.min)

p2 = ggplot() + 
  geom_line(data=di$lasso.aucDf.se1[[1]], aes(x=FalsePositive, y=TruePositive, color='a'), size=1) +
  geom_line(data=di$lasso.aucDf.se1[[2]], aes(x=FalsePositive, y=TruePositive, color='b'), size=1) +
  geom_line(data=di$lasso.aucDf.se1[[3]], aes(x=FalsePositive, y=TruePositive, color='c'), size=1) +
  geom_line(data=di$lasso.aucDf.se1[[4]], aes(x=FalsePositive, y=TruePositive, color='d'), size=1) +
  geom_line(data=di$lasso.aucDf.se1[[5]], aes(x=FalsePositive, y=TruePositive, color='e'), size=1) +
  geom_line(data=di$lasso.aucDf.se1[[6]], aes(x=FalsePositive, y=TruePositive, color='f'), size=1) +
  geom_line(data=di$lasso.aucDf.se1[[7]], aes(x=FalsePositive, y=TruePositive, color='g'), size=1) +
  ylab("True positive rate (sensitivity)") + xlab("False positive rate (1-specificity)") +
  theme_minimal(base_size=18) +
  theme(legend.justification=c(1,0), legend.position=c(0.86,0.17), legend.background = element_rect()) +
  scale_color_manual(name="LASSO - 1 SE (AUC, 95% CI)", 
                     values = c('a'='dark gray','b'='red', 'c'="blue", 'd'="green", 'e'="orange", 'f'="yellow", 'g'="light gray"), 
                     labels = di$lasso.legend.se1)

#ROC - xgb - time plot
#legends
di$xgb.legend.min = rep(-1, length(di$trainTest))
for(i in 1:length(di$trainTest)){
  di$xgb.legend.min[i] = paste(di$pathsPrint[i]," (",format(round(di$xgb.test$min.auc[i],2),nsmall=2),", ",format(round(di$xgb.test$min.low[i],2),nsmall=2),"-",format(round(di$xgb.test$min.high[i],2),nsmall=2),")",sep="")
}

di$xgb.legend.se1 = rep(-1, length(di$trainTest))
for(i in 1:length(di$trainTest)){
  di$xgb.legend.se1[i] = paste(di$pathsPrint[i]," (",format(round(di$xgb.test$se1.auc[i],2),nsmall=2),", ",format(round(di$xgb.test$se1.low[i],2),nsmall=2),"-",format(round(di$xgb.test$se1.high[i],2),nsmall=2),")",sep="")
}

di$xgb.aucDf.min = list()
for(i in 1:length(di$trainTest)){
  di$xgb.aucDf.min[[i]] = sl_xgb_aucDf(di$trainTest[[i]], di$xgb.predict.min[[i]])
}

di$xgb.aucDf.se1 = list()
for(i in 1:length(di$trainTest)){
  di$xgb.aucDf.se1[[i]] = sl_xgb_aucDf(di$trainTest[[i]], di$xgb.predict.se1[[i]])
}

#min
p3 = ggplot() + 
  geom_line(data=di$xgb.aucDf.min[[1]], aes(x=FalsePositive, y=TruePositive, color='a'), size=1) +
  geom_line(data=di$xgb.aucDf.min[[2]], aes(x=FalsePositive, y=TruePositive, color='b'), size=1) +
  geom_line(data=di$xgb.aucDf.min[[3]], aes(x=FalsePositive, y=TruePositive, color='c'), size=1) +
  geom_line(data=di$xgb.aucDf.min[[4]], aes(x=FalsePositive, y=TruePositive, color='d'), size=1) +
  geom_line(data=di$xgb.aucDf.min[[5]], aes(x=FalsePositive, y=TruePositive, color='e'), size=1) +  
  geom_line(data=di$xgb.aucDf.min[[6]], aes(x=FalsePositive, y=TruePositive, color='f'), size=1) +
  geom_line(data=di$xgb.aucDf.min[[7]], aes(x=FalsePositive, y=TruePositive, color='g'), size=1) +
  ylab("True positive rate (sensitivity)") + xlab("False positive rate (1-specificity)") +
  theme_minimal(base_size=18) +
  theme(legend.justification=c(1,0), legend.position=c(0.86,0.17), legend.background = element_rect()) +
  scale_color_manual(name="XGBoost - Max AUC (AUC, 95% CI)", 
                     values = c('a'='dark gray','b'='red', 'c'="blue", 'd'="green", 'e'="orange", 'f'="yellow", 'g'="light gray"), 
                     labels = di$xgb.legend.min)

#se1
p4 = ggplot() + 
  geom_line(data=di$xgb.aucDf.se1[[1]], aes(x=FalsePositive, y=TruePositive, color='a'), size=1) +
  geom_line(data=di$xgb.aucDf.se1[[2]], aes(x=FalsePositive, y=TruePositive, color='b'), size=1) +
  geom_line(data=di$xgb.aucDf.se1[[3]], aes(x=FalsePositive, y=TruePositive, color='c'), size=1) +
  geom_line(data=di$xgb.aucDf.se1[[4]], aes(x=FalsePositive, y=TruePositive, color='d'), size=1) +
  geom_line(data=di$xgb.aucDf.se1[[5]], aes(x=FalsePositive, y=TruePositive, color='e'), size=1) +  
  geom_line(data=di$xgb.aucDf.se1[[6]], aes(x=FalsePositive, y=TruePositive, color='f'), size=1) +
  geom_line(data=di$xgb.aucDf.se1[[7]], aes(x=FalsePositive, y=TruePositive, color='g'), size=1) +
  ylab("True positive rate (sensitivity)") + xlab("False positive rate (1-specificity)") +
  theme_minimal(base_size=18) +
  theme(legend.justification=c(1,0), legend.position=c(0.86,0.17), legend.background = element_rect()) +
  scale_color_manual(name="XGBoost - 1 SE (AUC, 95% CI)", 
                     values = c('a'='dark gray','b'='red', 'c'="blue", 'd'="green", 'e'="orange", 'f'="yellow", 'g'="light gray"), 
                     labels = di$xgb.legend.min)
#save
ggsave(p1, filename = "ROC (Lasso - Min).png", path="C:\\Users\\George\\Desktop", dpi = 300, type = "cairo")
ggsave(p2, filename = "ROC (Lasso - 1SE).png", path="C:\\Users\\George\\Desktop", dpi = 300, type = "cairo")
ggsave(p3, filename = "ROC (XGB - Min).png", path="C:\\Users\\George\\Desktop", dpi = 300, type = "cairo")
ggsave(p4, filename = "ROC (XGB - 1SE).png", path="C:\\Users\\George\\Desktop", dpi = 300, type = "cairo")


#comparison of variables
#lasso
lasso.vars.min.combined = c() #min models
for(i in 1:length(di$lasso.vars.min)){
  lasso.vars.min.combined = c(lasso.vars.min.combined, di$lasso.vars.min[[i]])
}
lasso.vars.se1.combined = c() #se1 models
for(i in 1:length(di$lasso.vars.se1)){
  lasso.vars.se1.combined = c(lasso.vars.se1.combined, di$lasso.vars.se1[[i]])
}
lasso.vars.combined = c(lasso.vars.min.combined, lasso.vars.se1.combined) #all models

#counts by type and table
di$varCounts.lasso.min = as.data.frame(table(lasso.vars.min.combined))
di$varCounts.lasso.min = di$varCounts.lasso.min[order(-di$varCounts.lasso.min$Freq),]
di$varCounts.lasso.se1 = as.data.frame(table(lasso.vars.se1.combined))
di$varCounts.lasso.se1 = di$varCounts.lasso.se1[order(-di$varCounts.lasso.se1$Freq),]
di$varCounts.lasso.all = as.data.frame(table(lasso.vars.combined))
di$varCounts.lasso.all = di$varCounts.lasso.all[order(-di$varCounts.lasso.all$Freq),]

#xgb
xgb.vars.min.combined = c() #min models
for(i in 1:length(di$lasso.vars.min)){
  j = match(1, di$xgb$cv_summary[[i]]$lambda.min)
  xgb.vars.min.combined = c(xgb.vars.min.combined, di$xgb$modelXNames[[i]][[j]])
}
xgb.vars.se1.combined = c() #se1 models
for(i in 1:length(di$lasso.vars.min)){
  j = match(1, di$xgb$cv_summary[[i]]$lambda.se1)
  xgb.vars.se1.combined = c(xgb.vars.se1.combined, di$xgb$modelXNames[[i]][[j]])
}
xgb.vars.combined = c(xgb.vars.min.combined, xgb.vars.se1.combined) #all models

#counts by type and table
di$varCounts.xgb.min = as.data.frame(table(xgb.vars.min.combined))
di$varCounts.xgb.min = di$varCounts.xgb.min[order(-di$varCounts.xgb.min$Freq),]
di$varCounts.xgb.se1 = as.data.frame(table(xgb.vars.se1.combined))
di$varCounts.xgb.se1 = di$varCounts.xgb.se1[order(-di$varCounts.xgb.se1$Freq),]
di$varCounts.xgb.all = as.data.frame(table(xgb.vars.combined))
di$varCounts.xgb.all = di$varCounts.xgb.all[order(-di$varCounts.xgb.all$Freq),]

#both models
di$varCounts.all = as.data.frame(table(c(lasso.vars.combined, xgb.vars.combined)))
di$varCounts.all = di$varCounts.all[order(-di$varCounts.all$Freq),]

#count by time periods
# di$varCounts.byTime = list()
# for(i in 1:length(di$lasso.vars.min)){
#   
#   j = match(1, di$xgb$cv_summary[[i]]$lambda.min)
#   xgb.vars.min.combined = c(xgb.vars.min.combined, di$xgb$modelXNames[[i]][[j]])
# }

##output
di$patientCounts #study population
di$output_missPosNeg
di$output_counts
di$lasso.train
di$lasso.test
di$lasso.vars.min #work on consolidating variables chosen into a dataframe
di$lasso.vars.se1

colnames(di$trainTest[[i]]$train.data)
di$xgb$varsPerRun
di$xgb$varNums
di$xgb$cv_summary

#variable counts
di$varCounts.all
di$varCounts.xgb.all
di$varCounts.lasso.all
di$varCounts.xgb.min
di$varCounts.xgb.se1
di$varCounts.lasso.min
di$varCounts.lasso.se1

#demographics
mean(di$dfs_missingRemoved[[1]]$AgeAtBaseline)
sd(di$dfs_missingRemoved[[1]]$AgeAtBaseline)
prop.table(table(di$dfs_missingRemoved[[1]]$IsMale))
prop.table(table(di$dfs_missingRemoved[[1]]$Sta3nId))
#mean(di$dfs_missingRemoved[[1]]$VisitOutpt1YrPCOnly5Code)
median(di$dfs_missingRemoved[[1]]$VisitOutpt1YrPCOnly5Code)
quantile(di$dfs_missingRemoved[[1]]$VisitOutpt1YrPCOnly5Code)

#table of variable selection
source('C:/Users/George/Desktop/BCR_ABL_timeStudy2_varTable.R', echo=TRUE)

#debug