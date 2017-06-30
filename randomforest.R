#package
#random forest
library(ranger)

#parallel computing 
library(foreach)
library(doParallel)
library(neuralnet)
## data Data preprocessing##

#print(paste0("obs before remove NA records is: ",dim(df.x)[1]))

data.x.pre = df.x
#for(v in varlist) {
#  data.x.pre <- data.x.pre [!is.na(df.x[[v]]),]
#}
#print(paste0("obs after remove NA records is: ",dim(data.x.pre )[1]))

#geo_add_on
#tran_freq(done), average(logerror)??

#data.x.geo = geo_addon(df.y,data.x.pre )
data.x = data.x.geo
data.x$tranfreq = data.x$transactionNum/data.x$neighborNum


# geo_variable analysis


var_remove = c("bathroomcnt",
               "bedroomcnt","month","latitude","longitude","area_effect",
               "tranfreq","fips","logerror")
var_to0 = c("poolcnt","garagetotalsqft","calculatedbathnbr","numberofstories" )
var_tomean = c("yearbuilt","lotsizesquarefeet","landtaxvaluedollarcnt","finishedsquarefeet12","calculatedfinishedsquarefeet",
               "taxvaluedollarcnt","taxamount")

data.x = ReplaceMissing(data.x,var_remove,var_to0,var_tomean)
n = c(var_remove,var_to0,var_tomean)

predictor = n[!n %in% c("logerror",'parcelid','transactionNum')]
f_geo <- as.formula(paste("logerror ~", paste(predictor, collapse = " + ")))

obs= dim(data.x)[1]

#core_num=detectCores(all.tests = FALSE, logical = TRUE)-2
#cl <- makeCluster(2)
#registerDoParallel(cl)

#$result = foreach (i = 1:4,.combine=rbind,.packages="ranger") %dopar%
#{

set.seed(i)
sampleindex = sample(obs,size  = obs*0.8)
train = data.x[sampleindex,n]
test = data.x[-sampleindex,n]

index = remove_outlier(train$logerror,outlierscalar = 4)
train = train[index,]
train_obs =dim(train)[1]
holdout_index = sample(train_obs,size  = train_obs*0.1)
holdout = rep(1,train_obs)
holdout[holdout_index]=0
#randomF = ranger(f, train,importance='impurity') 
#pred=predict(randomF, test)
#layer one:
#random Forest:
  train = train[!is.na(train$logerror),]
  
  randomF_geo = ranger(f_geo,data=train,minprop = 0.1,importance = 'impurity',mtry=4,min.node.size=5,splitrule ="maxstat",num.trees = 200)
  
  pred_layer_1=predict(randomF_geo, test)$predictions
  
 
  if (layer_2){
    pred_train=predict(randomF_geo, train)$predictions
    train$logerror_error = train$logerror - pred_train
    train_nn = train[!is.na(train$logerror_error),]
    train_nn$month = as.numeric(train_nn$month)
    mins = apply(train_nn,2,'min')
    maxs = apply(train_nn,2,'max')
    scaled_train <- as.data.frame(scale(train_nn, center = mins, scale = maxs - mins))
    
    test_nn = test
    test_nn$month = as.numeric(test_nn$month)
    scaled_test <-as.data.frame(scale(test_nn, center = mins[n], scale = maxs[n] - mins[n]))
    
    f_nn <- as.formula(paste("logerror_error ~", paste(n[!n %in% c("logerror_error","logerror",'parcelid','transactionNum')], collapse = " + ")))
    nn = neuralnet(f_nn,rep=3,data = scaled_train,threshold = 0.2,learningrate = 0.01,hidden=c(20,17,15,10,5,3),lifesign = 'full',algorithm = 'rprop+',linear.output=T,stepmax =1000)
    
    entrylist = nn$model.list$variables
    pred.nn = compute(nn,scaled_test[,entrylist])
    
    pred_layer_2 = pred.nn$net.result*(maxs['logerror_error'] - mins['logerror_error'])+mins['logerror_error']
    
    pred = pred_layer_1+pred_layer_2
    error = mean(abs(pred - test$logerror),na.rm = TRUE)
  }


  error_geo = mean(abs(pred_layer_1 - test$logerror),na.rm = TRUE)
  
  benchmark = mean(abs(test$logerror))
  improve = benchmark-error_geo

print(paste("error:",error))
print(paste("error_geo",error_geo))
print(paste("banchmark",benchmark))
print(paste("improve",improve))

#out = data.frame(improve = benchmark-error_geo,error_geo = error_geo,benchmark = benchmark)
#}

#stopCluster(cl)


