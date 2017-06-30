source("neighbor_statistics.R")

remove_outlier <- function(data,outlierscalar = 3){
position = 1:length(data)
flag = TRUE
while (flag) {
  mid = mean(data[position])
  std = sd(data[position])
  remain = abs(data-mid)/std<=outlierscalar
  newposition = which(remain == TRUE)
  if (length(newposition) == length(position)){
    flag = FALSE
  }else{
    position = newposition
  }
}
return(position)
}

fill_mean<- function(data){
    data[is.na(data)] = mean(data[!is.na(data)])
    return(data)
}

########################################################################
####################### Functions ######################################
########################################################################
## Check the type and NA value fraction of each column in the data frame
checkType <- function(df) {
  type.df <- data.frame(freq=sapply(df, function(x) round(sum(is.na(x)) / length(x) * 100, 3)), 
                        type=sapply(df, class))
  type.df$name <- rownames(type.df)
  rownames(type.df) <- NULL
  type.df <- type.df[,c("name", "type", "freq")]
  type.df$nlevel <- sapply(df, function(x) {
    if(class(x) == "factor") {
      length(unique(x))
    } else {
      0
    }
  })
  type.df
}

########

NAto0 = function(data,varlist){
  for (i in varlist){
   data[,i][is.na(data[,i])]=0 
  }
return(data)
}


######
NAtomean = function(data, varlist){
  for (i in varlist){
    data[,i]=fill_mean(data[,i])
  }
  return(data)
}


#####
removeNA = function(data,varlist){
  for (i in varlist){
    data=data[!is.na(data[,i]),]
  }
  return(data)
}


#####
ReplaceMissing =function(data,remove_NA,NA_to_0,NA_to_mean,keep=NULL){
  data = removeNA(data,remove_NA)
  data = NAto0(data,NA_to_0)
  data = NAtomean(data,NA_to_mean)
  data = data[,c(remove_NA,NA_to_0,NA_to_mean,keep)]
  return (data)
}