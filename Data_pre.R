rm(list=ls())
library(data.table)
source("function.R")
#get df.x, df.y df.y.test
#purify dataset, missing value, log

########################################################################
########################### Data import ################################
########################################################################
datapath <- "data"
df.x <- suppressWarnings(fread(paste(datapath, "properties_2016.csv", sep="/")))
df.y <- fread(paste(datapath, "train_2016.csv", sep="/"))

df.y.test <- fread(paste(datapath, "sample_submission.csv", sep="/"), header = TRUE)
df.y.test <- data.frame(df.y.test, check.names = FALSE)
df.x <- data.frame(df.x)
df.y <- data.frame(df.y)
########################################################################
###################### Datatype conversion #############################
########################################################################
## Boolean data conversion
df.x$hashottuborspa <- ifelse(df.x$hashottuborspa=="true", TRUE, FALSE)
df.x$pooltypeid10 <- ifelse(df.x$pooltypeid10==1, TRUE, FALSE)
df.x$pooltypeid2 <- ifelse(df.x$pooltypeid2==1, TRUE, FALSE)
df.x$pooltypeid7 <- ifelse(df.x$pooltypeid7==1, TRUE, FALSE)
df.x$fireplaceflag <- ifelse(df.x$fireplaceflag=="true", TRUE, FALSE)

## Merge pooltypeid2/7/10 into one variable
df.x$pooltypeid <- 0
df.x[which(df.x$pooltypeid2),]$pooltypeid <- 2
df.x[which(df.x$pooltypeid7),]$pooltypeid <- 7
df.x[which(df.x$pooltypeid10),]$pooltypeid <- 10

factor.vars <- c("airconditioningtypeid", 
                 "architecturalstyletypeid",
                 "decktypeid", 
                 "heatingorsystemtypeid",
                 "propertycountylandusecode", 
                 "propertylandusetypeid", 
                 "propertyzoningdesc",
                 "regionidcity", 
                 "regionidcounty", 
                 "regionidneighborhood", 
                 "regionidzip",
                 "storytypeid", 
                 "fips",
                 "typeconstructiontypeid", 
                 # "rawcensustractandblock", 
                 # "censustractandblock", 
                 "buildingclasstypeid", 
                 "pooltypeid")
for(v in factor.vars) {
  df.x[,v] <- as.factor(df.x[,v])
}

for(v in colnames(df.x)) {
  if(class(df.x[,v]) %in% c("integer", "integer64")) {
    df.x[,v] <- as.numeric(df.x[,v])
  } else if(class(df.x[,v]) %in% c("logical", "character")) {
    df.x[,v] <- as.factor(df.x[,v])
  }
}

########################################################################
########## Remove cols that are redundant (1st pass) ###################
########################################################################
## variables to be removed for model training
remove.vars <- c("pooltypeid2",    # Encoded in pooltypeid
                 "pooltypeid7",    # Encoded in pooltypeid
                 "pooltypeid10"   # Encoded in pooltypeid
)

remove.idx <- which(colnames(df.x) %in% remove.vars)

df.x <- df.x[,-remove.idx]

########################################################################
################# Remove rows that are mostly NAs ######################
########################################################################
#row.keep <- which(apply(df.x,1,function(x) sum(is.na(x))) < 30)
#df.x <- df.x[row.keep,]

log.vars <- c("calculatedfinishedsquarefeet","finishedsquarefeet12",
              "finishedsquarefeet15",
              "finishedsquarefeet50", # log10(x+500)
              "lotsizesquarefeet",
              "yardbuildingsqft17",
              "structuretaxvaluedollarcnt",
              "taxvaluedollarcnt",
              "landtaxvaluedollarcnt",
              "taxamount")


###################### Create month variable ###########################
df.y$month = as.factor(month(as.Date(df.y$transactiondate)))
remove.vars <- c("date", 
                 "transactiondate")
remove.idx <- which(colnames(df.y) %in% remove.vars)
df.y <- df.y[,-remove.idx]

###################### Replce NA with 0 ###########################
#df.x$poolcnt[is.na(df.x$poolcnt)] = 0
#df.x$numberofstories[is.na(df.x$numberofstories)]=0
#df.x$finishedsquarefeet13[is.na(df.x$finishedsquarefeet13)]=0
#df.x$finishedsquarefeet12[is.na(df.x$finishedsquarefeet12)]=0

#df.x$garagetotalsqft[is.na(df.x$garagetotalsqft)]=0
#df.x$bathroomcnt[is.na(df.x$bathroomcnt)] = 0
#df.x$bedroomcnt[is.na(df.x$bedroomcnt)] = 0
#df.x$fullbathcnt[is.na(df.x$bedroomcnt)] = 0




###################### Replce NA with mean ###########################
#df.x$taxvaluedollarcnt = fill_mean(df.x$taxvaluedollarcnt)
##df.x$taxamount = fill_mean(df.x$taxamount)
#df.x$yearbuilt = fill_mean(df.x$yearbuilt)
#df.x$landtaxvaluedollarcnt = fill_mean(df.x$landtaxvaluedollarcnt)

#df.x$calculatedfinishedsquarefeet =  fill_mean(df.x$calculatedfinishedsquarefeet)

