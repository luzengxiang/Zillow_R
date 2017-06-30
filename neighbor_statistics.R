#neighbor number, transaction frequency:
#1 mile  longitude 1/70*10^6 =15000, latitude 1/53*10^6 =19000

# has geo infomation
#add merge trainData, property. generate  transactionnumber and neighbor_number.
geo_addon <- function(trainData,property,mile=1) {

property_geo = subset(property,!is.na(latitude)&!is.na(longitude))

#discrete longitude
longitude_order = order(property_geo$longitude)
property_geo = property_geo[longitude_order,]
property_geo$longitude_dis = floor(property_geo$longitude/15000/mile*2)
longitude_dis = data.frame(longitude=property_geo$longitude_dis,rownum=1:(dim(property_geo)[1]))
maxs=aggregate(rownum~longitude,data=longitude_dis,"max" )
mins=aggregate(rownum~longitude,data=longitude_dis,"min" )
longtitude_table = merge(maxs,mins,by="longitude")
names(longtitude_table) = c('longitude_index','max','min')


data = merge(trainData,property_geo,by = "parcelid")
property_geo = as.data.frame(property_geo)
property_geo = property_geo[,c("parcelid" ,"latitude","longitude","longitude_dis")]

data = data[order(data$longitude_dis),]

data_geo =data[,c("latitude","longitude","month","longitude_dis","logerror")]
longitude_dis_data = data.frame(longitude=data_geo$longitude_dis,rownum=1:(dim(data)[1]))
maxs=aggregate(rownum~longitude,data=longitude_dis_data,"max" )
mins=aggregate(rownum~longitude,data=longitude_dis_data,"min" )
longtitude_table_data = merge(maxs,mins,by="longitude")
names(longtitude_table_data) = c('longitude_index','max','min')
longtitude_table_data = merge(x=longtitude_table_data,y=data.frame(longitude_index=longtitude_table$longitude_index),
                              by="longitude_index",all.y =TRUE)

y_neighbornum = c()
y_tran = c()
area_effect =c()
n=dim(data_geo)[1]
k=1
sepNum=dim(longtitude_table)[1]
for (i in 1:sepNum){
 message=paste(c(i,'/',dim(longtitude_table)[1]),collapse = '')
 print(message)
if (!is.na(longtitude_table_data$min[i])){
  data_analysis = data_geo[longtitude_table_data$min[i]:longtitude_table_data$max[i],] 
  min_index = i
  max_index = i
  if (!is.na(longtitude_table_data$min[i-1])){
    min_index = i-1
  }
  
  if (!is.na(longtitude_table_data$max[i+1])){
   max_index = i+1
  }
  transaction_data = data_geo[longtitude_table_data$min[min_index]:longtitude_table_data$max[max_index],] 
  geo_sub = property_geo[longtitude_table$min[max(0,i-1)]:longtitude_table$max[min(i+1,sepNum)],]
 
   for (j in 1:dim(data_analysis)[1]){
  neighbor_index=abs( geo_sub$longitude-data_analysis$longitude[j])<=15000*mile &
                  abs( geo_sub$latitude-data_analysis$latitude[j])<=19000*mile
  neighbor_number = sum(neighbor_index)
  transaction_index = abs( transaction_data$longitude-data_analysis$longitude[j])<=15000*mile &
                     abs( transaction_data$latitude-data_analysis$latitude[j])<=19000*mile &
                      transaction_data$month == data_analysis$month[j]
  

  area_effect_data = transaction_data[transaction_index,]
  if (dim(area_effect_data)[1]<=9){
  area_effect[k] = 0
  }else{
    index = remove_outlier(area_effect_data$logerror)
    if (length(index)<5){
      area_effect[k] = 0
    }else{
      area_effect_data = area_effect_data[index,]
      area_effect[k] =mean(area_effect_data$logerror)
    }
    
  }

  
   y_neighbornum[k]=neighbor_number
  
  y_tran[k] = sum(transaction_index)
  k = k+1
  }
}
}
  data$neighborNum = y_neighbornum
  data$transactionNum = y_tran
  data$area_effect = area_effect
  
  remove.vars <- c("longitude_dis")
  remove.idx <- which(colnames(data) %in% remove.vars)
  data <- data[,-remove.idx]
return(data)
}


geo_addon_x <- function(df.y,df.x,mile=1) {
  df.y$month = as.numeric(df.y$month)
  df.x = subset(df.x,!is.na(latitude)&!is.na(longitude))
  
  house_position = unique(df.x[,c('parcelid','latitude','longitude')])
  house_transaction = merge(df.x,df.y)
  
  #discrete longitude & latitude for house_position
  house_position_order = order( house_position$longitude,house_position$latitude)
  house_position = house_position[house_position_order,]
  
  house_position$longitude_dis =  floor(house_position$longitude/15000/mile*2)
  house_position$latitude_dis = floor(house_position$latitude/19000/mile*2)
  
  dis_table = data.frame(longitude=house_position$longitude_dis,
                             latitude =house_position$latitude_dis,
                             rownum=1:(dim(house_position)[1]))
  
  maxs=aggregate(rownum~longitude+latitude,data=dis_table,"max" )
  mins=aggregate(rownum~longitude+latitude,data=dis_table,"min" )
  house_position_index = merge(maxs,mins,by=c("longitude","latitude"))
  names(house_position_index) = c('longitude_index','latitude_index','max','min')
  
  #discrete longitude & latitude for house_transition
  house_transaction_order = order(house_transaction$longitude,house_transaction$latitude)
  house_transaction =house_transaction[house_transaction_order,]
  
  house_transaction$longitude_dis =  floor(house_transaction$longitude/15000/mile*2)
  house_transaction$latitude_dis = floor(house_transaction$latitude/19000/mile*2)
  
  dis_table = data.frame(longitude=house_transaction$longitude_dis,
                         latitude =house_transaction$latitude_dis,
                         rownum=1:(dim(house_transaction)[1]))
  
  maxs=aggregate(rownum~longitude+latitude,data=dis_table,"max" )
  mins=aggregate(rownum~longitude+latitude,data=dis_table,"min" )
  house_transaction_index = merge(maxs,mins,by=c("longitude","latitude"))
  names(house_transaction) = c('longitude_index','latitude_index','max','min')
  
  #data.x
  #discrete longitude & latitude for df.x
  data.x = df.x
  data.x_order = order( data.x$longitude,data.x$latitude)
  data.x = data.x[data.x_order,]
  
  data.x$longitude_dis =  floor(data.x$longitude/15000/mile*2)
  data.x$latitude_dis = floor(data.x$latitude/19000/mile*2)
  
  dis_table = data.frame(longitude=data.x$longitude_dis,
                         latitude =data.x$latitude_dis,
                         rownum=1:(dim(data.x)[1]))
  
  maxs=aggregate(rownum~longitude+latitude,data=data.x,"max" )
  mins=aggregate(rownum~longitude+latitude,data=data.x,"min" )
  data.x_index = merge(maxs,mins,by=c("longitude","latitude"))
  names(data.x_index) = c('longitude_index','latitude_index','max','min')
  
  
  
  property_geo = as.data.frame(property_geo)
  data = property_geo
  
  data = data[order(data$longitude_dis),]
  data_geo =data[,c("latitude","longitude","month","longitude_dis","logerror")]
  longitude_dis_data = data.frame(longitude=data_geo$longitude_dis,rownum=1:(dim(data)[1]))
  maxs=aggregate(rownum~longitude,data=longitude_dis_data,"max" )
  mins=aggregate(rownum~longitude,data=longitude_dis_data,"min" )
  longtitude_table_data = merge(maxs,mins,by="longitude")
  names(longtitude_table_data) = c('longitude_index','max','min')
  longtitude_table_data = merge(x=longtitude_table_data,y=data.frame(longitude_index=longtitude_table$longitude_index),
                                by="longitude_index",all.y =TRUE)
  
  n=dim(data_geo)[1]
  sepNum=dim(longtitude_table)[1]
  for (i in 1:sepNum){
    message=paste(c(i,'/',dim(longtitude_table)[1]),collapse = '')
    print(message)
    if (!is.na(longtitude_table_data$min[i])){
      data_analysis = data_geo[longtitude_table_data$min[i]:longtitude_table_data$max[i],] 
      min_index = i
      max_index = i
      if (!is.na(longtitude_table_data$min[i-1])){
        min_index = i-1
      }
      
      if (!is.na(longtitude_table_data$max[i+1])){
        max_index = i+1
      }
      transaction_data = data_geo[longtitude_table_data$min[min_index]:longtitude_table_data$max[max_index],] 
      geo_sub = property_geo[longtitude_table$min[max(0,i-1)]:longtitude_table$max[min(i+1,sepNum)],]
      
      for (j in 1:dim(data_analysis)[1]){
        neighbor_index=abs( geo_sub$longitude-data_analysis$longitude[j])<=15000*mile &
          abs( geo_sub$latitude-data_analysis$latitude[j])<=19000*mile
        neighbor_number = sum(neighbor_index)
        transaction_index = abs( transaction_data$longitude-data_analysis$longitude[j])<=15000*mile &
          abs( transaction_data$latitude-data_analysis$latitude[j])<=19000*mile &
          transaction_data$month == data_analysis$month[j]
        
        
        area_effect_data = transaction_data[transaction_index,]
        if (dim(area_effect_data)[1]<=9){
          area_effect[k] = 0
        }else{
          index = remove_outlier(area_effect_data$logerror)
          if (length(index)<5){
            area_effect[k] = 0
          }else{
            area_effect_data = area_effect_data[index,]
            area_effect[k] =mean(area_effect_data$logerror)
          }
          
        }
        
        
        y_neighbornum[k]=neighbor_number
        
        y_tran[k] = sum(transaction_index)
        k = k+1
      }
    }
  }
  data$neighborNum = y_neighbornum
  data$transactionNum = y_tran
  data$area_effect = area_effect
  
  remove.vars <- c("longitude_dis")
  remove.idx <- which(colnames(data) %in% remove.vars)
  data <- data[,-remove.idx]
  return(data)
}

