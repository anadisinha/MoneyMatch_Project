library(data.table)
library(foreach)
data1 = fread("CSV_Trade1.csv", na.strings = ".",header = TRUE,nrow=300000) #taking input

#data1 <- as.data.frame(data) 
c=nrow(data1)
u=0
for (i in 1:c){
  u=u+1
  if(data1[i,'hr']==9 & data1[i,'mnts']==15){
    break
  }
}
data1=data1[-(1:(u-1)), ]
data1
b=nrow(data1)
data1$seconds = substring(toString(data1$time),6,16)
data1[1,'transaction type']="buy"

foreach(i = 2:b) %do% {
  if(i%%100 == 0)
    print(i)
  k=0
  for(j in 1:(i-1)){
    if((data1[i,'TradePrice'])>(data1[(i-j),'TradePrice'])){
      data1[i,'transaction type']="buy"
      break
    }else if((data1[i,'TradePrice'])<(data1[(i-j),'TradePrice'])){
      data1[i,'transaction type']="sell"
      break
    }else{
      data1[i,'transaction type']=data1[(i-1),'transation type']
    }
  
      
    
  }
}
foreach(i = 1:(b-1)) %do% {
 
  for(j in (i+1):b){
    if((data1[i,'TradePrice'])>(data1[j,'TradePrice'])){
      data1[i,'transaction type1']="buy"
      break
    }else if((data1[i,'TradePrice'])<(data1[j,'TradePrice'])){
      data1[i,'transaction type1']="sell"
      break
    
    }else{
     
    }
  }
}  

write.csv(data1, file = "MyData75.csv")





# write.csv(data1, file = "MyData2.csv")





# data1
# data1[9956,5]
# write.csv(data1, file = "MyData1.csv")
# getwd()
# #cleaning the input data
# #data1[1,1]
# library(lubridate)
# data1[2,5]
# data1[1,8]+data1[2,8]
# a=toString(data1[2,5])
# a
# z=substr(a, 3, 4)
# z
# for (i in 1:10){
#   data1[i,'second']=substring(toString(data1[i,5]),6,16)
#   
# }
# data1
# as.numeric(data1[2,21])+as.numeric(data1[3,21])