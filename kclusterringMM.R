dat1 = read.csv("demographic_user_data_clean (3).csv", na.strings = ".",header = TRUE) #taking input
#dat1
x=nrow(dat1)
x
set.seed(20)          #setting a fixed seed 
for (i in 12:12)  #finding the columns that are numeric in nature in order to remove the outliers
  if(sapply(dat1[1,], is.numeric)){
    e = dat1[,i]
    train_ts <- ts(e, frequency=12)
    y <- ts(e, frequency=12)
    remove_outliers <- function(x, na.rm = TRUE, ...) {
      qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
      H <- 1.5 * IQR(x, na.rm = na.rm)
      y <- x
      y[x < (qnt[1] - H)] <- NA
      y[x > (qnt[2] + H)] <- NA
      y
    }
    y <- remove_outliers(y)     
    dat1[,i]=y                     #all outliers removed
    
  }
#dat1                   

dat1<- sapply(dat1, as.character)    #Removing the "NA" values as these are not computed by function
dat1[is.na(dat1)] <- " "

dat1=as.data.frame(dat1)
#                                 #putting blank values in place of "NA" 


library(NbClust)
for (i in 3:13)   #we will start from a particular column (column from where we have to give it in the function,it wpuld be defined from which index we have to input columns)
  dat1[,i]<- scale(as.numeric(dat1[,i]))   
dat1             #scaled dataset (scaling only for the columns that are to be passed in the function)

nb <- NbClust(dat1[,3:12], distance = "euclidean", min.nc = 3,index = "hartigan" ,
              max.nc = 6, method = "kmeans")     #it calculates the optimal no of clusters
nb
a=nb$Best.partition#it gives the require plot as well as the most
a[200]
for(i in 1:x)
  dat1[i,'cluster']=a[i]
dat1[,'cluster']

write.csv(dat1, file="cluster2.csv")