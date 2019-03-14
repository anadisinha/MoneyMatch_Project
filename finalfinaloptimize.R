data= read.csv("Storeallocation.csv", na.strings = ".",header = TRUE) #taking input
data1 <- as.data.frame(data)           #cleaning the input data
data1[is.na(data1)]<- 0
data1
a=levels(data1$Part..)                  #calculating the various levels in the PART column
len=length(a)                           #length of the levels
k=0
j=0
xx=0
xxx=0
w=c()
w[1]=0
df1=data.frame()                                          #sorting the dataframe wrt part
dat1=data.frame()
for (z in 1:length(a)){
  for(i in 1:length(data1$Part..)){
    if(data1[i,'Part..']==a[z]){
      j=j+1
      df1[j,'Forecast']=data1[i,'Forecast']
      df1[j,'Current.Stock']=data1[i,'Current.Stock']
      df1[j,'Reqt']=data1[i,'Reqt']
      df1[j,'Stock.Situation']=data1[i,'Stock.Situation']
      w[z+1]=j                                                #calculating the index of the various levels in the complete dataframe.            
    }
    
  }    
}    
print(df1)
print(w)

for(i in 1:len){                                       #extractung the particular part of the dataframe depending on a
  xx=(w[i]+1)                                          #depending on the particular part and feeding it
  xxx=w[i+1]
  dat1=df1[xx:xxx,]

  myfunction <- function(dat1 ){
    efor=c()
    ereq=c()             
    ecur=c()
    sfor=c()
    scur=c()
    sreq=c()
    eid=c()
    sid=c()
    cm=c()
    df1=data.frame()
    df2=data.frame()
    df3=data.frame()
    j=1
    k=1
    #neglecting right columns
    for (i in 1:length(dat1$Stock.Situation))         #making vectors for excess stocks
      if(dat1[i,"Stock.Situation"]=="Excess"){
        efor[j]=dat1[i,"Forecast"]
        ereq[j]=dat1[i,"Reqt"]
        ecur[j]=dat1[i,"Current.Stock"]
        eid[j]=dat1[i,"Store"]
        j=j+1
      }else if(dat1[i,"Stock.Situation"]=="Short"){   #making vectors for short stocks  
        sfor[k]=dat1[i,"Forecast"]
        sreq[k]=dat1[i,"Reqt"]
        scur[k]=dat1[i,"Current.Stock"]
        sid[k]=dat1[i,"Store"]
        k=k+1
      }
    
    a=length(efor)                                    #calculating length of excess   
    b=length(sfor)                                    #calculating length of short stocks    
    a*b                                               #total no of variables 
    v=c()                                             #toal no of constrainsts would be a+b
    u=c()                                             #with the conditions that each variable is greater than zero and each shoukld be an integer
    f=c()                                                
    d=c()
    z=c()
    w=c()
    n=c()
    # for (i in 1:(a*b))                                
    #   n[i]="integer"
    
    for (i in 1:b)                                   # making vectors that would be use in writing the constrainsts
      v[i]=1
    v
    for (i in 1:(a*b-b))                            
      u[i]=0
    #for (i in 1:a)
    z=append(u, values=v, after=b)                                                                                                              
    for (i in 1:((b*a)-a))
      f[i]=0
    z=f
    for (i in 1:(a*b))
      w[i]=0
    lprec <- make.lp(0, (a*b))                                  #defining linear programming         
    lp.control(lprec, sense="min")                              #writing the sense of the LP whether it is max or min type 
    set.type(lprec, columns=1:(a*b), type ="integer")           #defining types of variables  
    set.bounds(lprec, lower = w, upper = NULL, columns = 1:(a*b))#setting bounds
    
    # for (i in 1:(a*b))
    #   cm[i]=1
    for (i in 1:(a*b))            #defining cost of tranferring to be 1 for each transporation from excess to short points
      cm[i]=1
    set.objfn(lprec,cm)           #setting the objective function  
    # for (i in 1:length(efor))
    #   c=()
    #add.constraint(lprec, c(0, 78.26, 0, 2.9), ">=", 92.3)
    k=-1
    for (i in 1:a)                #writing constraints  
      add.constraint(lprec,append(u,values=v,after=b*(i-1)),"=",(ecur[i]-efor[i]))
    #for (i in 1:length(sfor))
    for (i in 1:b){
      z=f
      k=k+1
      for (j in 1:a)
        z=append(z, 1, after=(b*(j-1)+k))
      add.constraint(lprec,z,"<",sreq[i])    #writing 2nd set of constraints 
    }
    lprec
    solve(lprec)                              #solving and getting variables of the linear programming
    get.variables(lprec)
    get.objective(lprec)
    get.constraints(lprec)
    for (i in 1:(a*b))
      df1[i,'get variables']=get.variables(lprec)[i]
    df1[1,'get objective']=get.objective(lprec)[1]
    for (i in 1:(a+b))
      df1[i,'get constraints']=get.constraints(lprec)[i]
    
    return(df1)
  }
  myfunction1 <- function(dat1 ){
    efor=c()
    ereq=c()             
    ecur=c()
    sfor=c()
    scur=c()
    sreq=c()
    eid=c()
    sid=c()
    cm=c()
    df1=data.frame()
    j=1
    k=1
    #neglecting right columns
    for (i in 1:length(dat1$Stock.Situation))         #making vectors for excess stocks
      if(dat1[i,"Stock.Situation"]=="Excess"){
        efor[j]=dat1[i,"Forecast"]
        ereq[j]=dat1[i,"Reqt"]
        ecur[j]=dat1[i,"Current.Stock"]
        eid[j]=dat1[i,"Store"]
        j=j+1
      }else if(dat1[i,"Stock.Situation"]=="Short"){   #making vectors for short stocks  
        sfor[k]=dat1[i,"Forecast"]
        sreq[k]=dat1[i,"Reqt"]
        scur[k]=dat1[i,"Current.Stock"]
        sid[k]=dat1[i,"Store"]
        k=k+1
      }
    
    a=length(efor)                                    #calculating length of excess   
    b=length(sfor)                                    #calculating length of short stocks    
    a*b                                               #total no of variables 
    v=c()                                             #toal no of constrainsts would be a+b
    u=c()                                             #with the conditions that each variable is greater than zero and each shoukld be an integer
    f=c()                                                
    d=c()
    z=c()
    w=c()
    n=c()
    # for (i in 1:(a*b))                                
    #   n[i]="integer"
    
    for (i in 1:b)                                   # making vectors that would be use in writing the constrainsts
      v[i]=1
    v
    for (i in 1:(a*b-b))                            
      u[i]=0
    #for (i in 1:a)
    z=append(u, values=v, after=b)                                                                                                              
    for (i in 1:((b*a)-a))
      f[i]=0
    z=f
    for (i in 1:(a*b))
      w[i]=0
    lprec <- make.lp(0, (a*b))                                  #defining linear programming         
    lp.control(lprec, sense="min")                              #writing the sense of the LP whether it is max or min type 
    set.type(lprec, columns=1:(a*b), type ="integer")           #defining types of variables  
    set.bounds(lprec, lower = w, upper = NULL, columns = 1:(a*b))#setting bounds
    
    # for (i in 1:(a*b))
    #   cm[i]=1
    for (i in 1:(a*b))            #defining cost of tranferring to be 1 for each transporation from excess to short points
      cm[i]=1
    set.objfn(lprec,cm)           #setting the objective function  
    # for (i in 1:length(efor))
    #   c=()
    #add.constraint(lprec, c(0, 78.26, 0, 2.9), ">=", 92.3)
    k=-1
    for (i in 1:a)                #writing constraints  
      add.constraint(lprec,append(u,values=v,after=b*(i-1)),"=",(ecur[i]-efor[i]))
    #for (i in 1:length(sfor))
    for (i in 1:b){
      z=f
      k=k+1
      for (j in 1:a)
        z=append(z, 1, after=(b*(j-1)+k))
      add.constraint(lprec,z,">",sreq[i])    #writing 2nd set of constraints 
    }
    lprec
    solve(lprec)                              
    get.objective(lprec)
    get.constraints(lprec)
    for (i in 1:(a*b))
      df1[i,'get variables']=get.variables(lprec)[i]
    df1[1,'get objective']=get.objective(lprec)[1]
    for (i in 1:(a+b))
      df1[i,'get constraints']=get.constraints(lprec)[i]
  
    return(df1)
  }
df2=myfunction1(dat1)[2]
df3=myfunction(dat1)[2]
df3[1,1]
df2[1,1]
k=1
if(df2[1,1]>df3[1,1]){
  s=myfunction(dat1)
  for (tt in 1:length(df2[,1]))
    s[tt,'Part']=a[i]
  
  s<- sapply(s, as.character)    #Removing the "NA" values as these are not computed bu function
  
  s[is.na(s)] <- " "
  
  s=as.data.frame(s)
  s  
  
  print(s) 
  mat = s[,,i]
  form = sprintf('Particular_Part_Transportation_%s.csv', i)                #writing the data to csv files
  write.csv(mat, file = form)
}else{
  s=myfunction1(dat1)
  for (tt in 1:length(df2[,1]))
   s[tt,'Part']=a[i]
  
  s<- sapply(s, as.character)    #Removing the "NA" values as these are not computed bu function
  s[is.na(s)] <- " "
  
  s=as.data.frame(s)
  s
  
  print(s)
  mat = s[,,i]
  form = sprintf('Particular_Part_Transportation_%s.csv', i)
  write.csv(mat, file = form)                      
  }
}

  
  
