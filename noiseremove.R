myfunc=function(){
testdata=read.csv("test.csv")
print(testdata)
}

myknn=function(ptrain,ptest,pcl,pk){
  require(class)
  knnm1=knn(train=ptrain,test =ptest ,cl=pcl,k=pk)
  return(knnm1)
}

getmode<-function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}


handlemissingvalues<-function(datamodel){
  
  columnnames<-colnames(datamodel)
  
  for (name in columnnames) {
    classtype<-class(datamodel[,name])
    updatevalues<-which(is.na(datamodel[,name]))

  if(classtype=="numeric" || classtype=="integer"){
    
   columnmean<-mean(datamodel[,name],na.rm = TRUE)
  
   datamodel[c(updatevalues),name]<-columnmean

  }else if(classtype=="factor"){
  
    datamodel[c(updatevalues),name]<-getmode(datamodel[,name])
    
  }
    
  }
  return(datamodel)
}


nearestneighbour<-function(sourcedata,predictinstance){

   columnnames<-colnames(sourcedata)
   
   i<-1
   i1<-1
   distance<-10000000
   nearestrow<-c()
   columnintegerindex<-c()
   
   for(name in columnnames){
   
   columntype<-class(sourcedata[,name])
   
   if(columntype=="numeric" || columntype=="integer"){
   columnintegerindex[i1]<-i
   i1<-i1+1
   }
   
   
   i<-i+1
   }
   print(columnintegerindex)
   
  for (i in 1:nrow(sourcedata)) {
    
    firstrow=sourcedata[i,]
    firstrow[,which(is.na(firstrow))]<-0
    predictinstance[,which(is.na(predictinstance))]<-0
    
   tempdistance<-sum(abs(firstrow[,columnintegerindex]-predictinstance[,columnintegerindex]))
   if(tempdistance!=0 && tempdistance < distance){
     nearestrow=sourcedata[i,]
     distance=tempdistance
   }
  }
  return(nearestrow)
}



three=read.csv("iris.csv")
gp=runif(nrow(three))
three=three[order(gp),]

traindata=three[1:129,c(1,2,3,4)]
testdata=three[130:150,c(1,2,3,4)]
traintarget=three[1:129,c(5)]
testtarget=three[130:150,c(5)]

knnresult=myknn(traindata,testdata,traintarget,5)
print(table(testtarget,knnresult))
headvalue=head(three$sepallength)
for (a in headvalue) {
  print(paste("my=",a))
}
testdata=read.csv("test.csv")
print(testdata)
updatethree<- handlemissingvalues(testdata)
print(updatethree)

nearrow=nearestneighbour(testdata,testdata[1,])
testdata[1,]=nearrow
print(head(testdata))
