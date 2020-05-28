databaseconnection=function(){
  library(RODBC)
  cn<-odbcDriverConnect(connection ="Driver={SQL Server};server=OCHIUDDIN;database=mytask2dodb;Uid=sa;Pwd=P;")
}


cn=databaseconnection()
mytask2dodata<-c()

totalusers<-cbind("f2a979f6-0c11-419b-85a4-7e24bf8f6d6a",
                  "02E74779-C554-4E4B-A4AD-F48EFEBE3D24",
                  "9701D8A8-C8D4-4AE3-93CC-E7AD07331CA4",
                  "993fb1e1-9888-4fd0-94fc-7f6a2fd221d7",
                  "ecb86322-5572-46d6-9b21-b7601dd666f3")

for (j in 1:ncol(totalusers)) {
  tmp<-cbind("select ActivityTime as activity,(EstimatedTime-TotalSpentTime) as estimation_error,DATEDIFF(DAY,EstimatedEndDate,EndDate) as taskisclosed from TK_ActivityLog,TK_Task,TK_Status where TK_Status.StatusId=TK_Task.StatusId and TK_Task.TaskId=TK_ActivityLog.TaskId and TK_Status.Status='Closed' and UserId=",totalusers[j]," order by activity desc")
sql<-paste(tmp,collapse = "'")
print(sql)
datafromsql=sqlQuery(cn,sql)
if(nrow(datafromsql) != 0){
for (i in 1:nrow(datafromsql)) {
  datafromsql$activityafterdays[i]<- as.numeric(as.Date(datafromsql[i,1],format="%m/%d/%Y")-as.Date(datafromsql[i+1,1],format="%m/%d/%Y")) 
  
  if(!is.na(datafromsql$taskisclosed[i]) && datafromsql$taskisclosed[i]<0){
    datafromsql$taskisclosed[i]<-0
  }
  
  if(!is.na(datafromsql$estimation_error[i]) && !is.na(datafromsql$taskisclosed[i]) && !is.na(datafromsql$activityafterdays[i]))
  {
   mytask2dodata<-rbind(mytask2dodata,datafromsql[i,])
  }
}
  
}

}

mytask2dodata$estimation_error<-abs(mytask2dodata$estimation_error)
mytask2dodata$activityafterdays<-abs(mytask2dodata$activityafterdays)

mytask2dodata=mytask2dodata[,c(2,3,4)]

kc<-kmeans(mytask2dodata,3)


mytask2dodata$class<-''
mytask2dodata$class[as.numeric(which(kc$cluster==3))]<-'bad'
mytask2dodata$class[as.numeric(which(kc$cluster==2))]<-'medium'
mytask2dodata$class[as.numeric(which(kc$cluster==1))]<-'good'



write.csv(mytask2dodata, file = "mytask2do.csv")

mytask2dodata$class[which(mytask2dodata$class=='medium' & mytask2dodata$taskisclosed>=40)]<-'bad'
mytask2dodata[which(mytask2dodata$class=='medium' & mytask2dodata$taskisclosed>=40),]
mytask2dodata$class[which(mytask2dodata$class=='good' & mytask2dodata$taskisclosed>12)]<-'medium'

require(C50)
mytask2dodatatrain=mytask2dodata[,-c(4)]
mytask2dodatatrain_target=mytask2dodata[,c(4)]
mytask2dodatatrain_target=as.factor(mytask2dodatatrain_target)
c5m1<-C5.0(mytask2dodatatrain,mytask2dodatatrain_target)
