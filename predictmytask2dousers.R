databaseconnection=function(){
  library(RODBC)
  cn<-odbcDriverConnect(connection ="Driver={SQL Server};server=OCHIUDDIN;database=mytask2dodb;Uid=sa;Pwd=Pass@123;")
}

cn=databaseconnection()

userid="f2a979f6-0c11-419b-85a4-7e24bf8f6d6a"

tmp<-cbind("SELECT temp.activity,(EstimatedTime-TotalSpentTime) AS estimation_error,DATEDIFF(DAY,EstimatedEndDate,EndDate) AS taskisclosed,TotalSpentTime as totalspenttime FROM (select MAX(ActivityTime) as activity,TaskId from TK_ActivityLog where UserId=",userid," group by TaskId) temp,TK_Task,TK_Status WHERE TK_Status.StatusId=TK_Task.StatusId AND TK_Task.TaskId=temp.TaskId AND TK_Status.Status='Closed' UNION SELECT temp.activity,(EstimatedTime-TotalSpentTime) AS estimation_error,DATEDIFF(DAY,EstimatedEndDate,GETDATE()) AS taskisclosed,TotalSpentTime FROM (select MAX(ActivityTime) as activity,TaskId from TK_ActivityLog where UserId=",userid," group by TaskId) temp,TK_Task,TK_Status WHERE TK_Status.StatusId=TK_Task.StatusId AND TK_Task.TaskId=temp.TaskId AND TK_Status.Status='Open' AND DATEDIFF(DAY,EstimatedEndDate,GETDATE())>0 ORDER BY activity DESC")

sql<-paste(tmp,collapse = "'")

print(sql)
datafromsql=sqlQuery(cn,sql)

if(nrow(datafromsql) != 0){
  
  good=0
  medium=0
  bad=0
  
  for (i in 1:nrow(datafromsql)) {
    
    taskisclosed=NULL
    estimation_error=NULL
    activityafterdays=NULL
    totalspenttime=NULL

    estimation_error<-datafromsql$estimation_error[i]
    totalspenttime<-datafromsql$totalspenttime[i]
    
    activityafterdays<- as.numeric(as.Date(datafromsql[i,1],format="%m/%d/%Y")-as.Date(datafromsql[i+1,1],format="%m/%d/%Y")) 
    if(!is.na(datafromsql$taskisclosed[i]) && datafromsql$taskisclosed[i]<0){
      taskisclosed<-0
    }else{
      taskisclosed<-datafromsql$taskisclosed[i]
    }
    
    
    if(!is.na(estimation_error) && !is.na(taskisclosed) && !is.na(activityafterdays) && !is.na(totalspenttime))
    {
      estimation_error=abs(estimation_error)
      activityafterdays=abs(activityafterdays)
      totalspenttime=abs(totalspenttime)
      
      
    if(taskisclosed <= 9 && estimation_error <= 11 && activityafterdays <= 2 && totalspenttime<=27.5){
          good=good+1
    }else if(taskisclosed <= 9 && estimation_error <= 11 && activityafterdays <= 2 && totalspenttime>27.5){
          bad=bad+1
          }
    else if(taskisclosed <= 9 && estimation_error <= 11 && activityafterdays > 2 && activityafterdays <= 4){
           good=good+1
             }
    else if(taskisclosed <= 9 && estimation_error <= 11 && activityafterdays > 2 && activityafterdays > 4 && activityafterdays<=9){
      medium=medium+1
    }else if(taskisclosed <= 9 && estimation_error <= 11 && activityafterdays > 2 && activityafterdays > 4 && activityafterdays>9){
          bad=bad+1
    }else if(taskisclosed <= 9 && estimation_error > 11 && totalspenttime <= 40){
       medium=medium+1
    }
   else if(taskisclosed <= 9 && estimation_error > 11 && totalspenttime > 40){
        bad=bad+1
    }else if(taskisclosed > 9 && taskisclosed <= 29){
       medium=medium+1
       }
     else if(taskisclosed > 9 && taskisclosed > 29){
        bad=bad+1
        }
      
      
      
      
    }
    
  }
  
  n=nrow(datafromsql)
  rgood=(100*good)/n
  rmedium=(100*medium)/n
  rbad=100-rgood-rmedium
  
  print(paste("Good Performed=",rgood,"%"))
  print(paste("Medium Performed=",rmedium,"%"))
  print(paste("Bad Performed=",rbad,"%"))
  
  
  
}

