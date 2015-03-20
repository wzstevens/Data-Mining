#course CS513 project
#project group: movie rating
#author: Zhe Wang
#item-based collaborative filtering

rm(list=ls())
#reference plyr package
library(plyr)

#Read the data set
train<-read.csv(file="C:/Users/WangZhe/Desktop/sam.csv",header=FALSE)
names(train)<-c("item","user","pref") 

#Calculate the user list
usersUnique<-function(){
  users<-unique(train$user)
  users[order(users)]
}

#Calculate the movie list
itemsUnique<-function(){
  items<-unique(train$item)
  items[order(items)]
}

# List of users
users<-usersUnique() 

# list of movies
items<-itemsUnique() 

#Setting up a good list of index
index<-function(x) which(items %in% x)
data<-ddply(train,.(user,item,pref),summarize,idx=index(item)) 
write.csv(data,file = "C:/Users/WangZhe/Desktop/data.csv");
#coorcurrence matrix
cooccurrence<-function(data){
  n<-length(items)
  co<-matrix(rep(0,n*n),nrow=n)
  for(u in users){
    #All of U in the corresponding data, 
    #the original table the user read 
    #the item number out to independence idx
    idx<-index(data$item[which(data$user==u)])
    m<-merge(idx,idx)
    for(i in 1:nrow(m)){
      co[m$x[i],m$y[i]]=co[m$x[i],m$y[i]]+1
    }
  }
  return(co)
}

#recommendation algorithm
recommend<-function(udata=udata,co=coMatrix,num=0){
  n<-length(items)
  
  # all of pref
  pref<-rep(0,n)
  pref[udata$idx]<-udata$pref
  # User ratings matrix
  userx<-matrix(pref,nrow=n)
  # score matrix * co-occurrence matrix
  r<-co %*% userx
  # Recommend sort results
  r[udata$idx]<-0
  idx<-order(r,decreasing=TRUE)
  topn<-data.frame(user=rep(udata$user[1],length(idx)),item=items[idx],val=r[idx])
  
  # Num before taking a recommendation results
  if(num>0){
    topn<-head(topn,num)
  }
  
  #return result
  return(topn)
}

#generate coorccurrence matrix
co<-cooccurrence(data) 

#Calculation results recommended
recommendation<-data.frame()
for(i in 1:length(users)){
  udata<-data[which(data$user==users[i]),]
  recommendation<-rbind(recommendation,recommend(udata,co,10)) 
} 
#normalize result set
rec<-data.frame(recommendation$val)
mmnorm <-function(x,minx,maxx) 
{z<-((x-minx)/(maxx-minx))
 return(z) 
}
recnorm<-mmnorm(rec,min(rec),max(rec))
recnorm<-mmnorm(rec,5,20)
recommendresult<-cbind(recommendation$user,recommendation$item,recnorm)