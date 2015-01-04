
stump=function(response, variable,d){
  ##make a matrix of the minimum and maximums of the predictors
  #we will use these as bounds to iterate over
  #minmax is a 2x(nuber of predictor) size matrix
  #the top row is mins and the bottom is maxs
  minmax=matrix(0,nrow=2,ncol=length(names(variable)))
  minmax[1,]=as.vector(unlist(apply(variable, 2,min)))
  minmax[2,]=as.vector(unlist(apply(variable, 2,max)))
  #Just make the response a numeric vector
  response=as.numeric(response)
  ##
  # Initialize a matrix of the best repsonse (error rate, feat)
  best.split=matrix(1,nrow=1,ncol=4)
  #Initialize a vector of best fit misclassified observations
  misclasified=vector()
  #A list for both
  ret.list=list()
  ##for every feature in the data set
  for(i in 1:length(names(variable))){
    #inicialize the breaks of the classifier
    #do 15 tests, this can be chaged
    breaks=seq(from=minmax[1,i]-1,to=minmax[2,i]+1,by=(minmax[2,i]-minmax[1,i])/15)
    #for each break
    for(j in 1:length(breaks)){
      #for each classification
      for(k in 1:2){
        if(k==1){
          class=-1} else{class=1}
        classif=as.vector(unlist(sapply(variable[i], function(x) breaks[j]>x)))
        #Change to 1 or -1
        #print(classif)
        classif=gsub(TRUE,class,classif)
        classif=as.numeric(gsub(FALSE,-1*class,classif))
        #print(classif)
        #Find error
        #We compare each classifcation with the response. A vecotor is generated such that
        #a correct match is 0 and an incorect match is 1. With that
        #we are able to multiple by D to weight the errors.
        compare.mat=matrix(c(classif,response),nrow=2,byrow=T)
        compare.mat=compare.mat[1,]+compare.mat[2,]
        compare.mat=gsub(0,1,compare.mat)
        compare.mat=gsub(-2,0,compare.mat)
        compare.mat=gsub(2,0,compare.mat)
        compare.mat=as.numeric(compare.mat)
        #print(compare.mat)
        error=sum(d*compare.mat)/sum(d)
        #print(error)
        if(error<best.split[1,1]){
          best.split=matrix(c(error,i,breaks[j],k),nrow=1,ncol=4)
          misclasified=compare.mat
          ret.list=c(best.split,misclasified,classif)
        }         
      }
    }
    
    
  }
  return(ret.list)  
}

#Function returns error from stump to use in alpha       
get.error=function(stump){
  return(stump[1])
}

#returns wrong/right binary 010101
last.stump=function(stump,response){
  return(stump[5:(4+length(response))])
}
#returns the actual last guess
last.guess=function(stump,response){
  return(stump[(5+length(response)):(4+2*length(response))])
}
#Calculate Alpha
alpha=function(error){
  return(.5*log((1-error)/(error),base=exp(1)))
}

english=function(stump,alpha){
  if(stump[4]==1){
    class=-1
  } else{
    class=1
  }
  cat("If X", stump[2], " is less than", stump[3], " then f(x)=", class,
      "\nThere is an error rate of e=", stump[1],
      "\nThere is an alpha of", alpha)  
}


#Last d is the last weights used
#last errors is the 0100101 vector
#error is the number from 
update.d=function(last.d,last.stump,error,alpha){
  new.d=rep(0,length=length(last.d))
  for(i in 1:length(last.d)){
    if(last.stump[i]==0){
      new.d[i]=(last.d[i]*exp(1)^(-1*alpha))
    } else{
      new.d[i]=(last.d[i]*exp(1)^(alpha))
    }
  }
  return(new.d/sum(new.d))
}
#This will tell us when to 
model.error=function(agg.Class.Est,response){
  sign.agg=sign(agg.Class.Est)
  return(sum(response!=sign.agg)/length(response))
  
}

adaboost=function(response, variable){
  #initial d
  agg.Class.Est=rep(0,length(response))
  d=rep((1/dim(variable)[1]),length=dim(variable)[1])
  for(i in 1:10){
    stump=stump(response, variable,d)
    #error from stump
    error=get.error(stump)
    #print(error)
    #pass error to alpha
    alpha=alpha(error)
    print(alpha)
    last.stump=last.stump(stump,response)
    last.guess=last.guess(stump,response)
    #print(last.guess)
    agg.Class.Est=agg.Class.Est+alpha*last.guess
    model.error=model.error(agg.Class.Est,response)
    eng=english(stump,alpha)
    print(eng)
    print(d)
    #print(sum(agg.Class.Est)/length(agg.Class.Est))
    print(paste("Model Error", model.error))
    if(model.error==0){break}
    d=update.d(d,last.stump,error,alpha)
  }
  

}

adaboost(dat[,1],dat[,2:3])



#####DATA#####
easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easydat.csv", header=T)
dat=easydat

ggplot(dat, aes(x1,x2,col=color))+scale_colour_manual(values=c("green", "red"))+geom_point(size=4)+ggtitle('Plot')

# ylab("x2 ")+xlab("x1")
#Change response to binary -1 1
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})


mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
mydata=mydata[,-4]
head(mydata)

#Change response to binary -1 1
mydata=within(mydata, {
  admit = gsub(0,-1,admit)  
})

adaboost(mydata[,1],mydata[,2:3])


ggplot(mydata, aes(gre,gpa,col=admit))+geom_point(size=2) +ggtitle('Plot')+
  geom_hline(aes(yintercept=3), colour="#990000", linetype="dashed")
#ylab("x2 ")+xlab("x1")+ggtitle('Plot')


##########Plotting example

adaboost(dat[,1],dat[,2:3])

#Step 1
ggplot(dat, aes(x1,x2,col=color))+geom_point(size=4)+ggtitle('Plot')+scale_colour_manual(values=c("darkgreen", "red"))
#step 2
easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easy2.csv", header=T)
dat=easydat
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})

ggplot(dat, aes(x1,x2,col=color,shape=wrong))+geom_point(size=4)+ggtitle('Plot')+scale_colour_manual(values=c("darkgreen", "red"))+
  geom_vline(aes(xintercept=1.3333), colour="#BB0000", linetype="dashed")+
  scale_shape_manual(values=c(16,8))
#step 3
easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easy3.csv", header=T)
dat=easydat
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})

ggplot(dat, aes(x1,x2,col=color,shape=wrong,size=weight))+geom_point()+ggtitle('Plot')+scale_colour_manual(values=c("darkgreen", "red"))+
  geom_vline(aes(xintercept=1.3333), colour="#BB0000", linetype="dashed")+
  scale_shape_manual(values=c(16,8))
#step 4
easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easy3.csv", header=T)
dat=easydat
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})

ggplot(dat, aes(x1,x2,col=color,shape=wrong,size=weight))+geom_point()+ggtitle('Plot')+scale_colour_manual(values=c("darkgreen", "red"))+
  geom_hline(aes(yintercept=1.02666), colour="#990000", linetype="dashed")+
  scale_shape_manual(values=c(16,8))
#step 5
easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easy4.csv", header=T)
dat=easydat
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})

ggplot(dat, aes(x1,x2,col=color,shape=wrong,size=weight))+geom_point()+ggtitle('Plot')+scale_colour_manual(values=c("darkgreen", "red"))+
  geom_hline(aes(yintercept=1.02666), colour="#990000", linetype="dashed")+
  scale_shape_manual(values=c(16,8))
#step 6
easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easy5.csv", header=T)
dat=easydat
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})

ggplot(dat, aes(x1,x2,col=color,shape=wrong,size=weight))+geom_point()+ggtitle('Plot')+scale_colour_manual(values=c("darkgreen", "red"))+
  geom_vline(aes(xintercept=.99), colour="#BB0000", linetype="dashed")+
  scale_shape_manual(values=c(16,8))
#step 7
easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easy7.csv", header=T)
dat=easydat
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})

ggplot(dat, aes(x1,x2,col=color,shape=wrong,size=weight))+geom_point()+ggtitle('Plot')+scale_colour_manual(values=c("darkgreen", "red"))+
  geom_vline(aes(xintercept=.99), colour="#BB0000", linetype="dashed")+
  scale_shape_manual(values=c(16,8))

