
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
          ret.list=c(best.split,misclasified)
        }         
      }
    }
    
    
  }
  return(ret.list)  
}

d=rep((1/dim(dat[,2:3])[1]),length=dim(dat[,2:3])[1])
length(stump(dat[,1],dat[,2:3],d))
       
get.error=function(stump){
  return(stump[1])
}


last.stump=function(stump){
  return(stump[5:length(stump)])
}
alpha=function(error){
  return(.5*log((1-error)/(error),base=exp(1)))
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

adaboost=function(response, variable){
  #initial d
  d=rep((1/dim(variable)[1]),length=dim(variable)[1])
  for(i in 1:10){
    stump=stump(response, variable,d)
    #error from stump
    error=get.error(stump)
    #print(error)
    #pass error to alpha
    alpha=alpha(error)
    print(alpha)
    last.stump=last.stump(stump)
    print(d)
    d=update.d(d,last.stump,error,alpha)
  }
  

}

adaboost(dat[,1],dat[,2:3])

get.misclassifieds(stump(dat[,1],dat[,2:3],d))
alpha(get.misclassifieds(stump(dat[,1],dat[,2:3],d)))


