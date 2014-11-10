easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easydat.csv", header=T)
dat=easydat
ggplot(dat, aes(x1,x2,col=color, size=2, shape=color))+geom_point() + 
  ylab("x2 ")+xlab("x1")+ggtitle('Plot')

#Change response to binary -1 1
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})


}

# Fit the decission stump classifier. We will iterate over each feature variable

stump=function(response, variable){
  ##make a matrix of the minimum and maximums of the predictors
  #we will use these as bounds to iterate over
  #minmax is a 2x(nuber of predictor) size matrix
  #the top row is mins and the bottom is maxs
  minmax=matrix(0,nrow=2,ncol=length(names(variable)))
  minmax[1,]=as.vector(unlist(apply(variable, 2,min)))
  minmax[2,]=as.vector(unlist(apply(variable, 2,max)))
  ##
  # Initialize a matrix of the best repsonse (error rate, feat)
  best.split=matrix(1,nrow=1,ncol=4)
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
        classif=gsub(TRUE,class,classif)
        classif=as.numeric(gsub(FALSE,-1*class,classif))
        #Find error
        proportion.mistakes=as.numeric(table(response==as.numeric(classif))[1])/length(response)
        if(proportion.mistakes<best.split[1,1]){
          best.split=matrix(c(proportion.mistakes,i,breaks[j],k),nrow=1,ncol=4)
        }         
      }
    }
    
    
  }
  return(best.split)  
}
#Say what stump matrix means
english=function(stump){
  if(stump[1,4]==1){
    class=-1
  } else{
    class=1
  }
  cat("If X", stump[1,2], " is greater than", stump[1,3], " then f(x)=", class, "\nThere is an error rate of e=", stump[1,1])
  
}

#Calculate alpha
alpha=function(error){
  return(.5*log((1-error)/(error),base=exp(1)))
}


#Scratch
alpha(s[1,1])
s=stump(dat[,1],dat[,2:3])
english(stump(dat[,1],dat[,2:3]))
range(dat[,1])[2]-range(dat[,1])[1]
stump(dat[,1],dat[,2:3])

1:length(names(dat[,-1]))
minmax=matrix(0,nrow=2,ncol=length(names(dat[,-1])))


minmax[1,]=as.vector(unlist(apply(dat[,-1], 2,min)))
minmax[2,]=as.vector(unlist(apply(dat[,-1], 2,max)))
typeof(minmax)
dim(minmax)

breaks=seq(from=minmax[1,1]-1,to=minmax[2,1]+1,by=(minmax[2,1]-minmax[1,1])/15)
classy=as.vector(unlist(sapply(dat[,2], function(x) breaks[1]>x)))
class=1
classy[3]=T
replace(classy, classy==FALSE,class)
classy=gsub(FALSE,class,classy)
classy=as.numeric(gsub(TRUE,-1*class,classy))
c(1,-1,1,-1,1)==as.numeric(dat[,1])
as.numeric(table(c(1,-1,1,-1,1)==as.numeric(dat[,1]))[1])/length(dat[,1])
"There is an error rate of e=", stump[1,1]