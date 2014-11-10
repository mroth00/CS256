easydat <- read.csv("C:/Users/mike/Dropbox/School/SJSU/Fall 2014/CS Stat/Project/easydat.csv", header=T)
dat=easydat
ggplot(dat, aes(x1,x2,col=color, size=2, shape=color))+geom_point() + 
  ylab("x2 ")+xlab("x1")+ggtitle('Plot')

#Change response to binary -1 1
dat=within(dat, {
  color = gsub("red",1,color)
  color = gsub("green",-1,color)
})

#Error funtion
error=function(incorect,total){
  return(incorect/total)
}

# Fit the decission stump classifier. We will iterate over each feature variable

stump=function(response, variable){
  #make a matrix of the minimum and maximums of the predictors
  #we will use these as bounds to iterate over
  #minmax is a 2x(nuber of predictor) size matrix
  #the top row is mins and the bottom is maxs
  minmax=matrix(0,nrow=2,ncol=length(names(variable)))
  minmax[1,]=as.vector(unlist(apply(variable, 2,min)))
  minmax[2,]=as.vector(unlist(apply(variable, 2,max)))
  #for every feature in the data set
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
        as.vector(unlist(apply(variable, 2,min)))
      }
      variable[,1]
    }
    
    
  }
    
}

#Scratch
range(dat[,1])[2]-range(dat[,1])[1]
stump(dat[,1],dat[,2:3])

1:length(names(dat[,-1]))
minmax=matrix(0,nrow=2,ncol=length(names(dat[,-1])))
minmax[1,]=as.vector(unlist(apply(dat[,-1], 2,min)))
minmax[2,]=as.vector(unlist(apply(dat[,-1], 2,max)))
typeof(minmax)
dim(minmax)