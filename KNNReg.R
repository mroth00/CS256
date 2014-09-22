library(plyr)
library(dplyr)

###A few notes
#These are a few exceptions I found that would be hard to correct for
#1) Your training data and testing data must contain strickly more than one numeric variable and strickly more than one catagorical varible
# An error occures because the program expects matrix dimentions and not vectors
#2) Your training data and testing data must be sufficiently diverse. If the training data contains catagorical variables with factors,
# an error will occure if only part of the factors in the testing data appear in the training data. Larger data sets will not have to
# worry about this exception
#


#Input the data frame and the column number of the reponse variable
#This function standardizes the continuous data and makes factors numeric
KNN.Standard<-function(data,resp){
  #remove and store the column of responses
  x=data[,resp]
  data[,resp]=NULL
  #which data columns are factors and which are int, double, numeric
  #separte the factors from the numbers
  sep.num<-function(data){
    y=sapply(as.vector(data[1,]),class)
    #Get index's of factors
    if("factor" %in% y){
      sep.index=vector()
      z=1
      for(i in 1:length(y)){
        if(y[i]=="factor"){
          sep.index[z]=i
          z=z+1
        }
      }
      #Separate numeric from factors into two dataframes to normalize
      num.frame=data[,-sep.index]
      fact.frame=data[,sep.index]
    } else{
      num.frame=data
    }
    return(num.frame)
  }
  sep.fact<-function(data){
    y=sapply(as.vector(data[1,]),class)
    #Get index's of factors
    if("factor" %in% y){
      sep.index=vector()
      z=1
      for(i in 1:length(y)){
        if(y[i]=="factor"){
          sep.index[z]=i
          z=z+1
        }
      }
      #Separate numeric from factors into two dataframes to normalize
      num.frame=data[,-sep.index]
      fact.frame=data[,sep.index]
    } else{
      num.frame=data
    }
    return(fact.frame)
  }
  #num.frame only contains numeric types
  num.frame=sep.num(data)
  #fact.frame only contains factor types
  fact.frame=sep.fact(data)
  #Normalize the numbers xi=(xi-min)/(max-min)
  Normalize<-function(num.frame){
    #Max of colums
    x.max=as.numeric(apply(num.frame, 2,max))
    #min of columns
    x.min=as.numeric(apply(num.frame, 2,min))
    mat.max=matrix(rep(x.max,nrow(num.frame)),nrow=nrow(num.frame),byrow=T)
    mat.min=matrix(rep(x.min,nrow(num.frame)),nrow=nrow(num.frame),byrow=T)
    denom.stan=mat.max-mat.min
    numerator.stan=num.frame-mat.min
    standardized=numerator.stan/denom.stan
  }
  standardized=Normalize(num.frame)
  
  #Let's compute numeric distances of a normalized set
  #note that will we compute factor differences then sum over the rows and sqrt  
  numeric.dist<-function(training,testing){
    differ=list()
    for(j in 1:dim(testing)[1]){
      row.mat=matrix(rep(testing[j,],dim(training)[1]),
                     nrow=dim(training)[1],byrow=T)
      differ=cbind(differ,matrix(apply((training-data.frame(matrix(unlist(row.mat), nrow=dim(training)[1],byrow=T)))^2,1,sum)))
      differ=matrix(unlist(differ),nrow=dim(training)[1],byrow=F)
    }
    
    return(sqrt(differ))
  }
  
  #let's compute the factor distance
  fact.dist<-function(training,testing){
    dist.fact.mat=matrix(0,nrow=nrow(training),ncol=nrow(testing))
    for(hh in 1:dim(training)[2]){
      dist.factor=list()
      #Value feature table
      V.F.T=cbind(count(training[,hh])[2],count(testing[,hh])[2])
      row.sums=apply(V.F.T,1,sum)
      row.sums=matrix(rep(row.sums,2),ncol=2)
      #Value difference Table
      V.D.T=(V.F.T/row.sums)
      for(qq in 1:length(testing[,hh])){
        #testing row
        vdt.row.test=which(levels(testing[,hh])==(testing[qq,hh]))
        for(rr in 1:length(training[,hh])){
          #training row
          vdt.row.train=which(levels(training[,hh])==(training[rr,hh]))
          dist.factor=c(dist.factor,abs(V.D.T[vdt.row.train,1]-V.D.T[vdt.row.test,1])+abs(V.D.T[vdt.row.train,2]-V.D.T[vdt.row.test,2]))
        }
      }
      dist.fact.mat=dist.fact.mat+matrix(unlist(dist.factor),nrow=dim(training)[1],byrow=F)
    }
    return(dist.fact.mat)
    
    
  }
  #sum the two distance matrix's 
  total.dist=function(numeric.dist,factor.dist){
    total=numeric.dist+factor.dist
    return(total)
  }
  
  #For the factors We can't do the same thing with a normalzied vector
  #but we can make a distance vector given the vector you want to find the distance with
  
  ##Let's optimize k, split the data 
  #Assign 75% of the data to be training data
  #Drop an observation to make a compatible training set
  split=cbind(standardized,fact.frame,x)
  if(dim(split)[1]%%4==1){
    split=split[-dim(split)[1],]
  } else if(dim(split)[1]%%4==2){
    split=split[-((dim(split)[1]-1):(dim(split)[1])),]                            
  } else if (dim(split)[1]%%4==3){
    split=split[-((dim(split)[1]-2):(dim(split)[1])),]
  }
  store.responses=split[,dim(split)[2]]
  split[,dim(split)[2]]=NULL
  split.row=.75*dim(split)[1]
  split.training=split[1:split.row,]
  split.test=split[(split.row+1):dim(split)[1],]
  split.train.num=split.training[,1:dim(standardized)[2]]
  split.test.num=split.test[,1:dim(standardized)[2]]
  split.train.fact=split.training[,(dim(standardized)[2]+1):(dim(split)[2])]
  split.test.fact=split.test[,(dim(standardized)[2]+1):(dim(split)[2])]
  #Numeric distance
  differ.num=numeric.dist(split.train.num,split.test.num)
  differ.fact=fact.dist(split.train.fact,split.test.fact)
  differ=total.dist(differ.num,differ.fact)
    
  
  #THIS MATRIX CONTAINS DISTANCES AND THE RESPONSE VALUE
  distance=cbind(differ,store.responses[1:dim(differ)[1]])
#   dist.mat=matrix(unlist(distance),nrow=.75*dim(split)[1],byrow=F)
  #stored actual reponses  
  y.hat=store.responses[(dim(differ)[1]+1):length(store.responses)]
  #here we are finding a k that minimizes SSE 
  #This is as suggested in Chapter 7.6 of Making Sense of Data by Glenn J. Myatt
   error.vector=list()
   for(t in 1:(dim(distance)[2]-1)){
    temp1=cbind(distance[,t],distance[,dim(distance)[2]])
    temp2=temp1[order(temp1[,1]),]
    for(k in 1:dim(temp2)[1]){
      temp3=temp2[1:k,]
      #This if statement is needed because temp3[,2] is a vector when k=1
      if(k==1){
        y.bar=temp3[2]
      } else if(k>1){
        y.bar=(sum(temp3[,2])/k)
      }
       error=(y.hat[t]-y.bar)^2
       error.vector=c(error.vector,error,k)
    }
  }
  #Sum error terms for each k. Make data types more friendly
  tochange=matrix(error.vector,ncol=2,byrow=T)
  tochange2=data.frame(matrix(unlist(tochange),ncol=2,byrow=F))
  #SSE for each value of k
  SEE.mat=ddply(tochange2,.(X2),sum)
  best.k=SEE.mat$X2[which.min(SEE.mat$V1)]
  #NOw Predict new observation
  #Normalize new data
  #new.standardized=Normalize(newdata)
#   for(jj in 1:dim(new.standardized)[1]){
#     
#   }
#   
# 
 }

#Example

set.seed(5)
#sample data set
test=data.frame(col1=sample(1:20,200,replace=T),
                col2=sample(31.5:50,200,replace=T),
                col3=sample(101:150,200,,replace=T),
                col4=sample(c('a','b','c'),200,replace=T),
                col5=sample(c('d','e','f'),200,replace=T))


head(test)
#Test the function
test2<-KNN.Standard(test,3)
test2

