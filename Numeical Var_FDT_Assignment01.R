#ITC 255-Fall 2022-Assignment 02

#uploading the dataset

irisData = read.csv(url('https://gist.githubusercontent.com/curran/a08a1080b88344b0c8a7/raw/0e7a9b0a5d22642a06d3d5b9bcbad9890c8ee534/iris.csv'))
names(irisData)

#Writing a function for getting the FDT for the Sepal_width variable

FDT = function(a){
  absloutFrequency = table(a)
  relativeFrequency = round(prop.table(absloutFrequency)*100,2)
  cumlativeFrequency = cumsum(relativeFrequency)
  
  iris_FDT = cbind(absloutFrequency,relativeFrequency,cumlativeFrequency)
  
  
  return(iris_FDT)
}
#Calling the function

FDT(irisData$sepal_width)


#numerical variable

newCat = c ()
summary(irisData)

for(a in 1:length(irisData$sepal_width) ) {
  if(irisData$sepal_width[a]<=2){
    newCat[a]="Small Digit"
  }else if(irisData$sepal_width[a]>2 &  irisData$sepal_width[a]<=3){
    newCat[a]="medium Digit "
  }else {
    newCat[a]="large Digit"
  }
}

    newVar = cbind(irisData$petal_length,newCat)
    
    head(irisData)
    View(newVar)

    #The distribution table
FDT(newCat)



x=c(2,-1,0)

y=c()

for (i in 1:3){
  
  
  y[k]=x[k]+1
  
}

print(y)

x=seq(1:10)
y= !(x>=2) 
y
