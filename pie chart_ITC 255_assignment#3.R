#Pie chart assignment_Wooldridge, 401KSUBS package#


install.packages("dplyr")
library(dplyr)

install.packages("wooldridge")
library(wooldridge)

myData = read.csv('C:\\Users\\USER\\Downloads\\401ksubs.csv')
head(myData)

ageGroup=c()
for(k in 1:length(myData$age)){
  if(myData$age[k]<35){
    ageGroup[k]="Young"
  } else if (myData$age[k] >=35 & myData$age[k]<50) {
    ageGroup[k]="Middle age"
  } else {
    ageGroup[k]="Old"
  }
}

cbind(myData$age, ageGroup)
myData$age_group = ageGroup

head(myData)

age_FDT = c()
FDT = function(a){
  absloutFrequency = table(a)
  relativeFrequency = round(prop.table(absloutFrequency)*100,2)
  cumlativeFrequency = cumsum(relativeFrequency)
  
  age_FDT = cbind(absloutFrequency,relativeFrequency,cumlativeFrequency)
  
  
  return(age_FDT)
}
 
age_tabel = FDT(myData$age_group)[,2]
age_tabel

pie (age_tabel, col = c ("red", "pink", "yellow") , main = "smoker Distribution", cex = 0.6)
