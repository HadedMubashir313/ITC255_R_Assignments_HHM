###### ITC 255_ Fall 2022_DPLYR package assignment#####3

install.packages("dplyr")
library(dplyr)

install.packages("wooldridge")
library(wooldridge)

myData = read.csv('C:\\Users\\USER\\Downloads\\401ksubs.csv')
head(myData)

str(myData)
category_1 = filter(myData,(age<40 & fsize<3))
head(category_1)
category_2 = filter(myData, age %in% c(20,30))
names(myData)
category_3 = (arrange(myData, desc(fsize), age))
head(category_3)

#select is used to create a separate dataset with specified colmns, or to bring modification in the location of colums
newDataSet= select(myData, c(fsize,male))
head(newDataSet)        

#the function "everything" will bring the col written before it in the begining followed by other cols
D2 = select( myData, marr,everything())
head(D2)
D3 = select (myData, male:nettfa)
head(D3)

# we use the sign negation "-"  to exclude desired cols
D4 = select(myData, -(fsize:pira))
head(D4)

#to rename cols, we use rename function
modification = rename(myData, Age=age)

#to generate new variables from existing variables in dataset
head(mutate(myData, total = nettfa+inc))
summarise (myData, mean(age))


ageGroup=c()
for(k, in 1:lenght(myData$age)){
  if(myData$age[k]<35){
    ageGroup[k]="Young"
  } else if (myDataset$age[k] >=35 & myDataset$age[k]<50) {
    ageGroup[k]="Middle age"
  } else {
    ageGroup[k]="Old"
  }
}
myData$new = ageGroup
category_4 = group_by(myData,age )
p
# pull is used to extract a variable as a vector
pull(myData,age)
#sample_n function draw a random sample of specific size form dataset 
category_5 = sample_n(myData,10)
cat5
