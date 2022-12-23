#Statistical data Analysis - ITC 255 ___Dr. Assadullah Jawid___
#Individual Project-Fall 2022
#Hadia Haded Mubashir - 37285- 2022.12.22

#For this project , we have two variables on which we have generated random data to use as our sample
#1. X = Time spent studying by each student- Numeric (Independent Variable)
#2. Y= Securing Grad A  by students - Binary (Dependent Variable),


# Mean of Time spent studying = 4
# Stranded deviation = 1.5

# we are generating random data and use it as a sample of our population using rbinom and rnorm as such:

securing_A = rbinom (1000,1, prob = 0.2)
time_spent_studying_per_day = abs(round(rnorm (1000, 4, 1.5),2))

#now we will be running the regression model on the sample data that we have gathered from 1000 people
#as the X and Y are linearly accosiated, we use the linear function to express the relationship. Y = a+bx
# we are looking for p-value, ahat, and bhat to make inferences .


stu_grade_estimation = data.frame(Grade = securing_A, Studying_time = time_spent_studying_per_day)

liner_model = lm(stu_grade_estimation$Studying_time~stu_grade_estimation$Grade)
summary (liner_model)

#OBSERVATIONS:

#The population model is as follow:  y = a + bX 
# Y = 4.03 -0.17X
# The coefficient of X,  b = -0.17 which is less than 0
# bhat<0 indicates that there is an indirect affect of 
#  X (amount of time spent studying) on Y (Securing Grade A)

#H0 says b=0  meaning that there is no effect Time spent studying on Securing grad A
#H1 says b>0 meaning that there is direct relationship between  Time spent studying and Securing A 

#Making inference: alpha= 0.05 while p-value = 0.1454
#p-value>Alpha

#Hence we reject H1 in favor of H0
# ahat = 4.0 : Y = 4 - 0.17X

#_____________________________________________________________________________

#Two way table (FDT)

absloutFrequency = table(stu_grade_estimation)
relativeFrequency = round(prop.table(absloutFrequency)*100,2)
cumlativeFrequency = cumsum(relativeFrequency)

stu_Grade_A_FDT = cbind(absloutFrequency,relativeFrequency,cumlativeFrequency)
head(stu_Grade_A_FDT)


#______________________________________________________________________________

#Adding new colum Status that is simply a qualitative form of Binary variable Securing_A

status =c()
for(a in 1:length(stu_grade_estimation$Grade) ) {
  if(stu_grade_estimation$Grade[a]==0){
    status[a]="Not Secured"
  }else {
    status[a]="Secured"
  }
}

stu_grade_estimation =  cbind(stu_grade_estimation, status)
head(stu_grade_estimation)
#_______________________________________________________________________________

#Visual representation of our variables:

#Visualizing Binary variabel Secuing grade A through Bar chart.

library(ggplot2)
tableGrade=table(stu_grade_estimation$Grade)
tableGrade=as.data.frame(tableGrade)
colnames(tableGrade)=c('Grade', 'Count')

g0=ggplot(tableGrade, aes(x=Grade, y=Count, fill=Grade))
g0+geom_bar(stat='identity')+
  theme_classic()+
  theme(legend.position = '')+
  theme(axis.title.x = element_text(),
        axis.title.y = element_text(),
        plot.title = element_text(face = 'bold', hjust=.3))+
  ggtitle('Securing Grade A Distribution')+
  geom_text(aes(label=Count), vjust=2)+
  scale_fill_manual(values=c('#EAA697', '#E3EC98'))
ggsave('gradeBar.png')
 
#The Bar Graph indicated that frequency of "Not secuing grade A"(0)
#is dramatically higher than the "securing grade A"(1)

#_____________________________________________________________
       

#Visualizing Time spent studying through histogram
       
g0=ggplot(stu_grade_estimation, aes(x=stu_grade_estimation$Studying_time))
g0+geom_histogram(bins = 10, fill='#EAA697', colour=1)+
theme_light()+
theme(plot.title = element_text(face = 'bold',
hjust = .5), 
  axis.title.x = element_text(), 
  axis.title.y = element_text())+
         ggtitle('Time Distribution')+
         xlab('Time spend studying')+
         ylab('Frequency')+
         geom_vline(xintercept = 4,
                    linetype='dashed',
                    color='black', 
                    size=1)
    ggsave('TimeDistHist.png')
    
#The histogram representing the time spend studying by students.
#Histogram is the best way to represent our data since the Variable Time Spend Studying has a continuous normal distribution.
    
#our observation is that the Majority of the students spend almost 3.5-4.5 hours per day studying.
#The red dashed line is the x-intercept( mean = 4)
# we have an almost symmetric histogram which has  same identical distribution in both sides
    