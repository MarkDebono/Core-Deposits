### Importing and Viewing the data of monthly deposits

setwd("C:/") #set the working directory to the folder in which you save the file "Data.csv"
Deposits<-read.csv("Data.csv",sep=",",header=TRUE)

head(Deposits)
tail(Deposits)

### Plotting the monthly balances ###

library(ggplot2)
library(gridExtra)

gglist<-list()
for(i in 1:ncol(Deposits))
{gglist[[i]]<-local({i<-i;ggplot(Deposits,aes(x=seq(1:nrow(Deposits)),y=Deposits[,i]))+geom_line()+xlab("Month")+ylab("Amount")+ggtitle(paste("Customer",i))})}
grid.arrange(grobs=gglist, nrow = 2,ncol=3)

TotalDeposits<-data.frame(Month=seq(1:nrow(Deposits)),TotalDeposits=rowSums(Deposits))
ggplot(TotalDeposits,aes(x=Month,y=TotalDeposits))+geom_line()+ylab("Amount")+ggtitle("Total Deposits")


### Differencing the data ###

DepDiff<-apply(Deposits,2,diff)
head(DepDiff)

### The simulation ###

alpha<-0.95 #the level of significance associated with the core deposits
k<-24 #the number of months ahead for which we calculate the core deposits
iter_no<-100 #the number of iterations used

CoreDepositsAveraged<-rep(0,k)

for(iter in 1:iter_no)

{  
SimData<-Deposits[rep(nrow(Deposits),nrow(DepDiff)*4),]
rownames(SimData) <- 1:nrow(SimData)
SimData

CoreDeposits<-c()

for(m in 1:k)
{
for(i in 1:ncol(SimData))
{SimData[,i]<-SimData[,i]+sample(rep(DepDiff[,i],4))}
SimData[SimData<0]<-0 #Removing the negative balances

SimDataDash<-SimData
for(i in 1:ncol(SimDataDash))
{SimDataDash[,i][SimDataDash[,i]>Deposits[nrow(Deposits),][[i]]]<-Deposits[nrow(Deposits),][[i]]}

CoreDeposits<-c(CoreDeposits,quantile(rowSums(SimDataDash),1-alpha))
}


CoreDepositsAveraged<-(CoreDepositsAveraged*(iter-1)+CoreDeposits)/iter


}

### Output from Simulation ###

Results<-data.frame(Months.Ahead = seq(1,k),Core.Deposits=CoreDepositsAveraged)
Results
ggplot(Results,aes(x=Months.Ahead,y=Core.Deposits))+geom_line()+xlab("Months Ahead")+ylab("Amount")+ggtitle("Core Deposits")

for(i in 2:nrow(Results))
{if(Results$Core.Deposits[[i]]>Results$Core.Deposits[[i-1]]){Results$Core.Deposits[[i]]<-Results$Core.Deposits[[i-1]]}}

Results
ggplot(Results,aes(x=Months.Ahead,y=Core.Deposits))+geom_line()+xlab("Months Ahead")+ylab("Amount")+ggtitle("Core Deposits")

for (i in 2:length(CoreDepositsAveraged)){
  if(CoreDepositsAveraged[[i]]>CoreDepositsAveraged[[i-1]]){CoreDepositsAveraged[[i]]<-CoreDepositsAveraged[[i-1]]}
}

Results$Core.Deposits.Perc<-round(Results$Core.Deposits*100/rowSums(Deposits[nrow(Deposits),]),2)
Results
ggplot(Results,aes(x=Months.Ahead,y=Core.Deposits.Perc))+geom_line()+xlab("Months Ahead")+ylab("Percentage (%)")+ggtitle("Core Deposits Percentages")


#########
