#################################################################
## Marketing analytics                                         ##
## Lecture 2 Exercise in Market Response Models				         ##
## EuropeanSales Model                                    ##
##                                                             ## 
#################################################################

# free memory if need be
#rm(list = ls())
#gc()


# Get the working directory. If needed, you can set the working directory to another folder.
getwd()
setwd("C:\")


# Read the Data files from a directory  
EuropeanSalesData<-read.csv("EuropeanSales.csv",header=T)


#Show attributes  
attributes(EuropeanSalesData)

# draw Yearly sales
plot(EuropeanSalesData$GDPperHead,EuropeanSalesData$SalesPerCapita, ylab="GDPperHead", xlab="SalesPerCapita")
plot(EuropeanSalesData$GDPperHead,EuropeanSalesData$ComputerSales, ylab="GDPperHead", xlab="ComputerSales")
plot(EuropeanSalesData) 


#Correlation between attributes
cor(EuropeanSalesData$UnemploymentRate,EuropeanSalesData$SalesPerCapita)
cor(EuropeanSalesData$EducationSpending,EuropeanSalesData$SalesPerCapita) 
cor(EuropeanSalesData$GDPperHead,EuropeanSalesData$SalesPerCapita)
cor(EuropeanSalesData$Population,EuropeanSalesData$SalesPerCapita)

cor(EuropeanSalesData$UnemploymentRate,EuropeanSalesData$ComputerSales)
cor(EuropeanSalesData$EducationSpending,EuropeanSalesData$ComputerSales) 
cor(EuropeanSalesData$GDPperHead,EuropeanSalesData$ComputerSales)
cor(EuropeanSalesData$Population,EuropeanSalesData$ComputerSales)

#Correlation for all attributes
cor(EuropeanSalesData[,])


# Fit the Data to the model
model <- lm(SalesPerCapita ~ EducationSpending + GDPperHead, data=EuropeanSalesData)
summary(model)

#Model attributes and coefficients  
attributes(model)
model$coefficients



#Find the SalesPerCapita for EducationSpending 10 and GDPperHead 50000   
SalespcapitaED10GDP50000 <- model$coefficients[[1]]+model$coefficients[[2]]*10+model$coefficients[[3]]*50000

#SalespcapitaED10GDP50000 = 97918.37
SalespcapitaED10GDP50000


#A simpler Model without GDPperHead
model <- lm(SalesPerCapita ~ EducationSpending, data=EuropeanSalesData)
summary(model)



# differences between observed values and fitted values
residuals(model) 


# Show the plot of the model
#layout(matrix(1)) # one graph per page 
plot(model)


# Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(model)



# Compare Different Models
model1 <- lm(SalesPerCapita ~ EducationSpending + GDPperHead, data=EuropeanSalesData)
summary(model1)

model2 <- lm(SalesPerCapita ~ EducationSpending , data=EuropeanSalesData)
summary(model2)

# Fit the Data to the model
model <- lm(ComputerSales ~ Population, data=EuropeanSalesData)
summary(model)

#Model attributes and coefficients  
attributes(model)
model$coefficients

# Show the plot of the model
#layout(matrix(1)) # one graph per page 
plot(model)

#Find the ComputerSales for Population 50   
ComputerSalesPOP50 <- model$coefficients[[1]]+model$coefficients[[2]]*50

#ComputerSalesPOP50 = 4292.065
ComputerSalesPOP50