install.packages("pastecs")
install.packages("ggplot2")
library(readxl)
library(stats4)
library(stats)
library(pastecs)
library(ggplot2)

BankChurn <- read_excel("OneDrive - LEAD University/Estadística multivariada/BankChurn.xlsx")

View(BankChurn)

BankChurn<- data.frame(BankChurn)
stat.desc(BankChurn)

CreditScore=BankChurn$CreditScore
Tenure=BankChurn$Tenure
Age=BankChurn$Age

varCreditScore=as.data.frame(stat.desc(CreditScore))
varCreditScore

varAge=as.data.frame(stat.desc(Age))
varAge

varTenure=as.data.frame(stat.desc(Tenure))
varTenure

par(mfrow=c(1,2))
hist(CreditScore ,main= 'Histograma Credit Score', xlab = 'CreditScore', ylab = 'Score',probability = TRUE,breaks = 100)

hist(Age ,main= 'Histograma Age', xlab = 'Age', ylab = 'Years',probability = TRUE,breaks = 100)
hist(Tenure ,main= 'Histograma Tenure', xlab = 'Tenure', ylab = 'Years',probability = TRUE,breaks = 100)

W=Age+Tenure

varW=as.data.frame(stat.desc(W))
varW=as.data.frame(W)
varW
par(mfrow=c(2,2))
hist(W,main= 'Histograma  W Años en total', xlab = 'W', ylab = 'Densidad',probability = TRUE,breaks = 100)

var(W)
mean(Age)
mean(Tenure)

ggplot(varW, aes(x = Tenure, y = Age)) + geom_point()
ggplot(varW, aes(x = Age, y = Tenure)) + geom_point()

cor(Age, Tenure, method = "pearson")

cov(Age, Tenure, method = "pearson")
