# TUGAS AKHIR ANALISIS DATA KATEGORIK
# Regresi Logistik Biner

titanic <- read.csv(file.choose(), header = TRUE, row.names = "PassengerId")   
#mengecek missing value
sapply(titanic, function(x) sum(is.na(x))) 
#panjang vektor tiap variabel
sapply(titanic, function(x) length(unique(x)))

#cek data
str(titanic)
#menghapus beberapa kolom yg tidak dipakai
titanic <- titanic[,-c(3,8,10)]
#mengubah bentuk data
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)  
#cek ulang beberapa datanya 
head(titanic)

## Cek Multikolinearitas ##
# Total Model
titanic.logit <- glm(Survived ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data = titanic, family = binomial)
library(car)
vif(titanic.logit) #terpilihlah Embarked,Sex dan Parch

## Statistika Deskriptif ##
summary(titanic) #notes: cuma diambil beberapa

## Pemilihan Model ##
# Seleksi Backward dengan pengaruh interaksi
# Model 1
fit <- glm(Survived ~ Sex*Parch*Embarked, data = titanic, family = binomial)
summary(fit)
# Cek signifikansi
drop1(fit, test = "Chisq") #Sex*SibSp*Embarked dibuang
# Model 2
fit2 <-glm(Survived ~ Sex*Parch + Sex*Embarked + Parch*Embarked, data = titanic, family = binomial)
# Cek signifikansi
drop1(fit2, test = "Chisq") #sex*embarked dibuang dan parch*embarked
# Model 3
fit3 <- glm(Survived ~ Embarked + Sex*Parch, data = titanic, family = binomial)
summary(fit3)
# Cek signifikansi
drop1(fit3, test = "Chisq") #Embarked + Sex + SibSp + SibSp*Sex
# AIC = 727.98

# Cek Ulang apakah proses tadi sudah benar
# Otomatis Backward
step(fit, test = "Chisq")
# Otomatis Forward
step(glm(Survived ~ 1, family = binomial), scope = ~SibSp*Embarked*Sex, direction = "forward", test = "Chisq")
#keduanya menghasilkan kesimpulan yg sama, model fit yeaaay!

# Uji LRT : G
# cara 1
library(pscl) 
pR2(fit4) 
qchisq(0.95, 3) #G2>Tabel, tolak H0
# cara 2
library(lmtest)
lrtest(fit4, fit) #p value > alpha, terima H0
qchisq(0.95, 3) #Chisq < Chitabel

# Uji Wald 
summary(fit3)
qnorm(0.05, 1)

# Uji Kesesuaian Model
# Hosmer Lemeshow
# H0 : model sesuai
# H1 : model tidak sesuai
library(ResourceSelection)
hoslem.test(titanic$Survived, fitted(fit4), g=10)
qchisq(0.95,8) #X2<Xtabel, terima H0

# Interpretasi

# Hasil Klasifikasi
table(true = titanic$Survived, pred = round(fitted(fit4)))
((370+188)/712)*100 #cakep lah
exp(coef())
# Odds Ratio
exp(coef(fit4))  
or <- fit4$fitted.values
odds.ratio <- fit4$fitted.values/(1-fit4$fitted.values)
head(cbind(Fitted = round(or,2), Odds = round(odds.ratio,2)))
