cereal <- read.csv(file.choose(), header=TRUE)
#mengecek missing value
sapply(cereal, function(x) sum(is.na(x))) 
#cek data
str(cereal)
#mengubah bentuk data
cereal$manufacturer <- as.factor(cereal$manufacturer)
cereal$calories <- as.integer(cereal$calories)
cereal$proteins <- as.integer(cereal$proteins)
cereal$fat <- as.integer(cereal$fat)
cereal$sodium <- as.integer(cereal$sodium)
cereal$sugar <- as.integer(cereal$sugar)
cereal$potassium <- as.integer(cereal$potassium)

#cek ulang beberapa datanya 
str(cereal)
head(cereal)

# Uji Statistika Deskriptif
library(tidyverse)
library(pastecs)
#G
G <- filter(cereal, manufacturer == "G")
summary(G)
stat.desc(G, basic = F)
#Kellogs
K <- filter(cereal, manufacturer == "K")
summary(K)
stat.desc(K, basic = F)
#Q
Q <- filter(cereal, manufacturer == "Q")
summary(Q)
stat.desc(Q, basic = F)

# Uji Multikol
model <- glm(manufacturer ~ calories+proteins+fat+sodium+fiber+carbohydrates+sugar+potassium, data = cereal, family = binomial)
library(car)
vif(model) 
#calories,fiber,carbohydrates,sugar,potassium > 10

#Uji Kenormalan Data
cereal1 <- cereal[,-c(1)]
library(MVN)
mvn(data = cereal1, multivariatePlot = 'qq')
cereal2 <- mvn(cereal1, multivariateOutlierMethod = "adj", showNewData = TRUE)
cereal2 <- cereal2$newData

# ANALISIS KOMPONEN UTAMA
library(DT)
library(factoextra)

#standarisasi data
cereal_standarisasi <- scale(x = cereal2)
#matriks ragam-peragam
cereal_cov <- cov(cereal_standarisasi)
#menghitung vektor eigen dan nilai eigen
cereal_eigen <- eigen(cereal_cov)

#membentuk fungsi
cereal.pca <- cereal2
pca <- prcomp(cereal.pca, center = T, scale. = T)
#jumlah KU
summary(pca)
#persamaan AKU
pca$rotation

#plot nilai eigen
plot(pca, type = "l")
fviz_pca(pca)

#prediksi
prediksi <- predict(pca, newdata = cereal.pca)
cerealku <- prediksi[,1:3]

#Uji Kenormalan Data Setelah AKU
library(MVN)
mvn(data = cerealku, multivariatePlot = 'qq')
cereal3 <- mvn(cerealku, multivariateOutlierMethod = "adj", showNewData = TRUE)
cereal3 <- cereal3$newData
mvn(data = cereal3, multivariatePlot = 'qq')

# DISKRIMINAN ANALISIS
library(DT)
library(MVN)
library(MASS)
library(biotools)
diskriminan <- read.csv(file.choose(), header=TRUE)

# Cek Multivariate Normal
mvn(data = diskriminan[,2:4], multivariatePlot = 'qq') #cross-check
# Cek matriks ragam-peragam antara kategori
boxM(data = diskriminan[,2:4], grouping = diskriminan[,1])
#perbedaan rata-rata
m <- manova(formula = cbind(diskriminan$PC1, diskriminan$PC2, diskriminan$PC3) ~ diskriminan$manufacturer)
summary(object = m, test = 'Wilks')


diskriminan2 <- read.csv(file.choose(), header=TRUE)
# Cek Multivariate Normal
mvn(data = diskriminan2[,2:4], multivariatePlot = 'qq') #cross-check
# Cek matriks ragam-peragam antara kategori
boxM(data = diskriminan2[,2:4], grouping = diskriminan2[,1])
#perbedaan rata-rata
diskriminan2$manufacturer <- as.factor(diskriminan2$manufacturer)
m <- manova(formula = cbind(diskriminan2$PC1, diskriminan2$PC2, diskriminan2$PC3) ~ diskriminan2$manufacturer)
summary(object = m, test = 'Wilks')

dependen <- cbind(diskriminan2$PC1, diskriminan2$PC2, diskriminan2$PC3)
model1 <- manova(dependen ~ diskriminan2$manufacturer)
summary(model1)


#membagi data
set.seed(1234)
train.index <- sample(seq(nrow(diskriminan2)), size = floor(0.8 * nrow(diskriminan2)), replace = F)
data.train <- diskriminan2[train.index, ]
data.test <- diskriminan2[-train.index, ]

#membentuk fungsi
linear <- lda(formula = manufacturer ~., data = data.train)
linear

#prediksi
prediksi <- predict(object = linear, newdata = data.test)
prediksi
table(actual = data.test$manufacturer, predicted = prediksi$class)

#APER
aper <- ((1)/(3+1+1))*100
aper
