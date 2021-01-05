library(readxl)
kakao <- read_excel("D:/I/VII/Kapita Selekta Statistika/kakao.xlsx")

#mengecek missing value
sapply(kakao, function(x) sum(is.na(x))) 
#cek data
str(kakao)
#menghapus beberapa kolom yg tidak dipakai
kakao <- kakao[,-c(1:3,20)]
#mengubah bentuk data
kakao$hv <- as.integer(kakao$hv)
kakao$blk <- as.integer(kakao$blk)
kakao$wilt <- as.integer(kakao$wilt)
kakao$loss <- as.integer(kakao$loss)
kakao$can <- as.factor(kakao$can)
kakao$flu <- as.factor(kakao$flu)
kakao$flw <- as.factor(kakao$flw)
kakao$ty <- as.integer(kakao$ty)
kakao$sml <- as.integer(kakao$sml)
kakao$lrg <- as.integer(kakao$lrg)
kakao$maxt <- as.numeric(kakao$maxt)
kakao$mint <- as.numeric(kakao$mint)
kakao$midt <- as.numeric(kakao$midt)

#cek ulang beberapa datanya 
str(kakao)
head(kakao)

## Statistika Deskriptif ##
summary(kakao)
library(Hmisc)
describe(kakao)

## SELEKSI MODEL 
# Tahap 1: menghapus character
# Tahap 2: uji multikol
fit <- lm(hv ~ blk+wilt+loss+can+flu+flw+ty+sml+lrg+pcp+smi+lmi+maxt+mint+midt, data = kakao)
library(car)
vif(fit)
#terambil blk,wilt,loss,can,flu,flw,ty,sml,lrg,pcp,smi,maxt

# Tahap 3: uji korelasi
cor.test(kakao$hv, kakao$blk) #ambil
cor.test(kakao$hv, kakao$wilt) #ambil
cor.test(kakao$hv, kakao$loss) #ambil
regresi1 <- lm(hv ~ can, kakao)
summary(regresi1)
regresi2 <- lm(hv ~ flu, kakao)
summary(regresi2) 
regresi3 <- lm(hv ~ flw, kakao)
summary(regresi3)
cor.test(kakao$hv, kakao$ty) 
cor.test(kakao$hv, kakao$sml) #ambil
cor.test(kakao$hv, kakao$lrg) #ambil
cor.test(kakao$hv, kakao$pcp) 
cor.test(kakao$hv, kakao$smi) 
cor.test(kakao$hv, kakao$maxt) #ambil
#terambil blk,will,loss,sml,lrg,maxt

# Tahap 4: backward
fit2 <- lm(hv ~ blk+wilt+loss+sml+lrg+maxt, data = kakao)
step(fit2, test = 'F')
#terambil blk,wilt,sml,lrg,maxt

fit2 <- lm(hv ~ blk+wilt+loss+sml+lrg+maxt, data = kakao)
drop1(fit2, test = 'F')

fit3 <- lm(hv ~ blk+wilt+sml+lrg+maxt, data = kakao)
drop1(fit3, test = 'F')

fit4 <- lm(hv ~ blk+sml+lrg+maxt, data = kakao)
drop1(fit4, test = 'F')

## input data setelah di transformasi
transformasi <- read.csv("D:/I/VII/Kapita Selekta Statistika/transformasi.csv")
fix <- lm(hv ~ blk+sml+lrg+maxt, transformasi)
View(fix)

## Diagnosis model ##
resid <- fix$residuals # residual model
fv <- fix$fitted.values

## Cek Normalitas ##
# histogram
hist(resid)
# QQplot
qqnorm(resid)
# shapiro test
qqline(resid)
shapiro.test(resid)

## cek variansi konstan ##
# plot residual vs. fitted value
plot(resid,fv) # tersebar acak
library(lmtest)
bptest(fix, studentize = F, data = transformasi) 

## cek independent menggunakan durbin-watson ##
library(lmtest)
dwtest(fix, alternative = "two.sided") 

## Cek Multikolinearitas ##
# Total Model
library(car)
vif(fix) 

## Regresi Berganda ##
summary(fix)
