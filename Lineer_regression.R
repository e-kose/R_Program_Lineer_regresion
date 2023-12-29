setwd("/home/ekose/Desktop/rprograming")
getwd()
df <- read.csv("insurance.csv")
head(df)
sapply(df, class)
names(df) <- c("yaş","cinsiyet","vücut_kitle","çocuk_sayisi","sigara_kullanimi","bölge","fatura")
sayisal_sütünlar <- c("yaş","vücut_kitle","çocuk_sayisi","fatura")
sayisal_olmayan <- c("cinsiyet","sigara_kullanimi","bölge")

summary(df)
print(sum(is.na(df)))
df <- na.omit(df)
print(sum(is.na(df)))
duplicated_rows <- df[duplicated(df), ]
duplicated_rows
install.packages("dplyr")
library(dplyr)
cleaned_df <- distinct(df)
hist(df$fatura, xlab="Fatura", ylab="Frekans", col=rainbow(10), ylim=c(0,500))
install.packages("nortest")
library(nortest)
ad.test(df$fatura)
df[sayisal_olmayan] <- lapply(df[sayisal_olmayan],as.factor)
sapply(df,class)
summary(df[sayisal_olmayan])

barplot(table(df$cinsiyet), xlab="cinsiyet", col=rainbow(1), ylim=c(0,700), ylab="Frekans")
boxplot(df$fatura~df$cinsiyet, data=df)

barplot(table(df$sigara_kullanimi), xlab="Kullanım", col=rainbow(2), ylim=c(0,1500), ylab="Frekans")
boxplot(fatura~df$sigara_kullanimi, data=df)

barplot(table(df$bölge), xlab="Bölge", col=rainbow(1), ylim=c(0,500), ylab="Frekans")
boxplot(fatura~df$bölge, data=df)

summary(df[sayisal_sütünlar])

hist(df$yaş, xlab="Yaş",ylab="Frekans",col=rainbow(11), ylim=c(0,300))
boxplot(df$fatura~df$yaş, data=df)

hist(df$vücut_kitle, xlab="kitle_indeksi",ylab="Frekans",col=rainbow(11), ylim=c(0,500))
boxplot(df$fatura~df$vücut_kitle, data=df)

hist(df$çocuk_sayisi, xlab="Çocuk sayısı",ylab="Frekans",col=rainbow(11), ylim=c(0,700),xlim = c(0,5))
boxplot(df$fatura~df$çocuk_sayisi, data=df)

korelasyon_matrix <- cor(df[sayisal_sütünlar])
korelasyon_matrix

install.packages("corrplot")
library(corrplot)
corrplot(korelasyon_matrix,method = "number",col =rainbow(80))

library("fastDummies")
df_dummy <- dummy_cols(df, select_columns = c("cinsiyet", "sigara_kullanimi","bölge"))
sapply(df_dummy, class)
head(df_dummy)

df_dummy <- subset(df_dummy, select = -c(cinsiyet,cinsiyet_female, sigara_kullanimi,sigara_kullanimi_no
                                         ,sigara_kullanimi_,bölge,bölge_northeast,bölge_))
head(df_dummy)
df_dummy <- df_dummy[,c(1,2,3,5,6,7,8,9,4)]
head(df_dummy)
sapply(df_dummy,class)
df_dummy$yaş <-as.numeric(df_dummy$yaş)
df_dummy$çocuk_sayisi <-as.numeric(df_dummy$çocuk_sayisi)
df_dummy$cinsiyet_male <-as.factor(df_dummy$cinsiyet_male)
df_dummy$sigara_kullanimi_yes <-as.factor(df_dummy$sigara_kullanimi_yes)
df_dummy$bölge_northwest <-as.factor(df_dummy$bölge_northwest)
df_dummy$bölge_southeast <-as.factor(df_dummy$bölge_southeast)
df_dummy$bölge_southwest <-as.factor(df_dummy$bölge_southwest)
sapply(df_dummy,class)


##Normalizasyon
df_scaled <- as.data.frame(scale(df_dummy[c(1,2,3)]))
df_scaled <- cbind(df_scaled,df_dummy[c(4,5,6,7,8,9)])
str(df_scaled)

#lm(liner model)
model_1 <- lm(formula = fatura~yaş + vücut_kitle + çocuk_sayisi + cinsiyet_male + sigara_kullanimi_yes 
              +bölge_northwest + bölge_southwest + bölge_southeast , data = df_scaled )
summary(model_1)

# anlamlılık katsayıalrına göre model
model_2 <- lm(formula = fatura~yaş + vücut_kitle + çocuk_sayisi + sigara_kullanimi_yes, data = df_scaled)
summary(model_2)
#r-kare Ve düzeltilmiş r-kare 
 ## model_1 de ve model ikide r-kare ve düzeltilmiş r-kare farkı yok bundan doalyı model_1 den çıkardığımız verilerin 
  # veriyi açıklamada etkisi yok
# Düzeltilmiş R-kare, R-kare'nin düzeltilmiş bir versiyonudur ve modeldeki bağımsız değişken
# sayısını dikkate alır. Düzeltilmiş R-kare, eklenen her bağımsız değişkenin R-kare'ye olan katkısını kontrol eder.
# Bu sayede, modeldeki gereksiz değişkenler eklenip eklenmediğini değerlendirebiliriz.

#vıf değerleri
install.packages("car")
library(car)
vif_values <- vif(model_2)
vif_values
barplot(vif_values, main = "VIF Değerleri",ylim =c(0,1.20) , col = "red")
anova(model_2)

#normallik testi
par(mfrow=c(1,2))
hist(model_2$residuals)
shapiro.test(model_2$residuals)
# p-value < 0.05 küçük olduğu için hatalar normal dağılmamıştır

#varyans homojenliği
install.packages('lmtest')
library("zoo")
library("lmtest")
bptest(model_2)
#p-values 0.05 küçük oludğu için heteroskedastisite vardır(hatalar sabit varyanslı değldir)
weights <- 1 / residuals(model_2)^2
wls_model <- lm(fatura ~ yaş + vücut_kitle + çocuk_sayisi  + sigara_kullanimi_yes,
                weights = weights, data = df_scaled)
bptest(wls_model)
#p-value 0.05 büyük olduğu içi hatalar sabit varyanslıdır

#modelin hata değerlerini görmek için
hatvalues(wls_model)
#modelin öngörülerinden elde edilen hat değerlerinin ortalamasının iki katından daha büyük olan aykırı gözlemleri bulma
which(hatvalues(wls_model)>2*mean(hatvalues(wls_model)))
#ortalama kare hatasının toplamı
mse <- sum(wls_model$residuals ^ 2) / wls_model$df
mse
#modelin öngörülerinden elde edilen hat değerlerinin ortalamasının iki katından daha büyük olan aykırı gözlemleri bulma
which(hatvalues(wls_model)>2*mean(hatvalues(wls_model)))
stand_res <- wls_model$residuals / mse ^ 0.5
stand_res
which(abs(stand_res) > 2)
par(mfrow=c(2,2))
plot(wls_model)

plot(hatvalues(wls_model), stand_res)
abline(h=c(-2,2), v=4/35)

