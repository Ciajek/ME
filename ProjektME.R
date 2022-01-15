rm(list=ls())

# Instalowanie pakietów ---------------------------------------------------

install.packages("AER")
install.packages("gridExtra")
install.packages("stargazer")
install.packages("lmtest")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("sandwich")
install.packages("tseries")

# Wgrywanie pakietów i danych---------------------------------------------------

library(gridExtra)
library(AER)
library(stargazer)
library(lmtest)
library(dplyr)
library(ggplot2)
library(sandwich)
library(tseries)
library(tidyverse)
library(caret)

setwd("D:/POBIERANIE/Downloads")
DATA=read.csv('final2.csv',sep=";")

# Obróbka danych ----------------------------------------------------------

N = nrow(DATA)
DATA$air.pollution2 = DATA$air.pollution^2
DATA$gini.index2 = DATA$gini.index^2
DATA$life.expectancy2 = DATA$life.expectancy^2
DATA$employment.industry2 = DATA$employment.industry^2
DATA$health.expenditure2 = DATA$health.expenditure^2
DATA$ldeath.rate = log(DATA$death.rate)
DATA$lair.pollution = log(DATA$air.pollution)
DATA$lhealth.expenditure = log(DATA$health.expenditure)
DATA$llife.expectancy = log(DATA$life.expectancy)
DATA$lgini.index = log(DATA$gini.index)

# Wizualizacja danych -----------------------------------------------------

  #zbiór histogramów

hist.death = ggplot(DATA, mapping = aes(death.rate)) + 
  geom_histogram( colour = "black", fill = "white")
hist.gini = ggplot(DATA, mapping = aes(gini.index)) + 
  geom_histogram( colour = "black", fill = "white")
hist.life = ggplot(DATA, mapping = aes(life.expectancy)) + 
  geom_histogram( colour = "black", fill = "white")
hist.employ = ggplot(DATA, mapping = aes(employment.industry)) + 
  geom_histogram( colour = "black", fill = "white")
hist.health = ggplot(DATA, mapping = aes(health.expenditure)) + 
  geom_histogram( colour = "black", fill = "white")
hist.lhealth = ggplot(DATA, mapping = aes(lhealth.expenditure)) + 
  geom_histogram( colour = "black", fill = "white")
hist.air = ggplot(DATA, mapping = aes(air.pollution)) + 
  geom_histogram( colour = "black", fill = "white")

  #print histogramów

plot(hist.death)
plot(hist.gini)
plot(hist.life)
plot(hist.employ)
plot(hist.health)
plot(hist.lhealth)
plot(hist.air)


  #histogramy na jednym princie

grid.arrange(hist.death, hist.gini, hist.life, hist.employ, hist.air, hist.lhealth, top = "Histogramy zmiennych")

  #zbiór wykresów liniowych

death.gini <- ggplot(DATA, aes(gini.index, death.rate)) +
  geom_line() +
  geom_line(data = DATA, aes(), colour = 'red', size = 0.5)

death.life <- ggplot(DATA, aes(life.expectancy, death.rate)) +
  geom_line() +
  geom_line(data = DATA, aes(), colour = 'red', size = 0.5)

death.employ <- ggplot(DATA, aes(employment.industry, death.rate)) +
  geom_line() +
  geom_line(data = DATA, aes(), colour = 'red', size = 0.5)

death.air <- ggplot(DATA, aes(air.pollution, death.rate)) +
  geom_line() +
  geom_line(data = DATA, aes(), colour = 'red', size = 0.5)

death.health <- ggplot(DATA, aes(health.expenditure, death.rate)) +
  geom_line() +
  geom_line(data = DATA, aes(), colour = 'red', size = 0.5)

  #print wykresów liniowych

plot(death.gini)
plot(death.life)
plot(death.employ)
plot(death.air)
plot(death.lhealth)

  #Wykresy liniowe na jednym princie

grid.arrange(death.gini, death.life, death.air, death.employ, death.health)

  #zbiór wykresów punktowych

death.gini.p <- ggplot(DATA, aes(gini.index, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5)

death.life.p <- ggplot(DATA, aes(life.expectancy, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5)

death.employ.p <- ggplot(DATA, aes(employment.industry, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5)

death.air.p <- ggplot(DATA, aes(air.pollution, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5)

death.lhealth.p <- ggplot(DATA, aes(lhealth.expenditure, death.rate)) +
  geom_point() + 
  geom_point(data = DATA, aes(), colour = "red", size = 0.5)

death.health.p <- ggplot(DATA, aes(health.expenditure, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5)

  #print wykresów punktowych

plot(death.gini.p)
plot(death.life.p)
plot(death.employ.p)
plot(death.air.p)
plot(death.lhealth.p)

  #Wykresy punktowe na jednym princie

grid.arrange(death.gini.p, death.life.p, death.air.p, death.employ.p, death.health.p)

# Modelowanie -------------------------------------------------------------

  #model3

model3=lm(death.rate~lhealth.expenditure +
            gini.index + life.expectancy +
            employment.industry + air.pollution,
          data = DATA)

  #model4

model4=lm(death.rate~lhealth.expenditure +
            lgini.index + life.expectancy +
            employment.industry + lair.pollution,
          data = DATA)

  #podsumowanie model3

summ.model3 <- summary(model3)
AIC.model3 <- AIC(model3)
BIC.model3 <- BIC(model3)
coef(model3)
print(summ.model3)
adj.r2.m3 <- 0.3032
coef.m3 <- summ.model3$coefficients
wsp.m3 <- c(AIC.model3, AIC.model3, adj.r2.m3)


  #podsumowanie modelu4

summ.model4 <- summary(model4)
AIC.model4 <- AIC(model4)
BIC.model4 <- BIC(model4)
coef(model4)
print(summ.model4)
adj.r2.m4 <- 0.2821
coef.m4 <- summ.model4$coefficients
wsp.m4 <- c(AIC.model4, AIC.model4, adj.r2.m4)


  #tabela porownawcza kryteriów i R2

df.wsp <- rbind(wsp.m3, wsp.m4)
rownames(df.wsp) <- c("Model3", "Model4")
colnames(df.wsp) <- c("AIC", "BIC", "adj. R2")
print(df.wsp)

# Walidacja krzyżowa ------------------------------------------------------


  #podział danych

set.seed(123)
training.samples <- DATA$death.rate %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- DATA[training.samples, ]
test.data <- DATA[-training.samples, ]

  #model1 - odpowiednik modelu3

model <- lm(death.rate~ lhealth.expenditure +
              gini.index + life.expectancy +
              employment.industry + air.pollution, data = train.data)

  #predykcje modelu1

predictions <- model %>% predict(test.data)
df.model1 <- data.frame( R2 = R2(predictions, test.data$death.rate),
            RMSE = RMSE(predictions, test.data$death.rate),
            MAE = MAE(predictions, test.data$death.rate))

  #model2 - odpowiednik modelu4

model2 <- lm(death.rate~ lhealth.expenditure +
              lgini.index + life.expectancy +
              employment.industry + lair.pollution, data = train.data)

  #predykcje modelu2

predictions <- model2 %>% predict(test.data)
df.model2 <- data.frame( R2 = R2(predictions, test.data$death.rate),
            RMSE = RMSE(predictions, test.data$death.rate),
            MAE = MAE(predictions, test.data$death.rate))

  #tabelka porownawcza ze wspolczynnikami 

df.comp <- rbind(df.model1, df.model2)
rownames(df.comp) <- c("model1", "model2")
print(df.comp)


# Test RESET i White'a----------------------------------------------------------

DATA$ehat = model3$residuals
DATA$ehat2 = model3$residuals^2
plot(DATA$gini.index, DATA$ehat2)
plot(DATA$air.pollution, DATA$ehat2)


  #Test RESET

reset(model3, type = "fitted")

  #Test WHite

test.white=lm(ehat2 ~ air.pollution2 + gini.index2 + health.expenditure2 + 
                employment.industry2 + life.expectancy2 + air.pollution +
                gini.index + health.expenditure + employment.industry + 
                life.expectancy + I(air.pollution*gini.index) + 
                I(air.pollution*health.expenditure) +
                I(air.pollution*employment.industry) + 
                I(air.pollution*life.expectancy) +
                I(gini.index*life.expectancy) + 
                I(gini.index*health.expenditure) + 
                I(gini.index*employment.industry) +
                I(employment.industry*life.expectancy) + 
                I(employment.industry*health.expenditure) + 
                I(health.expenditure*life.expectancy)
              ,data=DATA)

summary(test.white) 

LM=N*summary(test.white)$r.squared # statystyka testu mnoznika Lagrange
pchisq(LM,15, lower.tail=FALSE)

# Współliniowość --------------------------------------------------------

vif(model3)

# Normalność składnika losowego -------------------------------------------

jarque.bera.test(model3$residuals)

# Metody odporne na heteroskedastyczność ----------------------------------

  #odporne błędy standardowe

VCOVHC=vcovHC(model3,type="HC3")

coeftest(model3)
coeftest(model3,vcov.=VCOVHC)

  #UMNK

DATA$weights = 1/(DATA$gini.index)

umnk1 = lm(log(death.rate)~health.expenditure +gini.index+life.expectancy+employment.industry+air.pollution, data = DATA, weights = DATA$weights) 
umnk2 = lm(log(death.rate)~log(health.expenditure) +gini.index+life.expectancy+employment.industry+air.pollution, data = DATA, weights = DATA$weights)
umnk3 = lm(death.rate~log(health.expenditure) +gini.index+life.expectancy+employment.industry+air.pollution, data = DATA, weights = DATA$weights)
summary(umnk1)
summary(umnk2)
summary(umnk3)

stargazer(model3,umnk3, type = "text")

# Endogeniczność ----------------------------------------------------------

cor.gini = cor(DATA$gini.index,DATA$ehat2)
cor.poll = cor(DATA$air.pollution,DATA$ehat2)
cor.life = cor(DATA$life.expectancy,DATA$ehat2)
cor.employ = cor(DATA$employment.industry,DATA$ehat2)
cor.health = cor(DATA$health.expenditure,DATA$ehat2)


Zmienna <- c("Gini Index", "Pollution", "Life expectancy", "Employment", "Health Expenditure")
Cor_ze_skladnikiem <- c(cor.gini, cor.poll, cor.life, cor.employ, cor.health)

df <- data.frame(Zmienna, Cor_ze_skladnikiem)

print(df)
summary(model3)

