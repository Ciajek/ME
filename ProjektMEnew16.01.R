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
DATA$lgini.index2 = DATA$lgini.index^2
DATA$lair.pollution2 = DATA$lair.pollution^2
DATA$lhealth.expenditure2 = DATA$lhealth.expenditure^2

# Wizualizacja danych -----------------------------------------------------

  #zbiór histogramów

hist(DATA$death.rate, 
     col="orange", 
     border="black",
     prob = TRUE, 
     ylim = c(0, 0.20),
     xlab = "Death Rate",
     main = "Gęstość Death Rate")
lines(density(DATA$death.rate),
      lwd = 3, 
      col = "black")
abline(v = mean(DATA$death.rate),
       col = "darkblue",
       lwd = 2)
legend(x = "topright",
       c("Gęstość", "Średnia"),
       col = c("black", "darkblue"),
       lwd = c(2, 2))

hist(DATA$gini.index,
     col="orange", 
     border="black",
     prob = TRUE,
     breaks = 10,
     xlim = c(20,70),
     ylim = c(0, 0.06),
     xlab = "Gini Index",
     ylab = "Gęstość",
     main = "Gęstość Gini Index")
lines(density(DATA$gini.index),
      lwd = 3, 
      col = "black")
abline(v = mean(DATA$gini.index),
       col = "darkblue",
       lwd = 2)
legend(x = "topright",
       c("Gęstość", "Średnia"),
       col = c("black", "darkblue"),
       lwd = c(2, 2))

hist(DATA$life.expectancy,
     breaks = 10,
     col="orange",
     border="black",
     prob = TRUE, 
     ylim = c(0, 0.12),
     xlab = "Life Expectancy",
     main = "Gęstość Life Expectancy")
lines(density(DATA$life.expectancy),
      lwd = 3, 
      col = "black")
abline(v = mean(DATA$life.expectancy),
       col = "darkblue",
       lwd = 2)
legend(x = "topleft",
       c("Gęstość", "Średnia"),
       col = c("black", "darkblue"),
       lwd = c(2, 2))

hist(DATA$air.pollution,
     breaks = 10,
     col="orange", 
     xlim = c(0,70),
     border="black",
     prob = TRUE,
     xlab = "Air Pollution",
     ylab = "Gęstość",
     main = "Gęstość Air Pollution")
lines(density(DATA$air.pollution),
      lwd = 3, 
      col = "black")
abline(v = mean(DATA$air.pollution),
       col = "darkblue",
       lwd = 2)
legend(x = "topright",
       c("Gęstość", "Średnia"),
       col = c("black", "darkblue"),
       lwd = c(2, 2))

hist(DATA$health.expenditure,
     breaks = 10,
     col="orange", 
     xlim = c(0,20),
     border="black",
     prob = TRUE,
     ylim = c(0, 0.20),
     xlab = "Health Expenditure",
     ylab = "Gęstość",
     main = "Gęstość Health Expenditure")
lines(density(DATA$health.expenditure),
      lwd = 3, 
      col = "black")
abline(v = mean(DATA$health.expenditure),
       col = "darkblue",
       lwd = 2)
legend(x = "topright",
       c("Gęstość", "Średnia"),
       col = c("black", "darkblue"),
       lwd = c(2, 2))

hist(DATA$employment.industry,
     breaks = 12,
     col="orange", 
     xlim = c(5,40),
     ylim = c(0, 0.13),
     border="black",
     prob = TRUE,
     xlab = "Employment in Industry",
     ylab = "Gęstość",
     main = "Gęstość Employment in Industry")
lines(density(DATA$employment.industry),
      lwd = 3, 
      col = "black")
abline(v = mean(DATA$employment.industry),
       col = "darkblue",
       lwd = 2)
legend(x = "topright",
       c("Gęstość", "Średnia"),
       col = c("black", "darkblue"),
       lwd = c(2, 2))

  #zbiór wykresów punktowych

death.gini.p <- ggplot(DATA, aes(gini.index, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5) + 
  labs(title = "Gini Index / Death Rate") + theme(plot.title = element_text(hjust = 0.5))

death.life.p <- ggplot(DATA, aes(life.expectancy, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5) +
  labs(title = "Life Expectancy / Death Rate") + theme(plot.title = element_text(hjust = 0.5))

death.employ.p <- ggplot(DATA, aes(employment.industry, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5) +
  labs(title = "Employment in Industry / Death Rate") + theme(plot.title = element_text(hjust = 0.5))

death.air.p <- ggplot(DATA, aes(air.pollution, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5) +
  labs(title = "Air Pollution / Death Rate") + theme(plot.title = element_text(hjust = 0.5))

death.lhealth.p <- ggplot(DATA, aes(lhealth.expenditure, death.rate)) +
  geom_point() + 
  geom_point(data = DATA, aes(), colour = "red", size = 0.5) +
  labs(title = "Health Expenditure / Death Rate") + theme(plot.title = element_text(hjust = 0.5))

death.health.p <- ggplot(DATA, aes(health.expenditure, death.rate)) +
  geom_point() +
  geom_point(data = DATA, aes(), colour = 'red', size = 0.5) +
  labs(title = "Health Expenditure / Death Rate") + theme(plot.title = element_text(hjust = 0.5))

  #print wykresów punktowych

plot(death.gini.p)
plot(death.life.p)
plot(death.employ.p)
plot(death.air.p)
plot(death.health.p)
plot(death.lhealth.p)

grid.arrange(death.health.p,death.lhealth.p)

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
DATA$ehat24 = model4$residuals^2
plot(DATA$gini.index, DATA$ehat2)
plot(DATA$air.pollution, DATA$ehat2)


  #Test RESET

reset(model3, type = "fitted")
reset(model4, type = "fitted")

  #Test WHite model 3

test.white3 = lm(ehat2 ~ air.pollution2 + gini.index2 + health.expenditure2 + 
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

summary(test.white3) 

LM=N*summary(test.white3)$r.squared # statystyka testu mnoznika Lagrange
pchisq(LM,15, lower.tail=FALSE)

#Test Whitea model 4

test.white4 = lm(ehat24 ~ lair.pollution2 + lgini.index2 + lhealth.expenditure2 + 
                employment.industry2 + life.expectancy2 + lair.pollution +
                lgini.index + lhealth.expenditure + employment.industry + 
                life.expectancy + I(lair.pollution*lgini.index) + 
                I(lair.pollution*lhealth.expenditure) +
                I(lair.pollution*employment.industry) + 
                I(lair.pollution*life.expectancy) +
                I(lgini.index*life.expectancy) + 
                I(lgini.index*lhealth.expenditure) + 
                I(lgini.index*employment.industry) +
                I(employment.industry*life.expectancy) + 
                I(employment.industry*lhealth.expenditure) + 
                I(lhealth.expenditure*life.expectancy)
              ,data=DATA)

summary(test.white4) 

LM=N*summary(test.white4)$r.squared # statystyka testu mnoznika Lagrange
pchisq(LM,15, lower.tail=FALSE)



# Współliniowość --------------------------------------------------------

vif(model3)
vif(model4)

# Normalność składnika losowego -------------------------------------------

jarque.bera.test(model3$residuals)
jarque.bera.test(model4$residuals)
jarque.bera.test(umnk4$residuals)

# Metody odporne na heteroskedastyczność ----------------------------------

  #odporne błędy standardowe

VCOVHC=vcovHC(model3,type="HC3")

coeftest(model3)
coeftest(model3,vcov.=VCOVHC)

  #WLS model 3

DATA$weights3 = 1/(DATA$gini.index)

wls3 = lm(death.rate~ lhealth.expenditure +gini.index+life.expectancy+employment.industry+air.pollution, data = DATA, weights = DATA$weights3)


stargazer(model3, wls3, type = "text")

  # WLS model 4

DATA$weights4 = 1/(DATA$lgini.index)
model4.wagi = lm(log(ehat24)~ lhealth.expenditure + lgini.index + life.expectancy+employment.industry+lair.pollution, data = DATA)
DATA$weights4.1 = 1/(exp(model4.wagi$fitted.values))
wls4 = lm(death.rate~ lhealth.expenditure + lgini.index + life.expectancy+employment.industry+lair.pollution, data = DATA, weights = DATA$weights4)
wls4.1 = lm(death.rate~ lhealth.expenditure + lgini.index + life.expectancy+employment.industry+lair.pollution, data = DATA, weights = DATA$weights4.1)


summary(wls4)
summary(wls4.1)


summary
stargazer(wls4, wls4.1, model4, column.labels = c("wls4", "wls4.1", "model2"), type = "text")

# Endogeniczność ----------------------------------------------------------

#model 4

cor.gini = cor(DATA$gini.index,DATA$ehat2)
cor.poll = cor(DATA$air.pollution,DATA$ehat2)
cor.life = cor(DATA$life.expectancy,DATA$ehat2)
cor.employ = cor(DATA$employment.industry,DATA$ehat2)
cor.health = cor(DATA$health.expenditure,DATA$ehat2)


Zmienna <- c("Gini Index", "Pollution", "Life expectancy", "Employment", "Health Expenditure")
Cor_ze_skladnikiem <- c(cor.gini, cor.poll, cor.life, cor.employ, cor.health)

#model 4

cor.gini4 = cor(DATA$lgini.index,DATA$ehat24)
cor.poll4 = cor(DATA$lair.pollution,DATA$ehat24)
cor.life4 = cor(DATA$life.expectancy,DATA$ehat24)
cor.employ4 = cor(DATA$employment.industry,DATA$ehat24)
cor.health4 = cor(DATA$lhealth.expenditure,DATA$ehat24)

Zmienna <- c("Gini Index", "Pollution", "Life expectancy", "Employment", "Health Expenditure")
Cor_ze_skladnikiem4 <- c(cor.gini4, cor.poll4, cor.life4, cor.employ4, cor.health4)

df <- data.frame(Zmienna, Cor_ze_skladnikiem)
df4 <- data.frame(Zmienna, Cor_ze_skladnikiem4)
print(df4)
summary(model3)

