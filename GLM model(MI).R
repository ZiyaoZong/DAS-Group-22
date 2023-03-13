library(ggplot2)
library(dplyr)
library(moderndive)
library(gapminder)
library(skimr)
library(mvtnorm)
library(gridExtra)
library(kableExtra)
library(tidyr)
library(mice)
library(MASS)
data <- read.csv("C:/Users/Lenovo/Desktop/dataset22.csv", header = TRUE)
nas <- c()
coun = 1
for(i in 1:nrow(data)){
  if(is.na(data$depth[i])){
    if(is.na(data$height[i])){
      if(is.na(data$width[i])){
        nas[coun] = i
        coun = coun + 1
      }
    }
    else
      next
  }
  else
    next
}
data_new <- data[-nas, ]
data_new = data_new[, -c(1:2)]
data_new$baprice[data_new$price > 1000] <- 1
data_new$baprice[data_new$price <= 1000] <- 0
data_new$baprice <- factor(data_new$baprice, levels = c(0, 1), labels = c("Below", "Above"))
imp <- mice(data_new, 1000)
fit <- with(imp, glm(baprice ~ category + sellable_online + other_colors + depth + height + width, family = binomial(link = "logit")))
pooled <- pool(fit)
summary(pooled)
pooled

fit_new <- with(imp, glm(baprice ~ depth + height + width, family = binomial(link = "logit")))
pooled_new <- pool(fit_new)
summary(pooled_new)
pooled_new


# data <- read.csv("C:/Users/Lenovo/Desktop/dataset22.csv", header = TRUE)
# data_new = na.omit(data)
# data_new = data_new[, -c(1:2)]
# data_new$baprice[data_new$price > 1000] <- 1
# data_new$baprice[data_new$price <= 1000] <- 0
# data_new$baprice <- factor(data_new$baprice, levels = c(0, 1), labels = c("Below", "Above"))
# 
# fit <-glm(formula = baprice ~ category + sellable_online + other_colors + depth + height + width, data = data_new, family = binomial(link = "logit"))
# 
# summary(fit)
# stepAIC(fit)
# 
# fit_new <-glm(formula = baprice ~ height + width, data = data_new, family = binomial(link = "logit"))
# summary(fit_new)
# anova(fit_new, fit, test = "Chisq")
# stepAIC(fit_new)
