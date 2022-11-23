# Week 13
# Data: Rebel experience and nuclear technology (2015)

library(tidyverse)
library(haven)

## Upload data
nukes <- read_dta("~/Week13_Uncertainty_III/FH Dataset 2014-0519.dta")

# Bivariate regression model
lm(pursuit ~ rebel, data = nukes)

# Bivariate regression model: include uncertainty aspect
summary(lm(pursuit ~ rebel, data = nukes))

# Multivariate regression model
summary(m <- lm(pursuit ~ factor(rebel) + factor(milservice) + polity2, data = nukes))


