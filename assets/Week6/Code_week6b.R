# Bush 631-600: Week 6
# Data files: facial appearance experiment; health; Florida voting; International trade

library(tidyverse)

# Upload data
face <- read.csv("~/Week7_Prediction_II/face.csv")
health <- read.csv("~/Week7_Prediction_II/health.csv")
florida <- read.csv("~/Week7_Prediction_II/florida.csv")
trade <- read_dta("~/Downloads/doces_magee_data.dta")

### Facial appearance experiment (Book pp.) ###
## Create vote share (both parties)
face$d.share <- face$d.votes /
  (face$d.votes + face$r.votes)

face$r.share <- face$r.votes /
  (face$d.votes + face$r.votes)

## Calculate difference in mean vote share 
face$diff.share <- face$d.share - face$r.share

## Plot
face$w.party <- as.character(face$w.party)
plot(face$d.comp, face$diff.share, pch = 16,
     col = ifelse(face$w.party == "R", "red", "blue"),
     xlim = c(0, 1), ylim = c(-1, 1),
     xlab = "Competence scores for Democrats",
     ylab = "Democratic margin in vote share",
     main = "Facial Competence and Vote Share")

## Correlation
cor(face$d.comp, face$diff.share)

### Health data ###
## Remove missing values
health <- na.omit(health)

## Scatter plot: steps and weight (added regression line)
plot(health$steps.lag, health$weight, pch = 19,
     col =  "navyblue",
     xlim = c(0, 27), ylim = c(150, 180),
     xlab = "Steps on day prior (in 1000s)",
     ylab = "Weight",
     main = "Weight and Steps")
abline(lm(weight~steps.lag, data = health), col = "red")

## Correlation
cor(health$steps.lag, health$weight)

### International Trade data (2015 article)
## Explore data
dim(trade)
head(trade, n=5)

## Economic growth and democracy level
## Scatter plot: tidyverse approach
ggplot(trade, aes(ln_gdppc8,polity2)) + 
  geom_jitter(color = "red") + xlab("GDP/cap") + ylab("Polity Score") +
  theme_bw()

## Correlation
cor(trade$polity2,trade$ln_gdppc8, use = "complete")

## Gravity: trade and regional trade volume
## Scatter plot: tidyverse approach
trade %>% 
  filter(open3 < 450) %>% 
  ggplot(aes(open3,region_open_20)) + 
  geom_jitter(color = "maroon") + xlab("Openness") + ylab("Regional Openness") +
  geom_smooth(method = "lm", se = F) + theme_bw()

## Correlation
cor(trade$open3,trade$region_open_20, use = "complete")

## Only labor abundant countries
# Create subset of labor abundant countries
labor.trade <- trade %>% 
  filter(above_median_kl8 == 0)

## Check correlations
# Trade and religious diversity
cor(labor.trade$open3, labor.trade$religion_num2, use = "complete")
# Trade and working population
cor(labor.trade$open3, labor.trade$pop_15_64, use = "complete")
# Trade and Democracy
cor(labor.trade$open_hat1, labor.trade$polity2, use = "complete")

# Only capital abundant countries
# Create subset of capital abundant countries
cap.trade <- trade %>% 
  filter(above_median_kl8 == 1)

## Check correlations
# Trade and religious diversity
cor(cap.trade$open3, cap.trade$religion_num2, use = "complete")
# Trade and linguistic diversity
cor(cap.trade$open3, cap.trade$lang_num2, use = "complete")
# Trade and Democracy
cor(cap.trade$open_hat1, cap.trade$polity2, use = "complete")

### Fit linear model ###
# Fit the model
fit <- lm(polity2 ~ open3, data = trade)
fit

# Directly obtain coefficients
coef(fit)
# Directly pull fitted values
head(fitted(fit))

## Plot the regression model
## Scatter plot: tidyverse
## Added: dashed lines for mean X and Y
## Added: regression line
## Added: labels for dashed lines
ggplot(trade, aes(open3,polity2)) + 
  geom_jitter(color = "gray") +
  geom_smooth(method = "lm", se = FALSE) +
  geom_hline(yintercept = mean(trade$polity2, na.rm = T), linetype = "dashed", color = "black") +
  geom_vline(xintercept = mean(trade$open3, na.rm = T), linetype = "dashed", color = "black") +
  geom_curve(x=500,y=5,xend=400,yend=-1,color="red",arrow = arrow()) +
  geom_text(aes(x=440, y=-1, label = "Here is \n the reg. line")) +
  geom_text(aes(x = 200, y = 1.5, label = "Mean X")) +
  geom_text(aes(x = 120, y = 9, label = "Mean Y")) +
  xlab("Trade flows") + ylab("Democracy") +
  theme_bw()

## Regression models for 2 subsets of countries
# Labor abundant
fit2 <- lm(polity2 ~ open3, data = labor.trade)
fit2

# Capital abundant
fit3 <- lm(polity2 ~ open3, data = cap.trade)
fit3

# Plot of predicted values
# Install the package prior to running this code
library(ggeffects)

pred1 <- ggpredict(fit2, "open3", interval = "confidence")
pred2 <- ggpredict(fit3, "open3", interval = "confidence")

plot(pred1,ci.style = "dash") +
  labs(x="Trade flows",
       y="Polity score",
       title = "Predicted Democratization: Labor")

plot(pred2,ci.style = "dash") +
  labs(x="Trade flows",
       y="Polity score",
       title = "Predicted Democratization: Capital")

### Model fit ###
## Fit the model: voting for independents (1996/2000)
## Use summary function for additional information (inc. R squared)
summary(fit3 <- lm(Buchanan00 ~ Perot96, data = florida))

## Fit the model: voting for democratic candidates (1996/2000)
summary(lm(Gore00 ~ Clinton96, data = florida))

## Fit the model: voting for republican candidates (1996/2000)
summary(lm(Bush00 ~ Dole96, data = florida))

## Fit the model: independent (1996) and voting for republican candidate (2000)
summary(lm(Bush00 ~ Perot96, data = florida))

## Create residual plot to identify outliers
## Plotting residuals and fitted values 
plot(fitted(fit3), resid(fit3), xlim = c(0,1500), ylim = c(-750,2500),
     xlab = "Fitted values", ylab = "Residuals")
abline(h=0)

## Identify outlier data point: Palm beach county
florida$county[resid(fit3) == max(resid(fit3))]

## Create subset: removing Palm beach county
florida_cut <- subset(florida, subset = (county != "PalmBeach"))

## Fit the model: voting for independents (Subset no plam beach)
summary(lm(Buchanan00 ~ Perot96, data = florida_cut))


