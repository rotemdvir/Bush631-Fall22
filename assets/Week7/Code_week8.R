# Experiment data: Wood, Hoy and Pryke (2020)
# Obs data: Fuhrmann (2020)

library(readxl)
library(tidyverse)
library(viridis)
library(haven)
library(ggeffects)
library(ggpubr)

### Textbook experiment (pp. 162-165)   ###
women <- read.csv("~/Week8_Prediction_III/women.csv")

# Explore data
dim(women)
head(women)

## Diff-in-means estimators
# drinking-water facilities
mean(women$water[women$reserved == 1]) -
  mean(women$water[women$reserved == 0])

# Irrigation facilities
mean(women$irrigation[women$reserved == 1]) -
  mean(women$irrigation[women$reserved == 0])

## Linear model estimation == diff-in-means
lm(water ~ reserved, data = women)
lm(irrigation ~ reserved, data = women)

### Australia Public views of forign aid ###
aus <- read_dta("~/Week8_Prediction_III/AussieData_JEPS2021.dta")

# Explore and proportions for equal exp. groups
dim(aus)
table(aus$treatment_group)
prop.table(table(aus$treatment_group))

# Overall means for measures
gen.means <- aus %>%
  summarise(Too_much = mean(aus$too_much_aid, na.rm = T),
            Too_little = mean(aus$too_little_aid, na.rm = T),
            more_pac = mean(aus$more_to_pac, na.rm = T),
            Aussie_first = mean(aus$favour_aus, na.rm = T),
            Poor_first = mean(aus$favour_poor, na.rm = T)) %>%
  gather(Measure, mn_prop, Too_much:Poor_first) %>%
  mutate(mn_prop = mn_prop * 100) %>%
  arrange(-mn_prop)

gen.means

# Diff-in-means by experimental groups: AUS provides too much aid
mean(aus$too_much_aid[aus$treatment_group == 1], na.rm = T) - mean(aus$too_much_aid[aus$treatment_group == 2], na.rm = T)
mean(aus$too_much_aid[aus$treatment_group == 1], na.rm = T) - mean(aus$too_much_aid[aus$treatment_group == 3], na.rm = T)
mean(aus$too_much_aid[aus$treatment_group == 2], na.rm = T) - mean(aus$too_much_aid[aus$treatment_group == 3], na.rm = T)

# Diff-in-means: AUS provides too little aid
mean(aus$too_little_aid[aus$treatment_group == 1], na.rm = T) - mean(aus$too_little_aid[aus$treatment_group == 3], na.rm = T)
mean(aus$too_little_aid[aus$treatment_group == 1], na.rm = T) - mean(aus$too_little_aid[aus$treatment_group == 2], na.rm = T)
mean(aus$too_little_aid[aus$treatment_group == 2], na.rm = T) - mean(aus$too_little_aid[aus$treatment_group == 3], na.rm = T)

# Linear model coefficients = diff-in-means estimators
aus2 <- aus%>%
  filter(treatment_group < 3)

lm(too_much_aid ~ treatment_group, data = aus2)
lm(too_little_aid ~ treatment_group, data = aus)

aus3 <- aus%>%
  filter(treatment_group > 1)

lm(too_much_aid ~ treatment_group, data = aus3)
lm(too_little_aid ~ treatment_group, data = aus3)

# Diff-in-means estimators: other measures
mean(aus$more_to_pac[aus$treatment_group == 1], na.rm = T) -
  mean(aus$more_to_pac[aus$treatment_group == 2], na.rm = T)

mean(aus$favour_aus[aus$treatment_group == 1], na.rm = T) -
  mean(aus$favour_aus[aus$treatment_group == 2], na.rm = T)

mean(aus$favour_poor[aus$treatment_group == 1], na.rm = T) -
  mean(aus$favour_poor[aus$treatment_group == 2], na.rm = T)

# Plot for overall views: focus on 3 measures
gen.means2 <- gen.means %>%
  filter(Measure == "Too_much" | Measure == "more_pac" | Measure == "Poor_first")

ggplot(gen.means2, aes(x = reorder(Measure,mn_prop), y = mn_prop, fill = mn_prop)) +
  geom_bar(stat = "identity", width = 0.6) + ylim(0,90) +
  geom_text(aes(label = round(mn_prop, 2)), hjust = -0.1, color = "red", size = 5) +
  scale_x_discrete(breaks = c("Too_much","more_pac","Poor_first"),
                   labels = c("Too much Aid", "More to Pacific", "Favor poor countries")) +
  xlab("") + ylab("Proportion of support") +
  coord_flip() +
  scale_fill_viridis() +
  theme_bw() +
  theme(legend.position = "none") 


### Factor variable (for multiple predictors analysis)
# rename levels and run
aus$grp <- NA
aus$grp[aus$treatment_group == 1] <- "Control"
aus$grp[aus$treatment_group == 2] <- "Measured"
aus$grp[aus$treatment_group == 3] <- "Forceful"

levels(factor(aus$grp))

# Linear model (as if multiple predictors)
lm(favour_poor ~ factor(grp), data = aus)

# Linear model: see all 3 coefficients (remove intercept)
fit3 <- lm(favour_poor ~ -1 + factor(grp), data = aus)
fit3

# Same with tapply()
tapply(aus$favour_aus, aus$grp, mean, na.rm = T)

# coef() for calculating ATE
coef(fit3)["factor(grp)Control"] - coef(fit3)["factor(grp)Measured"]

#### Model fit R squared and adjusted R squared
summary(lm(favour_poor ~ grp + urban + hhold_income + academic, data = aus))

### Heterogeneous treatment effect  ###
# Subsets by respondent age (over/under 50)
aus.age <- subset(aus, over_fifty == 1)

# Diff-in-means: support for aid by groups
mean(aus.age$more_to_pac[aus.age$treatment_group == 1], na.rm = T) -
  mean(aus.age$more_to_pac[aus.age$treatment_group == 2], na.rm = T)

# Subset of younger respondents
aus.age2 <- subset(aus, over_fifty == 0)

# Diff-in-means: support for aid by groups
mean(aus.age2$more_to_pac[aus.age2$treatment_group == 1], na.rm = T) -
  mean(aus.age2$more_to_pac[aus.age2$treatment_group == 2], na.rm = T)

# Estimated treatment effect for gender by group
(mean(aus.age$more_to_pac[aus.age$treatment_group == 1], na.rm = T) -
    mean(aus.age$more_to_pac[aus.age$treatment_group == 2], na.rm = T)) -
  (mean(aus.age2$more_to_pac[aus.age2$treatment_group == 1], na.rm = T) -
     mean(aus.age2$more_to_pac[aus.age2$treatment_group == 2], na.rm = T))

# Effect of age across experimental conditions
mean(aus$more_to_pac[aus$over_fifty == 1], na.rm = T) - 
  mean(aus$more_to_pac[aus$over_fifty == 0], na.rm = T)

# Interaction model (respondent gender and experimental group)
summary(lm(favour_poor ~ treatment_group * male, data = aus2))


### ICB data age effect #####
library(haven)
mydata <- read_dta("/Data/ICB_Feb2020.dta")

# Age variable distribution
summary(mydata$lead_age)

# Interaction model
summary(fit.age <- lm(crismg ~ triggr * lead_age, data = mydata))

# Calculate marginal effects and create plot
# Calculate predicted values
pred1 <- ggpredict(fit.age, terms = c("lead_age", "triggr[1,9]"), interval = "confidence")

# Plot: marginal effects
plot(pred1, ci.style = "dash", alpha = 0.15) +
  labs(x = "Leader Age", y = "Predicted response method",
       title = "Crisis response: Interaction model results",
       colour = "Trigger Event") +
  scale_colour_brewer(palette = "Set1", labels = c("Verbal-Political Act", "Violent Act")) +
  theme(legend.position = "bottom",
        legend.background = element_rect(size = 0.5, linetype = "solid", colour = "black"))


###   Linear model and observational data (Fuhrmann 2020 data)  ### 
library(readxl)
matt1 <- read_excel("/Week8_Prediction_III/matt_defspend.xlsx")

# Re-shape data to create leaders and spending data
def.dat <- matt1 %>%
  gather(year, def.exp, '1949':'2020',-Country, -ccode) %>%
  arrange(Country) 

def.dat$year <- as.numeric(def.dat$year)

# Read leaders data
matt <- read_dta("/Week8_Prediction_III/defense_spending.dta")

# Combine leaders and spending datasets
def.matt <- left_join(matt, def.dat, by = c('ccode', 'year'))

# Add change in spending variable
def.matt <- def.matt %>%
  group_by(ccode) %>%
  mutate(def.delta = ((def.exp - lag(def.exp))/lag(def.exp)*100))

## Effect of business experience: diff-in-means
# subsets by business experience
no.business <- subset(def.matt, subset = (business == 0))
business <- subset(def.matt, subset = (business == 1))

# diff-in-means estimator and plot
business.effect <- mean(business$def.delta, na.rm = T) - mean(no.business$def.delta, na.rm = T)

# Linear model estimator
lm(def.delta ~ business, data = def.matt)

## Placebo test: effect of business experience on non-defense spending
# diff-in-means estimator and plot
business.effect.nodef <- mean(business$nondefspend_ch, na.rm = T) - mean(no.business$nondefspend_ch, na.rm = T)

# Linear model estimator
lm(nondefspend_ch ~ business, data = def.matt)

## Summary plots of both analyses
# Generate data for main analysis and placebo test
# Values based on predicted change in spending
business.p <- data.frame(business.experience = c("Yes", "No"),
                         expend.change = c(0.711,2.846))

business.p.nodef <- data.frame(business.experience = c("Yes", "No"),
                         expend.change = c(3.03,3.16))

# Plot main analysis
p1 <- ggplot(business.p, aes(x = business.experience, y = expend.change, fill = expend.change)) +
  geom_bar(stat = "identity", width = 0.6) + ylim(0,4) +
  geom_text(aes(label = round(expend.change,2)), hjust = -0.1) +
  xlab("Business Experience") + ylab("Change in defense spending") +
  coord_flip() +
  scale_fill_viridis() + theme_bw() + theme(legend.position = "none")

# Plot placebo
p2 <- ggplot(business.p.nodef, aes(x = business.experience, y = expend.change, fill = expend.change)) +
  geom_bar(stat = "identity", width = 0.6) + ylim(0,4) +
  geom_text(aes(label = round(expend.change,2)), hjust = -0.1) +
  xlab("") + ylab("Change in non-defense spending") +
  coord_flip() +
  scale_fill_viridis() + theme_bw() + theme(legend.position = "none")

# Combine plots
library(ggpubr)
ggarrange(p1,p2,ncol = 2,nrow = 1)

