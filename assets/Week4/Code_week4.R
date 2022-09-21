# Bush 631-600: Week 4
# Dvir et al. (2021) Data
# Sagan and Valentino (2019) Data

## Upload survey data
mydata <- read.csv("~Week4_Measurement_I/Bush_TerrorSurvey.csv")
View(mydata)

# Explore data
summary(mydata)

## Tablulate different proportion of the data
# Concern for conventional vs. cyber terrorism
prop.table(table(conventional = mydata$concern_bomb, cyber = mydata$concern_cyber))

# Lethality of conventional terrorism and respondent gender
prop.table(table(Lethality = mydata$severity_bomb, Gender = mydata$PPGENDER))

# Likelihood of conventional terrorism and policy: more airport security
prop.table(table(Attack_Coming = mydata$likely_bomb, Airport_Checks = mydata$Pol_screenUS))

### NA's  ###

## Looking at NAa in the data
# Display 15 observations, NA stands for missing
head(mydata$likely_bomb, n = 15)

# Vector of logical values (TRUE/FALSE) for missing values
# Output presents TRUE for missing
head(is.na(mydata$Pol_force), n=15)

# Counting number of missing values in a variable (likelihood of conventional attack)
sum(is.na(mydata$likely_bomb))

# Proportion of missing (based on counting number of TRUE in vector)
# Proportion of missing in a variable (likelihood of conventional attack)
mean(is.na(mydata$likely_bomb))

# Proportion of missing in a variable (policy: restrict immigration from Mid-East)
mean(is.na(mydata$Pol_immig))

# Proportion of missing across multiple variables (likelihood of conventional attack & policy: more airport security)
prop.table(table(Attack_Coming = mydata$likely_bomb, Airport_Checks = mydata$Pol_screenUS, exclude = NULL))

# Working with NAs 
mean(mydata$Pol_survMusl)

# Add na.rm=TRUE
mean(mydata$Pol_survMusl, na.rm = TRUE)

### Listwise deletion
# na.omit (): Remove observation (row) when missing value detected in any column (variable)

# Total number of observations in data 
nrow(mydata)

# Create data set with removed observations
mydata.del <- na.omit(mydata)

# Total number of observations after removal (much smaller)
nrow(mydata.del)

# Same for a single variable (column in data)
length(mydata$concern_bomb)
length(na.omit(mydata$concern_bomb))

### Plots (base and tidyverse)  ###
## Upload war ethics data
library(haven)
wardata <- read_dta("/Week4_Measurement_I/WarEthics.dta")
View(wardata)

## Base R: Barplot
# Create data: proportion of support for each value (approval of artillery 1-6 scale)
artillery.tab <- prop.table(table(Support = wardata$artillery_approve, exclude = NULL))

# Map using barplot function
# main, xlab,ylab are labels -> can be changed
barplot(artillery.tab, main = "Support for using artillery option",
        xlab = "Response category", ylab = "Proportion of respondents")

# Create data: proportion of support for each value (how ethical is artillery 1-6 scale)
artillery.ethic <- prop.table(table(Support = wardata$artillery_ethical, exclude = NULL))

# Map using barplot function
# main, xlab,ylab are labels -> can be changed
barplot(artillery.ethic, main = "Views of artillery as ethical",
        xlab = "Response category", ylab = "Proportion of respondents")

# Create data: proportion of support for each value (support artillery binary - 0/1 vales)
artillery.binary <- prop.table(table(Support_Artillery = wardata$approve_artillery_dummy, exclude = NULL))

# Map using barplot function
# main, xlab,ylab are labels -> can be changed
# names.arg: rename values 0/1 to No and Yes.
barplot(artillery.binary, main = "Support for using artillery option - Yes/No",
        xlab = "Response category", ylab = "Proportion of respondents",
        names.arg = c("No","Yes"))

## Using tidyverse and ggplot
library(tidyverse)

# ggplot syntax: data and then variables to map in aes
# add elements using the + sign
# define variable (support artillery binary) as factor
# geom_bar: creates the barplot (must define the y value) -> width can be removed or changed.
ggplot(wardata, aes(x=factor(prefer_artillery_dummy))) + 
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.7, fill = "darkblue") + 
  scale_y_continuous(labels=scales::percent) + ggtitle("Support for using artillery option - Yes/No") +
  theme_classic() 

## Histogram
# Create age variable in data
wardata$age <- (2014 - wardata$birthyr)
summary(wardata$age)

# Histogram using base R with hist function
# freq = FALSE creates density (proportion in bins)
# breaks and seq functions to create bins (min, max and interval)
# xlab and main can be changed
hist(wardata$age, freq = FALSE, breaks = seq(from = 15, to = 95, by = 5),
     xlab = "Age",
     main = "Distribution of respondents' age")

# Same plot using freq=TRUE display counts (frequency in each bin)
hist(wardata$age, freq = TRUE, breaks = seq(from = 15, to = 95, by = 5),
     xlab = "Age",
     main = "Counts of respondents' age")

## Histogram: Tidyverse version
# define data; then which variable to map (in aes)
# geom_histogram: creates the plot (color and fill can be changed) 
ggplot(wardata, aes(x=artillery_approve)) +
  geom_histogram(color="black", fill="white")

# adding elements: ylab and xlab (labels for x-axis and y-axis)
# theme can be changed
ggplot(wardata, aes(x=age)) +
  geom_histogram(color="black", fill="blue") +
  theme_classic() + ylab("Counts") + xlab("Respondents' Age")

# different theme
ggplot(wardata, aes(x=age)) +
  geom_histogram(color="black", fill="blue") +
  ylab("Counts") + xlab("Respondents' Age") +
  theme_bw()

# adding elements: 
# geom_vline (vertical line, must define aes - in this case the mean value of variable; color, type, size can be changed).
# geom_text: add text to plot (define location with x/y values, then label is the text on plot)
ggplot(wardata, aes(x=age)) +
  geom_histogram(color="black", fill="lightblue") +
  theme_classic() + ylab("Counts") + xlab("Respondents' Age") +
  geom_vline(aes(xintercept=mean(age)),
             color="black", linetype="dashed", size=1) +
  geom_text(x = 48, y = 40, label = "Mean")

# adding elements:
# geom_density: add distribution of age without bins (alpha is transparency, color can be changed)
# geom_vline is the same (this time defined as median value)
# geom_text is the same (added color to text on plot)
ggplot(wardata, aes(x=age)) +
  geom_histogram(aes(y=..density..), colour="black", fill="lightgrey")+
  geom_density(alpha=.2, fill="#56B4E9") +
  xlab("Age") + ylab("Density") + theme_bw() + ggtitle("Survey Respondents Age") +
  geom_vline(aes(xintercept=median(age)),
             color="maroon", linetype="dashed", size=1) +
  geom_text(x = 48, y = 0.03, label = "Median", col = "red")

## Boxplots: base R
# define variable to plot; ylab and main can be changed
boxplot(wardata$age, ylab = "Age",
        main = "Distribution of respondents' age")

# define variables to plot (y-axis ~ x-axis and data name)
boxplot(educ ~ gender, data = wardata)
boxplot(age ~ gender, data = wardata)
boxplot(educ ~ agegroup, data = wardata, xlab = "Age groups", ylab = "Education categories",
        main = "Education - Age")

# tapply to calculate average support along both gender groups
tapply(wardata$artillery_approve, wardata$gender, mean)

# plot differences men/women
boxplot(artillery_approve ~ gender, data = wardata, xlab = "Respondent Gender",
        ylab = "Approving Artillery", names = c("Male", "Female"))

## Tidyverse version: boxplots
# define data; then variables in aes (both x&y, color can be changed, allows to differentiate b-w groups)
# geom_boxplot: creates the plot
# geom_jitter: add the data points distribution
# theme(legend.position="none") removes the legend (unnecessary in this plot)
ggplot(wardata, aes(x=factor(agegroup), y = artillery_approve, color = factor(agegroup))) +
  geom_boxplot() +
  geom_jitter(shape=16, position=position_jitter(0.2)) +
  xlab("Age groups") + ylab("Support for Artillery option") +
  theme_classic() + theme(legend.position = "none")

### log transform ###
# Afghan village data
afghan <- read.csv("~/Week4_Measurement_I/afghan-village.csv")

# outliers in population varaible
# max value is much larger than mean/median - potential for outlier cases
summary(afghan$population)

# add indicator for how many villages have more than 2000/1000 residents
# ifelse condition is TRUE, value is 1; otherwise value is 0
afghan$pop_out <- ifelse(afghan$population > 2000, 1,0)
afghan$pop_out2 <- ifelse(afghan$population > 1000, 1,0)

# proportion of outlier villages (less than 5%)
prop.table(table(outliers = afghan$pop_out))
prop.table(table(outliers = afghan$pop_out2))

# plotting the population (original data)
ggplot(afghan, aes(x=population)) +
  geom_histogram(color="black", fill="blue") +
  theme_classic() + ylab("Counts") + xlab("Village Population")

# create variable with logged population values
afghan$pop_l <- log(afghan$population, 10)

# plot new variable
ggplot(afghan, aes(x=pop_l)) +
  geom_histogram(color="black", fill="maroon") +
  theme_classic() + ylab("Counts") + xlab("Population: Logged")

# reminder: logging a variable does not remove it from the data
# logging re-scales the values to reduce the effect of outlier values
