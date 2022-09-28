# Week 5
# Congress data
# UN Voting data
# Military spending data

library(tidyverse)
library(lubridate)  #install this package (helps working with dates)
library(viridis)    # install this package (more colors)
library(readxl)

## Upload Congress data
congress <- read.csv("~/Week5_Measurement_II/congress.csv")
View(congress)
head(congress)

# Explore data
summary(congress)

# Create scatter plots for ideology: 80th and 112th congress (same as textbook pp.98-99)
# Create subsets for Republican and Democratic party
rep <- subset(congress, subset = (party == "Republican"))
dem <- subset(congress, subset = (party == "Democrat"))

# Create subsets for 80th/112th congress for each party
rep80 <- subset(rep, subset = (congress == 80))
dem80 <- subset(dem, subset = (congress == 80))
rep112 <- subset(rep, subset = (congress == 112))
dem112 <- subset(dem, subset = (congress == 112))

# Scatterplot (using base R)
plot(dem80$dwnom1, dem80$dwnom2, pch = 16, col = "blue",
     xlim = c(-1.5,1.5), ylim = c(-1.5,1.5),
     xlab = "Liberalism/Conservatism: Economic scale",
     ylab = "Liberalism/Conservatism: Racial scale",
     main = "The 80th Congress")
points(rep80$dwnom1,rep80$dwnom2, pch = 16, col = "red")
text(-0.75,1,"Dems")
text(1,-1, "Reps")
abline(v = 0, col = "grey")
abline(h = 0, col = "grey")

## Calculate median ideology score by party (across full time frame)
dem.med <- tapply(dem$dwnom1, dem$congress, median)
rep.med <- tapply(rep$dwnom1, rep$congress, median)

# Plot trend line of ideology over time by party (same as textbook p. 100)
plot(names(dem.med), dem.med, col = "blue", type = "l",
     xlim = c(80,115), ylim = c(-1,1), xlab = "Congress",
     ylab = "DW-NOMINATE Score")
lines(names(rep.med), rep.med, col = "red")
text(110, -0.6, "Democrats")
text(110,0.8, "Republicans")


### UN Voting Data   ###
mydata <- read.csv("~/Week5_Measurement_II/unvoting.csv")
View(mydata)

## Tidyverse code to manage data
# Create data set of mean proportion of voting with US and Russia by year
annual.agree <- mydata %>%
  group_by(Year) %>%
  summarize(us.agree = mean(PctAgreeUS, na.rm = T),
            ru.agree = mean(PctAgreeRUSSIA, na.rm = T))

# Plot the proportion of voting with US/USSR over time 
ggplot(data = annual.agree) +
  geom_line(mapping = aes(x = Year, y = us.agree), color = "blue") +
  geom_line(mapping = aes(x = Year, y = ru.agree), color = "red") +
  geom_text(aes(x = 2000, y = 0, label = "Voting with US"), color = "blue", data = data.frame()) +
  geom_text(aes(x = 2000, y = 1, label = "Voting with Russia"), color = "red", data = data.frame()) +
  geom_vline(aes(xintercept = 1989), linetype = "dotted", color = "grey") +
  geom_text(aes(x = 1993, y = 0.5, label = "Cold War Ends"), color = "black") +
  ylab("Proportion voting with Superpower") + theme_classic()

## Tables that show who votes with US/Russia (tidyverse approach)
# USA: create data of mean proportion of voting with US for all countries (over time)
# Arrange the data from highest to lowest proportion, display top 10 observations
# Remove US from list
mydata %>%
  group_by(CountryName) %>% 
  summarise(mean.pctUS = mean(PctAgreeUS)) %>%
  arrange(desc(mean.pctUS)) %>%
  head(n = 11) %>%
  filter(CountryName != "United States of America")
  
# Russia (same procedure as USA)
mydata %>%
  group_by(CountryName) %>%
  summarise(mean.pctRU = mean(PctAgreeRUSSIA)) %>% 
  arrange(desc(mean.pctRU)) %>%
  head(n=11) %>%
  filter(CountryName != "Russia")

### Q-Q plot
# Relationship b-w proportion of voting with US/USSR (explore the entire distribution)
qqplot(mydata$PctAgreeUS, mydata$PctAgreeRUSSIA, xlab = "UN voting with US",
       ylab = "UN voting with Russia",
       main = "UN voting with superpower: trend over time")
abline(0,1) 

### Correlation ###
cor(mydata$idealpoint, mydata$PctAgreeUS, use = "pairwise")
cor(mydata$idealpoint, mydata$PctAgreeRUSSIA, use = "pairwise")

# Upload data 1999-2020
mil_exp <- read_excel("~/Week6_Prediction_I/mil_exp.xlsx")

# Upload data 1999-2019
mil_exp2 <- read_excel("~/Week6_Prediction_I/mil_exp2.xlsx")
View(mil_exp) 
View(mil_exp2)

# Explore data
dim(mil_exp)
head(mil_exp, n=8)

# loop data preparation with gather()
## I create the variable year for all the years in the data (1999-2019)
## I create the variable exp for the expenses of every country in each year
## The variables with (-) sign before them will remain as they were in the original dataset
## arrange() will organize the data in from A-Z by country name
spend_long <- mil_exp2 %>%
  gather(year, exp, '1999':'2019',-Country, -Group1, -Subgroup1) %>%
  arrange(Country) 

# Create loop
## Create empty vector for all 157 countries
pred.mean <- rep(NA,157)

## Pull the names of states and join them to the empty vector
c.names <- unique(spend_long$Country)
names(pred.mean) <- as.character(c.names)

## The loop: counter i runs for all 157 countries
for (i in 1:157){
  ## c.dat: subset of data for each country
  c.dat <- subset(spend_long, subset = (Country == c.names[i]))
  ## assign the mean of expenses across 1999-2019 for each country in slot [i] in the vector
  pred.mean[i] <- mean(c.dat$exp, na.rm = T)
}

pred.mean

## Check for errors
## Create vector of errors (actual spending - predicted values)
errors <- mil_exp$`2020` - pred.mean

## Assign country names to errors vector 
names(errors) <- c.names

## Average prediction error
mean(errors, na.rm = T)

## RMSE
sqrt(mean(errors^2, na.rm = T))

# Base R: plot the distribution of errors and mean prediction error
hist(errors, freq = FALSE)
abline(v = mean(errors, na.rm = T), lty = "dashed", col = "blue")

## Scatter plot: fit between predicted and actual values (tidyverse approach)
# Prepare data: arrange by country
n.dat <- mil_exp %>%
  arrange(Country) 

# Add vector of predicted expenses (vector created with loop)
n.dat <- cbind(n.dat, pred.mean)  

# Add variable for errors: 2020 data minus predicted values
n.dat <- n.dat %>%
  mutate(error = `2020` - pred.mean)

# Create plot and fit 45 degree line
ggplot(n.dat, aes(x = pred.mean, y = `2020`, label = Country)) +
  geom_abline(color = "red", size = 2) +
  geom_text() +
  theme_bw()

# Identify outliers
## Check the distribution of the error variable
summary(n.dat$error)

## Create variable to identify 'large' outliers
## Much more <- errors larger than 0.01 (actual spending is much more than predicted)
## Much less <- errors smaller than 0.01 (actual spending is much less than predicted)
n.dat$large.inc <- NA
n.dat$large.inc[n.dat$error > 0.01] <- "Much More"
n.dat$large.inc[n.dat$error < -0.01] <- "Much Less"

# Tabulate the subsets of outliers
## Countries which spend more than predicted
n.dat1 <- n.dat %>%
  filter(large.inc == "Much More") %>%
  mutate(error = error * 100) %>%
  select(Group1, error)

head(n.dat1, n=15)
tail(n.dat1, n=15)

## Countries which spend less than predicted
n.dat2 <- n.dat %>%
  filter(large.inc == "Much Less") %>%
  mutate(error = error * 100) %>%
  select(Group1, error)

head(n.dat2, n=10)
tail(n.dat2, n=10)

# Create time series plot (actual spending)
## Filter original data to 3 countries
## Remove variables Subgroup1, error, large.inc
dat3 <- n.dat %>%
  filter(Country == "USA" | Country == "China" | Country == "Iran") %>%
  select(-Subgroup1, -error, -large.inc) 

## Reshape data to long-form
dat3.l <- dat3 %>%
  gather(year, exp, '1999':'2020',-Country, -Group1, -pred.mean) %>%
  arrange(Country) %>%
  mutate(exp = round(exp*100,2)) 

## Define year variable using the lubridate package  
dat3.l$year.f <- as.Date(dat3.l$year, format = "%Y")
dat3.l$year.f2 <- year(dat3.l$year.f)

## Create time series plot for three countries; Add prediction values 
dat3.l %>%
  mutate(pred.mean = pred.mean * 100) %>%
  ggplot(aes(x = year.f2, y = exp)) +
  geom_point(aes(color = factor(Country))) +
  geom_point(aes(x=2020, y=pred.mean), color = "red") +
  geom_text(aes(x=2020, y = 15.5, label = "Iran \n Predicted")) +
  geom_text(aes(x=2020, y = 10, label = "USA \n Predicted")) +
  geom_text(aes(x=2020, y = 7.5, label = "China \n Predicted")) +
  geom_line(aes(group = factor(Country), color = factor(Country))) + 
  ylim(0,20) + ylab("Military spending (% of gov't spending)") + xlab("") + labs(color = "Country") +
  scale_color_viridis(discrete = T, option = "plasma") +
  theme_classic() + theme(legend.position = "top")


