# Leader data (Horowitz 2015)

library(tidyverse)
library(haven)
library(readxl)

## Aggies NFL data: collapse to OF/DF position groups
AgsData <- read_excel("/Week10_Probability_II/Ags_Data.xlsx")
skillposition <- subset(AgsData, subset = (Group == "OF" | Group == "DF"))

## Total of 34 players: Offensive and Defensive breakdown (12 D; 22 O)
skillposition %>%
  count(Group)

prob_off <- 22/34 
prob_def <- 12/34 

## Bernoulli: Sum of two random vars
# P(Ag1 + Ag2)  = P(Ag1) * P(Ag2)

# Offense:Offense (OO) = 0.4186
prob_off * prob_off

# Offense:Defense (OD) = 0.2283
prob_off * prob_def

# Offense:Defense (DO) = 0.2283
prob_def * prob_off

# Defense:Defense (DD) = 0.1245
prob_def * prob_def

## Binomial Distribution
# 3 Draws from a larger sample of 500, how many OF players?
rbinom(n=3, size = 500, prob = 0.647)


# Simulate data (10k draws)
sims <- 10000
draws <- rbinom(sims, size = 500, prob = 0.647)
head(draws, n=8)

# Mean number of OF in our draws
mean(draws)

# Plot the simulated data and mean value
hist(draws, freq = FALSE, xlim = c(0, 600), ylim = c(0, 0.04))
abline(v = 323.3, col = "red", lwd = 2)

# Simulation of senate calls and gender balance
# Simulate calls (p=0.26)
sims2 <- 10000
draws2 <- rbinom(sims, size = 1000, prob = 0.26)
head(draws2, n=9)

# Mean number of women called in 10k simulations
mean(draws2)

## Plot PMF based on Aggie data
plot.dat <- data.frame(k = c("0", "1"), y = c("0.353", "0.647"))
ggplot(plot.dat, aes(k,y)) +
  geom_bar(stat = "identity", width = 0.5, fill = "maroon") + ylab("P(X = k)") +
  ggtitle("Bernoulli r.v. of drawing OF Aggie") + theme_bw()

# Same values as table (Offensive player in each draw) and plot
dbinom(x = c(0,1,2), size = 2, prob = 22/34)

plot.dat2 <- data.frame(x = c("0", "1", "2"), y = c("0.1245", "0.4567", "0.4186"))
ggplot(plot.dat2, aes(x,y)) +
  geom_bar(stat = "identity", width = 0.65, fill = "maroon") + ylab("f(x)") +
  theme_bw()


## Working with tidyverse()
# upload insurgent group data - use Data_ClassTask1.dta
DataManage <- read_dta("~/Data_ClassTask1.dta")
head(DataManage)

# Subset groups operating in Iraq
sub.insurg <- DataManage %>% 
  filter(hbase == "Iraq")

sub.insurg

# Create new var
# Define name, use ifelse to define values
DataManage <- DataManage %>%
  mutate(yrs90 = ifelse(year < 2000, "90s Rock","You're too old"))

prop.table(table(NewProp. = DataManage$yrs90))

# Organize column (variable) by value (numeric or text)
DataManage %>% select(year,hbase,rev_fatlties) %>% head(n=4)

DataManage %>% select(year,hbase,rev_fatlties) %>% 
  arrange(-rev_fatlties) %>% head(n=4)

# New dataset with summary stats for selected variables per group
new.dat <- DataManage %>% 
  group_by(group) %>% 
  summarise(fatal.mean = mean(rev_fatlties, na.rm = T),
            fatal.med = median(rev_fatlties, na.rm = T),
            mx.battle = max(ucdpbd, na.rm = T),
            mn.battle = min(ucdpbd, na.rm = T))
new.dat


## Expectation and variance with Leader data
Leader <- read_dta("/LeaderDataset.dta")

age.lead <- Leader %>%
  dplyr::select(idacr,year,leader,age)

# Our population of leaders
head(age.lead, n=9)

# Mean value of age in the "population" (the full dataset)
mean(age.lead$age, na.rm = T)

# PLot the distribution of age
ggplot(age.lead, aes(x=age)) +
  geom_density() + theme_classic()

# Simulation: 1000 times draw 1000 leaders and calculate mean age (store in vector)
sim.lead <- 1000
all.mn <- rep(NA, sim.lead)

for (i in 1:sim.lead){
  lead.draw <- sample_n(age.lead, 1000)
  all.mn[i] <- mean(lead.draw$age, na.rm = T)
}

# Mean value of age in our 1000 simulations
mean(all.mn, na.rm = T)

# Plot the simulated data (mean of simulated age)
d <- data.frame(x = all.mn)
ggplot(d, aes(x)) +
  geom_histogram(aes(y = stat(density)),fill="navyblue", color="black", alpha=0.9) + 
  xlab("Mean simulated Age") + ylab("Density") +
  geom_vline(xintercept = 57.122, color = "black", size = 1.2) +
  geom_text(aes(x = 57, y = 1.53, label = "Population mean")) +
  theme_classic()

ggplot(d, aes(x)) +
  geom_histogram(aes(y = stat(density)),fill="skyblue", color="black", alpha=0.9) +
  geom_density(col = "red") + xlab("Mean simulated Age") + ylab("Density") +
  geom_vline(xintercept = 57.122, color = "black", linetype = "dashed") +
  theme_classic()

## Normal distribution using R 
# 1SD: 68%
pnorm(1) - pnorm(-1)

# 2SD: 95%
pnorm(2) - pnorm(-2)

# 3SD: 99%
pnorm(3) - pnorm(-3)

## Empirical rule of normal distribution: Use the leader data
# Calculate mean and SD of data
mu <- mean(Leader$age, na.rm = T)
sig <- sd(Leader$age, na.rm = T)

# 1SD: 68% (using the leader data)
pnorm(mu+sig, mean = mu, sd = sig) - pnorm(mu-sig, mean = mu, sd = sig)

# Plot for leader age data with mean and portions by 1/2/3 SDs
ggplot(Leader, aes(age)) +
  geom_histogram(aes(y = stat(density)),fill="lightgrey", color="black", alpha=0.9) +
  geom_density(col = "red") + xlab("Mean Age") + ylab("Density") +
  geom_vline(xintercept = mu, color = "black") +
  geom_text(aes(x=mu, y = 0.045, label = "Mean"), size = 2.5) +
  geom_vline(xintercept = mu+sig, color = "darkblue", linetype = "dashed") +
  geom_text(aes(x=73, y = 0.045, label = "Mean + SD"), size = 2.5) +
  geom_vline(xintercept = mu-sig, color = "darkblue", linetype = "dashed") +
  geom_text(aes(x=43, y = 0.045, label = "Mean - SD"), size = 2.5) +
  geom_vline(xintercept = mu+ 2*sig, color = "maroon", linetype = "dashed") +
  geom_text(aes(x=83, y = 0.045, label = "Mean + 2SD"), size = 2.5) +
  geom_vline(xintercept = mu- 2*sig, color = "maroon", linetype = "dashed") +
  geom_text(aes(x=31, y = 0.045, label = "Mean - 2SD"), size = 2.5) +
  geom_vline(xintercept = mu+ 3*sig, color = "orange", linetype = "dashed") +
  geom_text(aes(x=95, y = 0.045, label = "Mean + 3SD"), size = 2.5) +
  geom_vline(xintercept = mu- 3*sig, color = "orange", linetype = "dashed") +
  geom_text(aes(x=20, y = 0.045, label = "Mean - 3SD"), size = 2.5) +
  theme_classic()
