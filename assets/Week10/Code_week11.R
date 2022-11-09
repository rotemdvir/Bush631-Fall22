# Week 11: Simulation data

library(tidyverse)
library(fabricatr)

### Simulation for SATE
# Create Sample, Control and treatment groups (means and SDs)
# Single sample
n <- 500
mu0 <- 0
sd0 <- 1
mu1 <- 1
sd1 <- 1

y0 <- rnorm(n, mean = mu0, sd = sd0)
y1 <- rnorm(n, mean = mu1, sd = sd1)
tau <- y1 - y0
SATE <- mean(tau)

### Repeat
sims <- 5000
diff.means <- rep(NA,sims)

# Loop for diff-in-means
for (i in 1:sims){
  treat <- sample(c(rep(1, n/2), rep(0, n/2)), size = n, replace = FALSE)
  diff.means[i] <- mean(y1[treat == 1] - mean(y0[treat == 0]))
}

est.error <- diff.means - SATE
summary(est.error)

# Plot estimator distribution 
hist(diff.means, freq = FALSE)
abline(v=SATE, col = "blue")

# SD of estimator
sd(diff.means)
sqrt(mean((diff.means - SATE)^2))

### Repeat simulation and add SE calculation
sims2 <- 5000
diff.means2 <- rep(NA,sims)
diff.se <- rep(NA, sims)

for (i in 1:sims){
  Y0 <- rnorm(n, mean = mu0, sd = sd0)
  Y1 <- rnorm(n, mean = mu1, sd = sd1)
  treat <- sample(c(rep(1, n/2), rep(0, n/2)), size = n, replace = FALSE)
  diff.means2[i] <- mean(Y1[treat == 1] - mean(Y0[treat == 0]))
  diff.se[i] <- sqrt(var(Y1[treat == 1])/(n/2) + var(Y0[treat == 0])/(n/2))
}

sd(diff.means2)
mean(diff.se)

### Confidence Intervals
# Find critical values with qnorm()
qnorm(0.05, lower.tail = FALSE)
qnorm(0.025, lower.tail = FALSE)
qnorm(0.005, lower.tail = FALSE)

# Create CIs for JCPOA survey example
# Sample size and proportion of support
n <- 2000
x.bar <- 0.6

# Calculate SE
Iran.se <- sqrt(x.bar * (1-x.bar)/n)

# Construct CIs
c(x.bar - qnorm(0.995) * Iran.se, x.bar + qnorm(0.995) * Iran.se)   #99%
c(x.bar - qnorm(0.975) * Iran.se, x.bar + qnorm(0.975) * Iran.se)   #95%
c(x.bar - qnorm(0.95) * Iran.se, x.bar + qnorm(0.95) * Iran.se)   #90%


### Use MOE for sample sizes
# Define MOEs
moe <- c(0.01, 0.02, 0.03, 0.05)

# Define vector of proportion of support (0-100 by 1%)
prop <- seq(from = 0.01, to = 0.99, by = 0.01)

# Using MOE and proportion for possible sample sizes
num <- 1.96^2 * prop * (1-prop) / moe[1]^2
head(num)

# Plot all values 
plot(prop, num, type = "l", col = "blueviolet")
lines(prop, 1.96^2 * prop * (1-prop) / moe[2]^2, col = "green")
lines(prop, 1.96^2 * prop * (1-prop) / moe[3]^2, col = "red")
lines(prop, 1.96^2 * prop * (1-prop) / moe[4]^2, col = "blue")
text(0.5, 9800, "MOE = 0.01")
text(0.5, 2500, "MOE = 0.02")
text(0.5, 1500, "MOE = 0.03")
text(0.5, 500, "MOE = 0.05")

### Using data: Calculate means, SEs and CIs, plot and check for overlap ###

# Simulation 1: environmental policy (fighting plastic bags)
# Simulate treatment group distribution
xt.sims <- rbinom(1000, size = 1000, prob = 0.43) / 1000
head(xt.sims)

# Simulate control group distribution
xc.sims <- rbinom(1000, size = 1000, prob = 0.32) / 1000
head(xc.sims)

# Mean ATE
mean(xt.sims-xc.sims)

# Plot with tidyverse
hp <- data.frame(mn = (xt.sims-xc.sims))
ggplot(hp, aes(mn)) +
  geom_histogram(fill="lightgrey", color="black", alpha=0.9) +
  geom_vline(xintercept = mean(hp$mn), color = "blue", linetype = "dashed", size = 1.5) +
  xlab("Estimated ATEs") + ylab("") + ggtitle("ATE Sampling Distribution") +
  theme_bw()

# Calculate SEs
x.se <- sqrt((0.43*0.57)/1000 + (0.32*0.68)/1100)

# Construct CIs
c(0.43 - qnorm(0.975) * x.se, 0.43 + qnorm(0.975) * x.se)   #95%
c(0.32 - qnorm(0.975) * x.se, 0.32 + qnorm(0.975) * x.se)   #95%

# Plot with tidyverse: check for meaningful effect
se_plot <- data.frame(x = c("Cash back","Plastics fee"),
                      y = c(0.43,0.32),
                      se = c(0.021,0.021))
  
ggplot(se_plot, aes(x,y)) +
  geom_errorbar(aes(ymin = y-2*se, ymax = y+2*se), width = 0.25, color = "purple") + 
  geom_point(size = 2) + ylim(0.25,0.5) +
  geom_hline(yintercept = 0.369, linetype = "dashed") +
  geom_text(x=1.5,y=0.37,label = "No \n Overlap", color = "red", size = 4.5) +
  geom_text(x=0.85,y=0.43,label = "Point \n Estimate", color = "blue", size = 4.5) +
  geom_text(x=2.15,y=0.32,label = "Point \n Estimate", color = "blue", size = 4.5) +
  ylab("Mean support") + xlab("Population Groups") +
  theme_bw()

### Generate data with fabricate
# Set seed for randomizer
set.seed(12345)

# Create data: treatments (assign 0.5 for equal groups)
exp.dat <- fabricate(
  N = 1000,
  trt1 = draw_binary(N = 1000, prob = 0.5),
  trt2 = draw_binary(N = 1000, prob = 0.5))

# Create data: outcome variables
random_vars <- fabricate(
  N = 1000,
  dv_cor1 = correlate(given = exp.dat$trt1, rho = 0.8,
                      draw_binary, N = 1000, prob = 0.65),
  dv_cor2 = correlate(given = exp.dat$trt2, rho = 0.65,
                      draw_binary, N = 1000, prob = 0.35),
  cont_cor1 = correlate(given = exp.dat$trt1, rho = 0.55,
                        rnorm, mean = 1500, sd = 30),
  cont_cor2 = correlate(given = exp.dat$trt2, rho = 0.75,
                        rnorm, mean = 1450, sd = 45))

exp.dat <- left_join(exp.dat, random_vars, by = "ID")
head(exp.dat)

# Check the data: random assignment: number of respondents in each group
n.zero <- sum(exp.dat$trt1 == 0)
n.one <- sum(exp.dat$trt1 == 1)

# Mean values of each group for cont. outcome variable
est.zero <- mean(exp.dat$cont_cor1[exp.dat$trt1 == 0])
est.one <- mean(exp.dat$cont_cor1[exp.dat$trt1 == 1])

# ATE
est.one - est.zero

# Mean values of each group for cont. outcome variable (tidyverse approach)
exp.dat %>%
  group_by(trt1) %>%
  summarise(mn1 = mean(cont_cor1),
            mn2 = mean(cont_cor2))

# Histograms with tidyverse: distribution of mean for each group
trt.lab <- c("Treatment=0","Treatment=1")
names(trt.lab) <- c(0,1)

trt_m <- exp.dat %>%
  group_by(trt1) %>%
  summarise(trt_mn  = mean(cont_cor1))
  
ggplot(exp.dat, aes(x=cont_cor1, group = factor(trt1))) +
  geom_histogram(fill="lightgreen", color="black", alpha=0.9) +
  geom_vline(data = trt_m, aes(xintercept = trt_mn, color = factor(trt1)), linetype = "dashed", size = 1.4) +
  facet_grid(trt1~., labeller = labeller(trt1 = trt.lab)) + 
  xlab("Outcome Variable Distribution") + ylab("") +
  theme_bw() + theme(legend.position = "none")

# Histogram of both group distribution (base R)
hist(exp.dat$cont_cor1[exp.dat$trt1 == 1], freq = FALSE, main = "Outcome Variable Distribution")
abline(v = mean(exp.dat$cont_cor1[exp.dat$trt1 == 1]), col = "blue")

hist(exp.dat$cont_cor1[exp.dat$trt1 == 0], freq = FALSE, main = "Outcome Variable Distribution")
abline(v = mean(exp.dat$cont_cor1[exp.dat$trt1 == 0]), col = "red")

# Calculate SEs and generate CIs
alpha <- 0.05
se.zero <- sd(exp.dat$cont_cor1[exp.dat$trt1 == 0]) / sqrt(n.zero)
se.one <- sd(exp.dat$cont_cor1[exp.dat$trt1 == 1]) / sqrt(n.one)

ci.zero <- c(est.zero - qnorm(1-alpha / 2) * se.zero, est.zero + qnorm(1-alpha / 2) * se.zero)
ci.one <- c(est.one - qnorm(1-alpha / 2) * se.one, est.one + qnorm(1-alpha / 2) * se.one)

# Plot both groups CIs: check for overlap 
se_plot2 <- data.frame(x = c("Aid to Dem","Aid to non-Dem"),
                       y = c(1509.973,1489.333),
                       se = c(1.06291, 1.068058))

ggplot(se_plot2, aes(x,y)) +
  geom_pointrange(aes(ymin = y-2*se, ymax = y+2*se), color = "blue", size = 1.75) + 
  geom_point(size = 2) + ylim(1480,1520) +
  geom_hline(yintercept = 1500, linetype = "dashed") +
  geom_text(x=1.5,y=1502,label = "No \n Overlap", color = "red", size = 4.5) +
  geom_text(x=0.8,y=1510,label = "Point \n Estimate", color = "purple", size = 4.5) +
  geom_text(x=2.2,y=1489,label = "Point \n Estimate", color = "purple", size = 4.5) +
  ylab("Mean Support") + xlab("Population Group") +
  theme_bw()

# CI: CLT vs. t-distribution
# Control group: CLT and t-dist.
ci.zero <- c(est.zero - qnorm(1-alpha / 2) * se.zero, est.zero + qnorm(1-alpha / 2) * se.zero)
ci.zeroT <- c(est.zero - qt(0.975,df = n.zero - 1) * se.zero,
              est.zero + qt(0.975,df = n.zero - 1) * se.zero)

# Treatment group: CLT and t-dist.
ci.one <- c(est.one - qnorm(1-alpha / 2) * se.one, est.one + qnorm(1-alpha / 2) * se.one)
ci.oneT <- c(est.one - qt(0.975,df = n.one - 1) * se.one,
             est.one + qt(0.975,df = n.one - 1) * se.one)


  

