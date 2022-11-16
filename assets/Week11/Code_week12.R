# Simulation data
# Mattes and Weeks (2020): Week 2 data

library(tidyverse)
library(haven)

### Code for distribution of guesses for tea tasting
# All possible options of guesses
true <- c(choose(4,0) * choose(4,4),
          choose(4,1) * choose(4,3),
          choose(4,2) * choose(4,2),
          choose(4,3) * choose(4,1),
          choose(4,4) * choose(4,0))

# Proportion of true guess (add names for number of true guesses)
true <- true / sum(true)
names(true) <- c(0,2,4,6,8)

# Save as data for plotting
t <- as_tibble(true)
t <- t %>%
  mutate(guess = c(0,2,4,6,8))

# Barplot
ggplot(t, aes(factor(guess), value)) +
  geom_bar(stat = "identity", fill = "blue") +
  xlab("Number of correct guesses") + ylab("Probability") +
  ggtitle("Sampling distribution of Tea Tasting") +
  theme_bw() 
  

## Simulation for Tea exp. distribution
sims <- 1000
guess <- c("M","T","T","M","M","T","T","M")
correct <- rep(NA, sims)

for (i in 1:sims){
  cups <- sample(c(rep("T", 4), rep("M", 4)), replace = FALSE)
  correct[i] <- sum(guess == cups)
}

# Proportions fit the original experiment (as barplot)
prop.table(table(correct))

### Hypotheses test with simulation
# Trump support simulation 
t_vote <- rbinom(n = 1000, size = 1363, prob = 0.475)
t_share <- t_vote / 1363
head(t_share)

# Plot with tidyverse
t_sim <- data.frame(votes = t_share)
ggplot(t_sim, aes(votes)) +
  geom_histogram(fill="lightgrey", color="black", alpha=0.9) + xlim(0.4,0.55) +
  geom_vline(xintercept = mean(t_sim$votes), color = "blue", linetype = "dashed", size = 1.3) +
  geom_vline(xintercept = 0.44, color = "red", size = 1.3) +
  geom_text(aes(x = 0.49, y = 130, label = "Simulation \n Mean")) +
  geom_text(aes(x = 0.43, y = 75, label = "Sample \n Mean")) +
  xlab("Estimated Proportions") + ylab("") + ggtitle("Trump support simulations") +
  theme_bw()

# Plot with base R
hist(t_share, xlim = c(0.4,0.55), xlab = "Simulted sample proportions supporting Trump", main = "")
abline(v = 0.44, col = "red", lwd = 3)
abline(v = 0.475, col = "orange", lwd = 3)

# Probability of error (two-sided) 
mean(t_share < 0.44) + mean(t_share > 0.51)

# Plot with tidyverse (two-sided error)
ggplot(t_sim, aes(votes)) +
  geom_histogram(fill="lightgrey", color="black", alpha=0.9) + xlim(0.4,0.55) +
  geom_vline(xintercept = mean(t_sim$votes), color = "blue", linetype = "dashed", size = 1.3) +
  geom_vline(xintercept = 0.44, color = "red", size = 1.3) +
  geom_vline(xintercept = 0.51, color = "red", size = 1.3) +
  xlab("Estimated Proportions") + ylab("") + ggtitle("Trump support simulations") +
  theme_bw()

# Plot with base R (two-sided error)
hist(t_share, xlim = c(0.4,0.55), xlab = "Simulted sample proportions supporting Trump", main = "")
abline(v = 0.44, col = "red", lwd = 3)
abline(v = 0.475, col = "orange", lwd = 3)
abline(v = 0.51, col = "red", lwd = 3)

# Probability of error (one-sided)
mean(t_share < 0.44)


### ISR thermometer
# Define all values
n <- 800
x_bar <- 58
mu <- 55
sd <- 30
se <- (sd/sqrt(n))

# Calculate z-score
z_ISR <- (x_bar - mu)/(sd/sqrt(n))

# What is the p-value?
pnorm(-z_ISR)

# Generate standard normal distribution for plot
p.norm <- data.frame(x = c(-4,4))

# Plot with areq
ggplot(p.norm, aes(x=x)) +
  stat_function(fun = dnorm) +
  geom_area(stat = "function",  fun = dnorm, fill = "#00998a", xlim = c(-4,-2.8)) +
  geom_vline(xintercept = -2.8, color = "red") +
  geom_text(aes(x=-3.5, y=0.05, label = "our \n z-stat")) +
  theme_bw()

### Test with CIs
# Construct 95% and 99% CIs for x_bar = 58
c(x_bar - qnorm(0.995) * se, x_bar + qnorm(0.995) * se)
c(x_bar - qnorm(0.975) * se, x_bar + qnorm(0.975) * se)

### Test with prop.test 
# 600 supporters out of 1363 sample (fit x-bar = 0.44)
prop.test(600, n = 1363, p = 0.475, correct = FALSE)

### Generate data with fabricate
library(fabricatr)
set.seed(12345)

exp.dat <- fabricate(
  N = 1000,
  trt1 = draw_binary(N = 1000, prob = 0.5),
  trt2 = draw_binary(N = 1000, prob = 0.5))

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

# t-test of treatment 1 and DV of cont_cor1
# Define DV and groups for comparison (trt1 == 0 and trt1 == 1)
t.test(exp.dat$cont_cor1[exp.dat$trt1 == 0], exp.dat$cont_cor1[exp.dat$trt1 == 1])

### Using Mattes and Weeks (2019) data (Week 2)
hawksdata <- read_dta("~/MattesWeeksEdit.dta")
table(hawksdata$hawk_t,hawksdata$hddv1)

# t-test of two groups (Hawk vs. Dove type)
t.test(hawksdata$hddv1[hawksdata$hawk_t == 1], hawksdata$hddv1[hawksdata$hawk_t == 2])

# Visualizing 'inflated SEs' (random data)
se_plot <- data.frame(x = c("Treatment","Control"),
                       y = c(100,85),
                       se = c(4.5, 4.6))

ggplot(se_plot, aes(x,y)) +
  geom_pointrange(aes(ymin = y-2*se, ymax = y+2*se), color = "purple", size = 1.75) + 
  geom_point(size = 2) + ylim(75,110) +
  geom_hline(yintercept = 92, linetype = "dashed") +
  geom_text(x=1.5,y=95,label = "CIs Overlap", color = "red", size = 4.5) +
  ylab("Mean") + xlab("Population Groups") +
  theme_bw()


### Power analysis
# Manual calculation of power
# Define parameters and values
n <- 250
p_bar <- 0.47
p <- 0.5
alpha <- 0.05
cr_value <- qnorm(1 - alpha/2)
se_bar <- sqrt(p_bar * (1-p_bar) / n) 
se_p <- sqrt(p * (1-p) / n)

# Check for power
pnorm(p - cr_value * se_p, mean = p_bar, sd = se_bar) +
  pnorm(p + cr_value * se_p, mean = p_bar, sd = se_bar, lower.tail = F)

## Various power analysis options
#  What is minimum group size? 
power.prop.test(p1 = 0.5, p2 = 0.45, sig.level = 0.05, power = 0.9)
power.prop.test(p1 = 0.5, p2 = 0.45, sig.level = 0.05, power = 0.8)

# What is minimum group size? 
power.prop.test(p1 = 0.05, p2 = 0.1, sig.level = 0.05, power = 0.8)

# Test value of power (define group size)
power.prop.test(n = 100, p1 = 0.05, p2 = 0.1, sig.level = 0.05)

# Use t.test version for continuous variable
# Minimum sample size: one sample
power.t.test(delta = 0.3, sd = 1, type = "one.sample", power = 0.85)
power.t.test(delta = 0.3, sd = 1, type = "one.sample", power = 0.9)

# Power calculation for one sample
power.t.test(n = 250, delta = 0.3, sd = 1, type = "one.sample")
power.t.test(n = 5000, delta = 0.03, sd = 1, type = "one.sample")

# Sample size for two-sample t.test of power
power.t.test(delta = 0.3, sd = 1, type = "two.sample", alternative = "one.sided", power = 0.85)
power.t.test(delta = 0.03, sd = 1, type = "two.sample", alternative = "one.sided", power = 0.85)
