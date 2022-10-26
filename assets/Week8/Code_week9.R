# Week 9 code
# ATOP data v5 (2021)

library(tidyverse)
library(viridis)
library(haven)
library(readxl)

# The birthday problem function
bday <- function(k){
  logdenom <- k * log(365) + lfactorial(365-k)
  lognumer <- lfactorial(365)
  pr <- 1 - exp(lognumer - logdenom)
  return(pr)
}

# Check group size
k <- 1:20
test_bday <- bday(k)
names(test_bday) <- k

# Display vector of values
test_bday[15:20]

### Aggies in NFL and conditional probability   ###
Ags <- read_excel("~/Ags_Data.xlsx")

# Tabulate data
t <- table(Conf = Ags$Conference, Pos.Grp = Ags$Group)
addmargins(t)

### ATOP data: Military alliances ###
atop.full <- read_dta("~/atop5_0m.dta")

# Clear data and add country names
# Focus on specific variables
atop <- atop.full %>%
  dplyr::select(atopid, member, yrent, moent, ineffect,
       estmode, pubsecr, secrart, length, 
       defense, offense, neutral, nonagg, consul, active, notaiden,
       terrres, specthrt, milaid, base, armred, ecaid)

# Upload ccode and country names data
ccodes <- read.csv("~/Downloads/COW country codes.csv")

# Join country names data
atop2 <- left_join(atop, ccodes, by = "member") %>%
  arrange(StateAbb)

### compute probabilities with prop.table

# Does alliance require domestic ratification (binary)
prop.table(table(Ratification = atop2$estmode))

# Does alliance have secret provisions?
prop.table(table(publicity = atop2$pubsecr))

# Focus on US: tidyverse style
atop.us <- atop2 %>%
  filter(member == 2) 

# US alliances: defensive treaties
prop.table(table(atop.us$defense))

# Conditional: types of military aid given that alliance has defensive provision
prop.table(table(atop2$milaid[atop2$defense == 1]))

# Joint probability: signing both defense and offense treaties
prop.table(table(def = atop2$defense, off = atop2$offense))

# Joint probability: signing an offensive treaty and with secret provisions
prop.table(table(secret = atop2$secrart, off = atop2$offense))

# Joint probability: signing defensive treaty  and different types of threat
prop.table(table(threat = atop2$specthrt, def = atop2$defense))

prop.table(table(aid = atop2$milaid, def = atop2$defense))

### Independence: defense treaty and providing economic aid
# Joint probability
x <- prop.table(table(a = atop2$ecaid, b = atop2$defense))

# Marginal probabilities for each option
x1 <- prop.table(table(atop2$ecaid))
x2 <- prop.table(table(atop2$defense))

# Scatter plot to check/shows the independence 
plot(c(x1 * x2["1"]), c(x[, "1"]), xlim = c(0,0.5), ylim = c(0,0.5),
     xlab = "P(Defense) * P(Econ Aid)", ylab = "Joint prob.: P(Defense&Econ Aid)",
     main = "Checking for independence of events: Military Alliances")
abline(0,1)
