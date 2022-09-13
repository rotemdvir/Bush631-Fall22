# Bush 631-603: Week 3
# Fuhrmann and Horowitz 2015 Data

## Upload data
library(haven)  # Stata type file
mydata <- read_dta("~/Week3_Causality_II/RebelsDataset_FH2015.dta")
View(mydata)

## Explore data
dim(mydata)
summary(mydata)

## Main variables used

# rebel experience: yes/no (coded 1/0)
table(rebels = mydata$rebel)

# revolutionary leader: yes/no (coded 1/0)
table(rev_leaders = mydata$revolutionaryleader)

# pursue nuclear tech: yes/no (coded 1/0)
# If you want to see the number of NAs add exclude = NULL to the table function
table(pursue_nukes = mydata$pursuit, exclude = NULL)

# Creating treatment and control groups 
## Group by rebel experience (yes/no)
lead_rebels <- subset(mydata, subset = (rebel == 1))
lead_norebels <- subset(mydata, subset = (rebel == 0))

dim(lead_rebels)

## Groups by revolutionary leader
## Variable revolutionaryleader values: 0 = no; 1 = yes
rev_leader <- subset(mydata, subset = (revolutionaryleader == 1))
rev_noleader <- subset(mydata, subset = (revolutionaryleader == 0))

dim(rev_leader)

## Calculating difference in means: compare rebel and no rebel groups
## Variable pursuit values: 0 = no pursuit 0f nuclear weapons; 1 = yes pursuit of nuclear weapons
mean(lead_rebels$pursuit, na.rm = TRUE)
mean(lead_norebels$pursuit, na.rm = TRUE)
mean(lead_rebels$pursuit, na.rm = TRUE) - mean(lead_norebels$pursuit, na.rm = TRUE)

## Variable bombprgm values: 0 = no nuclear program; 1 = yes nuclear program
table(bomb_program = mydata$bombprgm)

# Calculate difference-in-means: compare rebel/no rebel groups
mean(lead_rebels$bombprgm, na.rm = TRUE)
mean(lead_norebels$bombprgm, na.rm = TRUE)
mean(lead_rebels$bombprgm, na.rm = TRUE) - mean(lead_norebels$bombprgm, na.rm = TRUE)

## Compare revolutionary leaders and no revolutionary leaders (both ourcome measures)
mean(rev_leader$pursuit, na.rm = TRUE) - mean(rev_noleader$pursuit, na.rm = TRUE)
mean(rev_leader$bombprgm, na.rm = TRUE) - mean(rev_noleader$bombprgm, na.rm = TRUE)

### Confounders: Alliance with a superpower   ###
## Proportion of superpower alliance in rebel and no rebel groups
# Variable spally values: 0 = no alliance with superpower; 1 = yes alliance with a superpower
prop.table(table(lead_rebels$spally))
prop.table(table(lead_norebels$spally))

## Subset within alliance pre-treatment: rebels/no rebels 
rebel_ally <- subset(lead_rebels, subset = (spally == 1))
norebel_ally <- subset(lead_norebels, subset = (spally == 1))

# Compare rebel and no rebel leaders: pursuing nuclear weapons (both groups have alliance with superpower)
mean(rebel_ally$pursuit, na.rm = TRUE) - mean(norebel_ally$pursuit, na.rm = TRUE)

### Before and after design ###
## Before and after using the time indicator
## Create subsets for rebel leaders: 1st year in office; after year 1 in office
reb_one <- subset(lead_rebels, subset = (nonpuryrs == 0))
reb_after <- subset(lead_rebels, subset = (nonpuryrs > 0))
 
## Calculate difference-in-means for rebel leaders (year 1 versus other years)
mean(reb_one$pursuit, na.rm = T) - mean(reb_after$pursuit, na.rm = T)

### Descriptive stats ###

## Median
median(lead_rebels$pursuit, na.rm = TRUE)
median(lead_norebels$pursuit, na.rm = TRUE)

median(mydata$pursuit, na.rm = TRUE)

# Economic growth measures: GDP per capita
median(mydata$gdpcap, na.rm = TRUE)
mean(mydata$gdpcap, na.rm = TRUE)

# Involvement in MID: 5 year average
median(mydata$disputes, na.rm = TRUE)
median(lead_rebels$disputes, na.rm = TRUE)

## mean-median debate
v1 <- c(100,200,300)
mean(v1)
median(v1)

v2<- c(100,200,4000)
mean(v2)
median(v2)


## Quartiles and summary
summary(lead_rebels$gdpcap)
summary(lead_norebels$gdpcap)

## IQR: 50% of the data
IQR(lead_rebels$openness, na.rm = T)
IQR(lead_norebels$openness, na.rm = T)

## More ways to split the data
quantile(lead_rebels$gdpcap, probs = seq(from = 0, to = 1, by = 0.1), na.rm = T)
quantile(lead_norebels$gdpcap, probs = seq(from = 0, to = 1, by = 0.1), na.rm = T)

quantile(lead_rebels$disputes, probs = seq(from = 0, to = 1, by = 0.1), na.rm = T)
quantile(lead_norebels$disputes, probs = seq(from = 0, to = 1, by = 0.1), na.rm = T)

### Standard Deviation  ###
# Use R to calculate SD
sd(lead_rebels$pursuit, na.rm = TRUE)
sd(lead_norebels$pursuit, na.rm = TRUE)

sd(lead_rebels$disputes, na.rm = TRUE)
sd(lead_norebels$disputes, na.rm = TRUE)

