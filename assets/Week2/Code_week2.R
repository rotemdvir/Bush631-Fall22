# Bush 631-603: Week 2
# Mattes and Weeks 2020 Data

# Download the package haven, then upload it
library(haven)

## Upload data from your folder of choice
mydata <- read_dta("Dropbox/TAMU/Bush631_QuantMethods/Week2_Causality_I/MattesWeeksEdit.dta")
View(mydata)

## Exploring the data
dim(mydata)
head(mydata)

summary(mydata)

### Cross-tabs  ###
# Variable hawk_t values: 1 = president is hawk; 2 = president is dove
# Variable hddv1 values: 1= Strongly disapprove; 2 = somewhat disapprove; 3 = neither approve nor disapprove
#                        4 = somewhat approve; 5 = Strongly approve

table(type = mydata$hawk_t, support = mydata$hddv1)

# Variable party_t values: 1 = president is republican; 2 = president is democrat
tab2 <- table(support = mydata$hddv1, party = mydata$party_t)
addmargins(tab2)

## Calculating mean level of approval (1-5 scale)
mean1 <- sum(mydata$hddv1) / nrow(mydata)
mean2 <- sum(mydata[,14]) / nrow(mydata)

# Variable approve_b values: 1 = approve (if hddv1 = 4 or hddv1 = 5); 0 = disapprove (if hddv1 = 1 or hddv1 = 2)
#                     NA = missing (if hddv1  = 3)
table(mydata$approve_b, exclude = NULL)
mean3 <- mean(mydata$approve_b, na.rm = TRUE)

### Sub-setting the data  ###

# Logical values
class(FALSE)

# What is the numerical values of TRUE / FALSE
as.integer(TRUE)
as.integer(FALSE)

# Vector of logical values
v1 <- c(FALSE,TRUE,TRUE,FALSE,FALSE)

mean(v1)
sum(v1)

FALSE & TRUE
TRUE | FALSE
FALSE & TRUE & FALSE
TRUE & (FALSE | TRUE)

v2 <- c(TRUE,FALSE,FALSE,TRUE,TRUE)
v1 & v2

## Relational operations
12 > 9
"aggies" == "Aggies"
"Aggies" == "Aggies"

v3 <- c(4,8,-1,-9,7)
v3 < 0
v3 >= 4

# The operator != represent NOT equal
v3 != 7

### Sub-sets and calculations  ###

# Proportion of approval based on president party
mean(mydata$approve_b[mydata$party_t == 1], na.rm = TRUE)   # R president
mean(mydata$approve_b[mydata$party_t == 2], na.rm = TRUE)   # D president

# Approval (on a scale of 1-5) based on president party
mean(mydata$hddv1[mydata$party_t == 1], na.rm = TRUE)   # R president
mean(mydata$hddv1[mydata$party_t == 2], na.rm = TRUE)   # D president

# Proportion of approval based on president type (hawk/dove)
mean(mydata$approve_b[mydata$hawk_t == 1], na.rm = TRUE)   # R president
mean(mydata$approve_b[mydata$hawk_t == 2], na.rm = TRUE)   # D president

## Sub-set only president is hawk
mysubdata1 <- mydata[mydata$hawk_t == 1,]
View(mysubdata1)
dim(mysubdata1)

# Mean approval (1-5 scale) and proportion of approval for hawk president only 
mean(mysubdata1$hddv1)
mean(mysubdata1$approve_b, na.rm = TRUE)

### Sub-set command ###
# subset only dove president
mysubdata2 <- subset(mydata, subset = (hawk_t == 2))
View(mysubdata2)

# Variable gender values: 1 = male; 2 = female 
# subset only female respondents
mysubdata3 <- subset(mydata, subset = (gender == 2))

# subset only republican president
mysubdata_rep <- subset(mydata, subset = (party_t == 1))

# subset only democrat president
mysubdata_dem <- subset(mydata, subset = (party_t == 2))

# calculate difference in means between subsets (republican  - democrat)
mean(mysubdata_rep$approve_b, na.rm = TRUE) - mean(mysubdata_dem$approve_b, na.rm = TRUE) 

# subset hawk president AND male respondent
mysubdata4_male <- subset(mydata, subset = (hawk_t == 1 & gender == 1))

# subset hawk president AND female respondent
mysubdata4_female <- subset(mydata, subset = (hawk_t == 1 & gender == 2))

# calculate difference in means between subsets (women - men, only for hawk president)
mean(mysubdata4_female$approve_b, na.rm = TRUE) - mean(mysubdata4_male$approve_b, na.rm = TRUE) 

### Conditional statements with ifelse()  ###
# Variable voted16 values: 1 = no; 2 = usually vote, but did not in 2016; 3 = not sure; 4 = yes, definitely voted
# Variable new1 values: 1 = voted in 2016 AND female; 0 = everything else (all men and women who did not/unsure if voted in 2016)
mydata$new1 <- ifelse(mydata$voted16 == 4 &
                         mydata$gender == 2,1,0)

# Proportion of support based on new variable
table(female_voters = mydata$new1)
table(newvar = mydata$new1, support = mydata$approve_b)

# Variable hawk values: 1 = Disagree strongly; 2 = Disagree somewhat; 3 = Neither; 4 = Agree somewhat; 5 = Agree strongly
# Variable no_hawks values: 1 = no hawks (value of hawk is 4 or 5) ; 0 = everybody else (value of hawk is 3 or 2 or 1)
mydata$no_hawks <- ifelse(mydata$hawk>3,1,0)
table(NoHawks = mydata$no_hawks)

### Factor variables  ###
# Variable intl values: 1 = Disagree strongly; 2 = Disagree somewhat; 3 = Neither; 4 = Agree somewhat; 5 = Agree strongly
# The variable internatiolism is coded based on intl
class(mydata$internatiolism)

# define variable as factor
mydata$internatiolism <- as.factor(mydata$internatiolism)

# detail the levels - categories of values for the variable
levels(mydata$internatiolism)

# number of respondents in each level
table(mydata$internatiolism)

### tapply function ###
# calculate the proportion of support for the president's action based on level of internatiolism
app_int <- tapply(mydata$approve_b, mydata$internatiolism, mean, na.rm = TRUE)

# sort by order (smallest to largest)
sort(app_int)

# calculate the proportion of support for the president's action based on his type (hawk or dove)
tapply(mydata$approve_b, mydata$hawk_t, mean, na.rm = TRUE)

# calculate the proportion of support for the president's action based on his party (republican or democrat)
tapply(mydata$approve_b, mydata$party_t, mean, na.rm = TRUE)

## Create factor variable for groups of respondents based on party and policy treatments
# Variable party_policy values: 1 = president is republican, policy is conciliatory; 2 = president is republican, policy is status-quo;
#                               3 = president is democrat, policy is conciliatory; 4 = president is democrat, policy is status-quo
mydata$party_policy <- NA
mydata$party_policy[mydata$party_t == 1 & mydata$rapproche_t == 1] <- 1
mydata$party_policy[mydata$party_t == 1 & mydata$rapproche_t == 2] <- 2
mydata$party_policy[mydata$party_t == 2 & mydata$rapproche_t == 1] <- 3
mydata$party_policy[mydata$party_t == 2 & mydata$rapproche_t == 2] <- 4

# calculate the proportion of support for the president's action based on levels of party_policy 
tapply(mydata$approve_b, mydata$party_policy, mean, na.rm = TRUE)

## The code for the bar plot is a little more complicated so we'll skip it for now..:)  ##

