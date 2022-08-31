# BUSH 631-603: Week 1
# Data used: ags.xlsx

# Everything written following a # sign is comments - R does not run these lines of code
# To run a line of code, place the marker at the desired line and click Run (top right side of the Script panel)

## Using R for basic math

2+5
255+345

2*5
255*345

2/5
255/345

2*(5+255)
345/(2*255)

sqrt(9)
sqrt(255)

### Objects ###
## Creating Objects (assign value using the <- operator)
## If you want to assign character value, use "NAME".
number <- 5
no_number <- "5"
letter <- "W"
word <- "Aggies"

result <- 2+5

## To see the value of an object use the print() function
## You can also see the value by typing the object name in the console 
print(result)

result <- 345/(2*255)
print(result)

## Errors: R is case-sensitive (result is not like Result)
## You cannot implement math operations on character (no_number is defined as character above)
result/5
Result/5
no_number/5

sqrt(result)

## class() function to learn of the type of object
class(result)
class(letter)
class(word)
class(sqrt)

### Vectors ####
v1 <- c(1,2,3,4)
v2 <- c("A","B","C","D")
v3 <- c(11,12,13)

v_join1 <- c(v1,v2)
v_join2 <- c(v1,v3)

## Upload data: use the drop-down menu on the right panel ("Import Dataset")
## Select by data type (excel, csv is base, etc.)
## View() function opens the data in a new tab
View(ags) 

## Indexing and data exploring
class(ags)

## Look at the data in the console
ags

# Indexing: row 1 and all columns
ags[1,]

# Indexing: column 1 and all rows
ags[,1]

## Indexing: use the c() function to focus on specific rows/columns
ags[c(1,2,4),]
ags[,c(1,2,4)]
ags[1:3]
ags[c("Coaching")]
ags[1:10, c("Year","Bowl")]

## Look at data using the $ sign
## Add the $ sign and then your selected column
ags$Pct

## You can add indexing for specific elements in the column
ags$Year[5]
ags$Coaching[1:5]

## Removing an element from a vector
## create a vector with the values 1-15
rem <- c(1:15)

## The command below removes the second element in the vector
rem[-2]

## Math operations with the data vectors
ags_win_p <- ags$Pct * 100

ags_pre2020 <- ags$Wins / ags$Wins[1] # pre-2020 as percent of 2020 wins

ags_win_change <- ags$Pct - ags$Pct[1]  # change in win% versus 2020

### Functions ###
length(ags)
length(ags[1,])
length(ags$Wins)

min(ags$Losses)
max(ags$Wins)

mean(ags$Wins)
mean(ags$Pct)

range(ags$Wins)
range(ags$Coaching)

## An alternative way to calculate the mean of a column
sum(ags$Wins) / length(ags$Wins)

2012:2020
2020:2001

## Use the seq() and names() function
sec <- seq(from = 2012, to = 2020, by = 1)
sec_coach <- ags$Coaching[9:1]
names(sec_coach) <- sec
sec_coach

## Build your own function
jimbo.summary <- function(x){
  total_w <- sum(x)
  avg_w <- mean(x)
  most_w <- max(x)
  out <- c(total_w,avg_w,most_w)
  names(out) <- c("total wins","avergae # wins","most wins")
  return(out)
}

## Assign values to run the function
jimbo <- c(8,9,8,9)
jimbo.summary(jimbo)

### Datafiles ###
## Upload data: define your working directory (where you store the data file)
## Load the relevant data package
library(readxl)
ags <- read_excel("~/Dropbox/TAMU/Bush631_QuantMethods/ags.xlsx")
View(ags)

## More functions with the Aggies data
names(ags)
nrow(ags)
ncol(ags)
dim(ags)

## Important function to learn of all the columns in the data
summary(ags)

## More examples for working with indexing for our data frame
ags[1:5, "Wins"]
ags[c(1:5),]

ags$Coaching[seq(from = 1, to = nrow(ags), by = 3)]
ags[c(1,4,7,10,13,16), "Coaching"]

### Missing values (NAs)  ###
ags$Bowl

## Deal with NAs: create a vector with values 1-10 and a missing value
mis_vec <- c(1:10,NA)

## You cannot calculate the mean with a missing value
mean(mis_vec)

## Add the (na.rm = TRUE) argument to the function
mean(mis_vec, na.rm = TRUE)

### Saving objects
write.csv(ags,file = "aggies.csv")






