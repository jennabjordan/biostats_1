#' ---
#' title: "Set 2: R product 1"
#' output: html_document
#' date: 2023-08-22
#' author: Jenna Jordan
#' ---


# Vectors -----------------------------------------------------------------


#ex.1a manually create a vector using c()
x <- c(1,3,4,8)
x

#ex.1b character
x <- c("a", "b", "c")
x

#ex.1c logical
x <- c(TRUE, FALSE, FALSE)
x
as.numeric(x)

#ex.2 sequence of numbers
x <- 1:5
x

#ex.3a replicate same numbers or characters
x <- rep(2, 5) # replicate 2 five times
x

x <- rep(x =2, times = 5)
x

#ex.4a use seq() function
x <- seq(1, 5, by = 1)
x

#ex.4b use seq() function
x <- seq(1, 5, by = 0.1)
x

x <- c(1.2, 3.1, 4.0, 8.2)
x
class(x)
typeof(x)
length(x)
sum(x)
mean(x)

y <- c("a", "b", "c")
class(y)
length(y)

#access

x <- c(2,2,3,2,5)
x[2] # access element #2

x[c(2,4)] # access elements #2 and 4

x[2:4] # access elements #2-4

#equation

# creating a vector
x <- c(2,2,3,2,5)

# ex.1a equal
x == 2

# ex.1b larger than
x > 2 

# ex.2a equal
x[x == 2]

x[x > 2]

#matrix

#ex.1 cbind: combine objects by column
x <- cbind(c(1,2,3), c(4,5,6))
x

#ex.2 rbind: combine objects by row
x <- rbind(c(1,2,3), c(4,5,6))
x

#ex.3 matrix: specify elements and the number of rows (nrow) and columns (ncol)
x <- matrix(1:9, nrow = 3, ncol = 3)
x

#check features

x <- matrix(1:9, nrow = 3, ncol = 3)
x

class(x)
typeof(x)
dim(x)

#access in matrix

x <- matrix(1:9, nrow = 3, ncol = 3)
x
x[2,3] # access an element in row #2 and colum #3
x[2,] # access elements in row #2
x[c(2,3),] # access elements in rows #2 and 3
x[,c(2,3)] # access elements in columns #2 and 3

x ==2 #equal
x > 2 

x[x == 2]
x[x > 2]

which(x == 2, arr.ind = TRUE)
which(x > 2, arr.ind = TRUE)


# data frame --------------------------------------------------------------


# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0

colnames(df0) # call column names

df0$LakeType # access LakeType
df0$TSS # access TSS
df0[,1] # access column #1
df0[1,] # access row #1
df0[c(2,4),] # access row #2 and 4

# EXERCISES 

# exercise vector ---------------------------------------------------------
a <- c(1,2,3)
length(a)
b <- c(2,4,6,8,10,12)
length(b)
c <- c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,41)
length(c)

a <- c("a", "b", "c")
length(a)
b <- c("a", "b", "c", "d", "e", "f")
length(b)
c <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t")
length(c)

set.seed(1)
x <- rnorm(100)

x[x > 2] #ID

x > 2 #value

# exercise matrix ------------------------------------------------------------------

cbind(c(1, 1, 1, 1), c(2, 2, 2, 2), c(3, 3, 3, 3), c(4, 4, 4, 4))

rbind(c(1, 1, 1, 1), c(2, 2, 2, 2), c(3, 3, 3, 3), c(4, 4, 4, 4))

cbind(c("a", "a", "a", "a"), c("b", "b", "b", "b"), c("c", "c", "c", "c"), c("d", "d", "d", "d"))

rbind(c("a", "a", "a", "a"), c("b", "b", "b", "b"), c("c", "c", "c", "c"), c("d", "d", "d", "d"))

set.seed(1)
x <- matrix(rnorm(100), nrow = 10, ncol = 10)

x > 2

which(x > 2, arr.ind = TRUE)

x[1,7]
x[10,7]

x[1,7] + x[10,7]

(2.401618 + 2.172612) / 2 #mean?

# data frame --------------------------------------------------------------

x <- c(01, 02, 03, 04, 05, 06, 07, 08, 09, 10) #day of the month august 2023
y <-c(84, 82.9, 72, 80.1, 90, 91.9, 87.1, 84, 84.9, 82) #daily high temperature
z <-c("sunny", "cloudy", "rainy", "cloudy", "sunny", "sunny", "sunny", "sunny", "sunny", "cloudy") #weather conditions
df0 <- data.frame(DayAugust2023 = x, DHT = y, WC = z)
df0

class(x)
class(y)
class(z)

set.seed(1)
x <- rnorm(100, mean = 10, sd = 3)
y <- rpois(100, lambda = 10)
z <- rep(c("VA", "NC"), 50)
df0 <- data.frame(temperature = x, abundance = y, state = z)
df0

#mean for NC
df0$state == "NC"
which(df0$state == "NC")
df0$temperature[which(df0$state == "NC")]
mean(df0$temperature[which(df0$state == "NC")])

df0$state == "NC"
which(df0$state == "NC")
df0$abundance[which(df0$state == "NC")]
mean(df0$abundance[which(df0$state == "NC")])

#mean for VA

df0$state == "VA"
which(df0$state == "VA")
df0$temperature[which(df0$state == "VA")]
mean(df0$temperature[which(df0$state == "VA")])

df0$state == "VA"
which(df0$state == "VA")
df0$abundance[which(df0$state == "VA")]
mean(df0$abundance[which(df0$state == "VA")])

