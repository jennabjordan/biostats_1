x <- cbind(c(1,2,3), c(4,5,6))
x
x <- rbind(c(1,2,3), c(4,5,6))
x
x <- matrix(1:9, nrow = 3, ncol = 3)
x
x <- matrix(1:9, nrow = 3, ncol = 3)
x
class(x)
typeof(x)
dim(x)
y <- matrix(c("a","b","c","d","e","f"), nrow = 3, ncol = 2)
y
typeof(y)
x <- matrix(1:9, nrow=3, ncol=3)
x
x[2,3]
x == 2
x > 2
x[x == 2]
# Create data frame
x <- c("Pristine", "Pristine", "Disturbed", "Disturbed", "Pristine") # Lake type
y <- c(1.2, 2.2, 10.9, 50.0, 3.0) # TSS: total suspended solids (mg/L)
df0 <- data.frame(LakeType = x, TSS = y) # x is named as "LakeType" while y is named as "TSS"
df0
