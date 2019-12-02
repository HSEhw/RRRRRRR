insertNA <- function(arr) {
# replaces elements, which less than 0 or more than 100, with NA
  for (i in 1:length(arr)) {
    # arr[i] <- round(arr[i]) # if you want to round the elements of the array
    if ((arr[i]<0 || 100<arr[i])&&!is.na(arr[i])) {
      arr[i] <- NA
    }
  }
  return(arr)
}

moveTo5Point <- function(arr) { 
# exchange on 5-point system
  arr <- cut(x=arr, breaks = c(0, 50, 68, 81, 100)); arr
  arr.f <- factor(arr); arr.f
  levels(arr.f) <- c("2", "3", "4", "5"); arr.f
  arr.o <- ordered(arr.f, labels = c("2", "3", "4", "5")); arr.o
  return(arr.o)
}

moveToEUPoint <- function(arr) { 
# exchange on European(EU) system
  arr <- cut(x=arr, breaks = c(0, 30, 50, 60, 68, 85, 95, 100)); arr
  arr.f <- factor(arr); arr.f
  levels(arr.f) <- c("F", "FX", "E", "D", "C", "B", "A"); arr.f
  arr.o <- ordered(arr.f, labels = c("F", "FX", "E", "D", "C", "B", "A")); arr.o
  return(arr.o)
}

makeTable <- function(arr) { 
# makes table of probabiliry for one arr 
  a <- data.frame(row.names =levels(arr), table(arr)); a
  res <- data.frame(row.names =levels(arr), probability  = prop.table( a[, -1]))
  return(res)
}

makeDoubleTable <- function(arr1, arr2) { 
# makes table of probabiliry for two arrs for better comparison
  a <- data.frame(row.names =levels(arr1), table(arr1)); a
  b <- data.frame(row.names =levels(arr2), table(arr2)); b
  res <- data.frame(row.names =levels(arr1), probability1  = prop.table( a[, -1]), probability2  = prop.table( b[, -1]))
  return(res)
}

makePlot5 <- function(arr1, arr2) {
# function for 5-point system
# makes two histogramms and graphs the density functions of arr1 and arr2 
# 
  par(mfcol=c(1, 2))
  
  marks1 <- as.numeric(as.character(na.omit(arr1))); marks1
  hist(marks1, freq = FALSE)
  lines(density(marks1, na.rm = TRUE), col = "red", lwd = 2)
  
  marks2 <- as.numeric(as.character(na.omit(arr2))); marks2
  hist(marks2, freq = FALSE) 
  lines(density(marks2, na.rm = TRUE), col = "red", lwd = 2)
}

makePlotEU <- function(arr1, arr2) {
# function for EU-point system
# makes two histogramms and graphs the density functions of arr1 and arr2 
  par(mfcol=c(1, 2))
  
  marks1 <- as.numeric(na.omit(arr1)); marks1
  hist(marks1, freq = FALSE)
  lines(density(marks1, na.rm = TRUE), col = "red", lwd = 2)
  
  marks2 <- as.numeric(na.omit(arr2)); marks2
  hist(marks2, freq = FALSE) 
  lines(density(marks2, na.rm = TRUE), col = "red", lwd = 2)
}

N1 <- 51 # number of elements
math_expect1 <- 70 # mathematical expectation
st_dev1 <- 30 # standard deviation
marks1 <-rnorm(N1, mean = math_expect1, sd=st_dev1); marks1
marks1 <- insertNA(marks1); marks1

marks1.5 <- moveTo5Point(arr = marks1); table(marks1.5)
marks1.EU <- moveToEUPoint(marks1); table(marks1.EU)

N2 <- 62 # number of elements
math_expect2 <- 65 # mathematical expectation
st_dev2 <- 40 # standard deviation
marks2 <- rnorm(N2, mean = math_expect2, sd=st_dev2); marks2
marks2 <- insertNA(marks2); marks2

marks2.5 <- moveTo5Point(arr = marks2); table(marks2.5)
marks2.EU <- moveToEUPoint(marks2); table(marks2.EU)

# making tables
table1.5 <- makeTable(marks1.5); table1.5
table1.EU <- makeTable(marks1.EU); table1.EU

table2.5 <- makeTable(marks2.5); table2.5
table2.EU <- makeTable(marks2.EU); table2.EU

table.5 <- makeDoubleTable(marks1.5, marks2.5); table.5 
table.EU <- makeDoubleTable(marks1.EU, marks2.EU); table.EU

# making histograms
makePlot5(marks1.5, marks2.5)
makePlotEU(marks1.EU, marks2.EU)

# making boxes
dev.off()
par(mfcol=c(1, 4))

boxplot(marks1.5, main="marks1(5-point sys)", ylab="mark")
boxplot(marks1.EU, main="marks1(EU-point sys)", ylab="mark")
boxplot(marks2.5, main="marks2(5-point sys)", ylab="mark")
boxplot(marks2.EU, main="marks2(EU-point sys)", ylab="mark")
