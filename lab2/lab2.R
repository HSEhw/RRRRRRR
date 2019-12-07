# Number at list is | 15 |
# M = -2
# Sigma = 2.5


confIntMeanVar <- function(arr, S, G){
# Confidence interval for mean variance
# доверительный интервал для математического ожидания при известной дисперсии
# S - Sigma (variance)
# G - Gamma  
  arr=na.omit(arr)
  N = length(arr)
  d = qnorm(1-(1-G)/2) * S /sqrt(N); d # Delta
    
  L = mean(sample1) - d; L # Left 
  R = mean(sample1) + d; R # Right
  #res = c(L, R); res
  return(c(L, R))
}


confIntMeanwVar <- function(arr, G){
# Confidence interval for mean without variance
# доверительный интервал для математического ожидания при неизвестной дисперсии
# G - Gamma  
  arr=na.omit(arr)
  N = length(arr)
  t = qt(1-(1-G)/2, N-1); t
  D = var(arr)          # Sample Variance
  S = sqrt((N/(N-1))*D) # Corrected Sigma
  d = t * S /sqrt(N); d # Delta
  
  L = mean(sample1) - d; L # Left 
  R = mean(sample1) + d; R # Right
  #res = c(L, R); res
  return(c(L, R))
}


confIntVarwMean <- function(arr, G){
# Confidence interval for variance without mean
# вычисляет доверительный интервал для дисперсии при неизвестном математическом ожидании
# G - Gamma  
  arr=na.omit(arr)
  N  = length(arr)
  a1 = (1 - G)/2
  a2 = (1 + G)/2
  
  D = var(arr)          # Sample Variance
  S = sqrt((N/(N-1))*D) # Corrected Sigma
  
  L = sqrt(N-1)*S / sqrt(qchisq(a1, N-1)); L # Left 
  R = sqrt(N-1)*S / sqrt(qchisq(a2, N-1)); R # Right
  #res = c(L, R); res
  return(c(L, R))
}


intLen <- function(int){
#Длинна интервала
  len <- abs(max(int) - min(int))
  return (len)
}


plot1 <- function(arr1, arr2, S){
  arr1=na.omit(arr1); arr2=na.omit(arr2)
  confProps <- seq(from=0, to=1, by=0.02); confProps
  lengths1 <- c(1:length(confProps))
  lengths2 <- c(1:length(confProps))
  for(i in 1: length(confProps)){
    lengths1[i] <- intLen(confIntMeanVar(arr1, S, confProps[i]))
    lengths2[i] <- intLen(confIntMeanVar(arr2, S, confProps[i]))
  }
  plot(confProps, lengths2,  col = "red", type = "l", 
       xlab = "Дов. вер-ть", 
       ylab = "дов. интервал",
       main = "Доверительный интервал для оценки математического ожидания при известной дисперсии")
  lines(confProps, lengths1,  col = "green")
}


plot2 <- function(arr1, arr2, S){
  arr1=na.omit(arr1); arr2=na.omit(arr2)
  confProps <- seq(from=0, to=1, by=0.02); confProps
  lengths1 <- c(1:length(confProps))
  lengths2 <- c(1:length(confProps))
  for(i in 1: length(confProps)){
    lengths1[i] <- intLen(confIntMeanwVar(arr1, confProps[i]))
    lengths2[i] <- intLen(confIntMeanwVar(arr2, confProps[i]))
  }
  plot(confProps, lengths2,  col = "red", type = "l", 
       xlab = "Дов. вер-ть", 
       ylab = "дов. интервал",
       main = "Доверительный интервал для оценки математического ожидания при неизвестной дисперсии")
  lines(confProps, lengths1,  col = "green")
}


plot3 <- function(arr1, arr2, S){
  arr1=na.omit(arr1); arr2=na.omit(arr2)
  confProps <- seq(from=0, to=1, by=0.02); confProps
  lengths1 <- c(1:length(confProps))
  lengths2 <- c(1:length(confProps))
  for(i in 1: length(confProps)){
    lengths1[i] <- intLen(confIntVarwMean(arr1, confProps[i]))
    lengths2[i] <- intLen(confIntVarwMean(arr2, confProps[i]))
  }
  plot(confProps, lengths2,  col = "red", type = "l", 
       xlab = "Дов. вер-ть", 
       ylab = "дов. интервал",
       main = "Доверительный интервал для оценки дисперсии при неизвестной математическом ожидании")
  lines(confProps, lengths1,  col = "green")
}


M = -2   # Mean
S = 2.5  # Sigma
G = 0.90 # Gamma

N1 = 100
N2 = 200

set.seed(15)
sample1 <- rnorm(N1, mean = M, sd = S); sample1
sample2 <- rnorm(N2, mean = M, sd = S); sample2


interval1 <- confIntMeanVar(sample1, S, G); interval1
interval2 <- confIntMeanVar(sample2, S, G); interval2

interval1 <- confIntMeanwVar(sample1, G); interval1
interval2 <- confIntMeanwVar(sample2, G); interval2

interval1 <- confIntVarwMean(sample1, G); interval1
interval2 <- confIntVarwMean(sample2, G); interval2


dev.off()
par(mfcol=c(3, 1))
plot1(sample1,sample2, S)
plot2(sample1,sample2, S)
plot3(sample1,sample2, S)
    
