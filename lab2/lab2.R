# Number at list is | 15 |
# M = -2
# Sigma = 2.5


confIntMeanVar <- function(arr, S, G){
# Confidence interval for mean variance
# доверительный интервал для математического ожидания при известной дисперсии
# S - Sigma (variance)
# G - Gamma  
  N = length(arr)
  d = qnorm(G) * S /sqrt(N); d # Delta
    
  L = mean(sample1) - d; L # Left 
  R = mean(sample1) + d; R # Right
  #res = c(L, R); res
  return(c(L, R))
}


confIntMeanwVar <- function(arr, G){
# Confidence interval for mean without variance
# доверительный интервал для математического ожидания при неизвестной дисперсии
# G - Gamma  
  N = length(arr)
  t = qt(G, N-1); t
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
# доверительный интервал для математического ожидания при неизвестной дисперсии
# G - Gamma  
  N = length(arr)
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
  len <- abs(max(int) - min(int[2]))
  return (len)
}

M = -2   # Mean
S = 2.5  # Sigma
G = 0.90 # Gamma

N1 = 100
N2 = 200

sample1 <- rnorm(N1, mean = M, sd = S); sample1
sample2 <- rnorm(N2, mean = M, sd = S); sample2


interval1 <- confIntMeanVar(sample1, S, G); interval1
interval2 <- confIntMeanVar(sample2, S, G); interval2

interval1 <- confIntMeanwVar(sample1, G); interval1
interval2 <- confIntMeanwVar(sample2, G); interval2

interval1 <- confIntVarwMean(sample1, G); interval1
interval2 <- confIntVarwMean(sample2, G); interval2

basePr <- 0.01

confProps <- c(1:50)
for(i in 1: length(confProps)){
  confProps[i] <- basePr + (0.02 * (i-1))
}

lengths <- c(1:50)
for(i in 1: length(confProps)){
  lengths[i] <- intLen(confIntMeanVar(sample1, S, confProps[i]))
}
interval1 <- confIntMeanVar(sample1, S, confProps[20]); interval1
interval1 <- confIntMeanVar(sample1, S, confProps[40]); interval1

lengths
plot(lengths, confProps, col = "green")

