# Number at list is | 15 |
# M = -2
# Sigma = 2.5


Laplas <- function(t){
  return(round( ((pnorm(t)-pnorm(-t))/2), 4))
}
#Laplas(0.5)

find_t <- function(F){
  t = 0
  i = 0
  while (abs(Laplas(t)-F)>0.0001){
    if (Laplas(t)-F > 0){
      t = t - 5/2^i
    }
    else {  
      t = t + 5/2^i
    }
  i = i +1
  }
  return(round(t, 2))
}
#find_t(0.1914)

convInMeanKnownVar <- function(arr, S, G){
# доверительный интервал для математического ожидания при известной дисперсии
# S - Sigma (variance)
# G - Gamma  
  N = length(arr)
  t = find_t(G/2); t
  d = t * S /sqrt(N); d # Delta
  
  L = mean(sample1) - d; L # Left 
  R = mean(sample1) + d; R # Right
  #res = c(L, R); res
  return(c(L, R))
}


M = -2   # Mean
S = 2.5  # Sigma
G = 0.90 # Gamma

N1 = 100
N2 = 200

sample1 <- rnorm(N1, mean = M, sd = S); sample1
sample2 <- rnorm(N2, mean = M, sd = S); sample2

interval1 <- convInMeanKnownVar(sample1, S, G); interval1
interval2 <- convInMeanKnownVar(sample2, S, G); interval2

G = 0.95
k = 10 - 1

dt(0.95, 9, log = FALSE)
pt(0.95, 9, lower.tail = TRUE, log.p = FALSE)
qt(0.95, 9, lower.tail = TRUE, log.p = FALSE)
rt(1, 9,0.95)

t.test(conf.level = 9)
help(t.test)

dchisq(0.95, 9, ncp=0)
pchisq(0.95, 9)
pchisq(0.95, 9, ncp=0)
