# task 1
# Задание 1. Написать функцию в R для вычисления среднего квадратичного отклонения
# (стандартной ошибки) среднего. Учесть возможность существования в выборке NA.

findSE <- function(arr) {
# MQD - mean square deviation  
# SE - Standard error
  res <-sd(arr, na.rm = TRUE)/sqrt(length(na.omit(arr)))
  return(res)
}

data(mtcars); mtcars
mtcars$mpg
a <- c(mtcars$mpg, NA, NA, NA);a
res1 <- findSE(mtcars$mpg); res1
res2 <- findSE(a); res2

# task 2
# 2. Написать функции для вычисления ассиметрии и эксцесса и вычислить эти
# величины для выборки, объемом n = 100, взятой генеральной совокупности с
# экспоненциальной функцией распределения с параметром λ = 3. При написании функций
# учесть возможность существования в выборке NA.

findCS <- function(arr){
# CS - coefficient of skewness  
  X <- mean(arr, na.rm = TRUE); X
  M <- median(arr, na.rm = TRUE); M
  sigma <- findSE(arr); sigma
  res <- (X - M) / sigma
  return(res)
}

findCA <- function(arr) {
# coefficient of asymmetry
  m4 <- mean(arr^4); m4
  sigma <- findSE(arr); sigma
  res <- m4 / sigma^4
  return(res)  
}

N <- 100
lambda <- 3
arr <- rexp(N, rate = lambda); arr
res <- findCS(arr); res
res1 <- findCA(arr); res1

# task 3
# 3. Для всех нефакторных переменных из таблице данных mtcars вычислить
# одновременно median и mean с группировкой по переменным am и vs, включив в одну из
# нефакторных переменных 2 NA.

meanAndMedian <- function(a) {
  na.omit(a)
  return(c(median = median(a), mean = mean(a)))
}

?mtcars
mtcars
str(mtcars)
df <- cbind(mtcars[1:7],mtcars[10:11]); df

df$mpg[3] <- NA; df$cyl[3] <- NA; df$disp[3] <- NA; df$hp[3] <- NA; df$drat[3] <- NA; df$wt[3] <- NA; df$qsec[3] <- NA; 

df$mpg; df$cyl; df$disp; df$hp; df$drat; df$wt; df$qsec; 

#aggregate(df,by=list(mtcars$am, mtcars$vs), FUN = (median),na.rm=TRUE)
#aggregate(df,by=list(mtcars$am, mtcars$vs), FUN = (mean),na.rm=TRUE)

aggregate(df,by=list(mtcars$am, mtcars$vs), FUN = (meanAndMedian))
          