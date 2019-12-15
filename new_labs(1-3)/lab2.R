# Номер в списке: 13
# Мат. ожидание 3
# Среднеквадратичное отклонение 1.2

fun1 <- function(array, sd, g){
# доверительный интервал для математического ожидания при известной дисперсии
  array <- na.omit(array)
  N <- length(array)
  delta <- qnorm(1-(1-g)/2) * sd /sqrt(N)
  interval <- c(mean(sample1) - delta, mean(sample1) + delta)
  return(interval)
}

fun2 <- function(array, g){
# доверительный интервал для математического ожидания при неизвестной дисперсии
  array <- na.omit(array)
  N <- length(array)
  mean.c <- sqrt((N/(N-1))*var(array)) 
  delta <- qt(1-(1-g)/2, N-1) * mean.c / sqrt(N)
  interval <- c(mean(sample1) - delta, mean(sample1) + delta)
  return(interval)
}

fun3 <- function(array, g){
# вычисляет доверительный интервал для дисперсии при неизвестном математическом ожидании
  array <- na.omit(array)
  N <- length(array)
  mean.c <- sqrt((N/(N-1))*var(array))
  a1 <- (1 - g)/2
  a2 <- (1 + g)/2
  interval <- c(sqrt(N-1)* mean.c / sqrt(qchisq(a2, N-1)), sqrt(N-1)* mean.c / sqrt(qchisq(a1, N-1)))
}

findLen <- function(interval){
# функция для нахождения длины интервала  
  return(abs(max(interval) - min(interval)))
}

N1   <- 100  # кол-во элементов в первой выборке
N2   <- 200  # кол-во элементов во второй выборке
mean <- 3    # мат. ожидание
sd   <- 1.2  # среднеквадратичное отклонение
g    <- 0.9  # доверительная вероятность

# создаем две выбоки размером в 100 и 200 элементов
set.seed(13)
sample1 <- rnorm(N1, mean = mean, sd = sd); sample1
sample2 <- rnorm(N2, mean = mean, sd = sd); sample2

# находим доверительные интервалы для математического ожидания при известной дисперсии
interval1 <- fun1(sample1, sd = sd, g = g); interval1
interval2 <- fun1(sample2, sd = sd, g = g); interval2

# находим доверительные интервалы для математического ожидания при неизвестной дисперсии
interval1 <- fun2(sample1, g = g); interval1
interval2 <- fun2(sample2, g = g); interval2

# находим доверительные интервалы для математического ожидания при неизвестной дисперсии
interval1 <- fun3(sample1, g = g); interval1
interval2 <- fun3(sample2, g = g); interval2


# строим графики зависимости длины доверительного интервала от величины доверительной вероятности.
par(mfcol=c(2, 1))

G <- seq(from=0, to=1, by=0.02); G
len1 <- c(1:length(G))
len2 <- c(1:length(G))
len3 <- c(1:length(G))

for(i in 1: length(G)){
  len1[i] <- findLen(fun1(sample1, sd = sd, g = G[i]))
  len2[i] <- findLen(fun2(sample1, g = G[i]))
  len3[i] <- findLen(fun3(sample1, g = G[i]))
}

plot(G, len1,  col = "red", type = "l", 
     xlab = "Дов. вер-ть", 
     ylab = "дов. интервал",
     main = "Для выбоки в 100 элементов
     ")
lines(G, len2,  col = "green")
lines(G, len3,  col = "blue")

for(i in 1: length(G)){
  len1[i] <- findLen(fun1(sample2, mean = mean, g = G[i]))
  len2[i] <- findLen(fun2(sample2, g = G[i]))
  len3[i] <- findLen(fun3(sample2, g = G[i]))
}

plot(G, len1,  col = "red", type = "l", 
     xlab = "Дов. вер-ть", 
     ylab = "дов. интервал",
     main = "Для выбоки в 200 элементов
     ")
lines(G, len2,  col = "green")
lines(G, len3,  col = "blue")
