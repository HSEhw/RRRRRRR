# exersize №1

sampleAverage <- function(a)
{
  len <- length(a)
  sum <- 0
  for (i in 1:len)
  {
    sum <- sum + a[i]
  }
  sum <- sum / len
  #print(sum)
  return(sum)
}

sampleVariance <- function(a)
{
  len <- length(a)
  num <- sampleAverage(a)
  sum <- 0
  for (i in 1:len)
  {
    sum <- sum + ((a[i] - num)^2)
  }
  sum <- sum / (len - 1)
  #print(sum)
  return(sum)
}

sampleCorrelationCoefficient <- function(a,b)
{
  len <- length(a)
  numA <- sampleAverage(a)
  numB <- sampleAverage(b)
  sum <- 0
  for (i in 1:len)
  {
    sum <- sum + ((a[i] - numA)*(b[i] - numB))
  }
  sa <- sqrt(sampleVariance(a))
  sb <- sqrt(sampleVariance(b))

  sum <- sum / ( (len-1) * sa * sb)
  #print(sum)
  return(sum)
}


year<-c(1891, 1892, 1893, 1894, 1895, 1896, 1897, 1898, 1899, 1900, 1901, 1902, 1903)
Moscow<-c(-19.2, -14.8, -19.6, -11.1, -9.4, -16.9, -13.7, -4.9, -13.9, -9.4, -8.3, -7.9, -5.3)
NN<-c(-21.8, -15.4, -20.8, -11.3, -11.6, -19.2, -13.0, -7.4, -15.1, -14.4, -11.1, -10.5, -7.2)

print(Moscow)
print(NN)

print("Выборочное среднее для Москвы:")
sampleAverage(Moscow)
mean(Moscow)
print("Выборочное среднее для Нижнего Новгорода:")
sampleAverage(NN)
mean(NN)

print("Выборочная дисперсия для Москвы:")
sampleVariance(Moscow)
var(Moscow)
print("Выборочная дисперсия для Нижнего Новгорода:")
sampleVariance(NN)
var(NN)

print("Выборочный коэффициент корреляции средней температуры января в Москве и средней температуры января в Нижнем Новгороде:")
sampleCorrelationCoefficient(Moscow, NN)
cor(Moscow, NN)


# exersize №2

averageGrowth<- function(a, b)
{
  n <- length(a)
  len <- 0
  for (i in 1:n)
  {
    len <- len + b[i]
  }
  sum <- 0
  for (i in 1:n)
  {
    sum <- sum + a[i] * b[i]
  }
  sum <- sum / len
  #print(sum)
  return(sum)
}

makeRangeOfProbability <- function(a)
{
  len <- length(a)
  res <- 1:len
  U <- 0
  for (i in 1:len)
  {
    U <- U + a[i]
  }
  
  for (i in 1:len)
  {
    res[i] = round(a[i] / U , 3)
  }
  return(res)
}

makeStatisticalArr <- function(a)
{
  len <- length(a)
  res <- 1:len
  U <- 0
  for (i in 1:len)
  {
    U <- U + a[i]
  }
  
  for (i in 1:len)
  {
    sum <- 0 
    for (j in 1:i)
    {
      sum <- sum + a[j]
    }
    res[i] = round(sum / U , 3)
  }
  return(res)
}

growth1 <- c("143-146", "146-149", "149-152", "152-155", "155-158", "158-161", "161-164", "164-167", "167-170", "170-173", "173-176", "176-179", "179-182", "182-185", "185-188")
growth <- c(144.5, 147.5, 150.5, 153.5, 156.5, 160.5, 162.5, 165.5, 168.5, 171.5, 174.5, 177.5, 180.5, 183.5, 186.5)
number <- c(1, 2, 8, 26, 65, 120, 180, 201, 170, 120, 64, 28, 10, 3, 1)

#Средний рост
averageGrowth(growth, number)
weighted.mean(growth, number)

#Статистический ряд
arr = makeRangeOfProbability(number)
TABLE <- data.frame(Growth = growth1, Num = number, Prob = arr)

#Гистограмма
plot(growth, number, xlab = "Рост", ylab = "Кол-во", type = "h")

#Выборочная функция распределения
arr = makeStatisticalArr(number)
plot(growth, arr, xlab = "Рост", ylab = "Вероятность", type = "s")
