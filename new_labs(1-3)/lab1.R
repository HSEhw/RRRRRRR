# Номер в списке: 13

# Задание 1
enterNA <- function(array){
  for (i in 1:length(array)) {
    if ((array[i]<0 || 100<array[i])&&!is.na(array[i])) {
      array[i] <- NA
    }
  }
  return(array)
}

N1    <- 51  # кол-во элементов в выборке
mean1 <- 70  # мат. ожидание
sd1   <- 30  # среднеквадратичное отклонение

set.seed(13)

# создание выборки и замена значений, который <0 или >100, на NA
sample1 <- rnorm(N1, mean = mean1, sd = sd1); sample1 
sample1 <- enterNA(array = sample1); sample1

N2    <- 62  # кол-во элементов в выборке
mean2 <- 65  # мат. ожидание
sd2   <- 40  # среднеквадратичное отклонение

# создание выборки и замена значений, который <0 или >100, на NA
sample2 <- rnorm(N1, mean = mean1, sd = sd1); sample2 
sample2 <- enterNA(array = sample2); sample2


# Задание 2

# функция для перевода в пятибальную систему оцениванию
to5 <- function(array){
  array <- cut(x = array, breaks = c(0, 50, 68, 81, 100))
  array.f <- factor(array)
  levels(array.f) <- c("2", "3", "4", "5")
  array.o <- ordered(array.f, labels = c("2", "3", "4", "5"))
  return(array.o)
}

# функция для перевода в европейскую систему оценивания
toEU <- function(array){
  array <- cut(x = array, breaks = c(0, 30, 50, 60, 68, 85, 95, 100))
  array.f <- factor(array)
  levels(array.f) <- c("F", "FX", "E", "D", "C", "B", "A")
  array.o <- ordered(array.f, labels = c("F", "FX", "E", "D", "C", "B", "A"))
  return(array.o)
}

# переведем выборки с пятибальные и европейские системы счисления
marks1.5point <- to5(array = sample1); table(marks1.5point) 
marks1.EUpoint <- toEU(array = sample1); table(marks1.EUpoint) 

marks2.5point <- to5(array = sample2); table(marks2.5point) 
marks2.EUpoint <- toEU(array = sample2); table(marks2.EUpoint) 


# Задание 3

# создадим таблицу относительных частот для оценок в пятибальной системе счисления
t1 <- data.frame(row.names = levels(marks1.5point), table(marks1.5point)); t1
t2 <- data.frame(row.names = levels(marks2.5point), table(marks1.5point)); t2
table1 <- data.frame(row.names = levels(marks2.5point), marks1  = prop.table( t1[, -1]), marks2  = prop.table( t2[, -1])); table1

# создадим таблицу относительных частот для оценок в европейской системе счисления
t1 <- data.frame(row.names = levels(marks1.EUpoint), table(marks1.EUpoint)); t1
t2 <- data.frame(row.names = levels(marks2.EUpoint), table(marks1.EUpoint)); t2
table2 <- data.frame(row.names = levels(marks2.EUpoint), marks1  = prop.table( t1[, -1]), marks2  = prop.table( t2[, -1])); table2


# Задание 4

# разделение окна на 2 части для построения сразу двух гистограмм
par(mfcol=c(1, 2))

# построение гистограмм для пятибальной системы оценивания
marks1 <- as.numeric(as.character(na.omit(marks1.5point)))
hist(marks1, col = "lightblue", freq = FALSE)
lines(density(marks1, na.rm = TRUE), col = "red", lwd = 2)
marks2 <- as.numeric(as.character(na.omit(marks2.5point)))
hist(marks2, col = "lightblue", freq = FALSE) 
lines(density(marks2, na.rm = TRUE), col = "red", lwd = 2)

# построение гистограмм для пятибальной системы оценивания
marks1 <- as.numeric(na.omit(marks1.EUpoint)); marks1
hist(marks1, col = "lightgreen", freq = FALSE)
lines(density(marks1, na.rm = TRUE), col = "red", lwd = 2)
marks2 <- as.numeric(na.omit(marks1.EUpoint)); marks2
hist(marks2, col = "lightgreen", freq = FALSE) 
lines(density(marks2, na.rm = TRUE), col = "red", lwd = 2)


# Задание 5

dev.off()
# разделение окна на 4 части для построения сразу четырех коробок с усами
par(mfcol=c(1, 4))

boxplot(marks1.5point, main = "marks1 5-point sys", ylab = "mark")
boxplot(marks1.EUpoint, main = "marks1 EU-point sys", ylab = "mark")
boxplot(marks2.5point, main = "marks2 5-point sys", ylab = "mark")
boxplot(marks2.EUpoint, main = "marks2 EU-point sys", ylab = "mark")
