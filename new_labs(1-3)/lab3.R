# Задача 1

N <- 100  # кол-во элементов в выборке
sign.lvl <- 0.05  # уровень значимости

# Создадим выбоки и разделим их на две части по 49 и 51 элементам
z <- runif(N, min = 0, max = 1); z # создаем выборку
x <- z[1:49]; x 
y <- z[50:100]; y

# Проверка нулевой гипотезы с помощью критерия Колмагорова-Смирнова
h1.1 <- ks.test(X, Y, alternative = "two.sided"); h1.1
h1.2 <- ks.test(X, Y, alternative = "less"); h1.2
h1.3 <- ks.test(X, Y, alternative = "greater"); h1.3

# Проверка
h1.1.a$p.value > sign.lvl
h1.2$p.value > sign.lvl
h1.3$p.value > sign.lvl

# Очиска памяти
rm(list = ls()); gc()


# Задача 2

sapplytTest <- function(array, X, alt){
  res <- sapply(array, function(param) t.test(X, mu = param, alternative = alt)[[3]])
}

N <- 100  # кол-во элементов в выборке
sign.lvl <- 0.05  # уровень значимости

z <- runif(N, min = 2, max = 3); z # создаем выборку
x <- mean(z); x # выборочная средняя
s <- sd(z); s   # стандартное отклонение

# Найдем параметры а
a <- seq(x-s/2, x+s/2, length = N); a

n_eq <- sapplytTest(a, z, "two.sided"); n_eq
l    <- sapplytTest(a, z, "less"); l
g    <- sapplytTest(a, z, "greater"); g

plot(a, n_eq, type = "l", col = "black",
     xlab = "a",
     ylab = "p-value")
lines(a, l, type = "l", col = "red")
lines(a, g, type = "l", col = "green")

# Очиска памяти
rm(list = ls()); gc()


# Задача 3

l <- 0.2  # параметр для експоненциальному распределению
N <- 200  # кол-во элементов в выборке
sign.lvl <- 0.05  # уровень значимости

x <- rexp(N, l); x # создаем выборку
breaks <- c(-Inf, 1, 2, 3, 4, 5, 10, 15, 20, 25, Inf)
table  <- table(cut(x, breaks = breaks)); table
p      <- diff(pexp(breaks, l)); p; sum(p)

# Проверка гитотезы с помощью критерия Пирсона
h <- chisq.test(x = table, p = p); h$p.value
h$p.value > sign.lvl

# Очиска памяти
rm(list = ls()); gc()


# Задача 4

sapplyksTest <- function(fdegrees, x){
  sapply(fdegrees, function(param) ks.test(x, "pt", param) [[2]]) 
}

N <- 100  # Кол-во элементов в выборке
mean <- 8  # Мат. ожидание
sd <- 2  # Стандартное отклонение
fdegrees <- c(2:20)  # Число степеней свободы

# Создаем выборку нормального распределения
x <- rnorm(N, mean = mean, sd = sd); x

# Центрируем выборку
x <- sort(x - mean(x)); x

lvls <- sapplyksTest(fdegrees, x)
plot(fdegrees, lvls, type = "l", col = 3, lwd = 4,
     xlab = "Число степеней свободы",
     ylab = "Уровень значимости")

