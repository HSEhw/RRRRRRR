# task 1

N <- 100       # Кол-во элементов в выборке
Point <- 49    # Точка для разделения выборок на 2 части
min <- 0       # Левая граница интервала для выборки
max <- 1       # Правая граница интервала для выборки
alpha <- 0.05  # Уровень значимости

Z <- runif(N, min = min, max = max); Z 
X <- Z[1:Point]; X        # Часть выборки Z объемом 49  
Y <- Z[(Point+1):N]; Y    # Часть выборки Z объемом 51

# С помощью критерия однородности Смирнова проверяем 
# нулевую гипотезу о равенстве средних при альтернативной:
H1.a <- ks.test(X, Y, alternative = "two.sided"); H1.a  # не равны (X != Y)
H1.b <- ks.test(X, Y, alternative = "less"); H1.b       # меньше   (X < Y)
H1.c <- ks.test(X, Y, alternative = "greater"); H1.c    # больше   (X > Y)

H1.a$p.value > alpha
H1.b$p.value > alpha
H1.c$p.value > alpha

# Очиска памяти
rm(list = ls())
gc()


# task 2

# Фукнция для применения t.test к вектору значений
# Возвращает уровень значимости по критерию Стьюдента
tTest <- function(a, x, alternative){
  #res <- sapply(a, function(param) t.test(x, mu = param, alternative = alternative)$p.value) 
  res <- sapply(a, function(param) t.test(x, mu = param, alternative = alternative)[[3]]) 
  return(res)
}


N <- 100       # Кол-во элементов в выборке
min <- 2       # Левая граница интервала для выборки
max <- 3       # Правая граница интервала для выборки
alpha <- 0.05  # Уровень значимости

Z <- runif(N, min = min, max = max); Z 
X <- mean(Z); X   # Выборочная средняя
S <- sd(Z); S     # Стандартное отклонение

# Найдем границы интервала для параметра a
left  <- X - S/2; left
right <- X + S/2; right

a <- seq(left, right, length = N); a

not_equal <- tTest(a, Z, "two.sided"); not_equal
less <- tTest(a, Z, "less"); less
greater <- tTest(a, Z, "greater"); greater

plot(a, not_equal, type = "l", col = 1,
     xlab = "a",
     ylab = "p-value")
lines(a, less, type = "l", col = 2)
lines(a, greater, type = "l", col = 3)

abline(h = c(0, alpha), lty = 3)

# Очиска памяти
rm(list = ls())
gc()


# task 3

l <- 0.2       # Параметр для експоненциальному распределению
N <- 200       # Кол-во элементов в выборке
alpha <- 0.05  # Уровень значимости

X <- rexp(N, l); X  # Создаем выборку

# Задаем границы интервалы и создаем таблицу представлений в виде интервалов
breaks <- c(0, 1, 2, 3, 4, 5, 10, 15, 20, 25, 30)
tbl    <- table(cut(X, breaks = breaks)); tbl

# Рисуем гистограмму экспоненциального распредления случайной величины
hist(X, breaks = breaks, 
     col = "lightblue", 
     xlab = "X", 
     ylab = "Плотность вероятности")
lines(density(X), lwd = 3)

breaks <- c(-Inf, 1, 2, 3, 4, 5, 10, 15, 20, 25, Inf)
p      <- diff(pexp(breaks, l)); p; sum(p)

# Критерий Хи-квадрат (Критерий Пирсона)
res <- chisq.test(x = tbl, p = p)

# Проверка
res$p.value > alpha

# Очиска памяти
rm(list = ls())
gc()


# task 4

# Фукнция для применения ks.test к вектору значений
# Возвращает достигнутый уровень значимости от числа степеней свободы
ksTest <- function(m, X) {
  sapply(m, function(param) ks.test(X, "pt", param) [[2]]) 
}

N  <- 100      # Кол-во элементов в выборке
a  <- 8        # Мат ожидание
sd <- 2        # Стандартное отклонение
m  <- c(2: 20) # Число степеней свободы

# Создаем выборку нормального распределения
X <- rnorm(N, a, sd); X

# Центрируем выборку
X <- sort(X - mean(X)); X

levels <- ksTest(m, X)

plot(m, levels, type = "l", col = 4, lwd = 6,
     xlab = "Число степеней свободы",
     ylab = "Уровень значимости")
