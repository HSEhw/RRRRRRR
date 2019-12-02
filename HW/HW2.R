
name <- c("Nick", "Eugene", "Peter", "Alex", "Kate", "Basil", "George")
salary <- c(21000, 19000, 27000, 11000, 102000, 25000, 21000)
height <- c(172, 160, 184, 188, 170, 175, 165)
weight <- c(53, 58, 76, 80, 50, 65, 85)
size <- c("XS", "M", "L", "XXL", "S", "M", "XL")
sex <- c("m", "fm", "m", "m", "fm", "m", "m")

sex.f <- factor(sex); sex.f; plot(sex.f)
levels(sex.f) <- c("women", "men"); sex.f; plot(sex.f)

size.f <- factor(size); size.f; plot(size.f)
size.o <- ordered(size.f, labels = c("XS", "S", "M", "L", "XL", "XXL")); size.o

workdata <- (data.frame(row.names = name, "Salary" = salary, "Height" = height, "Weight" = weight, "Size Oddered" = size.o, "Sex Factor" = sex.f)); workdata
str(workdata)

workdata.sex_sorted <- workdata[order(sex.f),]; workdata.sex_sorted
workdata.sex_height_sorted <- workdata[order(sex.f, height),]; workdata.sex_height_sorted
workdata.column_sorted <- workdata[order(names(workdata))]; workdata.column_sorted
workdata.name_sorted <-  workdata[order(row.names(workdata)),]; workdata.name_sorted
workdata.name_column_sorted <- workdata.column_sorted[order(name), ]; workdata.name_column_sorted
