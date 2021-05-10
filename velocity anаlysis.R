setwd('/home/petr/Рабочий стол/python/Симулятор для коридорного алгоритма')
df5 <- read.csv('five_robots_velocity.csv')
df7 <- read.csv('seven_robots_velocity.csv')
df9 <- read.csv('velocity.csv')
names(df5) <- c('VX1', 'VY1', 'VX2', 'VY2', 'VX3', 'VY3', 'VX4', 'VY4', 'VX5', 'VY5')
names(df7) <- c('VX1', 'VY1', 'VX2', 'VY2', 'VX3', 'VY3', 'VX4', 'VY4', 'VX5', 'VY5', 'VX6', 'VY6', 'VX7', 'VY7')
names(df9) <- c('VX1', 'VY1', 'VX2', 'VY2', 'VX3', 'VY3', 'VX4', 'VY4', 'VX5', 'VY5', 'VX6', 'VY6', 'VX7', 'VY7', 'VX8', 'VY8', 'VX9', 'VY9')
View(df7)
View(df9)

V5 <- as.data.frame((df5[, 2*1:5-1]**2+df5[, 2*1:5]**2)**0.5)
V7 <- as.data.frame((df7[, 2*1:7-1]**2+df7[, 2*1:7]**2)**0.5)
V9 <- as.data.frame((df9[, 2*1:9-1]**2+df9[, 2*1:9]**2)**0.5)

names(V5) <- c('AV1', 'AV2', 'AV3', 'AV4', 'AV5')
names(V7) <- c('AV1', 'AV2', 'AV3', 'AV4', 'AV5', 'AV6', 'AV7')
names(V9) <- c('AV1', 'AV2', 'AV3', 'AV4', 'AV5', 'AV6', 'AV7', 'AV8', 'AV9')
View(V5)
View(V7)
View(V9)

colors = c('red', 'green', 'blue', 'yellow', 'black', 'pink', 'gray', 'purple')

TimeAxis = 0.02*1:length(V5[, 1])
plot(TimeAxis, V5[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 5 роботами. Модули скорости')
for(j in 2:5) {
  lines(TimeAxis, V5[, j], 's', col = colors[j])
}

plot(TimeAxis, df5[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 5 роботами. Скорость по оси X')
for(j in 2:5) {
  lines(TimeAxis, df5[, 2*j-1], 's', col = colors[j])
}

plot(TimeAxis, df5[, 2], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 5 роботами. Скорость по оси Y')
for(j in 2:5) {
  lines(TimeAxis, df5[, 2*j], 's', col = colors[j])
}

TimeAxis = 0.02*1:length(V7[, 1])
plot(TimeAxis, V7[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 7 роботами. Модули скорости')
for(j in 2:7) {
  lines(TimeAxis, V7[, j], 's', col = colors[j])
}

plot(TimeAxis, df7[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 7 роботами. Скорость по оси X')
for(j in 2:7) {
  lines(TimeAxis, df7[, 2*j-1], 's', col = colors[j])
}

plot(TimeAxis, df7[, 2], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 7 роботами. Скорость по оси Y')
for(j in 2:7) {
  lines(TimeAxis, df7[, 2*j], 's', col = colors[j])
}

plot(TimeAxis, V9[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 9 роботами. Модули скорости')
for(j in 2:9) {
  lines(TimeAxis, V9[, j], 's', col = colors[j])
}

plot(TimeAxis, df9[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 9 роботами. Скорость по оси X')
for(j in 2:9) {
  lines(TimeAxis, df9[, 2*j-1], 's', col = colors[j])
}

plot(TimeAxis, df9[, 2], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Эксперимент с 9 роботами. Скорость по оси Y')
for(j in 2:9) {
  lines(TimeAxis, df9[, 2*j], 's', col = colors[j])
}


setwd('/home/petr/Рабочий стол/python/Симуляция для IFAC')
df <- read.csv('velocity.csv')
names(df) <- c('VX1', 'VY1', 'VX2', 'VY2', 'VX3', 'VY3', 'VX4', 'VY4', 'VX5', 'VY5')
View(df)

V <- as.data.frame((df[, 2*1:5-1]**2+df[, 2*1:5]**2)**0.5)

names(V) <- c('AV1', 'AV2', 'AV3', 'AV4', 'AV5', 'AV6')
View(V)

colors = c('red', 'green', 'blue', 'yellow', 'black', 'pink', 'gray', 'purple')

TimeAxis = 0.02*1:length(V[, 1])
plot(TimeAxis, V[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Модули скорости')
for(j in 2:6) {
  lines(TimeAxis, V[, j], 's', col = colors[j])
}

plot(TimeAxis, df[, 1], 's', col = colors[1], ylim = c(0, 4), xlab = 'Время', ylab = 'Скорость', main = 'Скорость по оси X')
for(j in 2:6) {
  lines(TimeAxis, df[, 2*j-1], 's', col = colors[j])
}

plot(TimeAxis, df[, 2], 's', col = colors[1], ylim = c(-4, 4), xlab = 'Время', ylab = 'Скорость', main = 'Скорость по оси Y')
for(j in 2:6) {
  lines(TimeAxis, df[, 2*j], 's', col = colors[j])
}

X <- rnorm(10, mean = 0, sd = 1)
X <- c(X, rep(X[length(X)], 5))

