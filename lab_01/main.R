# Прямоугольный импульс
l <- 1
rect <- function(e) as.integer(abs(e) < 1)
uRect <- function(x) rect(x / l)

# Сигнал Гаусса
a <- 1
sigma <- 1
uGauss <- function(x) a * exp(-(x / sigma)^2)

# Смотрим значение сигнала с -f до f.
# Тогда шаг дискретизации должен быть 1/(2f).
f <- 4
deltaT <- 1/(2*f)

t <- seq(-f, f, deltaT)
gaussSource <- uGauss(t)
rectSource <- uRect(t)

# Восстановление "исходного" сигнала с шагом в 10 раз меньше.
deltaTRestored <- deltaT / 10
tRestored <- seq(-f, f, deltaTRestored)
gaussRestored <- numeric(length(tRestored))
rectRestored <- numeric(length(tRestored))

# При == 0 значение функции равно 1, так как это первый замечательный предел.
sinc <- function(x) if (x != 0) {
  sin(x)/x
} else {
  1
}

# uGauss(k * deltaT)...
# Мы знаем значение функции для заданного шага deltaT в диапазоне от -f до f.
# Соответственно, k * deltaT должно быть элементом t.
# Для этого нужны пределы такие, что lim * deltaT = f => lim = f / deltaT.
lim <- f/deltaT
for (i in 1:length(tRestored)) {
  for (k in -lim:lim) {
    # Так как индексы в массивах начинаются с 1, а не с -lim, то для нахождения индекса
    # Нужно взять k + lim + 1.
    index <- k + lim + 1
    gaussRestored[i] <- gaussRestored[i] + gaussSource[index] * sinc(2 * pi * f * (tRestored[i] - k/(2*f)))
    rectRestored[i] <- rectRestored[i] + rectSource[index] * sinc(2 * pi * f * (tRestored[i] - k/(2*f)))
  }
}

par(mfrow = c(2, 2))
plot(t, rectSource, main = "Прямоугольный импульс", sub = "Исходный", type = "l", xlab = "t", ylab = "")
plot(tRestored, rectRestored, main = "Прямоугольный импульс", sub = "Восстановленный", type = "l", xlab = "t", ylab = "")

plot(t, gaussSource, main = "Сигнал Гаусса", sub = "Исходный", type = "l", xlab = "t", ylab = "")
plot(tRestored, gaussRestored, main = "Сигнал Гаусса", sub = "Восстановленный", type = "l", xlab = "t", ylab = "")
