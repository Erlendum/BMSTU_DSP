# Прямоугольный импульс
l <- 1
rect <- function(e) as.integer(abs(e) < 1)
uRect <- function(x) rect(x / l)

# Сигнал Гаусса
a <- 1
sigma <- 2
uGauss <- function(x) a * exp(-(x / sigma)^2)

# Исправление функции.
fix <- function(u) {
  for (i in 1:length(u)) {
    u[i] <- (-1)^(i-1) * u[i]
  }
  u
}

# Дискретное преобразование Фурье.
dft <- function(u) {
  result <- numeric(length(u))
  for (k in 1:length(u)) {
    for (n in 1:length(u)) {
      result[k] = result[k] + u[n] * exp(-2 * pi * n * k * 1i / length(u))
    }
  }
  result
}

# Смотрим значение сигнала с -f до f.
# Тогда шаг дискретизации должен быть 1/(2f).
f <- 5
deltaT <- 1/(2*f)

t <- seq(-f, f, deltaT)
gaussSource <- uGauss(t)
rectSource <- uRect(t)

gaussFast <- fft(gaussSource)
gaussDiscrete <- dft(gaussSource)

gaussFastFixed <- fft(fix(gaussSource))
gaussDiscreteFixed <- dft(fix(gaussSource))

rectFast <- fft(rectSource)
rectDiscrete <- dft(rectSource)

rectFastFixed <- fft(fix(rectSource))
rectDiscreteFixed <- dft(fix(rectSource))

k = 0:(length(t)-1)

# 0.00017 БФТ
# 0.012 ДФТ

par(mfrow = c(5, 2))
plot(t, rectSource, main = "Прямоугольный импульс", sub = "Исходная", type = "l", xlab = "t", ylab = "")
plot(k, abs(rectFast), main = "Прямоугольный импульс", sub = "БПФ", type = "l", xlab = "k", ylab = "")
plot(k, abs(rectFastFixed), main = "Прямоугольный импульс", sub = "БПФ исправленное", type = "l", xlab = "k", ylab = "")
plot(k, abs(rectDiscrete), main = "Прямоугольный импульс", sub = "ДПФ", type = "l", xlab = "k", ylab = "")
plot(k, abs(rectDiscreteFixed), main = "Прямоугольный импульс", sub = "ДПФ исправленное", type = "l", xlab = "k", ylab = "")

plot(t, gaussSource, main = "Сигнал Гаусса", sub = "Исходная", type = "l", xlab = "t", ylab = "")
plot(k, abs(gaussFast), main = "Сигнал Гаусса", sub = "БПФ", type = "l", xlab = "k", ylab = "")
plot(k, abs(gaussFastFixed), main = "Сигнал Гаусса", sub = "БПФ исправленное", type = "l", xlab = "k", ylab = "")
plot(k, abs(gaussDiscrete), main = "Сигнал Гаусса", sub = "ДПФ", type = "l", xlab = "k", ylab = "")
plot(k, abs(gaussDiscreteFixed), main = "Сигнал Гаусса", sub = "ДПФ исправленное", type = "l", xlab = "k", ylab = "")