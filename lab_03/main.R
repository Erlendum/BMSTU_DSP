rectPulse <- function(x, c) {
  y <- numeric(length(x))
  y[abs(x) - c < 0] <- 1
  return(y)
}

gaussSignal <- function(x, sigma) {
  return(exp(-(x / sigma)^2))
}

graph_figure <- function(ind, t, x1, x2, y, tit) {
  plot(t, x1[1:length(t)], type='l', col='blue', xlim=c(min(t), max(t)), ylim=c(0, max(c(x1, x2, y))), xlab='t', ylab='Amplitude', main=tit)
  lines(t, x2[1:length(t)], col='red')
  lines(t, y, col='green')
  legend('topright', legend=c('Signal 1', 'Signal 2', 'Convolution'), col=c('blue', 'red', 'green'), lty=1, cex=0.5)
}

main <- function() {
  c <- 3.0
  sigma <- 1.0
  
  n <- as.numeric(readline('Введите количество точек: '))
  step <- as.numeric(readline('Введите шаг: '))
  
  t_max <- step * (n - 1) / 2
  t <- seq(-t_max, t_max, by=step)
  
  x1 <- c(rectPulse(t, c), rep(0, length(t)))
  x2 <- c(gaussSignal(t, sigma), rep(0, length(t)))
  x3 <- c(rectPulse(t, c/2), rep(0, length(t)))
  x4 <- c(gaussSignal(t, sigma/2), rep(0, length(t)))
  
  y1 <- convolve(x1, x2, type='open') * step
  y2 <- convolve(x1, x3, type='open') * step
  y3 <- convolve(x2, x4, type='open') * step
  
  start <- (length(y1) - length(t)) / 2 + 1
  y1 <- y1[start:(start + length(t) - 1)]
  y2 <- y2[start:(start + length(t) - 1)]
  y3 <- y3[start:(start + length(t) - 1)]
  
  par(mfrow=c(2, 2))
  graph_figure(1, t, x1, x2, y1, 'Прямоугольный + Гаусс')
  graph_figure(2, t, x1, x3, y2, 'Прямоугольный + Прямоугольный')
  graph_figure(3, t, x2, x4, y3, 'Гаусс + Гаусс')
}

main()
