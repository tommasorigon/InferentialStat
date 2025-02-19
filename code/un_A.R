set.seed(123)
y <- c(rnorm(8), 3, 6, 9)

m_huber <- function(y, k) {
  0.5 * y^2 * I(abs(y) <= k) + (k * abs(y) - 0.5 * k^2) * I(abs(y) >= k)
}

M_huber <- function(theta, y, k) {
  -mean(m_huber(y - theta, k))
}
M_huber <- Vectorize(M_huber, vectorize.args = "theta")

k_values <- c(0.5, 1, 2, 4, 6, 8)
m_hat <- numeric(length(k_values))
for (i in 1:length(k_values)) {
  m_hat[i] <- nlminb(start = median(y), objective = function(theta) -M_huber(theta, y = y, k = k_values[i]))$par
}

print(y)

tab <- c(median(y), m_hat)
names(tab) <- c(0, k_values)
# knitr::kable(t(tab), digits = 3)

library(ggplot2)
library(ggthemes)

n <- 4
data_plot <- data.frame(p = rep(seq(from = 0, to = 1, length = 1000), 2))
data_plot$n <- "Sample size (n) = 4"
data_plot$MSE <- c(data_plot$p[1:1000] * (1 - data_plot$p[1:1000]) / n, data_plot$p[1:1000] * 0 + n / (4 * (n + sqrt(n))^2))
data_plot$Estimator <- rep(c("Maximum likelihood", "Minimax"), each = 1000)

n <- 4000
data_plot2 <- data.frame(p = rep(seq(from = 0, to = 1, length = 1000), 2))
data_plot2$n <- "Sample size (n) = 4000"
data_plot2$MSE <- c(data_plot2$p[1:1000] * (1 - data_plot2$p[1:1000]) / n, data_plot2$p[1:1000] * 0 + n / (4 * (n + sqrt(n))^2))
data_plot2$Estimator <- rep(c("Maximum likelihood", "Minimax"), each = 1000)

data_plot <- rbind(data_plot, data_plot2)

ggplot(data = data_plot, aes(x = p, y = MSE, col = Estimator)) +
  geom_line() +
  facet_wrap(. ~ n, scales = "free_y") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("p") +
  ylab("MSE")

data_plot <- data.frame(n = rep(round(seq(from = 3, 50, length = 5000)), 2))
data_plot$BayesRisk <- c(1 / (6 * data_plot$n[1:5000]), data_plot$n[1:5000] / (4 * (data_plot$n[1:5000] + sqrt(data_plot$n[1:5000]))^2))
data_plot$Estimator <- rep(c("Maximum likelihood", "Minimax"), each = 5000)

ggplot(data = data_plot, aes(x = n, y = BayesRisk, col = Estimator)) +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("n") +
  ylab("Integrated risk (Bayes risk)")

data_plot <- data.frame(n = rep(round(seq(from = 3, 50, length = 5000)), 3))
data_plot$BayesRisk <- c(1 / (6 * data_plot$n[1:5000]), data_plot$n[1:5000] / (4 * (data_plot$n[1:5000] + sqrt(data_plot$n[1:5000]))^2), 1 / (6 * (data_plot$n[1:5000] + 2)))
data_plot$Estimator <- rep(c("Maximum likelihood", "Minimax", "Bayes"), each = 5000)

ggplot(data = data_plot, aes(x = n, y = BayesRisk, col = Estimator)) +
  geom_line() +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("n") +
  ylab("Integrated risk (Bayes risk)")

library(ggplot2)
library(ggthemes)

n <- 100
data_plot <- data.frame(p = rep(seq(from = 0, to = 1, length = 1000), 2))
data_plot$n <- "Sample size (n) = 100"
data_plot$MSE <- c(data_plot$p[1:1000] * (1 - data_plot$p[1:1000]) / n, data_plot$p[1:1000] * 0 + n / (4 * (n + sqrt(n))^2))
data_plot$Estimator <- rep(c("Maximum likelihood", "Minimax"), each = 1000)

n <- 10000
data_plot2 <- data.frame(p = rep(seq(from = 0, to = 1, length = 1000), 2))
data_plot2$n <- "Sample size (n) = 10000"
data_plot2$MSE <- c(data_plot2$p[1:1000] * (1 - data_plot2$p[1:1000]) / n, data_plot2$p[1:1000] * 0 + n / (4 * (n + sqrt(n))^2))
data_plot2$Estimator <- rep(c("Maximum likelihood", "Minimax"), each = 1000)

data_plot <- rbind(data_plot, data_plot2)

ggplot(data = data_plot, aes(x = p, y = MSE, col = Estimator)) +
  geom_line() +
  facet_wrap(. ~ n, scales = "free_y") +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("p") +
  ylab("MSE")
