data("FoodExpenditure", package = "betareg")
FoodExpenditure$food <- FoodExpenditure$food / 100
FoodExpenditure$Income <- factor(cut(FoodExpenditure$income, breaks = c(0, 50, 100)))
levels(FoodExpenditure$Income) <- c("Low", "High")

x <- FoodExpenditure$food[FoodExpenditure$Income == "Low"]
z <- FoodExpenditure$food[FoodExpenditure$Income == "High"]

m1_low <- mean(x)
m2_low <- mean(x^2)

alpha_low <- m1_low * (m1_low - m2_low) / (m2_low - m1_low^2)
beta_low <- (1 - m1_low) * (m1_low - m2_low) / (m2_low - m1_low^2)

m1_high <- mean(z)
m2_high <- mean(z^2)

alpha_high <- m1_high * (m1_high - m2_high) / (m2_high - m1_high^2)
beta_high <- (1 - m1_high) * (m1_high - m2_high) / (m2_high - m1_high^2)

print(x)

print(z)

ggplot(data = FoodExpenditure, aes(x = food, col = Income)) +
  geom_rug() +
  theme_light() +
  theme(legend.position = "top") +
  scale_color_tableau(palette = "Color Blind") +
  xlab("Food expenditure (proportion)") +
  geom_function(fun = function(x) dbeta(x, alpha_low, beta_low), col = "#1170aa") +
  geom_function(fun = function(x) dbeta(x, alpha_high, beta_high), col = "#fc7d0b") +
  xlim(c(0.02, 0.4)) +
  ylab("Density")

x <- c(16, 18, 22, 25, 27)

m1 <- mean(x)
m2 <- mean(x^2)
sigma2 <- m2 - m1^2
N_mm <- m1^2 / (m1 - sigma2)
p_mm <- m1 / N_mm

N_seq <- 50:300
loglik <- numeric(length(N_seq))

for (i in 1:length(N_seq)) {
  loglik[i] <- sum(dbinom(x, N_seq[i], prob = mean(x) / N_seq[i], log = TRUE))
}

N_MLE <- N_seq[which.max(loglik)]
p_MLE <- mean(x) / N_MLE
print(x)

ggplot(data = data.frame(N = N_seq, loglik = loglik), aes(x = N, y = loglik)) +
  theme_light() +
  xlab("N") +
  ylab("log-likelihood") +
  geom_point() +
  geom_vline(xintercept = N_MLE, linetype = "dashed")



library(BayesDA)
library(MASS)
data(light)
light_micro <- 0.001 * light + 24.8
print(light)

k_values <- c(5, seq(from = 10, to = 70, by = 10))
m_hat <- numeric(length(k_values))
for (i in 1:length(k_values)) {
  m_hat[i] <- huber(light, k = k_values[i] / 4.4478)$mu
}
tab <- c(median(light), m_hat)
names(tab) <- c(0, k_values)
# knitr::kable(t(tab), digits = 3)


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

baseball <- read.table("../data/efron-morris-75-data.tsv", header = TRUE)
baseball <- baseball[, c(1:5, 7)]

p_hat <- baseball$BattingAverage
q_hat <- sqrt(baseball$At.Bats) * asin(2 * p_hat - 1)

q_JS <- mean(q_hat) + (1 - (length(q_hat) - 3) / sum((q_hat - mean(q_hat))^2)) * (q_hat - mean(q_hat))

p_JS <- (sin(q_JS / sqrt(baseball$At.Bats)) + 1) / 2

baseball$JS <- p_JS
colnames(baseball) <- c("First name", "Last name", "At Bats", "Hits", "Average (MLE)", "Remaining average", "James-Stein")
# knitr::kable(baseball[c(1:4, 15:18), c(1:5, 7,6)], row.names = F, digits = 3)

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
