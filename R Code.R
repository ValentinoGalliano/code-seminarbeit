n <- 10000
set.seed(123)
w <- rbinom(n, size = 1, prob = 0.5)
m <- cumsum(w) / 1:n
e <- 0.5
plot(1:n, m, type = "l", col = "blue",
     xlab = "Anzahl der Würfe",
     ylab = "Empirischer Mittelwert",
     main = "Beispiel: Münzwurf")
abline(h = e, col = "red", lty = 2)
legend("topright", legend = c("Empirischer Mittelwert", "Erwartungswert (0.5)"),
       col = c("blue", "red"), lty = c(1,2), bty = "n")



n <- 10000
schritt <- 100
x <- runif(n)
y <- runif(n)
ist_drinnen <- x^2 + y^2 <= 1
pi <- numeric(n / schritt)
for (i in seq(schritt, n, by = schritt)) {
  pi[i / schritt] <- 4 * mean(ist_drinnen[1:i])
}
x_werte <- seq(schritt, n, by = schritt)
plot(x_werte, pi, type = "l", col = "blue", lwd = 2,
     main = expression("Monte Carlo-Schätzung von Pi"),
     xlab = "Anzahl zufälliger Punkte",
     ylim = c(2.5, 3.7))
abline(h = pi, col = "red", lty = 2, lwd = 2)
legend("topright", legend = c("Schätzung", "wahrer Wert von Pi"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

