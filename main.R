#Zadanie wariant 6

#f(x) = cos(x^2); x zawiera się w [1; 3]

# Instalacja pakietu neuralnet, jeśli jeszcze nie jest zainstalowany
# install.packages("neuralnet")

# Instalacja pakietu neuralnet, jeśli jeszcze nie jest zainstalowany
# install.packages("neuralnet")

# Załaduj potrzebne pakiety
library(neuralnet)

# Funkcja do generowania danych treningowych
generate_data <- function(n) {
  x <- seq(1, 3, length.out = n)
  y <- cos(x^2)
  data.frame(x = x, y = y)
}

# Ustaw ziarno losowości w celu reprodukowalności wyników
set.seed(123)

# Generuj dane treningowe
n <- 100
training_data <- generate_data(n)

# Definicja modelu sieci neuronowej
model <- neuralnet(
  y ~ x,
  data = training_data,
  hidden = c(5, 3),  # Liczba neuronów w warstwach ukrytych
  linear.output = TRUE  # Ustawienie wyjścia jako liniowe
)

# Wykres danych treningowych i rzeczywistej funkcji
plot(training_data$x, training_data$y, col = "blue", pch = 20, main = "Neural Network Fit - Training Data and True Function")
lines(training_data$x, cos(training_data$x^2), col = "black", lwd = 2)
legend("topleft", legend = c("Training Data", "True Function"), col = c("blue", "black"), pch = c(20, NA), lwd = c(NA, 2))

# Wykres danych treningowych i predykcji modelu
plot(training_data$x, training_data$y, col = "blue", pch = 20, main = "Neural Network Fit - Training Data and Model Prediction")
lines(training_data$x, predict(model, training_data), col = "red", lwd = 2)
legend("topleft", legend = c("Training Data", "Model Prediction"), col = c("blue", "red"), pch = c(20, NA), lwd = c(NA, 2))

# Testowanie modelu na nowych danych
new_data <- data.frame(x = seq(1, 3, length.out = 100))
predictions <- predict(model, newdata = new_data)

# Wykres predykcji na nowych danych
plot(new_data$x, predictions, col = "green", type = "l", lwd = 2, ylab = "Predicted f(x)", xlab = "x", main = "Model Prediction on New Data")
lines(new_data$x, cos(new_data$x^2), col = "black", lwd = 2)
legend("topleft", legend = c("Model Prediction", "True Function"), col = c("green", "black"), lwd = c(2, 2))
