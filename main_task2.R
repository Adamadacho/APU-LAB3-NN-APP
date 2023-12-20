# Instalacja paczki neuralnet, jeśli jeszcze nie jest zainstalowana
# install.packages("neuralnet")

# Załaduj potrzebne pakiety
library(neuralnet)
library(caret)

# Wczytaj dane treningowe
data <- read.csv("pralki.csv")

# Podziel dane na zbiór treningowy i testowy
set.seed(123)
splitIndex <- createDataPartition(data$cena, p = 0.8, list = FALSE)
train_data <- data[splitIndex, ]
test_data <- data[-splitIndex, ]

# Normalizacja danych (opcjonalnie, w zależności od potrzeb)
# train_data[, -which(names(train_data) == "cena")] <- scale(train_data[, -which(names(train_data) == "cena")])
# test_data[, -which(names(test_data) == "cena")] <- scale(test_data[, -which(names(test_data) == "cena")])

# Ustaw ziarno losowości w celu reprodukowalności wyników
set.seed(123)

# Konwersja kolumny "statusOpinii" na liczbę
train_data$statusOpinii <- as.numeric(as.factor(train_data$statusOpinii))
test_data$statusOpinii <- as.numeric(as.factor(test_data$statusOpinii))


# Usuń kolumnę "nazwaPralki"
train_data <- train_data[, !(names(train_data) %in% c("nazwaPralki"))]
test_data <- test_data[, !(names(test_data) %in% c("nazwaPralki"))]

# Definicja modelu sieci neuronowej
model <- neuralnet(
  cena ~ .,
  data = train_data,
  hidden = c(3, 2),  # Mniejsza liczba neuronów
  linear.output = TRUE
)

# Prognozowanie cen na danych testowych
predictions <- predict(model, newdata = test_data)

# Obliczanie błędu prognoz
test_error <- sqrt(mean((predictions - test_data$cena)^2))
cat("Root Mean Squared Error on Test Data:", test_error, "\n")

# Możesz dostosować strukturę modelu, parametry treningowe itp., aby uzyskać lepsze rezultaty

# Oblicz błędy prognoz
errors <- predictions - test_data$cena

# Narysuj wykres rozrzutu z linią trendu
plot(test_data$cena, predictions, main = "Porównanie Rzeczywistych i Przewidzianych Cen",
     xlab = "Rzeczywista Cena", ylab = "Przewidziana Cena", pch = 16, col = "blue")
abline(lm(predictions ~ test_data$cena), col = "red")


