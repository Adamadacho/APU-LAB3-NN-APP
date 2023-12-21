# Wczytanie danych z pliku CSV (przykładowy plik "pralki.csv")
data <- read.csv("pralki_dane.csv")

# 4.2. Normalizacja danych
# Create Vector of Column Max and Min Values
maxs <- apply(data[, 2:3], 2, max)
mins <- apply(data[, 2:3], 2, min)

# Use scale() and convert the resulting matrix to a data frame
scaled_data <- as.data.frame(scale(data[, 2:3], center = mins, scale = maxs - mins))

# Check out results
print(head(scaled_data, 10))

# 4.3. Po łączenie danych wejściowych i wyjściowych
# Column bind the data into one variable
# Załóżmy, że dane wyjściowe znajdują się w kolumnie "cena"
training_data <- cbind(scaled_data, Cena = data$cena)
colnames(training_data) <- c("Pojemnosc", "PredkoscWirowania", "Price")
print(training_data)

# 4.4. Nauczanie sieci neuronowej
# Train the neural network
# Going to have c(3, 2) hidden layers
# Threshold is a numeric value specifying the threshold for the partial derivatives of the error function as stopping criteria.
net_price <- neuralnet(Price ~ Pojemnosc + PredkoscWirowania, training_data, hidden = c(3, 2), threshold = 0.01)
print(net_price)

# 4.5. Wyświetlenie sieci neuronowej
# Plot the neural network
plot(net_price)

# 4.6. Prognozowanie z pomocą̨ sieci neuronowej
# Test the neural network on some testing data
# Załóżmy, że testowe dane znajdują się w pliku "testowe_dane.csv"
test_data <- read.csv("testowe_dane.csv")

scaled_test_data <- as.data.frame(scale(test_data[, 2:3], center = mins, scale = maxs - mins))
net_results <- compute(net_price, scaled_test_data)

# Run them through the neural network
# Let's see what properties net_price has
ls(net_results)

# Let's see the results
print(net_results$net.result)