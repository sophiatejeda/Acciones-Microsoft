# Cargar las librerías necesarias
library(tidyquant)
library(ggplot2)

# Obtener los datos utilizando tq_get del 2022 
datos <- tq_get("MSFT", from = "2022-01-01", to = "2022-12-31")

# Visualizar la estructura de los datos
str(datos)

# Resumen estadístico de los datos
summary(datos)

# Seleccionar las variables de interés
variables <- c("open", "high", "low", "close", "volume", "adjusted")
datos_seleccionados <- datos[, variables]

# Calcular la desviación estándar para cada variable
desviacion_estandar <- apply(datos_seleccionados, 2, sd)

# Imprimir los resultados
for (i in 1:length(variables)) {
  cat("Desviación estándar de", variables[i], ":", desviacion_estandar[i], "\n")
}

# Crear la gráfica de series de tiempo
ggplot(datos, aes(x = date)) +
  geom_line(aes(y = open, color = "Open")) +
  geom_line(aes(y = high, color = "High")) +
  geom_line(aes(y = low, color = "Low")) +
  geom_line(aes(y = close, color = "Close")) +
  geom_line(aes(y = adjusted, color = "Adjusted")) +
  labs(x = "Fecha", y = "Precio", title = "Series de tiempo del precio de las acciones de Microsoft") +
  scale_color_manual(values = c("Open" = "blue", "High" = "green", "Low" = "red", "Close" = "orange", "Adjusted" = "purple")) +
  theme_minimal()

# Gráfica de histograma del volumen de transacciones
ggplot(datos, aes(x = volume)) +
  geom_histogram(fill = "steelblue", bins = 30) +
  labs(title = "Histograma del Volumen de Transacciones",
       x = "Volumen", y = "Frecuencia")

# Estimacion por intervalos 

# Variables: open, high, low, close, volume, adjusted
variables <- c("open", "high", "low", "close", "volume", "adjusted")

for (variable in variables) {
  # Realizar la prueba t y obtener los resultados
  result <- t.test(msft_data[[variable]])
  
  # Obtener el intervalo de confianza
  conf_interval <- result$conf.int
  
  # Obtener el error de estimación
  error_estimation <- sd(msft_data[[variable]]) / sqrt(length(msft_data[[variable]]))
  
  # Imprimir los resultados
  cat("Variable: ", variable, "\n")
  cat("Intervalo de confianza (95%): [", conf_interval[1], ", ", conf_interval[2], "]\n")
  cat("Error de estimación: ", error_estimation, "\n")
}

# Hipotesis 1

# Realizar análisis de regresión lineal
lm_model <- lm(close - open ~ open * volume, data = msft_data)
lm_model
# Realizar prueba de hipótesis para la interacción
anova(lm_model)


# Hipotesis 2 

# Calcular el rango de precios diarios
msft_data$rango <- msft_data$high - msft_data$low

# Calcular la correlación entre el rango de precios y el volumen de transacciones
correlation <- cor(msft_data$rango, msft_data$volume)

# Calcular la correlación durante períodos de alta volatilidad (ejemplo: rango > 5)
high_volatility <- msft_data$rango > 5
correlation_high_volatility <- cor(msft_data$rango[high_volatility], msft_data$volume[high_volatility])






