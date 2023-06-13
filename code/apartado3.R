# Gestión de ceros y valores nulos
print('NA')
colSums(is.na(datos))
print('Blancos')
colSums(datos=="")

# Gráfico de histograma para "age"
hist_age <- ggplot(datos, aes(x = age)) +
  geom_histogram(fill = "cornflowerblue", color = "black", bins = 30) +
  labs(x = "Edad del paciente", y = "Contador de ocurrencias") +
  ggtitle("age")

# Gráfico de histograma para "trtbps"
hist_trtbps <- ggplot(datos, aes(x = trtbps)) +
  geom_histogram(fill = "cornflowerblue", color = "black", bins = 30) +
  labs(x = "Presión arterial en reposo", y = "Contador de ocurrencias") +
  ggtitle("trtbps")

# Gráfico de histograma para "chol"
hist_chol <- ggplot(datos, aes(x = chol)) +
  geom_histogram(fill = "cornflowerblue", color = "black", bins = 30) +
  labs(x = "Colesterol (mg/dL)", y = "Contador de ocurrencias") +
  ggtitle("Chol")

# Gráfico de histograma para "thalach"
hist_thalach <- ggplot(datos, aes(x = thalachh)) +
  geom_histogram(fill = "cornflowerblue", color = "black", bins = 30) +
  labs(x = "Frecuencia cardiaca máxima", y = "Contador de ocurrencias") +
  ggtitle("thalach")


# Mostrar resúmenes estadísticos
variables <- c("age", "trtbps", "thalachh", "chol")

# Filtrar las variables deseadas
filtered_data <- datos[, variables]

# Calcular el resumen estadístico
summary_data <- summary(filtered_data)

# Mostrar el resumen estadístico
print(summary_data)

# Crear la cuadrícula con ambos gráficos
grid.arrange(hist_age,  hist_trtbps, hist_chol, hist_thalach, ncol = 2)

# Gráfico de barras para "exng"
bar_exang <- ggplot(datos, aes(x = factor(exng))) +
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(x = "Angina inducida por ejercicio", y = "Contador de ocurrencias") +
  ggtitle("exng")

# Gráfico de barras para "output"
bar_out <- ggplot(datos, aes(x = factor(output))) +
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(x = "Probabulidad ataque cardiaco", y = "Contador de ocurrencias") +
  ggtitle("output")

# Gráfico de barras para "sex"
bar_sex <- ggplot(datos, aes(x = factor(sex))) +
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(x = "Género del paciente", y = "Contador de ocurrencias") +
  ggtitle("sex")

# Gráfico de barras para "fbs"
bar_fbs <- ggplot(datos, aes(x = factor(fbs))) +
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(x = "Azúcar en sangre en ayunas", y = "Contador de ocurrencias") +
  ggtitle("fbs")

# Gráfico de barras para "restecg"
bar_res <- ggplot(datos, aes(x = factor(restecg))) +
  geom_bar(fill = "cornflowerblue", color = "black") +
  labs(x = "Electrocardiograma en reposo.", y = "Contador de ocurrencias") +
  ggtitle("restecg")

# Crear la cuadrícula con ambos gráficos
multiplot(bar_exang, bar_out, bar_sex, bar_fbs, bar_res, cols = 3)

# Función para calcular la tabla de proporciones
calc_prop <- function(variable) {
  prop <- prop.table(table(variable))
  prop_df <- data.frame(Categoria = names(prop), Proporcion = prop)
  return(prop_df)
}

# Variables a analizar
variables <- c("exng", "output", "sex", "fbs", "restecg")

# Calcular las tablas de proporciones para las variables
prop_tables <- lapply(variables, function(var) calc_prop(datos[[var]]))

# Imprimir las tablas de proporciones
for (i in seq_along(prop_tables)) {
  cat(paste("Tabla de proporciones para", variables[i], ":\n"))
  print(prop_tables[[i]])
  cat("\n")
}