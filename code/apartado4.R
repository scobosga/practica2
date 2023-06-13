## 4.2 Comprobación de normalidad y de homogeneidad

### 4.2.1 Comprobación de normalidad con el test de Shapiro-Wilk

# Crear un vector con los nombres de las variables
variables <- c("age", "trtbps", "chol", "thalachh")

# Crear una matriz para almacenar los resultados
resultados <- matrix(NA, nrow = length(variables), ncol = 2,
                     dimnames = list(variables, c("p-value", "Test statistic")))

# Realizar el Shapiro-Wilk test para cada variable y guardar los resultados en la matriz
for (i in 1:length(variables)) {
  var <- variables[i]
  shapiro_test <- shapiro.test(datos[[var]])
  resultados[i, "p-value"] <- shapiro_test$p.value
  resultados[i, "Test statistic"] <- shapiro_test$statistic
}

# Imprimir la tabla de resultados
print(resultados)


### 4.2.2 Comprobación de homogeneidad de la varianza

# Realizar el test de Levene para cada variable
levene_test_age <- leveneTest(datos$age, datos$output)
levene_test_trtbps <- leveneTest(datos$trtbps, datos$output)
levene_test_chol <- leveneTest(datos$chol, datos$output)
levene_test_thalachh <- leveneTest(datos$thalachh, datos$output)

# Imprimir los resultados
print("Test de Levene para age:")
print(levene_test_age)
print("--------------------------------------------------------------------")

print("Test de Levene para trtbps:")
print(levene_test_trtbps)
print("--------------------------------------------------------------------")

print("Test de Levene para chol:")
print(levene_test_chol)
print("--------------------------------------------------------------------")

print("Test de Levene para thalachh:")
print(levene_test_thalachh)
print("--------------------------------------------------------------------")


## 4.3 Comparación de grupos

### 4.3.1 Relación "output" vs "age", " chol", "trtbps" y "thalachh"

```{r}
# Variables seleccionadas
selected_vars <- c("age", "chol", "trtbps", "thalachh")

# Carga la librería necesaria
library(ggplot2)

# Crea el gráfico de densidad con todas las variables
ggplot(reshape2::melt(datos, id.vars = "output", measure.vars = selected_vars),
       aes(x = value, fill = factor(output))) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ variable, scales = "free") +
  xlab("Valor") +
  ylab("Densidad") +
  ggtitle("Densidad según el output para variables seleccionadas") +
  scale_fill_manual(values = c("blue", "red"), 
                    breaks = c(0, 1),
                    labels = c("No ataque", "Ataque"),
                    name = "Output") +
  labs(x = "Valor",            # Etiqueta para el eje x
       y = "Densidad",         # Etiqueta para el eje y
       title = "Densidad según el output para variables seleccionadas") +
  theme_minimal()
```
# Prueba t de Student para "chol"
t_test_chol <- t.test(datos$chol ~ datos$output)
t_test_chol

# Prueba t de Student para "trtbps"
t_test_trtbps <- t.test(datos$trtbps ~ datos$output)
t_test_trtbps

# Prueba test de Welch para "age"
t_test_age <- t.test(age ~ output, data = datos, var.equal = FALSE)
t_test_age

# Preuba test de Welch para "thalachh"
t_test_thalachh <- t.test(thalachh ~ output, data = datos, var.equal = FALSE)
t_test_thalachh


### 4.3.2 Relación "output" vs "sex", "fbs", "restecg" y "exng"

# Definir etiquetas para la leyenda
labels <- c("0" = "Sin ataque", "1" = "Ataque")

# Gráfico para "output" vs "sex"
plot_sex <- ggplot(datos, aes(x = sex, fill = as.factor(output))) +
  geom_bar() +
  labs(x = "Sexo", y = "Frecuencia", fill = "Output") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = labels) +
  ggtitle("Relación entre Output y Sexo")

# Gráfico para "output" vs "fbs"
plot_fbs <- ggplot(datos, aes(x = fbs, fill = as.factor(output))) +
  geom_bar() +
  labs(x = "Fbs", y = "Frecuencia", fill = "Output") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = labels) +
  ggtitle("Relación entre Output y Fbs")

# Gráfico para "output" vs "restecg"
plot_restecg <- ggplot(datos, aes(x = restecg, fill = as.factor(output))) +
  geom_bar() +
  labs(x = "Restecg", y = "Frecuencia", fill = "Output") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = labels) +
  ggtitle("Relación entre Output y Restecg")

# Gráfico para "output" vs "exng"
plot_exng <- ggplot(datos, aes(x = exng, fill = as.factor(output))) +
  geom_bar() +
  labs(x = "Exng", y = "Frecuencia", fill = "Output") +
  scale_fill_manual(values = c("0" = "blue", "1" = "red"), labels = labels) +
  ggtitle("Relación entre Output y Exng")

# Crear el panel con los gráficos
panel <- grid.arrange(plot_sex, plot_fbs, plot_restecg, plot_exng,
                      nrow = 2, ncol = 2)

# Realizar el test de chi-cuadrado para cada variable
chi2_sex <- chisq.test(table(datos$sex, datos$output))
chi2_fbs <- chisq.test(table(datos$fbs, datos$output))
chi2_exng <- chisq.test(table(datos$exng, datos$output))

# Crear un data frame con los resultados
resultados <- data.frame(
  Variable = c("sex", "fbs", "exng"),
  ChiSquared = c(chi2_sex$statistic, chi2_exng$statistic, chi2_exng$statistic),
  df = c(chi2_sex$parameter, chi2_fbs$parameter, chi2_exng$parameter),
  p_value = c(chi2_sex$p.value, chi2_fbs$p.value, chi2_exng$p.value)
)

# Imprimir la tabla de resultados
print(resultados)

# Crear una tabla de contingencia para las variables fbs y output
tabla_contingencia <- table(datos$fbs, datos$output)

# Realizar el test de asociación de Fisher
fisher_test <- fisher.test(tabla_contingencia)

# Imprimir los resultados
print(fisher_test)