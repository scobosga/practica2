# Subselección del conjunto de datos
selected_vars <- c("age", "sex", "trtbps", "chol", "fbs", "restecg", "thalachh", "exng", "output")
datos <- df[, selected_vars]
head(datos)