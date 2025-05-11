# Cargar paquetes necesarios
library(tidyverse)
library(janitor)
library(readr)
library(xtable)

# Cargar y limpiar el dataset
df <- read_csv("../data/data_traducida.csv") %>% select(-colnames(df)[1])

# Convertir severidad a factor
df$severidad <- as.factor(df$severidad)

# Eliminar columnas con demasiados NA
df <- df %>% select(where(~ mean(is.na(.)) < 0.3))


# num_vars <- sapply(df, is.numeric)
# df_num <- df[, num_vars]
# 
# # 2. Aplicar la prueba de normalidad a cada variable numérica
# for (var in names(df_num)) {
#   cat("\n==============================\n")
#   cat("Variable:", var, "\n")
#   cat("N:", length(df_num[[var]]), "\n")
#   cat("Media:", mean(df_num[[var]], na.rm = TRUE), "\n")
#   cat("Desviación estándar:", sd(df_num[[var]], na.rm = TRUE), "\n")
# 
#   # Histograma
#   hist(df_num[[var]], main = paste("Histograma de", var), xlab = var, col = "skyblue", breaks = 50)
# 
#   # QQ plot
#   qqnorm(df_num[[var]], main = paste("Q-Q plot de", var))
#   qqline(df_num[[var]], col = "red")
# 
#   # Pausa para ver gráficos
#   readline(prompt = "Presiona [Enter] para continuar...")
# }


# Convertir categóricas a factores
df$iluminación <- as.factor(df$iluminación)
df$carretera <- as.factor(df$carretera)
df$clima <- as.factor(df$clima)
df$festivos <- as.factor(df$festivos)
df$severidad <- as.factor(df$severidad)

sapply(df, is.factor)
cat_vars <- names(df)[sapply(df, is.factor)]

cat_vars <- setdiff(cat_vars, "severidad")

for (var in cat_vars) {
  tabla <- table(df$severidad, df[[var]])
  print(paste("Variable:", var))
  print(tabla)
}


# Crear tabla de contingencia y guardarla como imagen
for (var in cat_vars) {
  
  # Crear tabla de contingencia
  tabla <- table(df$severidad, df[[var]])
  
  # Para usar con ggplot
  df_tabla <- as.data.frame(tabla)
  colnames(df_tabla) <- c("Severidad", "Categoría", "Frecuencia")
  
  df_tabla$fill_color <- ifelse(df_tabla$Frecuencia < 5, "pink", "lightblue")
  
  # Crear gráfico de la tabla de contingencia usando ggplot
  p <- ggplot(df_tabla, aes(x = Severidad, y = Categoría, fill = fill_color)) +
    geom_tile() +
    geom_text(aes(label = Frecuencia), color = "black", size = 6) +
    scale_fill_identity() + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Tabla de Contingencia: Severidad vs", var), fill = "Frecuencia")
  
  ggsave(paste0("../images/original_contingency_table_", var, ".jpg"), plot = p, width = 8, height = 6, dpi = 300)
}

#Observamos que existen algunos valores para los que no es posible aplicar la prueba chi-cuadrado, sin embargo, dado que para carretera e iluminación, estos son los valores de desconocido, se descartarán estas observaciones.

df <- read_csv("../data/data_traducida.csv") %>% select(-colnames(df)[1])
df <- df[!(df$carretera == "Final del asfalto" |
             df$iluminación == "Desconocido" |
             df$clima == "Granizo" |
             df$clima == "Nieve"), ]

# Convertir categóricas a factores
df$iluminación <- as.factor(df$iluminación)
df$carretera <- as.factor(df$carretera)
df$clima <- as.factor(df$clima)
df$festivos <- as.factor(df$festivos)
df$severidad <- as.factor(df$severidad)
sapply(df, is.factor)
cat_vars <- names(df)[sapply(df, is.factor)]

cat_vars <- setdiff(cat_vars, "severidad")

for (var in cat_vars) {
  
  # Crear tabla de contingencia
  tabla <- table(df$severidad, df[[var]])
  
  # Convertir la tabla a un formato 'data.frame' para usar con ggplot
  df_tabla <- as.data.frame(tabla)
  colnames(df_tabla) <- c("Severidad", "Categoría", "Frecuencia")
  
  # Crear gráfico de la tabla de contingencia usando ggplot
  p <- ggplot(df_tabla, aes(x = Severidad, y = Categoría, fill = Frecuencia)) +
    geom_tile() +
    geom_text(aes(label = Frecuencia), color = "white", size = 6) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste("Tabla de Contingencia: Severidad vs", var), fill = "Frecuencia")
  
  # Guardar la imagen en formato JPG
  ggsave(paste0("../images/contingency_table_", var, ".jpg"), plot = p, width = 8, height = 6, dpi = 300)
}








resultados <- list()

# Recorrer cada variable categórica
for (var in cat_vars) {
  
  # Crear tabla de contingencia
  tabla <- table(df$severidad, df[[var]])
  
  # Verificar que la tabla tenga al menos 2 filas y 2 columnas
  if (nrow(tabla) > 1 && ncol(tabla) > 1) {
    
    # Intentar prueba Chi-cuadrado
    chi <- tryCatch(chisq.test(tabla), error = function(e) NULL)
    
    if (!is.null(chi)) {
      min_esperado <- min(chi$expected)
      
      if (min_esperado < 5) {
        # Usar Fisher si hay esperados < 5
        prueba <- "Fisher"
        p_valor <- fisher.test(tabla, simulate.p.value = TRUE)$p.value
        valido <- FALSE
      } else {
        prueba <- "Chi-cuadrado"
        p_valor <- chi$p.value
        valido <- TRUE
      }
    } else {
      # Si la chi-cuadrado falla, usar Fisher
      prueba <- "Fisher"
      p_valor <- fisher.test(tabla, simulate.p.value = TRUE)$p.value
      valido <- FALSE
    }
    
    # Agregar resultado como data.frame a la lista
    resultados[[length(resultados) + 1]] <- data.frame(
      variable = var,
      prueba = prueba,
      p_value = signif(p_valor, 8),
      valido_chi = valido
    )
  }
}

# Combinar todos los resultados en un único data.frame
df_resultados <- do.call(rbind, resultados)

# Mostrar resultado
print(df_resultados)
print(xtable(df_resultados), type = "latex", file = "../tablas/chi-cuadrado.tex")


# Para la Prueba T de Student

# Agrupar la severidad
df$severidad_binaria <- ifelse(df$severidad %in% c("Accidente fatal", "Accidente grave"),
                               "Alta", "Baja")

df$victimas_fatales <- as.numeric(as.character(df$fatales))
df$heridas_menores <- as.numeric(as.character(df$lesiones_menores))
df$lesiones_graves <- as.numeric(as.character(df$lesiones_graves))
df$lesiones_graves <- as.numeric(as.character(df$velocidad_recomendada))
df$lesiones_graves <- as.numeric(as.character(df$limite_velocidad))

summary(df)

# Aplicar prueba t de Student para cada variable numérica
num_vars <- c("fatales", "lesiones_menores", "lesiones_graves", "velocidad_recomendada", "limite_velocidad")

df_resultados_t <- data.frame(
  variable = character(),
  media_alta = numeric(),
  media_baja = numeric(),
  p_value = numeric(),
  significativo = logical(),
  stringsAsFactors = FALSE
)

# Iterar sobre las variables numéricas
for (var in num_vars) {
  grupo_alta <- df[df$severidad_binaria == "Alta", ][[var]]
  grupo_baja <- df[df$severidad_binaria == "Baja", ][[var]]
  
  # Asegurarse de que sean numéricos
  if (is.numeric(grupo_alta) && is.numeric(grupo_baja)) {
    t_result <- t.test(grupo_alta, grupo_baja)
    
    nueva_fila <- data.frame(
      variable = var,
      media_alta = mean(grupo_alta, na.rm = TRUE),
      media_baja = mean(grupo_baja, na.rm = TRUE),
      p_value = t_result$p.value,
      significativo = t_result$p.value < 0.05
    )
    
    # Agregar la fila al dataframe
    df_resultados_t <- rbind(df_resultados_t, nueva_fila)
  }
}



# Ver resultado
print(df_resultados_t)

df_resultados_t$p_value <- format(df_resultados_t$p_value, scientific = TRUE)
df_resultados_t$media_alta <- format(df_resultados_t$media_alta, scientific = TRUE)
df_resultados_t$media_baja <- format(df_resultados_t$media_baja, scientific = TRUE)


print(xtable(df_resultados_t, caption = "Prueba t de Student según severidad binaria", digits = c(0, 0, 4, 4, 10, 0)),
      include.rownames = FALSE, caption.placement = "top", type = "latex", file = "../tablas/t-test.tex")


for (var in num_vars) {
  cat("\nVariable:", var, "\n")
  print(table(df$severidad_binaria, is.na(df[[var]])))
}
