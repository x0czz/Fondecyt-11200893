# Cargar la base de datos ESOCC desde el archivo CSV
library(tidyverse)
library(psych)
library(haven)

encuesta_raw <- read_sav("Encuesta Sociedad de Consumo 2023.sav")
esocc_creadas <- read_csv("ESOCC.csv", show_col_types = FALSE)

if (!"key" %in% names(encuesta_raw) || !"key" %in% names(esocc_creadas)) {
  stop("No se encontro la columna 'key' en una de las bases.")
}

# Variables extra: existen en ESOCC.csv, pero no en la encuesta original
vars_extra <- setdiff(names(esocc_creadas), names(encuesta_raw))

ESOCC <- encuesta_raw %>%
  left_join(esocc_creadas %>% select(key, all_of(vars_extra)), by = "key")



# Eliminar variables (columnas) duplicadas
ESOCC <- ESOCC[, !duplicated(names(ESOCC))]

# Crear las nuevas variables akatu_ basadas en los principios AKATU
ESOCC <- ESOCC %>%
  mutate(
    akatu_1 = O2_8 + O2_9,  # Principio 1: Planificar compras
    akatu_2 = O2_10 + O3_7,  # Principio 2: Evaluar impactos
    akatu_3 = C3_5,  # Principio 3: Consumir solo lo necesario (inverso de C3_5) #Revisar direccion.
    akatu_4 = O1_7 + O2_7,  # Principio 4: Reutilizar/Reparar
    akatu_5 = O2_6,  # Principio 5: Separar basura/Reciclar
    akatu_6 = O1_10 +  6-C3_2,  # Principio 6: Uso consciente del crédito
    akatu_7 = O1_8 + O3_1 + O3_4,  # Principio 7: Valorar RSE de empresas
    akatu_9 = O2_13 + O3_5 + O3_6,  # Principio 9: Mejoría de productos/Feedback
    akatu_10 = O3_2 + O3_3,  # Principio 10: Divulgar consumo consciente
    akatu_11 = O4_12 + C3_14  # Principio 12: Reflexión sobre valores
  )

  ESOCC <- ESOCC %>% 
    mutate(
        AKATU_TOTAL = akatu_1 + akatu_2 + akatu_3 + akatu_4 + akatu_5 + akatu_6 + akatu_7 + akatu_9 + akatu_10 + akatu_11)

# Etiquetas para variables AKATU creadas
attr(ESOCC$akatu_1, "label") <- "P1 Planificar compras"
attr(ESOCC$akatu_2, "label") <- "P2 Evaluar impactos"
attr(ESOCC$akatu_3, "label") <- "P3 Consumir solo lo necesario"
attr(ESOCC$akatu_4, "label") <- "P4 Reutilizar/Reparar"
attr(ESOCC$akatu_5, "label") <- "P5 Separar basura/Reciclar"
attr(ESOCC$akatu_6, "label") <- "P6 Uso consciente del credito"
attr(ESOCC$akatu_7, "label") <- "P7 Valorar RSE"
attr(ESOCC$akatu_9, "label") <- "P9 Feedback y mejora"
attr(ESOCC$akatu_10, "label") <- "P10 Divulgar consumo consciente"
attr(ESOCC$akatu_11, "label") <- "P12 Reflexion sobre valores"
attr(ESOCC$AKATU_TOTAL, "label") <- "Indice AKATU total"

# Guardar la base de datos actualizada de vuelta al CSV
write_csv(ESOCC, "ESOCC.csv")

# Generar tabla de descriptivos para akatu_1 a akatu_11
ESOCC %>%
  select(akatu_1, akatu_2, akatu_3, akatu_4, akatu_5, akatu_6, akatu_7, akatu_9, akatu_10, akatu_11) %>%
  describe()

ESOCC$AKATU_TOTAL %>% describe()

# Encontrar el número óptimo de clusters usando el método del codo
library(factoextra)
data_for_clust <- ESOCC %>% select(akatu_1, akatu_2, akatu_3, akatu_4, akatu_5, akatu_6, akatu_7, akatu_9, akatu_10, akatu_11)
fviz_nbclust(data_for_clust, kmeans, method = "wss")
akatu_vars <- ESOCC %>% select(akatu_1, akatu_2, akatu_3, akatu_4, akatu_5, akatu_6, akatu_7, akatu_9, akatu_10, akatu_11)
akatu_scaled <- scale(akatu_vars)
kmeans_result <- kmeans(akatu_scaled, centers = 3)  # Asumiendo 4 clusters como en tipologias.R

# Agregar el cluster a ESOCC y convertirlo en factor categórico
ESOCC$cluster_akatu <- factor(
  kmeans_result$cluster,
  levels = c(1, 2, 3),  
  labels = c("indiferente", "consciente activo", "iniciante pragmatico")
)

# Imprimir tamaños de clusters
print("Tamaños de clusters:")
print(kmeans_result$size)

# Grafico del k-medias (clusters sobre componentes principales)
fviz_cluster(
  kmeans_result,
  data = akatu_scaled,
  geom = "point",
  ellipse.type = "norm",
  ggtheme = theme_minimal(),
  main = "K-medias sobre variables AKATU"
)

# Medias de cada variable por cluster
medias_cluster <- aggregate(cbind(akatu_1, akatu_2, akatu_3, akatu_4, akatu_5, akatu_6, akatu_7, akatu_9, akatu_10, akatu_11) ~ cluster_akatu, data = ESOCC, mean)

print(medias_cluster)

# Varianza explicada
total_ss <- sum(scale(akatu_vars, scale = FALSE)^2)
within_ss <- kmeans_result$tot.withinss
explained_var <- 1 - (within_ss / total_ss)
print(paste("Varianza explicada:", explained_var))

# Tabla de resultados K-medias lista para Quarto
tabla_kmeans <- medias_cluster %>%
  left_join(
    ESOCC %>%
      count(cluster_akatu, name = "n") %>%
      mutate(porcentaje = round(100 * n / sum(n), 2)),
    by = "cluster_akatu"
  ) %>%
  mutate(
    across(starts_with("akatu_"), ~ round(.x, 2)),
    varianza_explicada = round(explained_var, 3)
  ) %>%
  relocate(n, porcentaje, .after = cluster_akatu)

knitr::kable(
  tabla_kmeans,
  caption = "Resultados del K-medias (centros por cluster, tamano y porcentaje)",
  align = "lrrrrrrrrrrrrr"
)

# Guardar nuevamente con el cluster
write_csv(ESOCC, "ESOCC.csv")


acm_subset <- ESOCC |>
  select(
    cluster_akatu, situacion_financiera.x, M15, NSE, SEXO, EDADR, B1, C2, M4, M9, `M13#1`, `M13#3`, `M13#7`
  )

# Convertir a factor usando labels de haven cuando existan
acm_subset <- acm_subset |>
  mutate(
    across(
      everything(),
      ~ if (inherits(.x, "haven_labelled") || inherits(.x, "labelled")) {
        haven::as_factor(.x, levels = "labels")
      } else {
        as.factor(.x)
      }
    )
  )

#ACM

# Scree plot para ver las dimensiones
library(factoextra)


# Realizar Análisis de Correspondencias Múltiples (ACM)
library(FactoMineR)
mca_result <- MCA(acm_subset, ncp = 4, graph = FALSE)

summary(mca_result)


fviz_screeplot(mca_result, addlabels = TRUE)

# Plot de correspondencias multiples (mas interpretable: solo categorias)
fviz_mca_var(
  mca_result,
  repel = TRUE,
  ggtheme = theme_minimal()
)

# Si prefieres ver individuos y categorias juntos, usa este biplot:
# fviz_mca_biplot(mca_result, repel = TRUE, ggtheme = theme_minimal())


