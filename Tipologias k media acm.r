# Cargar la base de datos ESOCC desde el archivo CSV
library(tidyverse)
library(psych)
library(haven)
options(scipen = 999)

encuesta_raw <- read_sav("Encuesta Sociedad de Consumo 2023.sav")
ESOCC <- read_csv("ESOCC.csv", show_col_types = FALSE)

if (!"key" %in% names(encuesta_raw) || !"key" %in% names(ESOCC)) {
  stop("No se encontro la columna 'key' en una de las bases.")
}

# Variables extra: existen en ESOCC.csv, pero no en la encuesta original
vars_extra <- setdiff(names(ESOCC), names(encuesta_raw))

ESOCC <- encuesta_raw %>%
  left_join(ESOCC %>% select(key, all_of(vars_extra)), by = "key")



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

# Crear variables dicotomicas segun punto medio observado
vars_akatu_dicot <- c(
  "akatu_1", "akatu_2", "akatu_3", "akatu_4", "akatu_5",
  "akatu_6", "akatu_7", "akatu_9", "akatu_10", "akatu_11", "AKATU_TOTAL"
)

puntos_medios <- ESOCC %>%
  summarise(
    across(
      all_of(vars_akatu_dicot),
      ~ (min(.x, na.rm = TRUE) + max(.x, na.rm = TRUE)) / 2
    )
  ) %>%
  as.list()

ESOCC <- ESOCC %>%
  mutate(
    across(
      all_of(vars_akatu_dicot),
      ~ case_when(
        is.na(.x) ~ NA_integer_,
        .x >= puntos_medios[[cur_column()]] ~ 1L,
        TRUE ~ 0L
      ),
      .names = "{.col}_d"
    )
  )

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
windows()
fviz_nbclust(data_for_clust, kmeans, method = "wss")
akatu_vars <- ESOCC %>% select(akatu_1, akatu_2, akatu_3, akatu_4, akatu_5, akatu_6, akatu_7, akatu_9, akatu_10, akatu_11)
akatu_scaled <- scale(akatu_vars)
kmeans_result <- kmeans(akatu_scaled, centers = 3)  # Asumiendo 4 clusters como en tipologias.R

# Reetiquetar clusters por nivel de compromiso AKATU para evitar permutaciones
ESOCC$cluster_raw <- kmeans_result$cluster

perfil_cluster <- ESOCC %>%
  mutate(akatu_promedio = rowMeans(select(., starts_with("akatu_")), na.rm = TRUE)) %>%
  group_by(cluster_raw) %>%
  summarise(score_medio = mean(akatu_promedio, na.rm = TRUE), .groups = "drop") %>%
  arrange(score_medio)

mapa_nombres <- setNames(
  c("indiferente", "iniciante pragmatico", "consciente activo"),
  perfil_cluster$cluster_raw
)

ESOCC$cluster_akatu <- factor(
  mapa_nombres[as.character(ESOCC$cluster_raw)],
  levels = c("indiferente", "iniciante pragmatico", "consciente activo")
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

# Chi-cuadrado para todas las combinaciones de variables (todas con todas)
vars_chi <- c(
  "situacion_financiera.x", "M15", "NSE", "SEXO", "EDADR", "B1", "C2", "M4", "M9", "M13#1", "M13#3", "M13#7"
)

chi_results <- data.frame(
  var1 = character(),
  var2 = character(),
  estadistico = numeric(),
  gl = integer(),
  p_valor = numeric(),
  stringsAsFactors = FALSE
)

for (i in 1:(length(vars_chi) - 1)) {
  for (j in (i + 1):length(vars_chi)) {
    v1 <- vars_chi[i]
    v2 <- vars_chi[j]

    datos_test <- ESOCC %>%
      select(all_of(c(v1, v2))) %>%
      mutate(across(everything(), ~ as.factor(.x)))

    tabla <- table(datos_test[[v1]], datos_test[[v2]], useNA = "no")

    if (all(dim(tabla) > 1)) {
      test <- suppressWarnings(chisq.test(tabla))

      chi_results <- rbind(
        chi_results,
        data.frame(
          var1 = v1,
          var2 = v2,
          estadistico = as.numeric(test$statistic),
          gl = as.integer(test$parameter),
          p_valor = as.numeric(test$p.value),
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

chi_results <- chi_results %>%
  mutate(
    estadistico = round(estadistico, 2),
    p_valor = round(p_valor, 2),
    p_ajustado_bh = round(p_ajustado_bh, 2)
  )

print(chi_results)

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

summary(mca_result, nbelements = Inf, nbind = 0)


fviz_screeplot(mca_result, addlabels = TRUE)

# Plot de correspondencias multiples (mas interpretable: solo categorias)
fviz_mca_var(
  mca_result,
  repel = TRUE,
  ggtheme = theme_minimal()
)


# Si prefieres ver individuos y categorias juntos, usa este biplot:
# fviz_mca_biplot(mca_result, repel = TRUE, ggtheme = theme_minimal())



res.hcpc <- HCPC(mca_result, nb.clust = -1, graph = FALSE)

datos_con_clusters <- res.hcpc$data.clust


plot(res.hcpc, choice = "tree")

fviz_cluster(res.hcpc, geom = "point", main = "Clusters de Individuos basados en ACM")



res.hcpc <- HCPC(mca_result, nb.clust = 5, graph = FALSE)

n_clusters <- max(as.numeric(res.hcpc$data.clust$clust))
print(paste("El número óptimo sugerido es:", n_clusters))

# Ver cuántas observaciones en cada cluster
table(res.hcpc$data.clust$clust)

# Ver a qué cluster pertenece cada observación
res.hcpc$data.clust

round(res.hcpc$desc.var$category[[5]], 2)

