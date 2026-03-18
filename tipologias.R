#K-PROTO
library(dplyr)
install.packages("factoextra")
library(factoextra)

# Librerías
library(dplyr)
library(clustMixType)

# 1️⃣ Selección y tipificación correcta de variables
set_original <- ESOCC %>%
  select(
    Consumo_Critico, 
    Eficiencia_Hogar, 
    Planificacion, 
    Facilidad_reciclaje, 
    Percep_responsabilidad,
    situacion_financiera.x, 
    M15
  ) %>%
  mutate(
    across(c(Consumo_Critico, Eficiencia_Hogar, Planificacion,
             Facilidad_reciclaje, Percep_responsabilidad), as.numeric),
    across(c(situacion_financiera.x, M15), as.factor)
  )

# 2️⃣ Crear copia estandarizada SOLO para clustering
set_scaled <- set_original

set_scaled[, 1:5] <- scale(set_scaled[, 1:5])

# 3️⃣ Fijar semilla para reproducibilidad
set.seed(123)

# 4️⃣ Estimar modelo permitiendo lambda automático
modelo <- kproto(set_scaled, k = 4, lambda = NULL)

# 5️⃣ Asignar clusters a base ORIGINAL (no escalada)
set_original$cluster <- modelo$cluster

# 6️⃣ Medias en ESCALA ORIGINAL (interpretables)
medias_cluster <- aggregate(
  cbind(Consumo_Critico, Eficiencia_Hogar, Planificacion,
        Facilidad_reciclaje, Percep_responsabilidad) ~ cluster,
  data = set_original,
  mean
)

medias_cluster

# 7️⃣ Distribución porcentual de variables categóricas
prop.table(table(set_original$cluster, 
                 set_original$situacion_financiera.x), 1)

prop.table(table(set_original$cluster, 
                 set_original$M15), 1)

# 8️⃣ Tamaño de clusters
modelo$size

# 9️⃣ Centros del modelo
modelo$centers





🧠 Reinterpretación completa de los clusters
🔵 Cluster 1 – Alto compromiso sostenible y estabilidad financiera

Valores altos en todas las conductas sostenibles

42.8% sin deudas

Solo 16.9% más endeudado

Mayoría sin déficit financiero

👉 Perfil: Consumidores responsables y financieramente estables
Es el grupo más sólido tanto en sostenibilidad como en situación económica.

🔴 Cluster 2 – Bajo compromiso sostenible pero financieramente estable

Valores bajos en conductas sostenibles

46.1% sin deudas (el mayor porcentaje)

22.4% más endeudado

Mayoría sin déficit financiero

👉 Perfil: Estables económicamente, pero poco comprometidos ambientalmente
No parece que la falta de conducta sostenible se deba a restricción financiera.

🟠 Cluster 3 – Alta vulnerabilidad financiera

49.8% más endeudado (el valor más alto)

Solo 12.7% sin deudas

63% en déficit financiero

Bajos niveles de conducta sostenible

👉 Perfil: Alta presión económica y bajo desempeño sostenible
Aquí sí se observa coherencia entre restricción económica y menor conducta sostenible.

🟢 Cluster 4 – Endeudamiento reciente con compromiso moderado

41.2% más endeudado

21.6% sin deudas

Conducta sostenible moderada

Mayoría sin déficit actual

👉 Perfil: Grupo en transición financiera
Podrían haber empeorado su situación en los últimos 3 años, pero no todos están en déficit actual.


F
# Ver el subconjunto de datos
vanguardia_df <- subset(datos, Consumo_Critico > 1.40)
