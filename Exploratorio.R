library(psych)
library(paran)
library(haven)
library(tidyverse)
#Separar la muestra en dos para exploratorio y confirmatorio

Encuesta_Sociedad_de_Consumo_2023 <- read_sav("Encuesta Sociedad de Consumo 2023.sav")

n_total <- nrow(Encuesta_Sociedad_de_Consumo_2023)
punto_medio <- floor(n_total / 2)

mitad_arriba <- Encuesta_Sociedad_de_Consumo_2023 %>% 
  slice(1:punto_medio)

mitad_abajo <- Encuesta_Sociedad_de_Consumo_2023 %>% 
  slice((punto_medio + 1):n_total)

mitad_abajo_cfa <- mitad_abajo %>%
  mutate(across(starts_with("O4_"), ~ as.numeric(.)))

mitad_arriba <- mitad_arriba %>%
  dplyr::select(-O1_10)

library(psych)
library(paran)
fa.parallel(analisis_factorial, fa="fa")
paran(analisis_factorial, iterations = 1000)


efa_modulo <- fa(mitad_arriba, nfactors = 2, rotate = "varimax", fm = "pa")
print(efa_modulo, cut = 0.3)
round(efa_modulo$residual, 2)







#PLANTILLA CONFIABILIDAD PARA LOS FACTORES (ALPHA Y OMEGA)
matrizpoli <- ESOCC %>%
  dplyr::select(O1_10, O2_8, O2_9) %>%
  psych::polychoric() %>%
  purrr::pluck("rho")


alpha(matrizpoli)
omega(matrizpoli)








todas_vars <- c(
  paste0("O1_", 1:10),
  paste0("O2_", 1:14),
  paste0("O3_", 1:10),
  paste0("O4_", 1:12)
)
df_completo <- Encuesta_Sociedad_de_Consumo_2023[, todas_vars]

df_completo <- Encuesta_Sociedad_de_Consumo_2023[, todas_vars]
poly_matrix_full <- polychoric(df_completo)
fa.parallel(poly_matrix_full$rho, n.obs = 1623, fa = "fa", 
            main = "Análisis Paralelo: Estructura Completa O1-O4")
fa.parallel(poly_cor$rho, n.obs = 1623, fa = "fa", main = "Análisis Paralelo")
resultado_efa_full <- fa(poly_matrix_full$rho, 
                         nfactors = 5, 
                         rotate = "oblimin", 
                         fm = "minres")


print(resultado_efa_full$loadings, cut = 0.3, sort = TRUE)

moduloc_completo <- fusionar_extremos(moduloc_corrected)
fit_afc <- cfa(modelo_cfa, 
               data = moduloc_completo,
               ordered = TRUE,
               estimator = "WLSMV")

