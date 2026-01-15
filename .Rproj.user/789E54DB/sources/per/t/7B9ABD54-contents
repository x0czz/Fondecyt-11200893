#Analisis bateria completa.

analisis_factorial <- Encuesta_Sociedad_de_Consumo_2023 %>%
  dplyr::select(O1_1:O1_9) %>%
  dplyr::mutate(across(everything(), as.numeric))

#Separar la muestra en dos para exploratorio y confirmatorio

n_total <- nrow(analisis_factorial)
punto_medio <- floor(n_total / 2)

mitad_arriba <- analisis_factorial %>% 
  slice(1:punto_medio)

mitad_abajo <- analisis_factorial %>% 
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

library(psych)
matrizpoli <- polychoric(datos) %>% select(C3_7, C3_9, C3_12, C3_14)

library(lavaan)
modelult <- '
F1 =~ O1_1 + O1_3 + O1_4 + O1_2
F2 =~ O1_5 + O1_6 + O1_7
'

modindices(Fit_modelult)

Fit_modelult <- cfa(modelult, 
              data = mitad_abajo,
              ordered = TRUE,
              estimator = "WLSMV")
summary(Fit_modelult, fit.measures = TRUE, standardized = TRUE)
modindices(Fit_modelult)

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
                         nfactors = 12, 
                         rotate = "oblimin", 
                         fm = "minres")

print(resultado_efa_full$loadings, cut = 0.3, sort = TRUE)

