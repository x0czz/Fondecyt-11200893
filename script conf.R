#Analisis bateria completa.

analisis_factorial <- datos %>% select(starts_with(c("C3_","O1_","O2_","O3_", "O4_")))

#Separar la muestra en dos para exploratorio y confirmatorio

n_total <- nrow(analisis_factorial)
punto_medio <- floor(n_total / 2)

mitad_arriba <- analisis_factorial %>% 
  slice(1:punto_medio)

mitad_abajo <- analisis_factorial %>% 
  slice((punto_medio + 1):n_total)



library(paran)
fa.parallel(analisis_factorial, fa="fa")
paran(analisis_factorial, iterations = 1000)


efa_modulo <- fa(mitad_arriba, nfactors = 15, rotate = "varimax", fm = "pa")
print(efa_modulo, cut = 0.3)
round(efa_modulo$residual, 2)

library(psych)
matrizpoli <- polychoric(datos) %>% select(C3_7, C3_9, C3_12, C3_14)


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

