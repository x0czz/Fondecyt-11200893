moduloc2 <- fusionar_extremos(segunda_mitad)

modelo_cfa <-'
  MR1 =~ O3_4 + O3_3 + O3_1 + O3_8 + O3_2 + O3_9 + O3_7 + O3_5 + O2_11 + O2_13 + O2_10
  MR2 =~ O2_8 + O2_4 + O2_1 + O2_5 + O2_3 + O2_2 + O2_10 + O2_14 + O3_8 + O3_8
  MR3 =~ O4_3 + O4_5 + O4_2
  MR5 =~ O4_8 + O4_1 + O4_12 + O4_10
  MR4 =~ O1_1 + O1_3 + O1_4 + O1_9
  MR6 =~ O1_5 + O1_7 + O3_10
  O3_4 ~~ O3_1
'
library(lavaan)
fit_afc <- cfa(modelo_cfa, 
               data = moduloc2,
               ordered = TRUE,
               estimator = "WLSMV")

summary(fit_afc, fit.measures = TRUE, standardized = TRUE)
modindices(fit_afc)

```

```{r}
moduloc_completo <- fusionar_extremos(moduloc_corrected)
fit_afc <- cfa(modelo_cfa, 
               data = moduloc_completo,
               ordered = TRUE,
               estimator = "WLSMV")
scores_regresion <- lavPredict(fit_afc, method = "regression")

scores_regresion <- scores_regresion %>% as.data.frame()
moduloc_scores <- cbind(moduloc_completo, scores_regresion)
```

```{r}
datos<- Encuesta_Sociedad_de_Consumo_2023 %>% select(numericalId ,SEXO, EDAD, EDADR, NSE, NSE_RECOD, NSE_sint, ZONA, B1, C1, C2, C3_1:C3_16, C5a, A1:`A2#4`, O5, E2, E3, E4, `S5#2`:`S5#99`, `T6#1`:`T6#99`, M14, M5, M6, M7, M8, M9, M10, M11_1:M11_8, M12_1:M12_6, `M13#1`:`M13#99`, M14, M15, M16_1:M16_9, M18_1:M18_7)
datos <- datos %>% left_join(moduloc_scores, by = "numericalId")

```

Analisis segundo modulo.

```{r}

modulo1 <- datos %>% dplyr::select(starts_with("C3_")) %>% fusionar_extremos()
mitad <- floor(nrow(modulo1) / 2)

# Mitad de arriba (de la fila 1 a la mitad)
muestra_arriba <- head(modulo1, n = mitad)

# Mitad de abajo (de la fila siguiente a la mitad hasta el final)
muestra_abajo <- tail(modulo1, n = nrow(modulo1) - mitad)
install.packages("paran")
library(paran)
fa.parallel(muestra_arriba, fa="fa")
paran(muestra_arriba, iterations = 1000)
```

```{r}
efa_modulo <- fa(muestra_arriba, nfactors = 3, rotate = "varimax", fm = "pa")
print(efa_modulo, cut = 0.3)
round(efa_modulo$residual, 2)

C3_7  C3_9 C3_12 C3_14 C3_16  C3_3  C3_8 C3_11  C3_1  C3_4  C3_6 C3_15 
0.456 0.324 0.490 0.268 0.327 0.417 0.295 0.322 0.173 0.108 0.155 0.247 

modelo_actualizado <- '
  # Factor 1: Estatus, Marcas y Tecnología
  # Incluye C3.10 (marcas dicen algo) por su contenido y h2=0.182
  PA1 =~ C3_7 + C3_9 + C3_12 + C3_14

  PA2 =~ C3_8 + C3_11 + C3_3 

'

modulo1 <- datos %>% dplyr::select(starts_with("C3_")) %>% fusionar_extremos()
modulo1 <- datos %>% dplyr::select(starts_with("C3_")) %>% fusionar_extremos()
mitad <- floor(nrow(modulo1) / 2)

# Mitad de arriba (de la fila 1 a la mitad)
muestra_arriba <- head(modulo1, n = mitad)

# Mitad de abajo (de la fila siguiente a la mitad hasta el final)
muestra_abajo <- tail(modulo1, n = nrow(modulo1) - mitad)


muestra_abajo <- muestra_abajo %>%
  mutate(across(starts_with("C3_"), ~ as.numeric(zap_labels(.x)))) 

fit <- cfa(modelo_actualizado, data = datos, estimator = "WLSMV", ordered = TRUE)

summary(fit, standardized = TRUE, fit.measures = TRUE)
modindices(fit)


datos %>% select(C3_7, C3_9, C3_12, C3_14) %>% omega()

muestra_abajo %>% select(C3_8, C3_11, C3_3) %>% alpha()


scores_regresion <- lavPredict(fit, method = "regression")

scores_regresion <- scores_regresion %>% as.data.frame()
datos_final <- cbind(datos, scores_regresion)
view(datos_final)

install.packages("writexl")
library(writexl)
write_xlsx(datos_final, "datos+factores")
