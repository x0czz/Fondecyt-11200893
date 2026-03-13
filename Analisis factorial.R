library(haven)
library(tidyverse)
library(lavaan)

Encuesta_Sociedad_de_Consumo_2023 <- read_sav("Encuesta Sociedad de Consumo 2023.sav")
-----#MODELO DE MEDIDA------
modelo_cfa <- '
  # F1: Consumo Crítico
  Consumo_Critico =~ O3_1 + O3_4 + O2_11 + O3_2 + O3_3 + O3_7

  # F2: Eficiencia y Ahorro (Dim economica-Ambiental)
  # Ahorro energetico quiza
  Eficiencia_Hogar =~ O2_1 + O2_2 + O2_3 + O2_5 + O2_4

  # F3: Planificación de gasto
  Planificacion =~ O1_10 + O2_8 + O2_9
  
  # F4: Reciclaje
  Facilidad_reciclaje =~ O4_2 + O4_3 + O4_5
  #F5: Responsabilidad
  Percep_responsabilidad  =~ O4_1 + O4_8 + O4_12
  
  # correlación de error
   O3_2 ~~  O3_3
'


modelo_consumo <- '
  # Factores Latentes
  PA1 =~ C3_7 + C3_9 + C3_12 
  PA2 =~ C3_3 + C3_8 + C3_11
'

#INVERTIR LAS VARIABLES QUE EMPIEZAN POR O4
Encuesta_Recodificada <- Encuesta_Sociedad_de_Consumo_2023 %>%
  mutate(across(starts_with("O4_"), 
                ~ 6 - ., 
                .names = "{.col}")) 

#ESTIMAR MODELO
fit_afc <- cfa(modelo_consumo, 
               data = Encuesta_Recodificada,
               ordered = TRUE,
               estimator = "WLSMV")

#SALIDA DEL MODELO.
summary(fit_afc, fit.measures = TRUE, standardized = TRUE) #
modindices(fit_afc) #INDICES DE MODIFICACION


#QUITAR PUNTIACIONES ANTERIORES DEL SET (por si se deben reemplazar)
ESOCC <- ESOCC %>%
  select(PA1 
        )


#PUNTUACIONES FACTORIALES POR METODO DE REGRESION
scores_regresion <- lavPredict(fit_afc, method = "regression")


#AGREGAR LAS PUNTUACIONES FACTORIALES COMO VARIABLES
scores_regresion <- scores_regresion %>% as.data.frame()
ESOCC <- cbind(ESOCC, scores_regresion)
colnames(ESOCC)



#GUARDAR BASE DE DATOS CON PUNTUACIONES FACTORIALES.
save(ESOCC, file = "ESOCC")
