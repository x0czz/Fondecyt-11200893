
options(scipen = 999)
# A mayor educacion, Mayor consumo critico.
cor.test(ESOCC$M4, ESOCC$MR1)
cor.test(ESOCC$M4, ESOCC$MR2)

#A Mayor edad mayor consumo critico.
cor.test(ESOCC$EDAD, ESOCC$MR1)
cor.test(ESOCC$EDAD, ESOCC$MR2)
#Las mujeres consumen mas criticamente que los hombres
t.test(MR1~ SEXO, ESOCC)
t.test(MR2~ SEXO, ESOCC)
#A mayor clase social, chance de encanchar con el consumo critico
cor.test(ESOCC$M7, ESOCC$MR1)
cor.test(ESOCC$M7, ESOCC$MR2)
#Estudiantes y trabajadores domesticos mas propensos a consumir criticamente
ESOCC$O3_1 <- factor(ESOCC$O3_1, levels = sort(unique(ESOCC$O3_4)),ordered = TRUE)
#Gente que nunca ha trabajado, participan menos en el consumo critico
modelo <- polr(O3_1 ~ M4 + M7 + E2 + E3, data = ESOCC, Hess = TRUE)
summary(modelo)
# La bibliografia indica que, los diferentes recursos pueden incidir mas en el buycot que en el boycot;
# los recursos por lo tanto predicen mejor el buycot que el boycot

cor.test(ESOCC$E2, ESOCC$MR2, method="spearman")



save(ESOCC, file = "base_de_datos_completa.RData")
