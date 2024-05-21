################################################################################
'Proyecto: Clase de Economia de la Educacion, Intec, mayo-junio 2024
Que hace: Compara Mincer & Spence
Autora: Marygracia Aquino
Ultima actualizacion: 21 mayo 2024'
################################################################################
'Indice
1. Creacion de variable: diploma mas alto adquirido
2. Regresiones
3. Exportar regresiones'

################################################################################
# 1. Creacion de variable: diploma mas alto adquirido

## data_reciente
data_reciente$primario <- ifelse(data_reciente$MAYOR_NIVEL_OBTENIDO==8,1,0)
data_reciente$secundario <- ifelse(data_reciente$MAYOR_NIVEL_OBTENIDO==1 |
                                     data_reciente$MAYOR_NIVEL_OBTENIDO==2,1,0)
data_reciente$licenciatura <- ifelse(data_reciente$MAYOR_NIVEL_OBTENIDO==3,1,0)
data_reciente$posgrado <- ifelse(data_reciente$MAYOR_NIVEL_OBTENIDO>3 &
                                     data_reciente$MAYOR_NIVEL_OBTENIDO<7,1,0)

## data_t2
data_t2$primario <- ifelse(data_t2$MAYOR_NIVEL_OBTENIDO==8,1,0)
data_t2$secundario <- ifelse(data_t2$MAYOR_NIVEL_OBTENIDO==1 |
                                     data_t2$MAYOR_NIVEL_OBTENIDO==2,1,0)
data_t2$licenciatura <- ifelse(data_t2$MAYOR_NIVEL_OBTENIDO==3,1,0)
data_t2$posgrado <- ifelse(data_t2$MAYOR_NIVEL_OBTENIDO>3 &
                                   data_t2$MAYOR_NIVEL_OBTENIDO<7,1,0)

################################################################################
# 2. Regresiones

## Mincer
mincer_reciente <- lm(ln_ingreso_mensual ~ anos_educativos + anos_experiencia_laboral
                      + anos_experiencia_laboral_2, data = data_reciente)
mincer_t2 <- lm(ln_ingreso_mensual ~ anos_educativos + anos_experiencia_laboral
                + anos_experiencia_laboral_2, data = data_t2)

##Spence
spence_reciente_sinexp <- lm(ln_ingreso_mensual ~ primario + secundario + licenciatura +
                        posgrado, data = data_reciente)
spence_reciente_conexp <- lm(ln_ingreso_mensual ~ primario + secundario + licenciatura +
                               posgrado + anos_experiencia_laboral + 
                               anos_experiencia_laboral_2, data = data_reciente)
spence_t2_sinexp <- lm(ln_ingreso_mensual ~ primario + secundario + licenciatura +
                               posgrado, data = data_t2)
spence_t2_conexp <- lm(ln_ingreso_mensual ~ primario + secundario + licenciatura +
                               posgrado + anos_experiencia_laboral + 
                               anos_experiencia_laboral_2, data = data_t2)

## Mincer vs Spence
mincer_spence_reciente <- lm(ln_ingreso_mensual ~ anos_educativos + primario + 
                               secundario + licenciatura + posgrado + 
                               anos_experiencia_laboral + anos_experiencia_laboral_2,
                             data = data_reciente)
mincer_spence_t2 <- lm(ln_ingreso_mensual ~ anos_educativos + primario + 
                               secundario + licenciatura + posgrado + 
                               anos_experiencia_laboral + anos_experiencia_laboral_2,
                             data = data_t2)

################################################################################
# 3. Exportar regresiones

## data_reciente
library(stargazer)
stargazer(mincer_reciente,spence_reciente_conexp,mincer_spence_reciente,
          type = "html", out = "Regresion_MincervsSpence_reciente.html",
          dep.var.labels=c("ln(ingreso mensual)"), 
          covariate.labels=c("Años educativos", "Diploma primario", 
                             "Diploma secundario", "Diploma licenciatura", 
                             "Diploma posgrado", "Experiencia laboral", 
                             "Experiencia laboral al cuadrado"))

## data_t2
stargazer(mincer_t2,spence_t2_conexp,mincer_spence_t2,
          type = "html", out = "Regresion_MincervsSpence_t2.html",
          dep.var.labels=c("ln(ingreso mensual)"), 
          covariate.labels=c("Años educativos", "Diploma primario", 
                             "Diploma secundario", "Diploma licenciatura", 
                             "Diploma posgrado", "Experiencia laboral", 
                             "Experiencia laboral al cuadrado"))
