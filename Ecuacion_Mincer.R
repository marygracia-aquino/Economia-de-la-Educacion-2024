################################################################################
'Proyecto: Clase de Economia de la Educacion, Intec, mayo-junio 2024
Que hace: Limpia base de datos de ENCFT 2022
Autora: Marygracia Aquino
Ultima actualizacion: 14 mayo 2024'
################################################################################
'Indice
1. Cargar base de datos
2. Determinar variable ingreso
3. Determinar variable educacion
4. Determinar variable experiencia laboral
5. Quitar individuos que no formaran parte de la regresion
6. Regresiones
7. Exportar regresiones'

if (!require(c("dplyr","stargazer"))) install.packages(c("dplyr","stargazer"))
################################################################################
# 1. Cargar base de datos

##establecer carpeta de trabajo
setwd(dir = "C:/Users/maryg/OneDrive/3_Jobs/8_intec/6_Eco Educ_mayo-julio 2024/Clases/U2")

##Cargar ENCFT 2022
library(readxl)
data <- read_excel("ENCFT2022.xlsx")

################################################################################
# 2. Determinar variable ingreso

##ingreso mensual
data$ingreso_mensual <- rowSums(data[,c("INGRESO_ASALARIADO","COMISIONES",
                                        "PROPINAS","HORAS_EXTRA","INGRESO_INDEPENDIENTES",
                                        "INGRESO_ASALARIADO_SECUN","INGRESO_INDEPENDIENTES_SECUN")])

### version logaritmica
data$ln_ingreso_mensual <- log(data$ingreso_mensual)

## ingreso por hora
data$ingreso_hora <- (data$ingreso_mensual/4)/data$HORAS_TRABAJO_EFECT_TOTAL

### version logaritmica
data$ln_ingreso_hora <- log(data$ingreso_hora)

################################################################################
# 3. Determinar variable educacion

##chequear variables educativas
table(is.na(data$NIVEL_ULTIMO_ANO_APROBADO),is.na(data$ULTIMO_ANO_APROBADO))

### algunass variables educativas estan vacias, por lo que se cambia a cero
data$nivel_educ <- ifelse(is.na(data$NIVEL_ULTIMO_ANO_APROBADO),0,
                         data$NIVEL_ULTIMO_ANO_APROBADO)
data$ultimo_ano <- ifelse(is.na(data$ULTIMO_ANO_APROBADO),0,data$ULTIMO_ANO_APROBADO)
table(is.na(data$nivel_educ),is.na(data$ultimo_ano))

## anos educativos
data$anos_educativos <- NA
data$anos_educativos[data$nivel_educ==1] <- 0  #preescolar
data$anos_educativos[data$nivel_educ==10] <- 0 #quisqueya aprende contigo
data$anos_educativos[data$nivel_educ==9] <- 0 #niguno
data[data$nivel_educ==2,]$anos_educativos <- data[data$nivel_educ==2,]$ultimo_ano #primario
data[data$nivel_educ==3,]$anos_educativos <- data[data$nivel_educ==3,]$ultimo_ano + 8 #secundario
data[data$nivel_educ==4,]$anos_educativos <- data[data$nivel_educ==4,]$ultimo_ano + 8 #secundario tecnico
data[data$nivel_educ==5,]$anos_educativos <- data[data$nivel_educ==5,]$ultimo_ano + 12 #universitario
data[data$nivel_educ==6,]$anos_educativos <- data[data$nivel_educ==6,]$ultimo_ano + 16 #postgrado
data[data$nivel_educ==7,]$anos_educativos <- data[data$nivel_educ==7,]$ultimo_ano + 16 #maestria
data[data$nivel_educ==8,]$anos_educativos <- data[data$nivel_educ==8,]$ultimo_ano + 18 #postgrado

################################################################################
# 4. Determinar variable experiencia laboral

## proxy anos de experiencia laboral
data$anos_experiencia_laboral <- data$EDAD - data$anos_educativos - 6

## experiencia al cuadrado
data$anos_experiencia_laboral_2 <- data$anos_experiencia_laboral**2

################################################################################
# 5. Quitar individuos que no formaran parte de la regresion

## quitar a los que ganan cero ingresos
data2 <- data[data$ingreso_mensual>0,]

## hay personas que se repiten
table(duplicated(data2$ID_PERSONA),data2$TRIMESTRE)

### quedarse con la persona con el valor más reciente
library(dplyr)
data_reciente <- data2 %>% group_by(ID_PERSONA) %>% filter(TRIMESTRE == max(TRIMESTRE))
table(duplicated(data_reciente$ID_PERSONA),data_reciente$TRIMESTRE)

### escoger un trimestre
data_t2 <- data2[data2$TRIMESTRE==20222,]

################################################################################
# 6. Regresiones

mincer_reciente_m <- lm(ln_ingreso_mensual ~ anos_educativos + anos_experiencia_laboral + anos_experiencia_laboral_2, data = data_reciente)
mincer_t2_m <- lm(ln_ingreso_mensual ~ anos_educativos + anos_experiencia_laboral + anos_experiencia_laboral_2, data = data_t2)

################################################################################
# 7. Exportar regresiones

## guardar modelos en una lista
library(stargazer)
stargazer(mincer_reciente_m,mincer_t2_m, type = "html", out = "Regresion.html",
          dep.var.labels=c("Ln(ingreso mensual)"),
          covariate.labels=c("Años educativos","Experiencia laboral", 
                             "Experiencia laboral al cuadrado"))