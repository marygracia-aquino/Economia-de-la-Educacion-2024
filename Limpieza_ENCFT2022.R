################################################################################
'Proyecto: Clase de Economia de la Educacion, Intec, mayo-junio 2024
Que hace: Limpia base de datos de ENCFT 2022
Autora: Marygracia Aquino
Ultima actualizacion: 14 mayo 2024'
################################################################################
'Indice
1. Cargar base de datos y quedarse con variables relevantes
2. Exportar datos'

if (!require(c('readxl','writexl'))) install.packages(c('readxl','writexl'))
################################################################################
# 1. Cargar base de datos y quedarse con variables relevantes

##establecer carpeta de trabajo
setwd(dir = "C:/Users/maryg/OneDrive")

##Cargar ENCFT 2022
library(readxl)
bd <- read_excel("9_Resources/ENFT & ENCFT/ENCFT_2022/Base ENCFT 20221 - 20224.xlsx", 
                 sheet = 3)

##Dejar variables relevantes
bd2 <- bd[,c(1:21,121:141,240,516:573)]

################################################################################
# 2. Exportar datos
library(writexl)
write_xlsx(bd2,"3_Jobs/8_intec/6_Eco Educ_mayo-julio 2024/Clases/U2/ENCFT2022.xlsx")
