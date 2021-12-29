
library(readxl)
datos_estudiantes <- read_excel("C:/Users/carlos Gomez/Desktop/proyecto estaditica/datos_estudiantes.xlsx")
datos_estudiantes

#cualitativas tablas
#Sexo
tablaf_sexo=table(datos_estudiantes$Sexo)
tablaf_sexo
prop.table(tablaf_sexo)
diagrama_sexo = barplot(tablaf_sexo, main="Diagrama Sexo" , xlab="sexo",names.arg = c("M","F"),col="#69b3a2")

#Episodios_por_semana_estres

tablaf_estres=table(datos_estudiantes$Episodios_por_semana_estres)
tablaf_estres
prop.table(tablaf_estres)
diagrama_sexo = barplot(tablaf_estres, main="Diagrama Estres" , xlab="Estres",names.arg = c("0","1","2","3","4"),col="#69b3a2")
#Episodios_por_semana_ira_frustacion

tablaf_ira=table(datos_estudiantes$Episodios_por_semana_ira_frustacion)
tablaf_ira
prop.table(tablaf_estres)
diagrama_sexo = barplot(tablaf_ira, main="Diagrama Ira&Frustracion" , xlab="Ira&Frustracion",names.arg = c("0","1","2","3","4"),col="#69b3a2")

#Episodios_por_semana_miedo
tablaf_miedo=table(datos_estudiantes$Episodios_por_semana_miedo)
tablaf_miedo
prop.table(tablaf_miedo)
diagrama_sexo = barplot(tablaf_miedo, main="Diagrama Miedo" , xlab="Miedo",names.arg = c("0","1","2","3"),col="#69b3a2")

#Episodios_por_semana_ansiedad
tablaf_ansiedad=table(datos_estudiantes$Episodios_por_semana_ansiedad)
tablaf_ansiedad
prop.table(tablaf_ansiedad)
diagrama_sexo = barplot(tablaf_ansiedad , main="Diagrama ansiedad" , xlab="Ansiedad",names.arg = c("0","1","2","3","4"),col="#69b3a2")
#cuantitativas