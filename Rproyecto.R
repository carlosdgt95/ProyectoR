
library(readxl)
datos_estudiantes <- read_excel("datos_estudiantes.xlsx")
datos_estudiantes

#cualitativas tablas
#Sexo
tablaf_sexo=table(datos_estudiantes$Sexo)
tablaf_sexo
prop.table(tablaf_sexo)
diagrama_sexo = barplot(tablaf_sexo, main="Diagrama Sexo" , xlab="sexo",names.arg = c("M","F"),col=c( 'lightblue', 'pink'))

#Episodios_por_semana_estres

tablaf_estres=table(datos_estudiantes$Episodios_por_semana_estres)
tablaf_estres
prop.table(tablaf_estres)
diagrama_sexo = barplot(tablaf_estres, main=" Diagrama Episodios Semanales de Estres" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col=c( '#F9E79F', '#F4D03F', '#B7950B', '#7D6608' ))
#Episodios_por_semana_ira_frustacion

tablaf_ira=table(datos_estudiantes$Episodios_por_semana_ira_frustacion)
tablaf_ira
prop.table(tablaf_estres)
diagrama_sexo = barplot(tablaf_ira, main="Diagrama Episodios Semanales de Ira & Frustracion" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col="#F9EF28")

#Episodios_por_semana_miedo
tablaf_miedo=table(datos_estudiantes$Episodios_por_semana_miedo)
tablaf_miedo
prop.table(tablaf_miedo)
diagrama_sexo = barplot(tablaf_miedo, main="Diagrama Episodios Semanales de Miedo" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col="#69b3a2")

#Episodios_por_semana_ansiedad
tablaf_ansiedad=table(datos_estudiantes$Episodios_por_semana_ansiedad)
tablaf_ansiedad
prop.table(tablaf_ansiedad)
diagrama_sexo = barplot(tablaf_ansiedad , main="Diagrama Episodios Semanales de ansiedad" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col="#2ECC71")
#cuantitativas