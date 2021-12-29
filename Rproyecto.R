
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
diagrama_estres = barplot(tablaf_estres, main=" Diagrama Episodios Semanales de Estres" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col=c( '#F9E79F', '#F4D03F', '#B7950B', '#7D6608' ))
#Episodios_por_semana_ira_frustacion

tablaf_ira=table(datos_estudiantes$Episodios_por_semana_ira_frustacion)
tablaf_ira
prop.table(tablaf_estres)
diagrama_ira = barplot(tablaf_ira, main="Diagrama Episodios Semanales de Ira & Frustracion" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col="#F9EF28")

#Episodios_por_semana_miedo
tablaf_miedo=table(datos_estudiantes$Episodios_por_semana_miedo)
tablaf_miedo
prop.table(tablaf_miedo)
diagrama_miedo = barplot(tablaf_miedo, main="Diagrama Episodios Semanales de Miedo" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col="#69b3a2")

#Episodios_por_semana_ansiedad
tablaf_ansiedad=table(datos_estudiantes$Episodios_por_semana_ansiedad)
tablaf_ansiedad
prop.table(tablaf_ansiedad)
diagrama_ansiedad = barplot(tablaf_ansiedad , main="Diagrama Episodios Semanales de ansiedad" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col="#2ECC71")
#cuantitativas

#peso
#frecuencia
peso=datos_estudiantes$Peso
peso1=range(peso,na.rm=TRUE)
amplitud=(peso1[2]-peso1[1])
amplitud
k=7
ancho=amplitud/k

tabla_peso=table.freq(hist(peso, breaks = seq(from=peso1[1],to=peso1[2],by=ancho), include.lowest = T, right = F, plot = F))
names(tabla_peso)=c("Límite inferior de clase","Límite superior de clase","Marca de clase",
                    "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                    "Frecuencia acumulada relativa en %")
tabla_peso

#medias estadistica
media=mean(is.na(peso))

mediana=median(is.na(peso))

desviacion=sd(is.na(peso))

q1=quantile(peso,c(0.25),na.rm=TRUE)[1]
q2=quantile(peso,c(0.50),na.rm=TRUE)[1]
q3=quantile(peso,c(0.75),na.rm=TRUE)[1]
curtosis=kurtosis(peso)
sesgo=skewness(peso)
medidas=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                 "Curtosis","Sesgo","Desviación Estándar")
medidas

#desviacion
hist(peso, breaks = seq(from=peso1[1],to=peso1[2],by=ancho), include.lowest = T, right = F,
     main="Histograma de frecuencias de Edad",ylab="Frecuencia absoluta",xlab="Peso")
#ojiva
# Ojiva Peso
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil = quantile(datos_estudiantes$Peso,probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil

plot(quartil,q, main = "Ojiva Peso", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil,q,type="l", col="brown")

#cajas de textos
boxplot(peso,main="Diagrama de Peso",ylab="Peso / Libras")