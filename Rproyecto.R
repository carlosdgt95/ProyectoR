
#"C:/Users/Sebastian/Documents/Proyecto/Estadistica/ProyectoR/datos_estudiantes.xlsx" - Joan

library(readxl)
library(agricolae)
datos_estudiantes <- read_excel("C:/Users/Sebastian/Documents/Proyecto/Estadistica/ProyectoR/datos_estudiantes.xlsx")
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
diagrama_miedo = barplot(tablaf_miedo, main="Diagrama Episodios Semanales de Miedo" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5"),col="#69b3a2")

#Episodios_por_semana_ansiedad
tablaf_ansiedad=table(datos_estudiantes$Episodios_por_semana_ansiedad)
tablaf_ansiedad
prop.table(tablaf_ansiedad)
diagrama_ansiedad = barplot(tablaf_ansiedad , main="Diagrama Episodios Semanales de ansiedad" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col="#2ECC71")


############## cuantitativas ##############

##############################################################
#Peso
##############################################################

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

#media del peso
media=mean(is.na(peso))

#mediana del peso
mediana=median(is.na(peso))

#desviacion del peso
desviacion=sd(is.na(peso))

#Cuartiles de la Altura
q1=quantile(peso,c(0.25),na.rm=TRUE)[1]
q2=quantile(peso,c(0.50),na.rm=TRUE)[1]
q3=quantile(peso,c(0.75),na.rm=TRUE)[1]

#Kurtosis del peso
curtosis=kurtosis(peso)

#Sesgo del peso
sesgo=skewness(peso)

medidas=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                 "Curtosis","Sesgo","Desviación Estándar")
medidas

#Histograma de Frecuencias del peso
hist(peso, breaks = seq(from=peso1[1],to=peso1[2],by=ancho), include.lowest = T, right = F,
     main="Histograma de frecuencias de Peso",ylab="Frecuencia absoluta",xlab="Peso")

#ojiva
# Ojiva Peso
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil = quantile(datos_estudiantes$Peso,probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil

plot(quartil,q, main = "Ojiva Peso", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil,q,type="l", col="brown")

#cajas de textos
boxplot(peso,main="Diagrama de Peso",ylab="Peso / Libras")



##############################################################
#Altura
##############################################################

#Sacamos la maxima y minima altura del conjunto de datos
altura = datos_estudiantes$Estatura
altura_rango = range(altura, na.rm = T)


amplitud_alturas = (altura_rango[2]- altura_rango[1])/7

tabla=  table.freq(hist(altura, breaks = seq(from=altura_rango[1], to=altura_rango[2], by=amplitud_alturas), include.lowest = T, right = F, plot = F))
names(tabla)= c("Límite inferior de clase","Límite superior de clase","Marca de clase",
                "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                "Frecuencia acumulada relativa en %")
tabla

#Media de la Altura
media=mean(altura, na.rm = T)

#Mediana de la Altura
mediana=median(altura, na.rm = T)

#Desviacion de la Altura
desviacion=sd(altura, na.rm = T)

#Cuartiles de la Altura
q1=quantile(altura,c(0.25), na.rm = T)[1]
q2=quantile(altura,c(0.50), na.rm = T)[1]
q3=quantile(altura,c(0.75), na.rm = T)[1]

#Kurtosis de la altura
curtosis=kurtosis(altura)

#Sesgo de la altura
sesgo=skewness(altura)

medidas=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                 "Curtosis","Sesgo","Desviación Estándar")
medidas

#Histograma de Frecuencias de la altura
hist(altura, breaks = seq(from=altura_rango[1],to=altura_rango[2],by=amplitud_alturas), include.lowest = T, right = F,
     main="Histograma de frecuencias de Altura",ylab="Frecuencia absoluta",xlab="Altura")


#ojiva
# Ojiva Altura
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_altura = quantile(altura, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_altura

plot(quartil_altura, q, main = "Ojiva Peso", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil_altura, q, type="l", col="brown")

#cajas de textos
boxplot(altura, main="Diagrama de Altura",ylab="Altura en metros")





##############################################################
#ICM
##############################################################

#Calculamos los ICM de cada estudiante
pesos_en_kg = datos_estudiantes$Peso/2.2
altura = datos_estudiantes$Estatura
icm = pesos_en_kg/(altura**2)
icm

#Sacamos el maximo valor de icm y el menor valor
icm_rango=range(icm, na.rm = T)


amplitud = (icm_rango[2]-icm_rango[1])/7 #amplitud de la clase
tabla=  table.freq(hist(icm, breaks = seq(from=icm_rango[1], to=icm_rango[2], by=amplitud), include.lowest = T, right = F, plot = F))
names(tabla)= c("Límite inferior de clase","Límite superior de clase","Marca de clase",
                "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                "Frecuencia acumulada relativa en %")
tabla

#Media del ICM
media=mean(icm, na.rm = T)

#Mediana del ICM
mediana=median(icm, na.rm = T)

#Desviacion del ICM
desviacion=sd(icm, na.rm = T)

#Cuartiles del ICM
q1=quantile(icm,c(0.25), na.rm = T)[1]
q2=quantile(icm,c(0.50), na.rm = T)[1]
q3=quantile(icm,c(0.75), na.rm = T)[1]

#Kurtosis
curtosis=kurtosis(icm)

#Sesgos
sesgo=skewness(icm)

#Tabla
medidas=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                 "Curtosis","Sesgo","Desviación Estándar")
medidas

#Histograma de Frecuencias
hist(icm, breaks = seq(from=icm_rango[1],to=icm_rango[2],by=amplitud), include.lowest = T, right = F,
     main="Histograma de frecuencias de ICM",ylab="Frecuencia absoluta",xlab="ICM")


#ojiva
#Ojiva ICM
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_icm = quantile(icm, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_icm

plot(quartil_icm, q, main = "Ojiva Peso", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil_icm, q, type="l", col="brown")

#cajas de textos
boxplot(icm, main="Diagrama de ICM",ylab="ICM")

