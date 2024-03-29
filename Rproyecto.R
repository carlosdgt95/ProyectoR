
#"C:/Users/Sebastian/Documents/Proyecto/Estadistica/ProyectoR/datos_estudiantes.xlsx" - Joan
#"C:/Users/Carlos Gomez/Desktop/proyecto2githunestaditica/ProyectoR/datos_estudiantes.xlsx"

library(readxl)
library(agricolae)
datos_estudiantes <- read_excel("C:/Users/Sebastian/Documents/Proyecto/Estadistica/ProyectoR/datos_estudiantes.xlsx")
datos_estudiantes

#cualitativas tablas

##############################
#Sexo
##############################

#Tabla de frecuencia
tablaf_sexo=table(datos_estudiantes$Sexo)
tablaf_sexo

#Tabla de frecuencia relativa
tabla_freq_relativa_sexo = prop.table(tablaf_sexo)
tabla_freq_relativa_sexo

#Colores
colores_diag_sexo = c('pink', 'lightblue')

#Diagrama de barras
diagrama_barras_sexo = barplot(tablaf_sexo, main="Diagrama Sexo" , xlab="sexo",names.arg = c("F", "M"),col=colores_diag_sexo)

#Diagrama circular de Sexo
etiqueta_diag_sexo = paste(round(tabla_freq_relativa_sexo*100, 2), '%', sep=' ')
diagrama_circular_sexo = pie(tablaf_sexo, labels = etiqueta_diag_sexo, col = colores_diag_sexo, main = 'Diagrama de Sexo')
legend('topleft', title = 'Sexo', c('Mujeres', 'Hombres'), cex=0.5, fill = colores_diag_sexo)


#################################
#Episodios_por_semana_estres
#################################

#Tabla de frecuencia
tablaf_estres=table(datos_estudiantes$Episodios_por_semana_estres)
tablaf_estres

#Tabla de frecuencia relativa
tabla_freq_relativa_estres = prop.table(tablaf_estres)
tabla_freq_relativa_estres

#Diagrama de barras de Estres

colores_diag_estres = c( '#F9E79F', '#F4D03F', '#B7950B', '#7D6608' )
diagrama_estres = barplot(tablaf_estres, main=" Diagrama Episodios Semanales de Estres" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col=colores_diag_estres)

#Diagrama circular de Estres
etiqueta_diag_estres = paste(round(tabla_freq_relativa_estres*100, 2), '%', sep=' ')
diagrama_circular_estres = pie(tablaf_estres, labels = etiqueta_diag_estres, col = colores_diag_estres, main = 'Diagrama de Estres')
legend('topleft', title = 'Episodios de Estres', c("0-1","2-3","4-5","5+"), cex=0.5, fill = colores_diag_estres)


#######################################
#Episodios_por_semana_ira_frustacion
#######################################

tablaf_ira=table(datos_estudiantes$Episodios_por_semana_ira_frustacion)
tablaf_ira

tabla_freq_relativa_ira = prop.table(tablaf_ira)
tabla_freq_relativa_ira

#Diagrama de barras de Ira
colores_diag_ira = c('#F0E68C', '#FFF68F', '#EEE685', '#CDC673')
diagrama_ira = barplot(tablaf_ira, main="Diagrama Episodios Semanales de Ira & Frustracion" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col=colores_diag_ira)

#Diagrama circular de Ira
etiqueta_diag_ira = paste(round(tabla_freq_relativa_ira*100, 2), '%', sep=' ')
diagrama_circular_estres = pie(tablaf_estres, labels = etiqueta_diag_ira, col = colores_diag_ira, main = "Diagrama Episodios Semanales de Ira & Frustracion")
legend('topleft', title = 'Episodios de Ira y Frustracion', c("0-1","2-3","4-5","5+"), cex=0.4, fill = colores_diag_ira)


##############################
#Episodios_por_semana_miedo
##############################

tablaf_miedo=table(datos_estudiantes$Episodios_por_semana_miedo)
tablaf_miedo

tabla_freq_relativa_miedo = prop.table(tablaf_miedo)
tabla_freq_relativa_miedo

colores_diag_miedo = c('#1bccb1', '#76e0d0', '#d1f5ef')
diagrama_miedo = barplot(tablaf_miedo, main="Diagrama Episodios Semanales de Miedo" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5"),col=colores_diag_miedo)

#Diagrama circular de Episodios de Miedo
etiqueta_diag_miedo = paste(round(tabla_freq_relativa_miedo*100, 2), '%', sep=' ')
diagrama_circular_estres = pie(tablaf_miedo, labels = etiqueta_diag_ira, col = colores_diag_miedo, main= 'Diagrama Episodios Semanales de Miedo')
legend('bottomleft', title = 'Episodios de Miedo', c("0-1","2-3","4-5"), cex=0.5, fill = colores_diag_miedo)


#################################
#Episodios_por_semana_ansiedad
#################################

tablaf_ansiedad=table(datos_estudiantes$Episodios_por_semana_ansiedad)
tablaf_ansiedad
tabla_freq_relativa_ansiedad = prop.table(tablaf_ansiedad)
tabla_freq_relativa_ansiedad

colores_diag_ansiedad = c('#2ecc71', '#62d995', '#97e6b8', '#cbf2dc')
diagrama_ansiedad = barplot(tablaf_ansiedad , main="Diagrama Episodios Semanales de ansiedad" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col=colores_diag_ansiedad)

#Diagrama circular de Episodios de Ansiedad
etiqueta_diag_ansiedad = paste(round(tabla_freq_relativa_ansiedad*100, 2), '%', sep=' ')
diagrama_circular_estres = pie(tablaf_ansiedad, labels = etiqueta_diag_ansiedad, col = colores_diag_ansiedad, main= 'Diagrama Episodios Semanales de ansiedad')
legend('topleft', title = 'Episodios de Ansiedad', c("0-1","2-3","4-5","5+"), cex=0.4, fill = colores_diag_ansiedad)

#################################
#Episodios_por_semana_tristeza
#################################

#Tabla de frecuencia
tablaf_tristeza=table(datos_estudiantes$Episodios_por_semana_tristeza)
tablaf_tristeza

#Tabla de frecuencia relativa
tabla_freq_relativa_tristeza = prop.table(tablaf_tristeza)
tabla_freq_relativa_tristeza

#Diagrama de barras de Tristeza

colores_diag_tristeza = palette("Alphabet")
diagrama_tristeza = barplot(tablaf_tristeza, main=" Diagrama Episodios Semanales de Tristeza" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col=colores_diag_tristeza)

#Diagrama circular de Tristeza
etiqueta_diag_triteza = paste(round(tabla_freq_relativa_tristeza*100, 2), '%', sep=' ')
diagrama_circular_tristeza = pie(tablaf_tristeza, labels = etiqueta_diag_triteza, col = colores_diag_tristeza, main = 'Diagrama de Tristeza')
legend('topleft', title = 'Episodios de Tristeza', c("0-1","2-3","4-5","5+"), cex=0.5, fill = colores_diag_tristeza)



############## cuantitativas ##############

#Calculamos el numero de intervalos para la tabla de frecuencias
#Mediante la regla de Sturges

k = round(1 + log(nrow(datos_estudiantes), 2))
k

##############################################################
#Peso
##############################################################

#frecuencia
peso=datos_estudiantes$Peso
rango_peso=range(peso,na.rm=TRUE)
amplitud_peso=(rango_peso[2]-rango_peso[1])/k
amplitud_peso

tabla_peso=table.freq(hist(peso, breaks = seq(from=rango_peso[1],to=rango_peso[2],by=amplitud_peso), include.lowest = T, right = F, plot = F))
names(tabla_peso)=c("L�mite inferior de clase","L�mite superior de clase","Marca de clase",
                    "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                    "Frecuencia acumulada relativa en %")
tabla_peso

#media del peso
media=mean(peso, na.rm = T)

#mediana del peso
mediana=median(peso, na.rm = T)

#desviacion del peso
desviacion=sd(peso, na.rm = T)

#Cuartiles del peso
q1=quantile(peso,c(0.25),na.rm=TRUE)[1]
q2=quantile(peso,c(0.50),na.rm=TRUE)[1]
q3=quantile(peso,c(0.75),na.rm=TRUE)[1]

#Kurtosis del peso
curtosis=kurtosis(peso)

#Sesgo del peso
sesgo=skewness(peso)

medidas_peso=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas_peso)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                      "Curtosis","Sesgo","Desviaci�n Est�ndar")
medidas_peso

#Histograma de Frecuencias del peso
hist(peso, breaks = seq(from=rango_peso[1],to=rango_peso[2],by=amplitud_peso), include.lowest = T, right = F,
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
#Estatura
##############################################################

#Sacamos la maxima y minima altura del conjunto de datos
estatura = datos_estudiantes$Estatura
estatura_rango = range(estatura, na.rm = T)

amplitud_estaturas = (estatura_rango[2]- estatura_rango[1])/k

tabla_estatura=  table.freq(hist(estatura, breaks = seq(from=estatura_rango[1], to=estatura_rango[2], by=amplitud_estaturas), include.lowest = T, right = F, plot = F))
names(tabla_estatura)= c("L�mite inferior de clase","L�mite superior de clase","Marca de clase",
                         "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                         "Frecuencia acumulada relativa en %")
tabla_estatura

#Media de la estatura
media=mean(estatura, na.rm = T)

#Mediana de la estatura
mediana=median(estatura, na.rm = T)

#Desviacion de la estatura
desviacion=sd(estatura, na.rm = T)

#Cuartiles de la estatura
q1=quantile(estatura,c(0.25), na.rm = T)[1]
q2=quantile(estatura,c(0.50), na.rm = T)[1]
q3=quantile(estatura,c(0.75), na.rm = T)[1]

#Kurtosis de la estatura
curtosis=kurtosis(estatura)

#Sesgo de la estatura
sesgo=skewness(estatura)

medidas_estatura=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas_estatura)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                          "Curtosis","Sesgo","Desviaci�n Est�ndar")
medidas_estatura

#Histograma de Frecuencias de la estatura
hist(estatura, breaks = seq(from=estatura_rango[1],to=estatura_rango[2],by=amplitud_estaturas), include.lowest = T, right = F,
     main="Histograma de frecuencias de Estatura",ylab="Frecuencia absoluta",xlab="Estatura")


#ojiva
# Ojiva estatura
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_estatura = quantile(estatura, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_estatura

plot(quartil_estatura, q, main = "Ojiva Peso", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil_estatura, q, type="l", col="brown")

#cajas de textos
boxplot(estatura, main="Diagrama de Estatura",ylab="Estatura en metros")

##############################################################
#IMC
##############################################################

#Calculamos los IMC de cada estudiante
pesos_en_kg = datos_estudiantes$Peso/2.2
estatura = datos_estudiantes$Estatura
IMC = pesos_en_kg/(estatura**2)
IMC

#Sacamos el maximo valor de IMC y el menor valor
IMC_rango=range(IMC, na.rm = T)


amplitud_IMC = (IMC_rango[2]-IMC_rango[1])/k #amplitud de la clase
tabla_IMC=  table.freq(hist(IMC, breaks = seq(from=IMC_rango[1], to=IMC_rango[2], by=amplitud_IMC), include.lowest = T, right = F, plot = F))
names(tabla_IMC)= c("L�mite inferior de clase","L�mite superior de clase","Marca de clase",
                    "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                    "Frecuencia acumulada relativa en %")
tabla_IMC

#Media del IMC
media=mean(IMC, na.rm = T)

#Mediana del IMC
mediana=median(IMC, na.rm = T)

#Desviacion del IMC
desviacion=sd(IMC, na.rm = T)

#Cuartiles del IMC
q1=quantile(IMC,c(0.25), na.rm = T)[1]
q2=quantile(IMC,c(0.50), na.rm = T)[1]
q3=quantile(IMC,c(0.75), na.rm = T)[1]

#Kurtosis
curtosis=kurtosis(IMC)

#Sesgos
sesgo=skewness(IMC)

#Tabla
medidas_IMC=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas_IMC)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                     "Curtosis","Sesgo","Desviaci�n Est�ndar")
medidas_IMC

#Histograma de Frecuencias
hist(IMC, breaks = seq(from=IMC_rango[1],to=IMC_rango[2],by=amplitud_IMC), include.lowest = T, right = F,
     main="Histograma de frecuencias de IMC",ylab="Frecuencia absoluta",xlab="IMC")


#ojiva
#Ojiva IMC
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_IMC = quantile(IMC, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_IMC

plot(quartil_IMC, q, main = "Ojiva ICM", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil_IMC, q, type="l", col="brown")

#cajas de textos
boxplot(IMC, main="Diagrama de IMC",ylab="IMC")


##############################################################
#Consumo_semanal_promedio_gaseosas
##############################################################


#Sacamos la maxima y minima del conjunto de datos
prom_gaseosa = datos_estudiantes$Consumo_semanal_promedio_gaseosas
prom_gaseosa_rango = range(prom_gaseosa, na.rm = T)


amplitud_gaseosa_rango = (prom_gaseosa_rango[2]- prom_gaseosa_rango[1])/k

tabla=  table.freq(hist(prom_gaseosa, breaks = seq(from=prom_gaseosa_rango[1], to=prom_gaseosa_rango[2], by=amplitud_gaseosa_rango), include.lowest = T, right = F, plot = F))
names(tabla)= c("L�mite inferior de clase","L�mite superior de clase","Marca de clase",
                "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                "Frecuencia acumulada relativa en %")
tabla

#Media del Consumo_semanal_promedio_gaseosas
media=mean(prom_gaseosa, na.rm = T)

#Mediana del Consumo_semanal_promedio_gaseosas
mediana=median(prom_gaseosa, na.rm = T)

#Desviacion del Consumo_semanal_promedio_gaseosas
desviacion=sd(prom_gaseosa, na.rm = T)

#Cuartiles del Consumo_semanal_promedio_gaseosas
q1=quantile(prom_gaseosa,c(0.25), na.rm = T)[1]
q2=quantile(prom_gaseosa,c(0.50), na.rm = T)[1]
q3=quantile(prom_gaseosa,c(0.75), na.rm = T)[1]

#Kurtosis del Consumo_semanal_promedio_gaseosas
curtosis=kurtosis(prom_gaseosa)

#Sesgo del Consumo_semanal_promedio_gaseosas
sesgo=skewness(prom_gaseosa)

medidas=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                 "Curtosis","Sesgo","Desviaci�n Est�ndar")
medidas

#Histograma de Frecuencias del Consumo_semanal_promedio_gaseosas
hist(prom_gaseosa, breaks = seq(from=prom_gaseosa_rango[1],to=prom_gaseosa_rango[2],by=amplitud_gaseosa_rango), include.lowest = T, right = F,
     main="Histograma de frecuencias del Promedio de consumo semanal de gaseosa",ylab="Frecuencia absoluta",xlab="Promedio consumo semanal")


#ojiva
# Ojiva del Consumo_semanal_promedio_gaseosas
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_prom_gaseosa = quantile(prom_gaseosa, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_prom_gaseosa

plot(quartil_prom_gaseosa, q, main = "Ojiva del Promedio de consumo semanal de gaseosa", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil_prom_gaseosa, q, type="l", col="brown")

#cajas de textos
boxplot(prom_gaseosa, main="Diagrama del Promedio de consumo semanal de gaseosa",ylab="Promedio consumo semanal")

##############################################################
#Consumo_promedio_semanal_agua
##############################################################


#Sacamos la maxima y minima del conjunto de datos
prom_sem_ag = datos_estudiantes$Consumo_promedio_semanal_agua
prom_sem_ag_rango = range(prom_sem_ag, na.rm = T)


amplitud_prom_sem_ag = (prom_sem_ag_rango[2]- prom_sem_ag_rango[1])/k

tabla=  table.freq(hist(prom_sem_ag, breaks = seq(prom_sem_ag_rango[1], to=prom_sem_ag_rango[2], by=amplitud_prom_sem_ag), include.lowest = T, right = F, plot = F))
names(tabla)= c("L�mite inferior de clase","L�mite superior de clase","Marca de clase",
                "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                "Frecuencia acumulada relativa en %")
tabla

#Media de la Consumo_promedio_semanal_agua
media=mean(prom_sem_ag, na.rm = T)

#Mediana de la Consumo_promedio_semanal_agua
mediana=median(prom_sem_ag, na.rm = T)

#Desviacion de la Consumo_promedio_semanal_agua
desviacion=sd(prom_sem_ag, na.rm = T)

#Cuartiles de la Consumo_promedio_semanal_agua
q1=quantile(prom_sem_ag,c(0.25), na.rm = T)[1]
q2=quantile(prom_sem_ag,c(0.50), na.rm = T)[1]
q3=quantile(prom_sem_ag,c(0.75), na.rm = T)[1]

#Kurtosis de la Consumo_promedio_semanal_agua
curtosis=kurtosis(prom_sem_ag)

#Sesgo de la Consumo_promedio_semanal_agua
sesgo=skewness(prom_sem_ag)

medidas=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                 "Curtosis","Sesgo","Desviaci�n Est�ndar")
medidas

#Histograma de la Consumo_promedio_semanal_agua
hist(prom_sem_ag, breaks = seq(from=prom_sem_ag_rango[1],to=prom_sem_ag_rango[2],by=amplitud_prom_sem_ag), include.lowest = T, right = F,
     main="Histograma de Promedio de Consumo de Agua Semanalmente",ylab="Frecuencia absoluta",xlab="Promedio de Consumo de Agua Semanalmente")


#ojiva
# Ojiva de la Consumo_promedio_semanal_agua
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_prom_sem_ag = quantile(prom_sem_ag, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_prom_sem_ag

plot(quartil_prom_sem_ag, q, main = "Ojiva del Promedio de Consumo de Agua Semanalmente", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil_prom_sem_ag, q, type="l", col="brown")

#cajas de textos
boxplot(prom_sem_ag, main="Diagrama de Promedio de Consumo de Agua Semanalmente",ylab="Promedio de Agua Consumida en Litros")

##############################################################
#Consumo_promedio_carbohidrato_por_comida
##############################################################


#Sacamos la maxima y minima del conjunto de datos
prom_carbh_com = datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida
prom_carbh_com_rango = range(prom_carbh_com, na.rm = T)


amplitud_prom_carbh_com = (prom_carbh_com_rango[2]- prom_carbh_com_rango[1])/k

tabla=  table.freq(hist(prom_carbh_com, breaks = seq(prom_carbh_com_rango[1], to=prom_carbh_com_rango[2], by=amplitud_prom_carbh_com), include.lowest = T, right = F, plot = F))
names(tabla)= c("L�mite inferior de clase","L�mite superior de clase","Marca de clase",
                "Frecuencia absoluta","Frecuencia relativa absoluta en %","Frecuencia acumulada",
                "Frecuencia acumulada relativa en %")
tabla

#Media de la Porcentaje_promedio_carbohidrato_por_comida
media=mean(prom_carbh_com, na.rm = T)

#Mediana de la Porcentaje_promedio_carbohidrato_por_comida
mediana=median(prom_carbh_com, na.rm = T)

#Desviacion de la Porcentaje_promedio_carbohidrato_por_comida
desviacion=sd(prom_carbh_com, na.rm = T)

#Cuartiles de la Porcentaje_promedio_carbohidrato_por_comida
q1=quantile(prom_carbh_com,c(0.25), na.rm = T)[1]
q2=quantile(prom_carbh_com,c(0.50), na.rm = T)[1]
q3=quantile(prom_carbh_com,c(0.75), na.rm = T)[1]

#Kurtosis de la Porcentaje_promedio_carbohidrato_por_comida
curtosis=kurtosis(prom_carbh_com)

#Sesgo de la Porcentaje_promedio_carbohidrato_por_comida
sesgo=skewness(prom_carbh_com)

medidas=data.frame(media,mediana,q1,q2,q3,curtosis,sesgo,desviacion,row.names = c("Medidas"))
names(medidas)=c("Media","Mediana","Cuartil 1","Cuartil 2","Cuartil 3",
                 "Curtosis","Sesgo","Desviaci�n Est�ndar")
medidas

#Histograma de la Porcentaje_promedio_carbohidrato_por_comida
hist(prom_carbh_com, breaks = seq(from=prom_carbh_com_rango[1],to=prom_carbh_com_rango[2],by=amplitud_prom_carbh_com), include.lowest = T, right = F,
     main="Histograma de Promedio de Carbohidrato por Comida",ylab="Frecuencia absoluta",xlab="Porcentaje de Carbohidratos")


#ojiva
# Ojiva de la Porcentaje_promedio_carbohidrato_por_comida
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_prom_carbh_com = quantile(prom_carbh_com, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_prom_carbh_com

plot(quartil_prom_carbh_com, q, main = "Ojiva del porcentaje de carbohidratos por Comida", xlab = "Puntaje", ylab = "Frecuencia Relativa")
lines(quartil_prom_carbh_com, q, type="l", col="brown")

#cajas de textos
boxplot(prom_carbh_com, main="Diagrama de Promedio de Consumo de Carbohidratos por Comida",ylab="
        Porcentaje de Carbohidrato")





##############################################################
#agregar icm
imc = (datos_estudiantes$Peso/2.2)/(datos_estudiantes$Estatura**2)
imc
datos_estudiantes$IMC = imc


###############################################################################
# Diagramas de cajas Vs variable cualitativa
###############################################################################


par(mfrow = c(1,2))

#Agua por Ansiedad
boxplot(datos_estudiantes$Consumo_promedio_semanal_agua~
          datos_estudiantes$Episodios_por_semana_ansiedad,
        ylab = 'Consumo de Agua', xlab = 'Episodios de Ansiedad', 
        names= c("0-1","2-3","4-5","5+"),
        main='Cons. de Agua por Ep. de Ansiedad'
)

#Gaseosa por Ansiedad
boxplot(datos_estudiantes$Consumo_semanal_promedio_gaseosas~
          datos_estudiantes$Episodios_por_semana_ansiedad,
        ylab = 'Consumo de Gaseosa', xlab = 'Episodios de Ansiedad', 
        names= c("0-1","2-3","4-5","5+"),
        main='Cons. de gaseosa por Ep. de Ansiedad'
)


par(mfrow = c(1,2))

#Carbohidratos por Ira
boxplot(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida~datos_estudiantes$Episodios_por_semana_ira_frustacion, 
        ylab = 'Consumo Prom. Carbohid', xlab = 'Episodios de Ira',
        names = c("0-1","2-3","4-5","5+"),
        main='Porc. Prom Carbohid por Ep. de Ira')


#IMC por Tristeza
boxplot(datos_estudiantes$IMC~datos_estudiantes$Episodios_por_semana_tristeza, 
        ylab = 'IMC', xlab = 'Episodios de Tristeza', 
        names= c("0-1","2-3","4-5","5+"),
        main='IMC por Ep. de Tristeza')


par(mfrow = c(1,2))

#Agua por Estres
boxplot(datos_estudiantes$Consumo_promedio_semanal_agua~
          datos_estudiantes$Episodios_por_semana_estres,
        ylab = 'Consumo de Agua', xlab = 'Episodios de Estres', 
        names= c("0-1","2-3","4-5","5+"),
        main='Cons. de Agua por Ep. de estres'
)


#Carbohidratos por Miedo
boxplot(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida~
          datos_estudiantes$Episodios_por_semana_miedo,
        ylab = 'Consumo de Carbohid', xlab = 'Episodios de Miedo', 
        names=c("0-1","2-3","4-5"),
        main='Cons. de Carbohid por Ep. de Miedo'
)


##############################################################
#Matriz de correlacion y covarianza
##############################################################

imc = (datos_estudiantes$Peso/2.2)/(datos_estudiantes$Estatura**2)
imc

datos_estudiantes$IMC = imc

datos_cuantitativos = datos_estudiantes[,c(5,6,16,18,20,38)]

names(datos_cuantitativos) = c('Peso', 'Est', 'Prom.Gaseos', 
                               'Prom.Agua', 'Prom.Carb.Comida', 'IMC')

matriz_cov = round(cov(na.omit(datos_cuantitativos)),2)
matriz_cov

matriz_corr = round(cor(na.omit(datos_cuantitativos)), 2)
matriz_corr

library(corrplot)

#type="upper"
corrplot(matriz_corr, method="circle", tl.srt=45)


##############################################################
#Intervalos de Confianza

intervaloConfianza <- function (muestra, nivelConfianza, z = T) {
  estadistico = 0
  media = mean(muestra, na.rm = T)
  significancia = (1 - (nivelConfianza/100))/2
  errorEstandar = sd(muestra, na.rm = T) / sqrt(length(muestra))
  
  if(z) {
    estadistico = qnorm(significancia, lower.tail = F) } 
  else {
    estadistico = qt(significancia, lower.tail = F, length(muestra) - 1) }
  
  c(media - (estadistico * errorEstandar), media + (estadistico * errorEstandar))
}

# IC Consumo gaseosas

set.seed(1)
muestra_gaseosas = sample(datos_estudiantes$Consumo_semanal_promedio_gaseosas, 
                          size=30, replace = F)

media_gaseosas = mean(muestra_gaseosas, na.rm = T)
media_gaseosas

int_confianza_gaseosas = intervaloConfianza(muestra_gaseosas, 95, T)
int_confianza_gaseosas


# IC Consumo Agua

set.seed(1)
muestra_agua = sample(datos_estudiantes$Consumo_promedio_semanal_agua, 
                      size=30, replace = F)

media_agua = mean(muestra_agua, na.rm = T)
media_agua

int_confianza_agua = intervaloConfianza(muestra_agua, 95, T)
int_confianza_agua

# IC Consumo Carbohidratos

set.seed(1)
muestra_carb = sample(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida, 
                      size=30, replace = F)

media_carb = mean(muestra_carb, na.rm = T)
media_carb

int_confianza_carb = intervaloConfianza(muestra_carb, 95, T)
int_confianza_carb


# IC IMC
imc = (datos_estudiantes$Peso/2.2)/(datos_estudiantes$Estatura**2)
datos_estudiantes$IMC = imc

set.seed(1)
muestra_imc = sample(datos_estudiantes$IMC, 
                     size=30, replace = F)

media_imc = mean(muestra_imc, na.rm = T)
media_imc

int_confianza_imc = intervaloConfianza(muestra_imc, 95, T)
int_confianza_imc

##############################################################
##############################################################
#Prueba de Hipotesis para la diferencia de medias de muestras peque�as

tprueba2=function(me1,me2, difH0,var1,var2,n1, n2,tipo){
  varc=((n1-1)*var1+(n2-1)*var2)/(n1+n2-2)
  z=((me1-me2)-difH0)/sqrt(varc*(1/n1+1/n2))
  if(tipo=="mayor"){
    p=pt(z,n1+n2-2,lower.tail=FALSE)
  }
  if(tipo=="menor"){
    p=pt(z,n1+n2-2,lower.tail=TRUE)
  }
  if(tipo=="diferente"){
    p=min((2*pt(z,n1+n2-2,lower.tail=FALSE)),(2*pt(z,n-1,lower.tail=TRUE)))
  }
  return(p)
}

##############################################################
#Prueba de Hipotesis

################################################################################
#Agua con Ansiedad
#A 95% nivel de confianza
#Diferencia de prom de consumo agua

set.seed(3)
ep_ansiedad = datos_estudiantes$Episodios_por_semana_ansiedad

#Muestra personas con 0-1 ep. ansiedad
muestra_agua1 = sample(datos_estudiantes$Consumo_promedio_semanal_agua
                       [ep_ansiedad==1], size = 20, replace = F)

media_muestra_agua1 = median(muestra_agua1, na.rm = T)
var_muestra1 = var(muestra_agua1, na.rm = T)
n_muestra1 = length(muestra_agua1)

#Muestra personas con mas de 2 ep. ansiedad
muestra_agua2 = sample(datos_estudiantes$Consumo_promedio_semanal_agua
                       [ep_ansiedad!=1], size = 20, replace = F)

media_muestra_agua2 = median(muestra_agua2, na.rm = T)
var_muestra2 = var(muestra_agua2, na.rm = T)
n_muestra2 = length(muestra_agua1)

valor_p = tprueba2(media_muestra_agua1, media_muestra_agua2, 
                   0, var_muestra1, var_muestra2, n_muestra1, n_muestra2, 'mayor')
valor_p


################################################################################
#Gaseosa con Ansiedad
#A 95% nivel de confianza
#Diferencia de prom de consumo gaseosas
set.seed(3)
ep_ansiedad = datos_estudiantes$Episodios_por_semana_ansiedad

#Muestra personas con 0-3 ep. ansiedad
muestra_gaseosa1 = sample(datos_estudiantes$Consumo_semanal_promedio_gaseosas
                          [ep_ansiedad<=2], size = 20, replace = F)

media_muest_gaseosa1 = median(muestra_gaseosa1, na.rm = T)
var_muestra1 = var(muestra_gaseosa1, na.rm = T)
n_muestra1 = length(muestra_gaseosa1)

#Muestra personas con mas de 4 ep. ansiedad
muestra_gaseosa2 = sample(datos_estudiantes$Consumo_semanal_promedio_gaseosas
                          [ep_ansiedad>2], size = 20, replace = F)

media_muest_gaseosa2 = median(muestra_gaseosa2, na.rm = T)
var_muestra2 = var(muestra_gaseosa2, na.rm = T)
n_muestra2 = length(muestra_gaseosa2)

valor_p = tprueba2(media_muest_gaseosa1, media_muest_gaseosa2, 
                   0, var_muestra1, var_muestra2, n_muestra1, n_muestra2, 'menor')
valor_p


################################################################################
#Carbohidratos por Ira
#A 95% nivel de confianza
#Diferencia de prom de consumo carbohid
set.seed(3)
ep_ira = datos_estudiantes$Episodios_por_semana_ira_frustacion

#Muestra personas con 0-1 ep. ira
muestra_ira1 = sample(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida
                      [ep_ira==1], size = 20, replace = F)

media_muest_ira1 = median(muestra_ira1, na.rm = T)
var_muestra1 = var(muestra_ira1, na.rm = T)
n_muestra1 = length(muestra_ira1)

#Muestra personas con mas de 2 ep. de ira
muestra_ira2 = sample(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida
                      [ep_ira>1], size = 20, replace = F)

media_muest_ira2 = median(muestra_ira2, na.rm = T)
var_muestra2 = var(muestra_ira2, na.rm = T)
n_muestra2 = length(muestra_ira2)

valor_p = tprueba2(media_muest_ira1, media_muest_ira2, 
                   0, var_muestra1, var_muestra2, n_muestra1, n_muestra2, 'menor')
valor_p

################################################################################
#IMC por tristeza
#A 95% nivel de confianza

imc = (datos_estudiantes$Peso/2.2)/(datos_estudiantes$Estatura**2)
datos_estudiantes$IMC = imc


#Diferencia de IMC
set.seed(3)
ep_tristeza = datos_estudiantes$Episodios_por_semana_tristeza

#Muestra personas con mas de 2 ep tristeza
muestra_tristeza1 = sample(datos_estudiantes$IMC
                           [ep_tristeza>=2], size = 20, replace = F)

media_muest_tristeza1 = median(muestra_tristeza1, na.rm = T)
var_muestra1 = var(muestra_tristeza1, na.rm = T)
n_muestra1 = length(muestra_tristeza1)

#Muestra personas con 0-1 ep tristeza
muestra_tristeza2 = sample(datos_estudiantes$IMC
                           [ep_tristeza<2], size = 20, replace = F)

media_muest_tristeza2 = median(muestra_tristeza2, na.rm = T)
var_muestra2 = var(muestra_tristeza2, na.rm = T)
n_muestra2 = length(muestra_tristeza2)

valor_p = tprueba2(media_muest_tristeza1, media_muest_tristeza2, 
                   0, var_muestra1, var_muestra2, n_muestra1, n_muestra2, 'mayor')
valor_p


################################################################################
#Agua por Estres
#A 95% nivel de confianza

#Diferencia de agua
set.seed(3)
ep_estres = datos_estudiantes$Episodios_por_semana_estres

#Muestra personas con 0-1 ep estres
muestra_estres1 = sample(datos_estudiantes$Consumo_promedio_semanal_agua
                         [ep_estres==1], size = 20, replace = F)

media_muest_estres1 = median(muestra_estres1, na.rm = T)
var_muestra1 = var(muestra_estres1, na.rm = T)
n_muestra1 = length(muestra_estres1)

#Muestra personas con mas de 2 ep estres
muestra_estres2 = sample(datos_estudiantes$Consumo_promedio_semanal_agua
                         [ep_estres!=1], size = 20, replace = F)

media_muest_estres2 = median(muestra_estres2, na.rm = T)
var_muestra2 = var(muestra_estres2, na.rm = T)
n_muestra2 = length(muestra_estres2)

valor_p = tprueba2(media_muest_estres1, media_muest_estres2, 
                   0, var_muestra1, var_muestra2, n_muestra1, n_muestra2, 'mayor')
valor_p


################################################################################
#Carbohidratos por Miedo
#A 95% nivel de confianza

#Diferencia de carbohidratos
set.seed(3)
ep_miedo = datos_estudiantes$Episodios_por_semana_miedo

#Muestra personas con 0-1 ep miedo
muestra_miedo1 = sample(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida
                        [ep_miedo==1], size = 20, replace = F)

media_muest_miedo1 = median(muestra_miedo1, na.rm = T)
var_muestra1 = var(muestra_miedo1, na.rm = T)
n_muestra1 = length(muestra_miedo1)

#Muestra personas con mas de 2 ep miedo
muestra_miedo2 = sample(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida
                        [ep_miedo!=1], size = 20, replace = F)

media_muest_miedo2 = median(muestra_miedo2, na.rm = T)
var_muestra2 = var(muestra_miedo2, na.rm = T)
n_muestra2 = length(muestra_miedo2)

valor_p = tprueba2(media_muest_miedo1, media_muest_miedo2, 
                   0, var_muestra1, var_muestra2, n_muestra1, n_muestra2, 'menor')
valor_p




##############################################################
##############################################################
#grafico de dispercion

IMC = datos_estudiantes$IMC

pairs(~ datos_estudiantes$Peso + 
        datos_estudiantes$Estatura +
        datos_estudiantes$Consumo_promedio_semanal_agua +
        datos_estudiantes$Consumo_semanal_promedio_gaseosas +
        datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida +
        IMC, data = datos_estudiantes)
#pesovsicm #Estecumple

regresion <- lm(IMC ~ datos_estudiantes$Peso, data.frame(IMC) )
summary(regresion)

plot(datos_estudiantes$Peso, IMC, xlab='Peso', ylab='IMC')
abline(regresion)
#PEsovsestatura# este cumple

regresion1 <- lm( datos_estudiantes$Peso ~ datos_estudiantes$Estatura 
                  , data= datos_estudiantes )
summary(regresion1)


plot( datos_estudiantes$Estatura, datos_estudiantes$Peso, 
      xlab='Estatura', ylab='Peso')
abline(regresion1)

#icmvsesatura #no cumple
IMC

regresion <- lm(datos_estudiantes$Estatura ~ IMC , data=datos_estudiantes )
summary(regresion)

plot(IMC, datos_estudiantes$Estatura, xlab='icm', ylab='estatura')
abline(regresion)

