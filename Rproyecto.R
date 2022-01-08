
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
diagrama_tristeza = barplot(tablaf_tristeza, main=" Diagrama Episodios Semanales de Estres" , xlab="Numero Episodios", ylab = 'Casos',names.arg = c("0-1","2-3","4-5","5+"),col=colores_diag_tristeza)

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
                      "Curtosis","Sesgo","Desviación Estándar")
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
names(tabla_estatura)= c("Límite inferior de clase","Límite superior de clase","Marca de clase",
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
                          "Curtosis","Sesgo","Desviación Estándar")
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
names(tabla_IMC)= c("Límite inferior de clase","Límite superior de clase","Marca de clase",
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
                     "Curtosis","Sesgo","Desviación Estándar")
medidas_IMC

#Histograma de Frecuencias
hist(IMC, breaks = seq(from=IMC_rango[1],to=IMC_rango[2],by=amplitud_IMC), include.lowest = T, right = F,
     main="Histograma de frecuencias de IMC",ylab="Frecuencia absoluta",xlab="IMC")


#ojiva
#Ojiva IMC
q = c(0.05,0.12,0.55,0.75,0.95) ##datos para grafico
quartil_IMC = quantile(IMC, probs = c(0.05,0.12,0.55,0.75,0.95),na.rm = T)
quartil_IMC

plot(quartil_IMC, q, main = "Ojiva Peso", xlab = "Puntaje", ylab = "Frecuencia Relativa")
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
names(tabla)= c("Límite inferior de clase","Límite superior de clase","Marca de clase",
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
                 "Curtosis","Sesgo","Desviación Estándar")
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
names(tabla)= c("Límite inferior de clase","Límite superior de clase","Marca de clase",
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
                 "Curtosis","Sesgo","Desviación Estándar")
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
names(tabla)= c("Límite inferior de clase","Límite superior de clase","Marca de clase",
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
                 "Curtosis","Sesgo","Desviación Estándar")
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

#Gasesosas por Estres
boxplot(datos_estudiantes$Consumo_semanal_promedio_gaseosas~
          datos_estudiantes$Episodios_por_semana_estres,
        ylab = 'Consumo de Gaseosa', xlab = 'Episodios de Estres', 
        names= c("0-1","2-3","4-5","5+"),
        main='Cons. de gaseosa por Ep. de estres'
        )

#Energizante por Tristeza
boxplot(datos_estudiantes$Consumo_promedio_semanal_energizantes~datos_estudiantes$Episodios_por_semana_tristeza, 
        ylab = 'Cons. Prom Energizantes', xlab = 'Episodios de Tristeza', 
        names=c("0-1","2-3","4-5","5+"),
        main='Cons. Prom Energizantes por Ep. de Tristeza')
par(mfrow = c(1,2))


par(mfrow = c(1,2))

#Agua por Ansiedad
boxplot(datos_estudiantes$Consumo_promedio_semanal_agua~
        datos_estudiantes$Episodios_por_semana_ansiedad,
        ylab="Cons. de agua", xlab= "Ansiedad por semana",
        names=c("0-1","2-3","4-5","5+"),
        main='Cons. Agua por Ep. de Ansiedad',
        col="turquoise")

#IMC por Miedo
boxplot(datos_estudiantes$IMC~datos_estudiantes$Episodios_por_semana_miedo, 
        ylab = 'IMC', xlab = 'Episodios de Miedo', 
        names=c("0-1","2-3","4-5"),
        main='IMC por Ep. de Miedo')
par(mfrow = c(1,2))


par(mfrow = c(1,2))

#Carbohidratos por Ira
boxplot(datos_estudiantes$Porcentaje_promedio_carbohidrato_por_comida~datos_estudiantes$Episodios_por_semana_ira_frustacion, 
        ylab = 'Consumo Prom. Carbohid', xlab = 'Episodios de Ira',
        names = c("0-1","2-3","4-5","5+"),
        main='Porc. Prom Carbohid por Ep. de Ira')

#Gasesosas por Tristeza
boxplot(datos_estudiantes$Consumo_semanal_promedio_gaseosas~datos_estudiantes$Episodios_por_semana_tristeza, 
        ylab = 'Cons. Prom Gaseosas', xlab = 'Episodios de Tristeza', 
        names=c("0-1","2-3","4-5","5+"),
        main='Cons. Prom Gaseosas por Ep. de Tristeza')
par(mfrow = c(1,2))




par(mfrow = c(1,2))

#IMC por Tristeza
boxplot(datos_estudiantes$IMC~datos_estudiantes$Episodios_por_semana_tristeza, 
        ylab = 'IMC', xlab = 'Episodios de Tristeza', 
        names= c("0-1","2-3","4-5","5+"),
        main='IMC por Ep. de Tristeza')

#Gaseosas por miedo
boxplot(datos_estudiantes$Consumo_semanal_promedio_gaseosas~
          datos_estudiantes$Episodios_por_semana_miedo, 
        ylab = 'Cons. Gaseosas', xlab = 'Episodios de Miedo', 
        names=c("0-1","2-3","4-5"),
        main='Cons. Gaseosas por Ep. de Miedo')

par(mfrow = c(1,2))



##############################################################
#Matriz de correlacion y covarianza
##############################################################

imc = (datos_estudiantes$Peso/2.2)/(datos_estudiantes$Estatura**2)
imc

datos_estudiantes$IMC = imc

datos_cuantitativos = datos_estudiantes[,c(5,6,16, 17,18,20,38)]

names(datos_cuantitativos) = c('Peso', 'Est', 'Prom.Gaseos', 'Prom.Energ', 
                               'Prom.Agua', 'Prom.Carb.Comida', 'IMC')

matriz_cov = round(cov(na.omit(datos_cuantitativos)),2)
matriz_cov

matriz_corr = round(cor(na.omit(datos_cuantitativos)), 2)
matriz_corr

library(corrplot)

#type="upper"
corrplot(matriz_corr, method="circle", tl.srt=45)

