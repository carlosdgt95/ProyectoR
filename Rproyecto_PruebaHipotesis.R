library(readxl)
library(agricolae)
library(BSDA)
datos_estudiantes <- read_excel("C:/Users/Sebastian/Documents/Proyecto/Estadistica/ProyectoR/datos_estudiantes.xlsx")

zprueba=function(mediam, mediaH0,varianzaH0,n,tipo){
  z=(mediam-mediaH0)/sqrt(varianzaH0/n)
  if(tipo=="mayor"){
    p=pnorm(z,0,1,lower.tail=FALSE)
  }
  if(tipo=="menor"){
    p=pnorm(z,0,1,lower.tail=TRUE)
  }
  if(tipo=="diferente"){
    p=min((2*pnorm(z,0,1,lower.tail=FALSE)),(2*pnorm(z,0,1,lower.tail=TRUE)))
  }
  return(p)
} 


##############################################################
#Prueba de Hipotesis

#Agua - Ansiedad
#Nivel de Significancia 5%
consumo_agua = datos_estudiantes$Consumo_promedio_semanal_agua
ep_ansiedad = datos_estudiantes$Episodios_por_semana_ansiedad

muestra_agua_ansiedad_2 = consumo_agua[ep_ansiedad!=1] #personas mas de 2 ep. ansiedad

media_h0 = mean(muestra_agua_ansiedad_2, na.rm = T)
var_h0 = (sd(muestra_agua_ansiedad_2, na.rm = T))**2

n = length(consumo_agua)
media = mean(consumo_agua, na.rm = T)

p_valor = zprueba(media, media_h0, var_h0, n, 'mayor')
p_valor


#Diferencia de medias
#Las personas que tienen hasta 1 episodio de ansiedad tienen un promedio mayor de
#consumo de agua

library(BSDA)

consumo_agua_uno_menos_ep_anisedad = consumo_agua[ep_ansiedad==1]
consumo_agua_mas_dos_ep_anisedad = consumo_agua[ep_ansiedad!=1]

boxplot(consumo_agua_uno_menos_ep_anisedad, consumo_agua_mas_dos_ep_anisedad)
length(consumo_agua_uno_menos_ep_anisedad)
length(consumo_agua_mas_dos_ep_anisedad)

#help('z.test')
z.test(consumo_agua_uno_menos_ep_anisedad, 
       consumo_agua_mas_dos_ep_anisedad, 
       sigma.x = sd(consumo_agua_uno_menos_ep_anisedad, na.rm = T),
       sigma.y = sd(consumo_agua_mas_dos_ep_anisedad, na.rm = T),
       alternative='greater')


#Diferencia de medias
#Las personas que tienen hasta 1 episodio de ansiedad tienen un promedio mayor de
#consumo de agua


#IMC con Tristeza
imc = (datos_estudiantes$Peso/2.2)/(datos_estudiantes$Estatura**2)
datos_estudiantes$IMC = imc

imc_datos = datos_estudiantes$IMC
ep_tristeza = datos_estudiantes$Episodios_por_semana_tristeza

prom_imc = mean(imc_datos, na.rm = T)
prom_imc

imc_max_1_ep_tristeza = imc_datos[ep_tristeza == 1]
imc_mayor_3_ep_tristeza = imc_datos[ep_tristeza >= 2]

boxplot(imc_max_1_ep_tristeza, imc_mayor_3_ep_tristeza)

length(imc_max_1_ep_tristeza)
length(imc_mayor_3_ep_tristeza)


z.test(imc_max_1_ep_tristeza, 
       imc_mayor_3_ep_tristeza, 
       sigma.x = sd(imc_max_1_ep_tristeza, na.rm = T),
       sigma.y = sd(imc_mayor_3_ep_tristeza, na.rm = T),
       alternative='greater')

help('z.test')
