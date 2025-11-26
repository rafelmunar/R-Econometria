## importa l'arxiu de dades en Excel : Datos Turismo

# EJERCICIO 1 ------------------------------------------------------------------------------
#**Apartado a**: Haz una regresión lineal simple del log_llegadas frente a la estabilidad política, estpol:
#¿Tienen los coeficientes los signos esperados?

#Haz una regresión lineal simple con el comando "lm" y crea el objeto `reg1` que recoge los resultados estándar de la estimación del MRL

summary(Datos_Turismo_1$log_llegadas)

summary(Datos_Turismo_1$estpol)

reg1 <- lm(log_llegadas ~ estpol, data = Datos_Turismo_1)

# Mira los resultados de la regresión que has guardado en ´reg1´ mediante el comando "summary()":

summary(reg1)
sum.reg1 <- summary(reg1)

#**Apartado b**: Comente la bondad del ajuste de la regresión. ¿Cuál es la varianza de los residuos
# Estos valores pueden verse en el sumario de ´reg1´, pero puede interesarnos tenerlos como objetos separadamente.
# Entonces creamos un objeto que recoja todo  el sumario de ´reg1´, que llamaremos ´sum.reg1´ :

sum.reg1 <- summary(reg1)

#Dentro de este sumario podemos separar los valores de los diferentes estadísticos y estimaciones que se han realizado.Ejemplos:
#Ejemplo 1: Aislamos el R cuadrado al que R llama r.squared:

r2sumreg1 <- sum.reg1$r.squared
r2sumreg1

#Ejemplo 2: Separamos la desviación estándar residual a la que R llama sigma:

stdsumreg1 <- sum.reg1$sigma
stdsumreg1

#Podemos calculamos la varianza residual creando un objeto var.res que sea el resultado de elevar al cuadrado sigma

var.res = sum.reg1$sigma^2

#Vemos var.res

var.res

#**Apartado c**:En el apartado a, contraste la significación del coeficiente de estabilidad política, estpol, al 5 % de significación. 
#  ¿El nivel de estabilidad política afecta las llegadas de turistas?
# Para ello podemos usar los valores del estadístico t que hay en el sumario de ´reg1´.


# EJERCICIO 2--------------------------------------------------------------------------------
#**Apartado a**: Haz una regresión lineal simple del log_pib frente a log_llegadas:
#¿Tienen los coeficientes los signos esperados?
#Haz una regresión lineal simple y nómbrala de `reg2`

reg2 <- lm(log_pib ~ log_llegadas, data = Datos_Turismo_1)

# Mira los resultados de la regresión mediante el comando `summary()`:

summary(reg2)
sum.reg2 <- summary(reg2)

#**Apartado b**: En el apartado a, mirando los resultados del summary , contraste la significación del coeficiente de log_llegadas, beta1, al 1 % de significación. ¿El turismo afecta a la economía?

sign.reg2 <- sum.reg2$coefficients[2, 1] / sum.reg2$coefficients[2, 2]
sign.reg2

#**Apartado c**: En el apartado a,  contraste la hipótesis H0: beta2 <= 1  frente a HA: beta2 > 1, con alpha = 5%.
#Per calcular l'estadístic de contrast

sign.reg2 <- sum.reg2$coefficients[2, 1] - 1/ sum.reg2$coefficients[2, 2]
sign.reg2

#Per obtenir el valor crític de la distribució t-student amb n graus de llibertat i alpha=0.05, contrast de una coa per la dreta.

qt(0.95, 150)
qt(0.05, 150, lower.tail = FALSE)3

#**Apartado d**: Comente la bondad del ajuste de la regresión y haz un intervalo de confianza del 95 % para el coeficiente de log_llegadas.
# El coeficient de determinació es troba en el summary

#Obtenir l'interval de confiança pels parámetres del model que per defecte el dóna per un nivell de confiança del 95%:

confint(reg2, level = 0.95)

#**Apartado e**: Haz un intervalo de confianza del 99 % para el coeficiente de log_llegadas.

confint(reg2, level = 0.99)

#**Apartado f**: Basándose en el MRLS del apartado a, haz una predicción puntual y por intervalo al 95 % de confianza para el log_pib(0) cuando log_llegadas(0) = 15.
#Crear el valor de x per a la predicció x(0)

#Hacer la predicció puntual de y(0)

pred_puntual <- data.frame(log_llegadas = 15)
predict(reg2, newdata = pred_puntual)

#Predicció per interval de de y(0)

pred_puntual <- data.frame(log_llegadas = 15)
predict(reg2, newdata = pred_puntual, interval = 'prediction', level = 0.95)

pred_puntual <- data.frame(log_llegadas = 15)
predict(reg2, newdata = pred_puntual, interval = 'confidence', level = 0.95)

#**Apartado g**: Basándose en el MRLS del apartado a, haz una predicción puntual y por intervalo al 95 % de confianza para el log_pib de la observación 13 (Bahamas).
#Crear el valor de x para la predicció x(0)


#Hacer la predicció puntual de y(0)




#Predicció per interval de de y(0)




#**Apartado h**: Repita el apartado anterior, pero utilizando el nivel de confianza 1-alpha = 90%.
#Haz la predicción por intervalo con nivel de confianza 0.90



#**Apartado i**: Basándose en el MRLS del apartado a, haz una predicción puntual y por intervalo al 95% de confianza para la media del log_pib, E(log_pib), de la observación 13 (Bahamas).

# EJERCICIO 3 ------------------------------------------------------------------------------

#**Apartado a**:Haz una regresión lineal simple de la tasa de mortalidad por enfermedades coronarias chd frente al consumo de cigarrillos per capita, en libras de tabaco, por persona mayor de 18 años, cig:

reg3 <- lm(chd ~ cig, data = DADES_CHD)

summary(reg3)

sum.reg3 <- summary(reg3)

#**Apartado b**:En el apartado a., contraste la significación del coeficiente de cig, β1, al 1 % de significación. ¿El consumo de cigarrillos per capita afecta la tasa de mortalidad por enfermedades coronarias?

sign.reg3 <- (sum.reg3$coefficients[2, 1] - 6.2) / sum.reg3$coefficients[2, 2]
sign.reg3

#**Apartado c**:En el apartado a., contraste la hipótesis H0 : β1 ≤ 6,2 frente a HA : β1 > 6,2, con α = 1 %.

qt(0.95, 32)
qt(0.05, 32, lower.tail = FALSE)

#**Apartado d**:Comente la bondad del ajuste de la regresión y haz un intervalo de confianza del 90 % para el coeficiente de cig

confint(reg3, level = 0.90)

#**Apartado e**:Haz un intervalo de confianza del 99 % para el coeficiente de cig.

confint(reg3, level = 0.99)

#**Apartado f**:Basándose en el MRLS del apartado a., haz una predicción puntual y por intervalo 95 % para el chd cuando cig0 = 9,5.

pred_puntual <- data.frame(cig = 9.5)
predict(reg3, newdata = pred_puntual)

pred_puntual <- data.frame(cig = 9.5)
predict(reg3, newdata = pred_puntual, interval = 'prediction', level = 0.95)

#**Apartado g**:Repita el apartado anterior, pero utilizando el nivel de confianza 1−α= 90 %.

pred_puntual <- data.frame(cig = 9.5)
predict(reg3, newdata = pred_puntual, interval = 'prediction', level = 0.90)

#**Apartado h**:Basándose en el MRLS del apartado a., haz una predicción puntual y por intervalo 95 % para la media del chd, E(chd), cuando cig0 = 9,5.

pred_puntual <- data.frame(cig = 15)
predict(reg3, newdata = pred_puntual, interval = 'confidence', level = 0.95)

#**Apartado i**:(Introducción a los modelos de regresión lineal múltiple) Haz una regresión lineal múltiple de la tasa de mortalidad por enfermedades coronarias chdi frente al consumo de cigarrillos per capita, en libras de tabaco, por persona mayor de 18 años (cigi), el consumo per capita de vino, en galones, por persona mayor de 18 años (winei), y el consumo de comestibles grasos y aceites per capita en libras (edfati):

reg4 <- lm(chd ~ cig + wine + edfat, data = DADES_CHD)
summary(reg4)
sum.reg4 <- summary(reg4)

#**Apartado j**:En el apartado anterior, contraste la significación del coeficiente de winei (β2) y de edfati (β3) al 5 % de significación. ¿El consumo de vino per capita afecta a la tasa de mortalidad por enfermedades coronarias? ¿El consumo de comestibles grasos y aceites per capita afecta a la tasa de mortalidad por enfermedades coronarias?

#**Apartado k**: Comente la bondad del ajuste de la regresión del apartado i.. Compárala con la bondad del ajuste de la regresión del apartado a.. ¿Cuál sería una forma correcta de comparar el ajuste de la regresión del apartado a. con la del apartado i.?

#**Apartado l**: Basándose en el MRLM del apartado i., haz una predicción puntual para el chd para cig0 = 9,5, wine0 = 1,20 y edfat0 = 45,0.

pred_puntual <- data.frame(cig = 9.5, wine = 1.2, edfat = 45)
predict(reg3, newdata = pred_puntual)

pred_puntual <- data.frame(cig = 9.5, wine = 1.2, edfat = 45)
predict(reg3, newdata = pred_puntual, interval = 'prediction', level = 0.95)

pred_puntual <- data.frame(cig = 9.5, wine = 1.2, edfat = 45)
predict(reg3, newdata = pred_puntual, interval = 'confidence', level = 0.95)








