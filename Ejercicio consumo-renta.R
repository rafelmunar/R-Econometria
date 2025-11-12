#ESTADÍSTICS DESCRIPTIUS

#Estadísticos descriptivos básicos de una variable de la base de datos 
summary(consum_renda_2$AL)

#Estadísticos descriptivos básicos de toda  la base de datos 
summary(consum_renda_2)

# Para ver la cabecera de la base de datos
head(consum_renda_2)

#Para ver la dimensión de la base de datos (nº de filas, nº de columnas)
dim(consum_renda_2)

#Calcular proporción de cada valor sobre el total para una variable de la tabla de datos (AL)
prop.table(consum_renda_2$AL)

# Transformar en logaritmos una variable de la base de datos
lnING<- log(consum_renda_2$ING)
lnING
#Añadir la nueva variable lnING a la base de datos
consum_renda=cbind(consum_renda_2,lnING)

# ESTIMACIÓ D'UN MRLS


#AL= beta1 + beta2 INGR + ui 
reg1 <- lm(AL ~ ING, data = consum_renda_2)
# Mira los resultados de la regresión mediante el comando `summary()`:
summary(reg1)
sum.reg1 <- summary(reg1)
sum.reg1$r.squared
vresidual <- sum.reg1$sigma^2
vresidual
