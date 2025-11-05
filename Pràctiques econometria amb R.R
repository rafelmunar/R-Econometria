#Primeros pasos en R
#Utilizar operadores
4+5
(4-6)/sqrt(9)

#Crear una variable X definiendo sus valores con concatenate
X=c(1,6,7,3,4,5,6)
#Ver los valores de X
X
# calcular la varianza de X 
var(X)
#Calcular Desviación estándar de X
sd(X)
# Obtener los principales estadísticos descriptivos de una variable
summary(X)
#Crear una variable Y definiendo sus valores con concatenate
Y=c(1,2,9,4,3,2,1)
#Ver los valores de Y
Y
#Calcular la correlación lineal entre X e Y
cor1 <- cor(X,Y)
#Calcular X por Y
X*Y
#Crear una variable con valores de 1 a 7
Z=1:7
Z
#Crear una secuencia de valors de 1 a 5 yendo de 0,5 en 0,5
seq(1,5,0.5)
#crear un vector con todos los valores iguales.
rep(9, 10)

#Crear constantes y variables como objetos con el operador asignar <- 
co<-10
OX<-X
#Ver los valores de un objeto
co
OX
#Crear directamente un objeto aplicando una función y ver sus valores
Z<- X+Y
Z

#Listar los objetos que se han creado y que están en la memoria
ls()

# Hacer un contraste sobre la media poblacional con varianza desconocida. Por ejemplo: H0: µ= 5  ; HA: µ≠ 5
# Paso 1: crear la muestra con concatenate
muestraedad=c(5,6,7,2,3,1,1,4,6,5)
#Paso 2: Escribir el codigo con los valores correspondientes:  t.test(muestra, mu=mu0, alternative="two.sided", conf.level = )
t.test(muestraedad, mu=5, alternative="two.sided", conf.level = 0.95 )
