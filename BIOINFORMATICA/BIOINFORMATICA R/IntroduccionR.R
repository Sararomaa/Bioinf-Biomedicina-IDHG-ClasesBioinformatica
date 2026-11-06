##################################################################################################        
                              INTRODUCCION A R                             
##################################################################################################


##### 1.- INTRODUCCION

# Para ejecutar este Script pulsa CTRL+R (Windows) o CMD+Enter (MAC)

# Para poner bien el directorio de trabajo:
	# "El directorio de trabajo tiene que ser el mismo del script."
	# Desde el menú: WINDOWS: Archivo -> Cambiar Dir
	#		     MAC: Miscelaneo -> Cambiar Directorio Trabajo
	# Con un comando:
	#	setwd("ruta")

# Para saber dónde estamos:
getwd()


##### 2.- INSTALACION Y CARGA DE PAQUETES

# Instalar de forma directa (comentar estas líneas si ya lo has instalado)
install.packages("UsingR")

# Cargar los paquetes instalados
library(UsingR)

# Instalar solo si no está instalado (avanzado)

paquetes <- c("UsingR","devtools","pacman","ggplot2")  # aquí poner los paquetes a instalar
instalar <- paquetes[!paquetes %in% installed.packages()]
if(length(instalar)) install.packages(instalar)
lapply(paquetes, library, character.only = TRUE)

# Acceso al manual
help.start()
help(hist) # aquí dentro de los paréntesis se pone la función sobre la que queremos ayuda


##### 3.- R COMO CALCULADORA

1+1

2+3*4

3^2

exp(1)

sqrt(10)

pi

2*pi*6378 #circunferencia de la Tierra


# EJERCICIO:
# Calcula el área de un círculo de radio 10 (A=pi*r^2).

pi*10^2


##### 4.- VARIABLES

# Para asignar valores se puede usar indistintamente "<-" o "="

# Numeric: contienen números con decimales
# Booleanos: TRUE or FALSE
# Strings: secuencias de caracteres

# Nombres de variables: pueden incluir puntos, "_" pero NUNCA "-". NUNCA empiezan por números.
# Cuidado con usar nombres con palabras especiales, como nombres de funciones para nombrar variables.

num <- 10     # numérico
num = 10
texto  <- "Hola" # carácter
texto
logico <- TRUE   # lógico
logico
x<-1
y<-3
z<-4

x*y*z

x*y*Z #Hay distinción entre mayúsculas y minúsculas!

This.Year<-2023


# EJERCICIO:
# Crea tres variables: tu edad, tu nombre y si te gusta R (TRUE/FALSE).

edad = 200
nombre = "Pepito"
gustaR = FALSE

##### 5.- LISTAS / VECTORES

# Series (listas o vectores)

# Diferentes maneras de crearlos y operar con ellos

# Crearlos con la función c(): 
x<-c(2,0,0,4)
y<-c(1,9,9,9)
x
y

x+y # suma de vectores solo si ambos tienen el mismo tamaño
x*4 # multiplicacion de un numero por todos los elementos del vector
sqrt(x) #aplicacion de la funcion sqrt() sobre todos los elementos del vector

# Crearlos con las funciones rep() y seq()
rep(1,10)

seq(2,6)

seq(4,20,10)

#Para navegar por ellos, se buscan sus posiciones
x[1]
x[-2]
x[1]<-3; x  # el ";" permite poner dos líneas de código en una sola, una detrás de la otra
x[-1]<-5;x  # reasignamos valores dentro del vector

y<9 # nos devuelve un booleano dependiendo de si los elementos de ese vector cumplen ese criterio
y[y<9]=2 # reasigna solo si cumple la condicion
y

notas <- c(mate = 8, historia = 7, fisica = 9) # Esto de aquí se parece un poco a un diccionario
notas								# En verdad es como una tabla
notas["mate"] # navegas por el nombre de la columna
notas[1]
mean(notas) # saca la media


# EJERCICIO:
# Crea un vector con tus 5 números favoritos y calcula su media.
vector_num <- c(3,5,7,12,21)
vector_num
mean(vector_num)

##### 6.- DATA FRAMES

# Son agrupaciones de vectores o tablas

# Es de lo más fácil y común de manejar en R

bp<-read.table("bp.txt",header=T) # así leemos el archivo
bp

bp[1,1]
bp[,1]

bp["WEIGHT"]
bp$WEIGHT
bp["WEIGHT"] 	# Accedemos al contenido de la columna
bp[,2] 		# Lo mismo pero no aparece el nombre de la columna
bp$WEIGHT 		# lo mismo
bp$WEIGHT[2] 	# Sacar un valor de la columna en concreto 
bp[2,] 		# Sacamos una fila entera
bp[2,][1]		# Sacamos un valor de la fila en concreto
bp[2,1]


iris
head(iris)       # dataset integrado en R
tail(iris)
str(iris)
summary(iris)
dim(iris)

head(iris)

iris$Species
iris[1:5, c("Sepal.Length", "Species")]

# EJERCICIO:
# Obtén la media de Sepal.Width de iris.
head(iris)
iris$Sepal.Width
iris[,2]
iris["Sepal.Width"]

mean(iris$Sepal.Width)

##### 7.- VECTORES / LISTAS AVANZADOS

# Colecciones de variables
# Se parecen mucho a las data frames
# Tenemos unos genes asociados a tres grupos

geneset1<-c(1,4)
geneset2<-c(2,5,7,8)
geneset3<-c(1,5,6)

# Almacenamos los datos en una lista
genesets<-vector("list",3)
genesets[[1]]<-geneset1
genesets[[2]]<-geneset2
genesets[[3]]<-geneset3
genesets

# Ahora le ponemos nombre a los grupos
names(genesets)=paste0("set",1:3)
genesets

# Ahora nombramos a los genes
genenames=paste0("gene",1:10)
genesets[[1]]=genenames[geneset1]
genesets[[2]]=genenames[geneset2]
genesets[[3]]=genenames[geneset3]
genesets

# Extraer elementos de la lista
genesets[1]
genesets[[1]]
genesets[[1]][1]
unlist(genesets[1])[1]
length(genesets)
length(genesets[[2]])

# Crear una lista vacía
lista_vacia<-list()
lista_vacia <- c()

# Ahora con tres elementos
lista_vacia<-vector("list",length=3)
# Ahora la llenamos con un bucle
for(j in 1:3){
lista_vacia[[j]] <- c(1,2,3*j)
}
lista_vacia

# Añadir elementos. Para eso el data frame es lo mismo que la lista
lista_vacia[[4]] <- data.frame(x=c(8,5,3),y=c(7,9,1))
lista_vacia

# Eliminar elementos
lista_vacia[[2]]<-NULL
#lista_vacia[-2] #equivalente
lista_vacia


##### 8.- CONDICIONALES: IF ELSE IFELSE

if(4 < 5) {"Verdadero"} else {"Falso"}

num <- 1:8

ifelse(num %% 2 == 0, "Par", "Non") # Forma comprimida, más dificil de entender

##### 9.- BUCLES-LOOPS

### BUCLE FOR

dado <- 1:6
dado
for(cara in dado) {
  print(cara ^ 2)
}

mi_vector<-NULL
for(cara in dado) {
  mi_vector[cara] <- cara ^ 2
}
mi_vector

### BUCLE WHILE

umbral <- 5
valor <- 0

while(valor < umbral) {
  print("Todavía no.")
  valor <- valor + 1
}

while(1 < 2) {
  print("Presiona ESC para detener")
}

### SALIDA DE BUCLES: BREAK Y NEXT

for(i in 1:10) {
  if(i == 3) {
    break
  }
  print(i)
}

numero <- 20
while(numero > 5) {
  if(numero == 15) {
    break
  }
  numero <- numero - 1
}
numero

for(i in 1:4) {
  if(i == 3) {
    next
  }
  print(i)
}


##### 10.- FUNCIONES: APPLY y  LAPPLY

### APPLY apply(x,margin,fun) 
# Margin, 1 es filas y 2 columnas. 
# Fun es la funcion que como requisito siempre tiene que admitir funciones

mi_df <- data.frame(v1 = 1:3, v2 = 4:6)
mi_df

# Coerción a matriz
mi_matriz <- as.matrix(mi_df)
mi_matriz

# Verificamos que sea matriz
is.matrix(mi_matriz)
class(mi_matriz)
mi_matriz

apply(mi_matriz,1,mean)
apply(mi_matriz,2,sum)

### LAPPLY vale para listas lapply(x,fun)

trees[1:5,] #esta por defecto en R
lapply(trees,mean)
arboles <- lapply(X = trees, FUN = mean)
arboles
class(arboles)

# Podemos usar lapply en lugar de un bucle
mi_vector <- 6:12
mi_vector
resultado <- NULL
posicion <- 1
for(numero in mi_vector) {
  resultado[posicion] <- sqrt(numero)
  posicion <- posicion + 1
}
resultado

resultado <- NULL
resultado <- lapply(mi_vector, sqrt)
resultado
as.numeric(resultado)

##### 10.- IMPORTAR Y EXPORTAR

# Exportar
write.csv(iris, "iris.csv", row.names = FALSE)

write.table(variable a guardar, "fichero.txt")

# Importar
iris2 <- read.csv("iris.csv")
head(iris2)


##### 11.- GRAFICOS

# Para los colores que valen http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

#### Histograma
hist(iris$Sepal.Length, main = "Histograma de Sepal Length",
     col = "lightblue", xlab = "Longitud")

### Dispersión
plot(iris$Petal.Length, iris$Petal.Width,
     col = iris$Species, pch = 19,
     main = "Relación pétalos", xlab = "Largo", ylab = "Ancho")

legend("topleft", legend = levels(iris$Species),
       col = 1:3, pch = 19, title = "Especies")

### GRÁFICO CIRCULAR
# Frecuencia de especies en iris
tabla_especies <- table(iris$Species)
pie(tabla_especies, col = c("red", "green", "blue"),
    main = "Proporción de especies")

legend("topright", legend = names(tabla_especies),
       fill = c("red", "green", "blue"))

# --- BOXPLOT ---
# Edad ficticia en "banco" → usamos iris para ejemplo
boxplot(Sepal.Length ~ Species, data = iris,
        col = c("red", "green", "blue"),
        main = "Distribución de Sepal.Length por especie",
        xlab = "Especies", ylab = "Sepal Length")

#########################################################
# Con ggplot2 (versión moderna y más estética)
#########################################################

library(ggplot2)

# Histograma con leyenda
ggplot(iris, aes(x = Sepal.Length, fill = Species)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  labs(title = "Histograma de Sepal Length", x = "Sepal Length") +
  theme_minimal()

# Dispersión con leyenda automática
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point(size = 3) +
  labs(title = "Relación entre largo y ancho de pétalos") +
  theme_minimal()

# Gráfico circular
ggplot(as.data.frame(tabla_especies), aes(x = "", y = Freq, fill = Var1)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Proporción de especies", fill = "Especie") +
  theme_void()

# Boxplot
ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  labs(title = "Distribución de Sepal.Length por especie") +
  theme_minimal()