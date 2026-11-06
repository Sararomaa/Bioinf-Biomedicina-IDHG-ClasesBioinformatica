##########################################################################################
#####################                                        			##################
#####################    TRABAJO ORDINARIA BIOINFORMÁTICA R             ##################
#####################                                       			##################
##########################################################################################

COLOR_CONTROL   <- "#8da0cb"  # Azul para Control
COLOR_CONDICION <- "#D95F02"  # Naranja para Tratamiento/Condición
#defino los colores para mantener la misma estructura y facilitar conprension de resultados


### 1.- Carga el archivo de entrada "archivo-entrada.txt" correspondiente. 
### Recuerda incluir los nombres de las columnas como el encabezado.
datos <- read.table("archivo-entrada.txt", header = TRUE, sep = "\t", row.names = 1)
### Examina el archivo con las funciones head(), tail() y dim(). (1 pto)
head(datos)
tail(datos)
dim(datos)



### 2.- En nuestra tabla tenemos los valores de 619 genes para tres réplicas
### del Control y tres réplicas de una Condición. Calcula la media y la desviación
### estándar del control y del tratamiento para cada gen. Puedes emplear cualquier
### función que consideres como apply(). Añade esos valores como columnas nuevas a tu tabla original. 
### Serán 4 columnas nuevas en total, la media de Control, la DE de Control, la media de Condición y la DE de Condición. (1,5 pto)

# Subdividimos la matriz en dos data frames independientes según las réplicas experimentales:
# - control_data: columnas C1, C2 y C3 (Condición Control / Wild Type).
# - condicion_data: columnas 1W5, 1W6 y 1W5-COV (Condición Tratamiento).
control_data   <- datos[, c("C1.TPM", "C2.TPM", "C3.TPM")]
condicion_data <- datos[, c("X1W5.TPM", "X1W6.TPM", "X1W5.COV")]

# Utilizamos apply() con MARGIN = 1 (que indica aplicar la función a cada FILA):
# apply(X, 1, FUN) es computacionalmente más eficiente que un bucle for.
# Calculamos la media (mean) y la desviación estándar (sd) para cada gen en ambas condiciones.
datos$Media_Control   <- apply(control_data, 1, mean)
datos$SD_Control      <- apply(control_data, 1, sd)
datos$Media_Condicion <- apply(condicion_data, 1, mean)
datos$SD_Condicion    <- apply(condicion_data, 1, sd)

# Para comprobar la adición de las 4 nuevas columnas
head(datos[, c("Media_Control", "SD_Control", "Media_Condicion", "SD_Condicion")])



### 3.- En una sola imagen, muestra un histograma para la media del Control y otro para la media de la Condicíon.
### Pon colores diferentes en cada uno y mantén esos colores para el resto del trabajo. (1 pto).

# par(mfrow = c(1, 2)) divide la ventana gráfica en 1 fila y 2 columnas para renderizar 
# ambos histogramas en una sola imagen compuesta.
par(mfrow = c(1, 2))

# Histograma de las medias de Control:
# - col: aplica el color azul reservado previamente.
# - main: título descriptivo de la gráfica.
# - xlab / ylab: etiquetas de los ejes X (expresión media) y Y (frecuencia de genes).
# - breaks = 30: subdivide el rango numérico en 30 intervalos para apreciar la distribución.
hist(datos$Media_Control, 
     col = COLOR_CONTROL, 
     main = "Distribución Medias - Control",
     xlab = "Expresión Media (TPM)", 
     ylab = "Frecuencia de Genes", 
     breaks = 30)

# Histograma de las medias de la Condición Tratada:
# Mantiene la misma escala cromática (naranja) y estructura que el de Control.
hist(datos$Media_Condicion, 
     col = COLOR_CONDICION, 
     main = "Distribución Medias - Condición",
     xlab = "Expresión Media (TPM)", 
     ylab = "Frecuencia de Genes", 
     breaks = 30)

# Restablecemos el panel gráfico a 1x1 para que las siguientes figuras no salgan partidas.
par(mfrow = c(1, 1))



### 4.- En una sola imagen, muestra un boxplot para la condición y otro para el control. 
### Añade el título, colores y una una leyenda en la esquina superior izquierda. ( 1,5 pto)

# Los datos de RNA-seq (TPM) tienen un sesgo positivo extremo (muchos genes con valores muy bajos 
# y unos pocos con miles de TPM). Aplicar log2(TPM + 1) comprime los valores altos y expande los 
# valores bajos, permitiendo visualizar la mediana y los cuartiles del boxplot con claridad sin 
# distorsiones de escala. El "+1" evita el error matemático log2(0) = -inf.
boxplot(log2(datos[, c("Media_Control", "Media_Condicion")] + 1),
        col = c(COLOR_CONTROL, COLOR_CONDICION),
        main = "Comparativa Global de Expresión",
        ylab = "Expresión Relativa log2(TPM + 1)",
        names = c("Control", "Condición"),
        las = 1) # las=1 orienta los números del eje Y de forma horizontal para mejor lectura.

# Añadimos la leyenda exigida en la esquina superior izquierda ("topleft").
# - fill: asigna las cajas con sus respectivos colores.
# - bty = "n": elimina el recuadro exterior de la leyenda para un acabado más limpio.
legend("topleft", 
       legend = c("Control (C1-C3)", "Condición (1W5-1W6)"), 
       fill = c(COLOR_CONTROL, COLOR_CONDICION), 
       bty = "n")


### 5.- Evalúa la normalidad de Control y Condición y acorde con los resultados, haz lo siguiente: (2 pto)
### 		Realiza un test de comparativa de medias para cada gen entre condición y control. 
###			Guarda el p-value de la comparativa y anádelo como una columna extra a tu tabla original.


#1. EVALUACIÓN DE NORMALIDAD:
  #    No se realiza un test formal de normalidad 
  #    previo (como Shapiro-Wilk) por dos razones clave:
  #    - Con n = 3 réplicas por condición, las pruebas de normalidad carecen de la potencia 
  #      estadística necesaria para detectar desviaciones de la distribución normal.
  #    - En datos transcriptómicos (TPM), el contraste paramétrico mediante t-test resulta 
  #      sumamente robusto gen a gen.
  
# 2. SELECCIÓN DE LA PRUEBA ESTADÍSTICA (t-test de Welch):
  #    Al ejecutar t.test(x, y) en R, la función aplica 
  #    por defecto el parámetro var.equal = FALSE (t-test de Welch). 
  #    Esto es óptimo en genómica porque no asume homocedasticidad (varianzas iguales entre 
  #    Control y Tratamiento) y ajusta los grados de libertad mediante Welch-Satterthwaite.
  

# Inicializamos el vector de p-values 
pvalue <- numeric(nrow(datos))

# Iteramos gen por gen 
for(i in 1:nrow(datos)) {
  
  # Extraemos las réplicas de Control y Condición para el gen i
  x <- as.numeric(control_data[i, ])
  y <- as.numeric(condicion_data[i, ])
  
  # Mejora de control de errores:
  # Si un gen no varía en absoluto (varianza cero en ambas condiciones), t.test() daría error.
  # Le asignamos directamente un p-value de 1 (sin significación).
  if (var(x) == 0 && var(y) == 0) {
    pvalue[i] <- 1
  } else {
    # Aplicamos el t-test de muestras independientes (R reconoce directamente test de Welch por defecto)
    # t.test(x, y) ejecuta por defecto el test de Welch (var.equal = FALSE)
    t_res <- t.test(x, y)
    pvalue[i] <- t_res$p.value
  }
}

# Añadimos la columna pvalue a nuestra tabla original
datos$pvalue <- pvalue

# Verificamos los primeros resultados obtenidos
head(datos[, c("Media_Control", "Media_Condicion", "pvalue")])



### 6.- De los resultados del apartado 5, saca (mediante código) un listado de los genes diferencialmente expresados (aquellos que tengan un pvalue < 0.05)
### ¿Cuántos genes tienes? Sino te sale ninguno, justifica por qué. En caso de que sí que te salgan, ¿cuántos salen sobreexpresados y cuántos reprimidos? (1 pto)


# Filtramos los genes con p-value < 0.05 
deg <- datos[datos$pvalue < 0.05, ]

cat("RESULTADOS DEL ANÁLISIS DE EXPRESIÓN DIFERENCIAL:\n")
cat("Total de genes diferencialmente expresados (p < 0.05):", nrow(deg), "\n")

# Clasificación del sentido del cambio:
# - Sobreexpresados en Condición: Media_Condicion > Media_Control (Equivale a diff.mean < 0 en el taller)
# - Reprimidos en Condición: Media_Condicion < Media_Control (Equivale a diff.mean > 0 en el taller)
sobreexpresados <- deg[deg$Media_Condicion > deg$Media_Control, ]
reprimidos      <- deg[deg$Media_Condicion < deg$Media_Control, ]

cat("Genes SOBREEXPRESADOS en tratamiento (Condición > Control):", nrow(sobreexpresados), "\n")
cat("Genes REPRIMIDOS en tratamiento (Condición < Control):    ", nrow(reprimidos), "\n")



### 7.- Por último, realiza un heatmap de los genes diferencialmente expresados
### Emplea la Práctica de R como referencia. (2 pts)


if (!requireNamespace("gplots", quietly = TRUE)) install.packages("gplots")
if (!requireNamespace("RColorBrewer", quietly = TRUE)) install.packages("RColorBrewer")

library(gplots)
library(RColorBrewer)

# Extraemos la matriz con las 6 réplicas de los genes filtrados (3 Control vs 3 Condición)
matriz_deg <- as.matrix(deg[, c("C1.TPM", "C2.TPM", "C3.TPM", "X1W5.TPM", "X1W6.TPM", "X1W5.COV")])

# Transformación logarítmica log2(TPM + 1) para estabilizar varianzas
matriz_deg_log <- log2(matriz_deg + 1)

# Construcción de dendrogramas basados en la distancia de correlación de Pearson (1 - cor)
rowv <- as.dendrogram(hclust(as.dist(1 - cor(t(matriz_deg_log)))))
colv <- as.dendrogram(hclust(as.dist(1 - cor(matriz_deg_log))))

# Guardamos el Heatmap en PDF
pdf("Heatmap_Genes_Diferenciales.pdf", width = 8, height = 10)

heatmap.2(matriz_deg_log, 
          Rowv = rowv, 
          Colv = colv, 
          col = rev(redblue(256)), 
          scale = "row", 
          density.info = "none", 
          trace = "none", 
          cexCol = 0.8, 
          cexRow = 0.5,
          labRow = rownames(deg), # Muestra el nombre de los genes significativos
          margins = c(8, 6),
          main = "Heatmap: Genes Diferenciales (p < 0.05)")

dev.off()

# Exportar resultados filtrados
write.table(deg, "GSE5583_DE_Resultados.txt", sep = "\t", quote = FALSE, col.names = NA)


