#############################################################################
#
# Taller R
#
# Expresión diferencial de genes de ratón
# Microarray de Affymetrix (Affymetrix Murine Genome U74A version 2 MG_U74Av2
# Origen de los datos: GEO GSE5583 (http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE5583)
# Publicación: Mol Cell Biol 2006 Nov;26(21):7913-28.  16940178 (http://www.ncbi.nlm.nih.gov/pubmed/16940178)
#
# Muestras: 3 Wild Type x 3 Histone deacetylase 1 (HDAC1)
#
# R código original (credits): Ahmed Moustafa
#
#
##############################################################################

# Instalar RCurl

if (!requireNamespace("BiocManager"))
    install.packages("BiocManager")
BiocManager::install("RCurl")

# Si esto falla, que seguro lo hace tratar de instalarlo usando el menú, Paquetes, Servidor Spain A Coruña, RCurl

# Cargamos el paquete y los datos
library(RCurl)
url = getURL ("http://bit.ly/GSE5583_data", followlocation = TRUE)
data = as.matrix(read.table (text = url, row.names = 1, header = T))

# Chequeamos las dimensiones de los datos, y vemos las primeras y las últimas filas
dim(data)
head(data)
tail(data)

# Hacemos un primer histograma para explorar los datos
hist(data, col = "gray", main="GSE5583 - Histogram")

# Transformamos los datos con un logaritmo 
# ¿Qué pasa si hacemos una transformación logarítima de los datos? ¿Para qué sirve?
data2 = log2(data)
hist(data2, col = "gray", main="GSE5583 (log2) - Histogram")
#La transformación logarítmica se usa normalmente si los datos tienen una distribución sesgada de forma positiva 
#(como se muestra a continuación) y algunos valores son muy grandes. Si estos valores grandes están en su área de estudio, 
#la transformación logarítmica le ayudará a que las varianzas sean más constantes y normalizará sus datos. 
#No cabe duda de que Gauss y su distribución con forma de campana son la base para la realización de gran parte de 
#las pruebas de contraste de hipótesis e inferencia de datos en estadística. Por eso, a nadie le llama 
#la atención que muchas pruebas solo puedan realizarse cuando la variable que se estudia sigue una distribución normal.
#Por ejemplo, si queremos comparar las medias de dos muestras, éstas tienen que ser independientes, 
#seguir una distribución normal y tener una varianza similar (homocedasticidad). 
#Lo mismo ocurre para muchas otras comparaciones, estudios de correlación, etc.
#Cuando tenemos la mala suerte de que nuestra muestra no sigue una distribución normal debemos recurrir a las pruebas 
#de contraste no paramétricas. Estas pruebas son igual de serias y rigurosas que las paramétricas, 
#pero tienen el inconveniente de que son mucho más conservadoras, en el sentido de que cuesta más alcanzar el 
#nivel de significación estadística necesario para poder rechazar la hipótesis nula. 
#Podría darse el caso de que no obtengamos significación estadística con la prueba no paramétrica mientras que, 
#si pudiésemos aplicarla, si podríamos obtenerla con la paramétrica.
#Para evitar que pueda pasarnos esto, a alguien se le debió ocurrir que podemos transformar los datos de tal forma 
#que los nuevos datos transformados sí sigan la distribución normal. Esto, que parece un truco sucio, 
#es perfectamente lícito, siempre en cuanto tengamos en cuenta que 
#luego tendremos que hacer la transformación inversa para interpretar correctamente los resultados.
#Pensemos un momento en los logaritmos decimales (base 10). 
#En la escala logarítmica hay la misma distancia entre 1 y 10 que entre 10 y 100 y que entre 100 y 1000. 
#¿Qué quiere decir esto?. Pues que si transformamos cada variable en su logaritmo, los valores entre 1 y 10 se expandirán, 
#mientras que los más altos se comprimirán. Por eso la transformación logarítmica es útil para transformar distribuciones 
#con sesgo positivo (con cola más larga hacia la derecha): la parte izquierda se expandirá, 
#mientras que la derecha se comprimirá, favoreciendo que la curva resultante se ajuste mejor a una normal.
#Esta transformación logarítmica solo vale para números mayores que cero, aunque si tenemos una distribución con 
#valores negativos podríamos sumar una constante a cada valor para que fuese mayor que cero antes de calcular su 
#logaritmo. Cuando la nueva curva se ajusta a la campana se dice que sigue una distribución lognormal.
#En ocasiones, si la distribución está muy sesgada, puede hacerse la transformación recíproca (1/x), más potente y 
#que produce un efecto similar a la logarítmica. Otra tercera posibilidad, menos potente que la logarítmica, 
#es transformar calculando la raíz cuadrada de cada valor.
#Cuando el sesgo de la distribución es negativo (cola más larga hacia la izquierda) nos interesará lo contrario: 
#comprimir la cola de la izquierda y extender la de la derecha. 
#Si lo pensamos, esto puede hacerse elevando cada valor al cuadrado o al cubo. 
#Los productos resultantes de los valores pequeños estarán menos alejados que los resultantes de valores grandes, 
#con lo que la distribución se parecerá más a una normal.

# Hacemos un boxplot con los datos transformados. ¿Qué significan los parámetros que hemos empleado?
# ¿Qué es un boxplot?
boxplot(data2, col=c("blue", "blue", "blue",
	"orange", "orange", "orange"),
	main="GSE5583 - boxplots", las=2)
#las son para los ejes verticales. main es el titulo, col los colores
#Un diagrama de caja es un método estandarizado para representar gráficamente 
#una serie de datos numéricos a través de sus cuartiles. De esta manera, se muestran a simple vista la mediana y 
#los cuartiles de los datos, ​ 
#y también pueden representarse sus valores atípicos

# Hacemos un hierarchical clustering de las muestras basándonos en un coeficiente de correlación ç
# de los valores de expresión. ¿Es correcta la separación?
hc = hclust(as.dist(1-cor(data2)))
plot(hc, main="GSE5583 - Hierarchical Clustering")
# Sí

#######################################
# Análisis de Expresión Diferencial 
#######################################

# Primero separamos las dos condiciones. ¿Qué tipo de datos has generado?
wt <- data2[,1:3]
ko <- data2[,4:6]
class(wt)

# Calcula las medias de las muestras para cada condición. Usa apply
wt.mean = apply(wt, 1, mean)
ko.mean = apply(ko, 1, mean)
head(wt.mean)
head(ko.mean)

# ¿Cuál es la media más alta?
limit = max(wt.mean, ko.mean)
limit

# Ahora hacemos un scatter plot (gráfico de dispersión)
plot(ko.mean ~ wt.mean, xlab = "WT", ylab = "KO",
	main = "GSE5583 - Scatter", xlim = c(0, limit), ylim = c(0, limit))
# Añadir una línea diagonal con abline
abline(0, 1, col = "red")

# ¿Eres capaz de añadirle un grid?
grid()
#abline(a, b): línea de pendiente b y ordenada en el origen a
#abline(h=y): línea horizontal
#abline(v=x): línea vertical
#abline(1, 2, col = "red")     # línea y = 2x + 1
#abline(h = 2, col = "green")  # línea y = 2
#abline(v = 3, col = "violet") # línea x = 3

# Calculamos la diferencia entre las medias de las condiciones
diff.mean = wt.mean - ko.mean

# Hacemos un histograma de las diferencias de medias
hist(diff.mean, col = "gray")

# Calculamos la significancia estadística con un t-test.
# Primero crea una lista vacía para guardar los p-values
# Segundo crea una lista vacía para guardar las estadísticas del test.
# OJO que aquí usamos los datos SIN TRANSFORMAR. ¿Por qué?
# ¿Cuántas valores tiene cada muestra?
pvalue = NULL 
tstat = NULL 
for(i in 1 : nrow(data)) { #Para cada gen
	x = wt[i,] # gene wt número i
	y = ko[i,] # gene ko número i
	
	# Hacemos el test
	t = t.test(x, y)
	
	# Añadimos el p-value a la lista
	pvalue[i] = t$p.value
	# Añadimos las estadísticas a la lista
	tstat[i] = t$statistic
}

head(pvalue)

# Ahora comprobamos que hemos hecho TODOS los cálculos
length(pvalue)
#La transformación solo se emplea para las gráficas

# Hacemos un histograma de los p-values.
# ¿Qué pasa si le ponemos con una transformación de -log10?
hist(pvalue,col="gray")
hist(-log10(pvalue), col = "gray")

# Hacemos un volcano plot. Aquí podemos meter la diferencia de medias y la significancia estadística
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano")

# Queremos establecer que el mínimo para considerar una diferencia significativa, es con una diferencia de 2 y un p-value de 0.01
# ¿Puedes representarlo en el gráfico?
diff.mean_cutoff = 2
pvalue_cutoff = 0.01
abline(v = diff.mean_cutoff, col = "blue", lwd = 3)
abline(v = -diff.mean_cutoff, col = "red", lwd = 3)
abline(h = -log10(pvalue_cutoff), col = "green", lwd = 3)

# Ahora buscamos los genes que satisfagan estos criterios
# Primero hacemos el filtro para la diferencia de medias (fold)
filter_by_diff.mean = abs(diff.mean) >= diff.mean_cutoff
dim(data[filter_by_diff.mean, ])

# Ahora el filtro de p-value
filter_by_pvalue = pvalue <= pvalue_cutoff
dim(data[filter_by_pvalue, ])

# Ahora las combinamos. ¿Cuántos genes cumplen los dos criterios?
filter_combined = filter_by_diff.mean & filter_by_pvalue
filtered = data[filter_combined,]
dim(filtered)
head(filtered)

# Ahora generamos otro volcano plot con los genes seleccionados marcados en rojo
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #2")
points (diff.mean[filter_combined], -log10(pvalue[filter_combined]),col = "red")

# Ahora vamos a marcar los que estarían sobreexpresados (rojo) y reprimidos (azul). ¿Por qué parece que están al revés?
plot(diff.mean, -log10(pvalue), main = "GSE5583 - Volcano #3")
points (diff.mean[filter_combined & diff.mean < 0],
	-log10(pvalue[filter_combined & diff.mean < 0]), col = "red")
points (diff.mean[filter_combined & diff.mean > 0],
	-log10(pvalue[filter_combined & diff.mean > 0]), col = "blue")
#diff.mean = wt.mean - ko.mean

# Ahora vamos a generar un mapa. Para ello primero tenemos que hacer un cluster de las columnas y los genes 
# ¿Qué es cada parámetro que hemos usado dentro de la función heatmap?
# ¿Eres capaz de cambiar los colores del heatmap? Pista: usar el argumento col y hcl.colors
rowv = as.dendrogram(hclust(as.dist(1-cor(t(filtered)))))
colv = as.dendrogram(hclust(as.dist(1-cor(filtered))))
heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,labRow=FALSE)

heatmap(filtered)
#cexCol es el tamaño de letra del eje x
#Colv y Rowv son los dendogramas
#heatmap(filtered, Rowv=rowv, Colv=colv, cexCol=0.7, col=hcl.colors(50))

# Ahora vamos a crear un heatmap más chulo. Para ello necesitamos dos paquetes: gplots y RcolorBrewer
#if (!requireNamespace("BiocManager"))
#    install.packages("BiocManager")
#BiocManager::install(c("gplots","RColorBrewer"))
install.packages("gplots")		
install.packages("RColorBrewer")	

library(gplots)
library(RColorBrewer)

# Hacemos nuestro heatmap
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row")

# Lo guardamos en un archivo PDF
pdf ("GSE5583_DE_Heatmap.pdf")
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,
	col = rev(redblue(256)), scale = "row",labRow=FALSE)
dev.off()
heatmap.2(filtered, Rowv=rowv, Colv=colv, cexCol=0.7,col = redgreen(75), scale = "row",labRow=FALSE)

# Guardamos los genes diferencialmente expresados y filtrados en un fichero
write.table (filtered, "GSE5583_DE.txt", sep = "\t",quote = FALSE)
