
# Tercera sesi�n de laboratorio en R

# Obtener la ruta de trabajo actual
getwd()

# Para fijar el directorio de trabajo deben usar el c�digo de abajo.
#debemos de marcar la ruta primero
# Utilicen el directorio de la carpeta con su nombre

setwd("C:/Users/Riccardo/Documents")

# Verificar que el directorio se fij� correctamente
getwd()

# Para guardar el script s�lo es: ctrl+s

# -------------------------------------------------------------------------

# En esta sesi�n

# Data frames
# Plots
# Dataframes & plots
#Instalando los paquetes que usaremos
#este es un paquete para regresiones y esyadistica
install.packages("AER")

# Cargamos la librer�a y la base de datos
library("AER")
#esta es la libreria (base de citas y editoriales)
data("Journals")

# summary nos da un resumen del input (es un resumen estad�stico)
summary(Journals)

# Creamos una matriz para comparar con un dataframe
JOURNAL = matrix(1:100)

# Head() regresa los primeros elementos de un dataframe
#es el primer elemneto con la etiqueta
head(Journals)

# Tail regresa los �ltimos elementos de un dataframe
#es el �ltimo vector
tail(Journals)

#Creamos una variable que diga el precio por cada cita
#"$"es para una base de datos de nuestra libreria, asi se llaman a las variables
Journals$citeprice <- Journals$price/Journals$citations

# attach() permite acceder a los elementos de un dataframe "directamente"
attach(Journals)
#es recomendable siempre mantener invariante la base de datos
#___________________________________________________________________________________
# Scatter plot
# plot() es la funci�n b�sica para gr�ficos en R
plot(log(subs), log(citeprice))

# rug() a�ade barras para indicar sobre los ejes en donde se encuentra una observaci�n
#manera de ver la densidad de la distribuci�n
rug(log(subs))
#lo mismo para la variable y
rug(log(citeprice), side = 2)

# detach() cierra el "f�cil acceso" al dataframe
detach(Journals)

# Es recomendable NO usar attach y detach.

# Para plotear lo mismo sin tener que "attachear" el dataframe:
plot(log(subs) ~ log(citeprice), data = Journals)

# Veamos la diferencia de plotear en niveles
plot(subs ~ citeprice, data = Journals)


# Exportando un gr�fico a PDF
#esto es muy para hacer trabajos en pdf, ya te arroja el gr�fico con el formato
pdf("testPDF.pdf", height = 5, width = 6)
plot(subs ~ citeprice, data = Journals)
dev.off()
#lo manda a documentos donde se fijo al inicio

## ?Devices para JPG y para PNG
#la diferencia es la calidad por pixeles
jpeg(filename = "testJPG.jpg",
     width = 1000, height = 1000, units = "px")
plot(subs ~ citeprice, data = Journals)
dev.off()

png(filename = "testPNG.png",
    width = 1000, height = 1000, units = "px")
plot(subs ~ citeprice, data = Journals)
dev.off()

### Histogramas
# Densidad
hist(Journals$citeprice, freq = FALSE)
# Lines es un gr�fico de l�nea, en este caso a�ade la apariencia de la densidad
lines(density(Journals$citeprice), col = 6)
# col indica el color, veamos otros
lines(density(Journals$citeprice), col = 2) # Rojo
lines(density(Journals$citeprice), col = 3) # Verde
lines(density(Journals$citeprice), col = 4) # Az�l
lines(density(Journals$citeprice), col = 5) # Verde agua
lines(density(Journals$citeprice), col = 6) # Rosa mexicano
lines(density(Journals$citeprice), col = 7) # Amarillo 
lines(density(Journals$citeprice), col = 8) # Gris

# Exportando el histograma a PDF

pdf("histoPDF.pdf", height = 5, width = 6)
hist(Journals$citeprice, freq = FALSE)
lines(density(Journals$citeprice), col = 2) # Rojo
dev.off()
#dev.off es el comando para exportar


# Embelleciendo el histograma
hist(Journals$citeprice, 
     freq = FALSE,
     border = NULL,
     main = paste("Distribuci�n de el costo por cita"),
     xlab = "USD por cita", 
     ylab = "Densidad")
lines(density(Journals$citeprice), col = 2) # Rojo

# Exportando nuevamente
pdf("histoPDFv2.pdf", height = 5, width = 6)
hist(Journals$citeprice, 
     freq = FALSE,
     border = NULL,
     main = paste("Distribuci�n del costo por cita"),
     xlab = "USD por cita", 
     ylab = "Densidad")
lines(density(Journals$citeprice), col = 2) # Rojo
dev.off()

# Distribuci�n de frecuencias
hist(Journals$citeprice)
lines(density(Journals$citeprice), col = 2) # Rojo
#la densidad es baja por que no es la misma escala

# Resumen de la variable
summary(Journals$publisher)

## Otros tipos de gr�ficos
tab <- table(Journals$publisher)
prop.table(tab)

# Gr�fico de barras (por categoria)
barplot(tab)

# Gr�fico de "pay"
#agrupaci�n de datos es necesaria para el entendimiento
pie(tab)

## Reordenando el database para mejorar las visualizaciones
sortedtab = sort(tab, decreasing = TRUE)
otros = sum(sortedtab[7:52])
main = sortedtab[1:6]

Editoriales = append(main,otros)
Editoriales

# Eso hace a nuestros plots mucho m�s entendibles

barplot(Editoriales)

pie(Editoriales, main="Mayores editoriales de Journals")

# Exportando los plots
pdf("piePDF.pdf", height = 5, width = 6)
pie(Editoriales, main="Mayores editoriales de Journals")
dev.off()



### Hacer un dataframe desde cero

## Obtenemos una muestra aleatoria normal

library(MASS)

n = 30 # Tama�o de la muestra

mx = c(4,2,0,0,2) # Vector de medias de X 
sx = matrix(c(4,-1,0,0,0,
              -1,1,0,0,0,
              0,0,1,0,0,
              0,0,0,9,2,
              0,0,0,2,4),5) # Varianza de X

Xsample = mvrnorm(n,mx,sx) # Muestra aleatoria de x1 y x2
Xsample

# Creamos las etiquetas para nombrar a nuestras "variables"
lbls <- c("US", "UK", "Australia", "Germany", "Mex")

# Aplicamos las etiquetas a las columnas de la matriz aleatoria
colnames(Xsample) = lbls

# Creamos un �ndice para poder graficar
index = matrix(1:30)

# Unimos las matrices en una sola
datos = cbind(index,Xsample)

# Convertimos la matriz en dataframe
datosF = as.data.frame(datos)

# Usaremos nuestro dataframe ficticio para generar plots

# Creamos un vector de colores 
plot_colors <- c("blue","red","forestgreen")

# Creamos el gr�fico base
plot(datosF$US, type="o", col=plot_colors[1], ylim=c(-2,13), ylab = "Crecimiento", xlab = "Periodos")

# Creamos otros gr�ficos encima del gr�fico base
lines(datosF$UK, type="o", pch=2, lty=5, col=plot_colors[2])
lines(datosF$Australia, type="o", pch=23, lty=3, col=plot_colors[3])

# Asignamos el t�tulo al gr�fico
title(main="Pa�ses", col.main="blue", font.main=4)

# Creamos un recuadro para referenciar a las l�neas graficadas
legend(25, 13, c("UK","Australia", "US"), c("blue","red","forestgreen"),cex=0.5)
#________________________________________________________________