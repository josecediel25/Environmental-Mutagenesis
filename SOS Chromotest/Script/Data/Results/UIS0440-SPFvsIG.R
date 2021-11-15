# Establecer carpeta de trabajo
setwd("D://7to_semestre_UIS/TRABAJO_DE_GRADO_1/Antigenotoxicidad/UIS0440/")


# Cargar datos ------------------------------------------------------------
tabla <- read.csv("UIS0440_SPFvsIG.csv", sep = ";")

# Agregar los datos a la ruta de Rstudio
attach(tabla)
names(tabla)

# Vectores para graficar error estandar
x1 <- tabla$Conc1
x2 <- tabla$Conc2
y1 <- tabla$SPFin_vitro
err1 <- EE
y2 <- tabla$X.IG
err2 <- EE.1

# Vector nombres ejes
#x1lab <- expression("Concentración del extracto (%)")
x1lab <- expression("Extract concentration (%)")
#y1lab <- expression("FPS" * italic("" ["in vitro"]))
y1lab <- expression("SPF" * italic("" ["in vitro"]))
#y2lab <- expression("%IG")
y2lab <- expression("%GI")

# Generar archivo tiff o pdf-----------------------------------------------
#jpeg("FIGURA-UIS0440.jpeg", width = 20, height = 16, units = "cm", res = 1200)
jpeg("FIGURE-UIS0440.jpeg", width = 20, height = 16, units = "cm", res = 1200)
par(mar=c(5, 4, 4, 4) + 0.1)

# Graficar SPF
plot(SPFin_vitro ~  Conc1, axes = T,
     xlab = "", ylab = "",
     xlim=c(0,50), ylim = c(0, 3), 
     type = "o", bty = "n",cex.lab=2.0, cex.axis=1.3
)+ 
  arrows(x1, y1 - err1, x1, y1 + err1, length = 0.09, angle = 90, code = 3) +
  lines(SPFin_vitro ~  Conc1,
        col = "black", type = "o",
        pch = 21, bg = "white", cex.lab=1.3, cex.axis=1.3, cex=2.0
  ) 
  

# Graficar %IG 

par(new=T)

plot(X.IG ~  Conc2, axes = F,
     xlab = "", ylab = "",
     xlim=c(0,50), ylim = c(0, 90), 
     type = "n", bty = "n"
) +
  arrows(x2, y2 - err2, x2, y2 + err2, length = 0.09, angle = 90, code = 3) +
  lines(X.IG ~  Conc2,
        col = "black", type = "o",
        pch = 22, bg = "white",cex.lab=2.0, cex.axis=1.3, cex=2.0
  )

# Titulo grafica ----------------------------------------------------------
#titulo <- expression("Extracto UIS0440")
titulo <- expression("Extract UIS0440")
title(list(titulo, cex = 1.3))

# Graficar todos los bordes
box()

# Graficar eje der
axis(4, ylim=c(0,100), col="black",col.axis="black",cex.lab=2.0, cex.axis=1.3)

# Nombre ejes
mtext(x1lab,side=1,col="black",line=2.5, cex = 1.3)
mtext(y1lab,side=2,col="black",line=2.5, cex = 1.3)
mtext(y2lab,side=4,col="black",line=2.5, cex = 1.3)

# Vector para los datos en la leyenda, deben ser editados manualmente------
#ley1 <- expression("FPS" * italic("" ["in vitro"]))
#ley2 <- expression("%IG")
ley1 <- expression("SPF" * italic("" ["in vitro"]))
ley2 <- expression("%GI")
leyenda <- c(ley2, ley1)

# Poner la leyenda en el grafico
legend("bottomright",
       legend = leyenda[1:2], pch = c(22, 21),
       pt.bg = c("white", "white"), cex = 1.3, pt.cex = 2.0, bty = "n"
)

legend(x = 30, y = 30, legend = c("Y = 5.31 + 43.676X", "R = 0.86 (p < 0.001)"), bty = "n", cex = 1.2)

# Finalizar la generacion del archivo jpeg
dev.off() 

# Correlacion
cor.test(x = SPFin_vitro, y = X.IG, method = "pearson")
modelo_lineal <- lm(X.IG ~ SPFin_vitro, tabla)
summary(modelo_lineal)

plot(X.IG ~ SPFin_vitro)
abline(modelo_lineal)
legend(x = 18, y = 50, legend = c("y = (10.86 + 2.01x)", "R = 0.90 (p = 0.014)"), bty = "n")
