# ABRIR FICHERO CON LOS DATOS RED WINE
wine_data <- read.csv("data/winequalityred.csv", header=TRUE, sep=",")

#dimensiones:
dim(wine_data)

#vemos las primeras lineas:
head(wine_data[,1:12])

#Tipo de datos:
sapply(wine_data, function(x) class(x))

#Ver summary de los datos:
summary (wine_data)

#ver si existen vacios:
sapply(wine_data, function(x) sum(is.na(x)))

#valores extremos:
boxplot(wine_data$fixed.acidity)
boxplot.stats(wine_data$fixed.acidity)$out
boxplot(wine_data$volatile.acidity)
boxplot.stats(wine_data$volatile.acidity)$out
boxplot(wine_data$citric.acid)
boxplot.stats(wine_data$citric.acid)$out
boxplot(wine_data$residual.sugar)
boxplot.stats(wine_data$residual.sugar)$out
boxplot(wine_data$chlorides)
boxplot.stats(wine_data$chlorides)$out
boxplot(wine_data$free.sulfur.dioxide)
boxplot.stats(wine_data$free.sulfur.dioxide)$out
boxplot(wine_data$total.sulfur.dioxide)
boxplot.stats(wine_data$total.sulfur.dioxide)$out
boxplot(wine_data$density)
boxplot.stats(wine_data$density)$out
boxplot(wine_data$pH)
boxplot.stats(wine_data$pH)$out
boxplot(wine_data$sulphates)
boxplot.stats(wine_data$sulphates)$out
boxplot(wine_data$alcohol)
boxplot.stats(wine_data$alcohol)$out

#eliminar outliners:
wine_data<-wine_data[!(wine_data$total.sulfur.dioxide>200),]
summary(wine_data$total.sulfur.dioxide)

# Guardar en csv:
write.csv(wine_data, file = "data/winedata_output.csv")

#grupos de datos:
wine_data.high_ph<-subset(wine_data, pH>=3.5)
wine_data.low_ph<-subset(wine_data, pH<3.5)
wine_data.high_citric<-subset(wine_data, citric.acid>=0.5)
wine_data.low_citric<-subset(wine_data, citric.acid<0.5)
wine_data.high_sugar<-subset(wine_data, residual.sugar>2.5)
wine_data.low_sugar<-subset(wine_data, residual.sugar<=2.5)

#comprobar normalidad:
library(nortest)
alpha= 0.05
col.names = colnames(wine_data)
for(i in 1:ncol(wine_data)){
  if (i == 1) cat("Variables que no siguen distribución normal:\n")
  if (is.integer(wine_data[,i]) | is.numeric(wine_data[,i])){
    p_val = ad.test(wine_data[,i])$p.value
    if (p_val < alpha){
      cat(paste(col.names[i],"\n"))
    }
  }
}

#añadir columnas según agrupación:
wine_data$ph.class = ifelse (wine_data$pH<3.5,0,1)
wine_data$citric.class = ifelse (wine_data$citric.acid<0.5,0,1)
wine_data$sugar.class = ifelse (wine_data$residual.sugar<=2.5,0,1)

summary(wine_data)
#comprobar homogeneidad:
fligner.test(quality ~ ph.class, data=wine_data)
fligner.test(quality ~ citric.class, data=wine_data)
fligner.test(quality ~ sugar.class, data=wine_data)


corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
# Calcular el coeficiente de correlación para cada variable cuantitativa
# con respecto al campo "quality"
for (i in 1:(ncol(wine_data) - 1)) {
  if (is.integer(wine_data[,i]) | is.numeric(wine_data[,i])) {
    spearman_test = cor.test(wine_data[,i],
                             wine_data[,length(wine_data)],
                             method = "spearman")
    corr_coef = spearman_test$estimate
    p_val = spearman_test$p.value
    # Add row to matrix
    pair = matrix(ncol = 2, nrow = 1)
    pair[1][1] = corr_coef
    pair[2][1] = p_val
    corr_matrix <- rbind(corr_matrix, pair)
    rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(wine_data)[i]
  }
}
print(corr_matrix)

#subset de la calidad segun residual.sugar
wine_data.high_sugar.quality <- wine_data[wine_data$residual.sugar>2.5,]$quality
wine_data.low_sugar.quality <- wine_data[wine_data$residual.sugar<=2.5,]$quality

#test para determinar si a más residual.sugar mas calidad
t.test(wine_data.low_sugar.quality, wine_data.high_sugar.quality, alternative="less")


#regresion
alcohol = wine_data$alcohol
sulphates = wine_data$sulphates
volatile = wine_data$volatile.acidity
citric = wine_data$citric.acid
total.sulfur = wine_data$total.sulfur.dioxide
chlorides = wine_data$chlorides
density = wine_data$density

# Variable a predecir
calidad = wine_data$quality
# Generación de varios modelos
modelo1 <- lm(calidad ~  alcohol + sulphates + volatile + citric , data = wine_data)
modelo2 <- lm(calidad ~ sulphates + volatile + citric + chlorides, data = wine_data)
modelo3 <- lm(calidad ~  volatile + citric + chlorides + density, data = wine_data)
modelo4 <- lm(calidad ~  citric + chlorides + density + alcohol, data = wine_data)
modelo5 <- lm(calidad ~ chlorides + density + alcohol + sulphates, data = wine_data)
modelo6 <- lm(calidad ~  density + alcohol + sulphates + volatile, data = wine_data)
tabla.coeficientes <- matrix(c(1, summary(modelo1)$r.squared,
                                2, summary(modelo2)$r.squared,
                                3, summary(modelo3)$r.squared,
                                4, summary(modelo3)$r.squared,
                                5, summary(modelo3)$r.squared,
                                6, summary(modelo3)$r.squared),
                             ncol = 2, byrow = TRUE)


colnames(tabla.coeficientes) <- c("Modelo", "R^2")
tabla.coeficientes

newdata <- data.frame(
  alcohol = 9.4,
  sulphates = 0.56,
  volatile = 0.7,
  citric = 0
)
# Predecir calidad
predict(modelo1, newdata)


#graficas en torno la variable quality:

plot(wine_data$quality)

#graficas de relación:
plot(wine_data$alcohol, wine_data$quality)
abline(lm(wine_data$quality~wine_data$alcohol),col="red",lwd=3)

plot(wine_data$sulphates, wine_data$quality)
abline(lm(wine_data$quality~wine_data$sulphates),col="red",lwd=3)

plot(wine_data$residual.sugar, wine_data$quality)
abline(lm(wine_data$quality~wine_data$residual.sugar),col="red",lwd=3)

#grafica correlación:
install.packages("corrplot")
library(corrplot)
corrplot(corr_matrix[,0], type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


scatterplot.matrix(~alcohol+sulphates|quality, data=wine_data,
                   main="Three Cylinder Options")
