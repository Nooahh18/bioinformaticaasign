# UNA MUESTRA T-TEST
set.seed(100) #crear una semilla para usar cosas al azar
x<-rnorm(50,mean=10,sd=0.5)#crear una distribucian R random normla, le gio q tenga 50 observaciones, con media de 10 y desviacion estandra de 0,5
t.test(x,mu=10)#x distribucion y mu la media que deberia
#nos da un p value de 0,48 asi que no hemos pasado el corte, asi que es hipotesis nula, no es sognificante hipotesis es valida a partir de menor de 0,05

# UNA MUESTRA WILCOXON TEST
numeric_vector<-c(20,29,24,19,20,22,28,23,19,19)
wilcox.test(numeric_vector,mu=20,conf.int=TRUE)
#hipotesis nula por menor a 0,05

# DOS MUESTRAS T-TEST y WILCOX
x <- c(0.80, 0.83, 1.89, 1.04, 1.45, 1.38, 1.91, 1.64, 0.73, 1.46)
mean(x)
y <- c(1.15, 0.88, 0.90, 0.74, 1.21)
mean(y)
wilcox.test(x,y,alternative="g")#alternative
# Por defecto R asume que no son pareados (son independientes) y que las varianzas son diferentes
t.test(x,y,var.equal=TRUE)
#sigue siendo nula, cambiar hipotesis 

# TRES O MÁS MUESTRAS
#ANOVA #si hay normaidad
datos_anova <- read.table("anova-datos.txt",sep=" ",header=TRUE)
anova <- aov(Vigilancia~Dosis,datos_anova)#vigilancia depende de dosis, estan relacionados, mira la relacion
summary(anova)

#Kruskal-Wallis #no normal
datos <- data.frame(
  condicion = c(rep("condicion1", 18), rep("condicion2", 18), rep("condicion3", 18)),
  n_huevos = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 16, 27, 28, 29, 30, 51, 52, 53, 342, 40,
               41, 42, 43, 44, 45, 46, 47, 48, 67, 88, 89, 90, 91,92, 93, 94, 293,
               19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 25, 36, 37, 58, 59, 60, 71, 72)
)
head(datos)
aggregate(n_huevos ~ condicion, data = datos, FUN = median)#otra funcion parecida a laply que permite calcular, mdia numero huevos por condicion
aggregate(n_huevos ~ condicion, data = datos, FUN = sd)#desviacion estdandar para cada condicion
kruskal.test(n_huevos ~ condicion, data = datos) #si que pasa el corte, hay una asociacion entre condicion y cosa

# SHAPIRO TEST#evalua normalidad, la hipotesis nula es que es normal
shapiro.test(numeric_vector)
# ¿Qué test sería el más correcto según el resultado de Shapiro?
shapiro.test(x)
shapiro.test(y)

set.seed(100)
normaly_disb <- rnorm(100,mean=5,sd=1)
shapiro.test(normaly_disb)
#mayopr de 0,05 es normal, menor no es normal
set.seed(100)
not_normaly_disb <- runif(100) #distribución uniforme
shapiro.test(not_normaly_disb)

# KOLMOGOROV AND SMIRNOV TEST
x<-rnorm(50)
y<-runif(50)
ks.test(x,y)
#p value, hipotesis alternatoiva es que no son de la misma poblacion

# CHI SQUARE #matrix con numero de datos mayor a 3/5
frec<-c(15,19)
chisq.test(frec)

matrix<-matrix(c(4,11,10,13,3,4,6,8),nrow=2)
matrix
chisq.test(matrix)

# FISHER #numero de datos mas chiquitos
var.test(x,y)
fisher.test(matrix)
#analisis varianza, se llama por los dos nombres

# CORRECIÓN P VALUE #pauete extra
install.packages("kableExtra")
library("kableExtra")
p_values_estudio_1 <- c(0.11, 0.8, 0.92, 0.68, 0.04, 0.15, 0.89, 0.47, 0.88, 0.85, 0.17, 0.59, 0.4, 0.33, 0.97, 0.48, 0.85, 0.07,0.66, 0.41, 0.64, 0.24, 0.72, 0.004, 0.67, 0.51, 0.26,0.94)
p_values_estudio_1
n_1 <- length(p_values_estudio_1)
sin_correccion_1 <- sum(p_values_estudio_1 <= 0.05)
bonferroni_1 <- sum(p.adjust(p =p_values_estudio_1, method="bonferroni") <= 0.05)
BH_1 <- sum(p.adjust(p = p_values_estudio_1, method = "BH") <= 0.05)
n_1
sin_correccion_1
bonferroni_1
BH_1

kable(data.frame(Numero_de_p.values = n_1, Sin_corrección = sin_correccion_1,
                 Bonferroni = bonferroni_1, Benjamini_Hochberg = BH_1 ), align="c")