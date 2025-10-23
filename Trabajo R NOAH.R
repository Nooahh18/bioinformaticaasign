# NoahGuilloFrias_Trabajo2.R
# Trabajo final Bioinformática - Curso 25/26
# Análisis de parámetros biomédicos por tratamiento

# 1. Cargar librerías (si necesarias) y datos del archivo "datos_biomed.csv". (0.5 pts)
install.packages("readr")#Instala la librería readr
library(readr) #Carga la librería para usarla
datos <- read_csv("datos_biomed.csv") #Lee el archivo CSV
library(ggplot2)#Carga la librería para gráficos
head(datos)#Verificar las primeras filas del dataset


# 2. Exploración inicial con las funciones head(), summary(), dim() y str(). ¿Cuántas variables hay? ¿Cuántos tratamientos? (0.5 pts)
head(datos)#Nos devuelve los 6 primeros datos
summary(datos)#Nos da un resumen de los datos estadisticos
dim(datos)   #Nos devuelve filas y columnas
str(datos) #Nos da los tipos de datos que se stan en el archivo

# Número de variables viendo el numero de columnas
num_variables <- ncol(datos)
num_variables

# Número de tratamientos lo vemos a través de las filas
num_tratamientos <- length(unique(datos$Tratamiento))
num_tratamientos


# 3. Una gráfica que incluya todos los boxplots por tratamiento. (1 pt)

ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) +  # Datos, ejes y colores por tratamiento
  geom_boxplot() +  # Boxplot con mediana, cuartiles y posibles valores atípicos
  labs(title = "Boxplots por tratamiento",  # Título del gráfico
       x = "Tratamiento",  # Etiqueta eje X
       y = "Glucosa") +    # Etiqueta eje Y
  theme_minimal()  # Estilo limpio y sencillo

# 4. Realiza un violin plot (investiga qué es). (1 pt)
#Un violin plot muestra cómo se distribuyen los datos en cada grupo, pareciendo un boxplot pero con una forma que indica dónde se concentran más los valores
ggplot(datos, aes(x = Tratamiento, y = Glucosa, fill = Tratamiento)) + #Cogemos lo datos, definimos nuestros ejes y decidimos el color
  geom_violin(trim = FALSE, alpha = 0.6) + #Nos hace el violon plot y le decimos que no nos quite los extremos, y decidimos la transparencia del color
  geom_boxplot(width = 0.1, color = "black", alpha = 0.8) + #Ponemos un boxplot en el centro de los violin plot para media y cuartiles y para tener más información
  labs( #Para poner nombre y titulos al gráfico
    title = "Distribución de Glucosa por Tratamiento (Violin Plot)", #Titulo del grafico
    x = "Tratamiento", #Eje X
    y = "Glucosa" #Eje Y
  ) +
  theme_minimal() + #Para un gráfico más limpio
  theme(legend.position = "none") #Que no aparezca la leyenda

# 5. Realiza un gráfico de dispersión "Glucosa vs Presión". Emplea legend() para incluir una leyenda en la parte inferior derecha. (1 pt)
# Definir colores para cada tratamiento (como no lo he hecho en ggplot necesito definirlos antes para que sean los mismos)
colores <- c("Placebo" = "lightblue", #azul
             "FarmacoA" = "lightcoral", #rojo
             "FarmacoB" = "lightgreen") #verde

# Gráfico de dispersión
plot(datos$Glucosa, datos$Presion, #hacemos un grafico de dispersion, eje x datos flucosa y eje y valores presion
     main = "Relación entre Glucosa y Presión", #Titulo
     xlab = "Glucosa",#Nombre eje x
     ylab = "Presión", #Nombre eje Y
     col = colores[datos$Tratamiento],  #Colores segun grupo
     pch = 19)#Tipo de puntos

# Leyenda
legend("bottomright", legend = names(colores), col = colores, pch = 19, title = "Tratamiento")# leyend() Posicion de la leyenda, names()nombres de los grupos que aparecen en la leyenda, col= colores que representa cada grupo, title= titulo de la leyenda 

# 6. Realiza un facet Grid (investiga qué es): Colesterol vs Presión por tratamiento. (1 pt)
#Es un hacer un mini gráfico por cada grupo de una variable, así puedes comparar todos los grupos a la vez
ggplot(datos, aes(x = Colesterol, y = Presion, color = Tratamiento)) + #Indica que en el eje x irá el colesterol, en el eje y la presión y colores se colorearan los puntos según los tratamientos
  geom_point(size = 2, alpha = 0.8) + #indica que crea los puntos de gráfico de dispersión, indicamos el tamaño y la transparencia de los puntos
  facet_grid(~Tratamiento) + # Divide el gráfico en mini-graficos, uno para cada categoriamdemla variable tratameinto, dentro de cada grupo de tratamiento
  labs(title = "Colesterol vs Presión por Tratamiento", #define los titulos del grafico y los ejes
       x = "Colesterol", # eje x es colesterol
       y = "Presión") + #eje y es presion
  theme_minimal() + #Nos hace un estilo limpio y simple
  theme(legend.position = "none")#Ocultamos la leyenda

# 7. Realiza un histogramas para cada variable. (0.5 pts)

ggplot(datos, aes(x = Glucosa, fill = Tratamiento)) + #Se usa ggplot con los datos, poniendo glucosa en el eje x, y se rellena cada barra según el tipo de tratamiento
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) + #geom crea el histograma, position = identity permite tener barras de distintos tratamientos se superpongan, alpha hace que el color sea semitransparente, y bins define el número de barras
  labs(title = "Histograma de Glucosa", x = "Glucosa") + # Añade el titulo y el titulo del eje x
  theme_minimal() #Se aplica un tema limpio y sencillo
#Hacemos lo mismo con este, pero sobre la presión
ggplot(datos, aes(x = Presion, fill = Tratamiento)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  labs(title = "Histograma de Presión", x = "Presión") +
  theme_minimal()
#Hacemos lo mismo pero sobre el colesterol
ggplot(datos, aes(x = Colesterol, fill = Tratamiento)) +
  geom_histogram(position = "identity", alpha = 0.6, bins = 20) +
  labs(title = "Histograma de Colesterol", x = "Colesterol") +
  theme_minimal()

# 8. Crea un factor a partir del tratamiento. Investiga factor(). (1 pt)
#Un factor es una variable que representa grupos o categorías, no números. Un gtupo de datos se convierten en un factor para que R los trate como grupos separados en lugar de simples palabras o números.
datos$Tratamiento <- factor(datos$Tratamiento) #Convertimos la columna de tratamiento del dataframe datos en un factor, asi R lo trata comouna variable categorica agrupada
str(datos$Tratamiento) #Muestra la estructura interna del objetivo


# 9. Obtén la media y desviación estándar de los niveles de glucosa por tratamiento. Emplea aggregate() o apply(). (0.5 pts)

# Media 
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = mean) #Aggregate agrupa los datos según la variable tratamiento y aplica la función mean (media) a otra variable, en este caso glucosa

# Desviación estándar 
aggregate(Glucosa ~ Tratamiento, data = datos, FUN = sd)#Aqui hacemos lo mismo pero le pedimos la desviación estandar

# 10. Extrae los datos para cada tratamiento y almacenalos en una variable. Ejemplo todos los datos de Placebo en una variable llamada placebo. (1 pt)

placebo <- datos[datos$Tratamiento == "Placebo", ]#Filtra las filas del grupo placebo y las guardamos en una variable
farmacoA <- datos[datos$Tratamiento == "FarmacoA", ]#Filtra el grupo Farmaco A y lo guardamos en una variable
farmacoB <- datos[datos$Tratamiento == "FarmacoB", ] #Filtra el grupo B y lo guarda en una variable

# 11. Evalúa si los datos siguen una distribución normal y realiza una comparativa de medias acorde. (1 pt)

# Evaluar normalidad de Glucosa por tratamiento usando el test Shapiro, en cada grupo de tratamiento
shapiro.test(datos$Glucosa[datos$Tratamiento == "Placebo"])
shapiro.test(datos$Glucosa[datos$Tratamiento == "FarmacoA"])
shapiro.test(datos$Glucosa[datos$Tratamiento == "FarmacoB"])
#El resultado nos lo da como un p-value. Como el resultado de los tres test nos da un p-value >0,05, no podemos rechazar la hipotesis nula, por ende los datos se consideran normales

# 12. Realiza un ANOVA sobre la glucosa para cada tratamiento. (1 pt)

anova_glucosa <- aov(Glucosa ~ Tratamiento, data = datos)#aov realiza el analisis, siendo glucosa la variable dependiente y tratamiento la variable categórica que define los grupos
summary(anova_glucosa)#Nos muestra los resultados
#F = 2.358 → muestra cuánta diferencia hay entre tratamientos respecto a la variación interna, y muestra que hay un poco de diferencia
#p = 0.1 → como es mayor a 0.05, no hay diferencias significativas entre los tratamientos
#Esto nos indica que las medias son parecidas, no se puede decir que un tratamiento sea mejor que otro


