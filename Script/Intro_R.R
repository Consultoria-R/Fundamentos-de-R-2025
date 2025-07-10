# Manipulación de bases de datos, estadística y econometría con R
# Introducción a R

# Operaciones básicas
2+2
5-2
2*5
10/5
2^3

# Condiciones lógicas
1<5  # Menor
1>5  # Mayor
1==5 # Igual
2==2
1!=5 # Diferente

# Funciones
# Secuencias
1:10
10:1
seq(from=0,to=20,by=1)
seq(from=0,to=20,by=2)
seq(0,20,2)

# Asignaciones (un solo elemento)
x <- 5
x
y <- 10
y
x+y
w <- x+y
w

# Concatenación (agrupación de varios elementos)
x <- c(9,7,2,5,1)
x

# Extracción por posición
x[1]
x[3]
x[c(1,3)]
x[c(4,2)]
x[1:3]
x[c(1,4)]
x[c(5,3,1)]

# Extracción por condición
x <- c(9,7,2,5,1)
x
x<=2
x[x<=2]
x[x>=4]
x[-1]
x[-2]

# Operaciones
x <- c(9,4,2,5,10)
x
sum(x)                    # suma
mean(x)                   # media
median(x)                 # mediana
sd(x)                     # desviaci?n estandar
min(x)                    # m?nimo
max(x)                    # m?ximo
cumsum(x)                 # suma acumulada
sort(x)                   # creciente
sort(x,decreasing = TRUE) # decreciente
length(x)                 # tama?o del objeto
summary(x)                # resumen
log(x)                    # logaritmo
sqrt(x)                   # ra?z cuadrada
class(x)                  # Tipo de objeto

# Definiendo nuevamente el vector x
x <- c(1,3,6,12,NA,4,8,10,2,6,4,NA,5)
x
sum(x)
sum(x,na.rm = TRUE)
mean(x)
mean(x,na.rm = TRUE)
median(x,na.rm = TRUE)
class(x)
length(x)

# Valores perdidos
is.na(x)
sum(is.na(x))  # Suma de valores perdidos
sum(!is.na(x)) # Diferentes de valores perdidos

# Reemplazo
x <- c(1,3,6,12,NA,4,8,10,2,6,4,NA,5)
x
x[3] <- 20
x[1:2] <- 40
x[c(4,7)] <- 80
x

# Asignaciones por condici?n
x[is.na(x)] <- 0
x[is.na(x)] <- mean(x,na.rm = TRUE)
x

# Objeto tipo carácter
x <- c("1","1","2","1")
x
class(x)

# Volver a tipo numérico
as.numeric(x)
x <- as.numeric(x)
x

# Objeto tipo factor o categórico (clasificar)
# Ejemplo I
factor(x,
       levels = 1:2,
       labels = c("Hombre","Mujer"))
x <- factor(x,
            levels = 1:2,
            labels = c("Hombre","Mujer"))
class(x)
x

# Contar variables categ?ricas
table(x)
prop.table(table(x))
prop.table(table(x))*100
round(prop.table(table(x))*100,4)

# Matrices
matrix(1:15,nrow = 5,ncol = 3)
mat <- matrix(1:15,5,3)
mat

# Extracción
mat[5,3]
mat[4:5,2:3]
mat[c(1,5),c(1,3)]
mat[1:5,1:2]
mat[,1:2]
mat[,-3]

# Valor perdido
mat[3,3] <- NA

# Operaciones de matrices
mat

# Filas
rowSums(mat)
rowSums(mat,na.rm = TRUE)

# Columnas
colSums(mat,na.rm = TRUE)

# Media
rowMeans(mat,na.rm = TRUE)
colMeans(mat,na.rm = TRUE)

# FIN
2+2
