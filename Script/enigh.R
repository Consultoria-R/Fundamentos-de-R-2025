# Bases de datos, estadística y econometría con R
# ENIGH 2022

# Borrar objetos de r
rm(list = ls())

# ENIGH 2024
# Cargar base de datos
library(haven)
enigh_2022 <- read_dta("concentradohogar_2022.dta")
enigh_2022

# Cargar libreria especializada en base de datos
library(tidyverse)
enigh_2022

# Verbo select: seleccionar columnas
colnames(enigh_2022)
select(enigh_2022,3,5,10:13,23,24,44,47,55,56,57,8)

# Guardar en objeto
master <- 
  select(enigh_2022,3,10:13,23,24,44,47,55,56,57,8)
master

# Por nombres
master <- 
  select(enigh_2022,ubica_geo,est_socio,sexo_jefe,edad_jefe,educa_jefe,
         tot_integ,ing_cor,ingtrab,rentas,transfer,estim_alqu,
         otros_ing,gasto_mon,factor)

# Crear columnas
# Verbo mutate: crear o recodificar columnas 
mutate(master,
       edad_jefe2=edad_jefe^2,.after = edad_jefe)
mutate(master,
       edad_jefe2=edad_jefe^2,.before = edad_jefe)
master <- 
  mutate(master,
         edad_jefe2=edad_jefe^2,.after = edad_jefe)
master

# Suma del ingreso corriente, pag. 192
mutate(master,
       ing_trim=ingtrab+rentas+transfer+estim_alqu+otros_ing,.after = otros_ing)
mutate(master,
       ing_trim=rowSums(across(.cols = ingtrab:otros_ing),na.rm = TRUE),.after = otros_ing)
mutate(master,
       gasto_pct=gasto_mon/ing_cor*100)

# Varias columnas
mutate(master,
       ingtrab_pct=ingtrab/ing_cor*100,
       rentas_pct=rentas/ing_cor*100,
       transfer_pct=transfer/ing_cor*100,
       estim_alqu_pct=estim_alqu/ing_cor*100,
       otros_ing_pct=otros_ing/ing_cor*100,
       .keep = "used")

# Entidad
master
mutate(master,
       entidad=str_sub(ubica_geo,1,2),.after = ubica_geo)
mutate(master,
       municipio=str_sub(ubica_geo,3,5),.after = ubica_geo)
master <- mutate(master,
                 entidad=str_sub(ubica_geo,1,2),.after = ubica_geo)

# Crear etiquetas
master

# Sexo
master <- 
  mutate(master,sexo_jefe=factor(sexo_jefe,
                                 levels = 1:2,
                                 labels = c("Hombre","Mujer")))

# Estatus socioeconómico
master <- 
  mutate(master,est_socio=factor(est_socio,
                                 levels = 1:4,
                                 labels = c("Bajo","Medio_bajo","Medio_alto","Alto")))

# Entidad: pág. 261
master <- 
  mutate(master,entidad=factor(as.numeric(entidad),
                               levels = 1:32,
                               labels = c("Ags","BC","BCS","Camp","Coah","Col","Chia",
                                          "Chih","CDMX","Dgo","Gto","Gro","Hgo","Jal",
                                          "Mex","Mich","Mor","Nay","NL","Oax","Pue",
                                          "Qro","QRoo","SLP","Sin","Son","Tabs","Tamps",
                                          "Tlax","Ver","Yuc","Zac")))

# Educación
master <- 
  mutate(master,educa_jefe=factor(as.numeric(educa_jefe),
                                    levels = 1:11,
                                    labels = c("Sin_instruccion",
                                               "Preescolar",
                                               "Primaria incompleta",
                                               "Primaria completa",
                                               "Secundaria incompleta",
                                               "Secundaria completa",
                                               "Preparatoria incompleta",
                                               "Preparatoria completa",
                                               "Profesional incompleta",
                                               "Profesional completa",
                                               "Posgrado")))
master

# Verbo Filter: filtrar filas
master
filter(master,
       entidad=="CDMX")
filter(master,
       entidad=="CDMX" & sexo_jefe=="Mujer")
filter(master,
       entidad=="CDMX" & sexo_jefe=="Mujer" & edad_jefe>=65)
filter(master,
       entidad %in% c("CDMX","Mex") & sexo_jefe=="Mujer")

# Verbo arrange: ordenar
master
arrange(master,edad_jefe)       # De menor a mayor
arrange(master,desc(edad_jefe)) # De mayor a menor
arrange(master,-edad_jefe)

# Estadísticas
# Ingreso
master
master$ing_cor
mean(master$ing_cor)
summary(master$ing_cor)

# Gasto
master
master$gasto_mon
mean(master$gasto_mon)
summary(master$gasto_mon)

# Media ponderada
select(master,ing_cor,factor)
weighted.mean(master$ing_cor,master$factor)

# Media del ingreso, edad y gasto
summarise(master,
          Ingreso=mean(ing_cor))
summarise(master,
          Ingreso=mean(ing_cor),
          Edad=mean(edad_jefe),
          Gasto=mean(gasto_mon))

# Media del ingreso, edad y gasto
summarise(master,
          Ingreso=weighted.mean(ing_cor,factor))
summarise(master,
          Ingreso=weighted.mean(ing_cor,factor),
          Gasto=weighted.mean(gasto_mon,factor))

# Varias columnas (excel, cuadro 3.5)
master
summarise(master,across(.cols = ing_cor:otros_ing,
                        .fns = ~weighted.mean(.x,factor)))

# Encadenamiento
summarise(master,
          Ingreso=weighted.mean(ing_cor,factor))

master %>% 
  summarise(Ingreso=weighted.mean(ing_cor,factor))

master %>% 
  select(entidad,2:3,ing_cor,gasto_mon,factor)

# Seleccionar variables y filtrar
master %>% 
  select(entidad,2:3,sexo_jefe,ing_cor,gasto_mon,factor) %>% 
  filter(sexo_jefe=="Hombre")

master %>% 
  select(entidad,2:3,sexo_jefe,ing_cor,gasto_mon,factor) %>% 
  filter(sexo_jefe=="Hombre") %>% 
  arrange(desc(ing_cor))     # De mayor a menor

# Verbo group_by: Agrupar
master %>% 
  summarise(Ingreso=weighted.mean(ing_cor,factor))

# Ingreso por sexo
master %>% 
  group_by(sexo_jefe) %>% 
  summarise(Ingreso=weighted.mean(ing_cor,factor))

# Ingreso por entidad
master %>% 
  group_by(entidad) %>% 
  summarise(Ingreso=weighted.mean(ing_cor,factor))

# Ingreso por entidad y sexo
master %>% 
  group_by(entidad,sexo_jefe) %>% 
  summarise(Ingreso=weighted.mean(ing_cor,factor))

# Deflactar
# ENIGH 2016, 2018 y 2020
# Cargar bases de datoss
enigh_2016 <- 
  read_dta("concentradohogar_2016.dta")
enigh_2018 <- 
  read_dta("concentradohogar_2018.dta")
enigh_2020 <- 
  read_dta("concentradohogar_2020.dta")

# Media ponderada del ingreso
(ing_cor_2016 <- weighted.mean(enigh_2016$ing_cor,enigh_2016$factor))
(ing_cor_2018 <- weighted.mean(enigh_2018$ing_cor,enigh_2018$factor))
(ing_cor_2020 <- weighted.mean(enigh_2020$ing_cor,enigh_2020$factor))
(ing_cor_2022 <- weighted.mean(enigh$ing_cor,enigh$factor))
ingreso <- c(ingreso2016=ing_cor_2016,ingreso2018=ing_cor_2018,ingreso2020=ing_cor_2020,ingreso2022=ing_cor_2022)

# Indice de precios
library(readxl)
ingreso
ind_prec <- 
  read_excel("series_tiempo.xlsx",
             sheet = "indice_precios")
ind_prec

# Calcular índice anual por anio
base_anual <- 
  ind_prec %>% 
  group_by(anio=year(periodo)) %>% 
  summarise(indice_anual=mean(indice),.groups = "drop")
base_anual

# Calcular deflactores a precios de 2022
deflactores <- 
  base_anual %>% 
  mutate(base_2022=indice_anual[anio==2022]/indice_anual) %>% 
  filter(anio %in% c(2016,2018,2020,2022)) %>% 
  pull(base_2022)
deflactores

# Mensual
ingreso/3
deflactores
ingreso*deflactores/3

# Variables categóricas
master
table(master$sexo_jefe)
table(master$educa_jefe)

# Porcentajes
prop.table(table(master$sexo_jefe))*100
prop.table(table(master$educa_jefe))*100

# dplyr
master %>% 
  count(sexo_jefe)
master %>% 
  count(educa_jefe)
master %>% 
  count(educa_jefe,sort = TRUE)
master %>% 
  count(educa_jefe,sexo_jefe)

# Con factor de expansión
master %>% 
  count(sexo_jefe,wt=factor)
master %>% 
  count(educa_jefe,wt=factor,sort = TRUE)

# Base regresión
master
base_reg <- 
  master %>% 
  select(entidad,-ubica_geo,ing_cor,gasto_mon,tot_integ,2:6,factor)
base_reg

# Deciles
# Ordenar la base de datos por ingresos
base_reg <- 
  base_reg %>% 
  arrange(ing_cor)
base_reg

# Creando la variable suma acumulada
base_reg <- 
  base_reg %>% 
  mutate(factor_acumulado=cumsum(factor))

# Creando la variable de porcentajes
base_reg <- 
  base_reg %>% 
  mutate(pct_factor=factor_acumulado/sum(factor))

# Generando la varaible decil
base_reg$decil <- NA

for(i in 1:10){
  base_reg$decil[base_reg$pct_factor>((i-1)/10) & base_reg$pct_factor <= i/10] <- i  
}

# Ingresos por decil
base_reg <- 
  base_reg %>% 
  mutate(decil=as.factor(decil))

base_reg %>% 
  group_by(decil) %>% 
  summarise(ingreso=weighted.mean(ing_cor,factor)/3) %>% 
  as.data.frame()

# Gráfico de dispersión
base_reg %>% 
  ggplot(aes(x=ing_cor,y=gasto_mon))+
  geom_point()+
  theme_bw()

# Gráfico de dispersión y linea de regresión
base_reg %>% 
  ggplot(aes(x=ing_cor,y=gasto_mon))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  theme_bw()

# Regresión
options(scipen = 5)
lm(gasto_mon~ing_cor,
   data = base_reg) %>% 
  summary()

# Con factor de expansión
base_reg %>% 
  ggplot(aes(x = ing_cor, y = gasto_mon)) +
  geom_point(aes(size = factor), alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  scale_size_continuous(name = "Factor de expansión", range = c(0.1, 5)) +
  labs(
    title = "Relación entre ingreso y gasto ponderados",
    x = "Ingreso corriente",
    y = "Gasto monetario"
  ) +
  theme_minimal()

# Gráfico de densidad
base_reg %>% 
  ggplot(aes(x = ing_cor, y = gasto_mon)) +
  geom_point(alpha = 0.1) +
  geom_density_2d(color = "red", linewidth = 0.6) +
  # labs(
  #   title = "Distribución de hogares por ingreso y gasto (curvas de densidad)",
  #   x = "Ingreso corriente",
  #   y = "Gasto monetario"
  # ) +
  theme_minimal()

# Regresión con factor de expansión
options(scipen = 5)
lm(gasto_mon~ing_cor,
   weights = factor,
   data = base_reg) %>% 
  summary()

# Con valores atípicos
library(MASS)
rlm(gasto_mon~ing_cor,
    weights = factor,
    data = base_reg) %>% 
  summary()

consumo <- 
  rlm(gasto_mon~ing_cor,
      weights = factor,
      data = base_reg)
summary(consumo)

# Predict
predict(consumo,newdata = data.frame(ing_cor=30000))
predict(consumo,newdata = data.frame(ing_cor=100000))
predict(consumo,newdata = data.frame(ing_cor=10000))
predict(consumo,newdata = data.frame(ing_cor=500000))

# Multiple
consumo <- 
  rlm(gasto_mon~ing_cor+sexo_jefe+tot_integ,
      weights = factor,
      data = base_reg)
summary(consumo)

# Predict
nuevos_datos <- 
  data.frame(ing_cor=c(30000,50000),
             sexo_jefe=c("Hombre","Mujer"),tot_integ=c(3,5))
predict(consumo,newdata = nuevos_datos)

# Maching learning
library(randomForest)
modelo_rf <- randomForest(
  gasto_mon ~ ing_cor,
  data = base_reg,
  weights = base_reg$factor,
  ntree = 200,
  importance = TRUE
)
modelo_rf

predict(modelo_rf,newdata = nuevos_datos)

# Arboles
modelo_rf <- randomForest(
  gasto_mon ~ ing_cor+sexo_jefe+tot_integ+edad_jefe+edad_jefe2,
  data = base_reg,
  weights = base_reg$factor,
  ntree = 100,
  importance = TRUE
)
modelo_rf

# Shap
library(DALEX)
library(iml)

# Datos sin la variable dependiente
X <- base_reg[,c("ing_cor","sexo_jefe","tot_integ","edad_jefe")]

# Variable objetivo
y <- base_reg$gasto_mon

# Crear objeto Predictor compatible con SHAP
predictor <- Predictor$new(
  model = modelo_rf,
  data = X,
  y = y
)

# SHAP para una observación específica (por ejemplo, la 1)
shap <- Shapley$new(predictor, x.interest = X[1, ])

# Visualizar contribuciones
plot(shap)+
  theme_bw()

# Crear funcion de prediccion compatible con iml
predict_function <- function(model, newdata) {
  predict(model, newdata = newdata)
}

# Calcular SHAP para muchas observaciones y combinar resultados
shap_values_list <- lapply(1:100, function(i) {
  shap <- Shapley$new(predictor, x.interest = X[i, ])
  shap$results
})
shap_df <- do.call(rbind, shap_values_list)

# Calcular efecto medio absoluto por variable (importancia global)
shap_summary <- aggregate(abs(phi) ~ feature, data = shap_df, mean)
shap_summary

barplot(
  shap_summary$`abs(phi)`,
  names.arg = shap_summary$feature,
  las = 2,
  col = "skyblue",
  main = "Importancia global SHAP (media absoluta por variable)",
  ylab = "Impacto promedio en la prediccion"
)

# SHAP dependence plots por variable
dep_ing_cor <- FeatureEffect$new(predictor, feature = "ing_cor", method = "pdp+ice")
plot(dep_ing_cor)

dep_tot_integ <- FeatureEffect$new(predictor, feature = "tot_integ", method = "pdp+ice")
plot(dep_tot_integ)

dep_sexo_jefe <- FeatureEffect$new(predictor, feature = "sexo_jefe", method = "pdp+ice")
plot(dep_sexo_jefe)


# No lineales
modelo_nl <- nls(
  gasto_mon ~ a * ing_cor^b,
  data = master,
  start = list(a = 100, b = 0.5),
  weights = factor
)

modelo_nl

# Modelo de Encuestas
library(survey)
enigh_2020 <-
  enigh_2022 %>% 
  select(ubica_geo,est_socio,sexo_jefe,edad_jefe,educa_jefe,
         tot_integ,ing_cor,gasto_mon,upm,est_dis,factor) %>% 
  mutate(entidad=str_sub(ubica_geo,1,2),.after = ubica_geo) %>% 
  mutate(sexo_jefe=factor(sexo_jefe,
                          levels = 1:2,
                          labels = c("Hombre","Mujer")),
         entidad=factor(as.numeric(entidad),
                        levels = 1:32,
                        labels = c("Ags","BC","BCS","Camp","Coah","Col","Chia",
                                   "Chih","CDMX","Dgo","Gto","Gro","Hgo","Jal",
                                   "Mex","Mich","Mor","Nay","NL","Oax","Pue",
                                   "Qro","QRoo","SLP","Sin","Son","Tabs","Tamps",
                                   "Tlax","Ver","Yuc","Zac")),
         est_socio=factor(est_socio,
                          levels = 1:4,
                          labels = c("Bajo","Medio_bajo","Medio_alto","Alto"))) %>% 
  select(-ubica_geo,-educa_jefe)

# Disenio muestral
enigh_2020_svy <- 
  svydesign(ids = ~upm,strata = ~est_dis,weights = ~factor,
            data = enigh_2020)
summary(enigh_2020_svy)
1/0.0057042
1/0.0001546
1/0.1666667

# Tabulados
# Ingreso
svymean(~ing_cor,design = enigh_2020_svy)
confint(svymean(~ing_cor,design = enigh_2020_svy),level = 0.90)
cv(svymean(~ing_cor,design = enigh_2020_svy))*100
440.34/63696*100

# Gasto
svymean(~gasto_mon,design = enigh_2020_svy)
confint(svymean(~gasto_mon,design = enigh_2020_svy),level = 0.90)
cv(svymean(~gasto_mon,design = enigh_2020_svy))*100

# Prueba T
svyby(~ing_cor,~sexo_jefe,enigh_2020_svy,svymean)
svyby(~ing_cor,~sexo_jefe,enigh_2020_svy,svymean,vartype = "ci", level=0.90)
svyttest(ing_cor ~ sexo_jefe, design = enigh_2020_svy)
enigh_2022

# Ratio: ingreso corriente total entre total de integrantes, per capita
svyratio(
  numerator = ~ing_cor,
  denominator = ~tot_integ,
  design = enigh_2020_svy,
)
confint(svyratio(numerator = ~ing_cor,denominator = ~tot_integ,
  design = enigh_2020_svy),level=0.90)

# Variables categóricas
svytable(~sexo_jefe, design = enigh_2020_svy)
svytable(~sexo_jefe, design = enigh_2020_svy,Ntotal=TRUE)*100

svytable(~sexo_jefe+est_socio, design = enigh_2020_svy)
svytable(~sexo_jefe+est_socio, design = enigh_2020_svy,Ntotal=TRUE)*100

totales <- svytotal(~sexo_jefe, design = enigh_2020_svy)
confint(totales,level = 0.90)

svytable(~sexo_jefe + est_socio, design = enigh_2020_svy)
svychisq(~sexo_jefe + est_socio, design = enigh_2020_svy)

# FIN