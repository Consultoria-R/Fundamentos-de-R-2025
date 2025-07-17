# Manipulación de bases de datos, estadística y econometría en R
# Encuesta Nacional Ocupación de Empleo (ENOE)

# Librerias
library(tidyverse)
library(haven)

# Directorio
setwd("~/Curso_R_2025/ENOE")

# Cargar base de datos
sdem_1t25 <- 
  read_dta("enoe_2025_trim1_dta/ENOE_SDEMT125.dta")
sdem_1t25

# Filtrar base de datos de acuerdo a la metodología del INEGI
sdem_1t25 <- 
  sdem_1t25 %>% 
  filter(r_def==0 & (c_res==1 | c_res==3))

# Total de población
sdem_1t25 %>% 
  summarise(pob=sum(fac_tri))

# Total de población de 15 anios y más
sdem_1t25 <- 
  sdem_1t25 %>% 
  filter(eda>=15 & eda<=98)
sdem_1t25 %>% 
  summarise(pop15=sum(fac_tri))

# Población ocupada
sdem_1t25 <- 
  sdem_1t25 %>% 
  filter(clase2==1)
sdem_1t25 %>% 
  summarise(pobocup=sum(fac_tri))

# Condicion de la ocupación
sdem_1t25 %>% 
  group_by(emp_ppal) %>% 
  summarise(pobocup=sum(fac_tri))

# Seleccionar variables
sdem_1t25 <- 
  sdem_1t25 %>% 
  select(upm,est_d_tri,fac_tri,t_loc_tri,ent,cd_a,sex,
         eda,eda7c,n_hij,seg_soc,rama,ing7c,ingocup,
         hij5c,anios_esc,hrsocup,emp_ppal,medica5c)

# Etiquetas
sdem_1t25 <- 
  as_factor(sdem_1t25)
sdem_1t25

# Marginación
marginacion_entidades <- tibble(
  ent = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche",
           "Coahuila de Zaragoza", "Colima", "Chiapas", "Chihuahua", "Ciudad de México",
           "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "México",
           "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", "Puebla",
           "Querétaro de Arteaga", "Quintana Roo", "San Luis Potosí", "Sinaloa",
           "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz de Ignacio de la Llave",
           "Yucatán", "Zacatecas"),
  indice_marginacion = c("Muy bajo", "Bajo", "Bajo", "Alto", "Muy bajo", "Bajo", "Muy alto",
                         "Medio", "Muy bajo", "Alto", "Medio", "Muy alto", "Alto", "Bajo",
                         "Bajo", "Alto", "Medio", "Alto", "Muy bajo", "Alto", "Alto", "Bajo",
                         "Medio", "Medio", "Bajo", "Bajo", "Alto", "Bajo", "Medio", "Alto",
                         "Alto", "Medio"))
marginacion_entidades

# Fusión de base y data.frame
sdem_1t25 <- 
  sdem_1t25 %>% 
  inner_join(marginacion_entidades,by="ent")

# Convertir en factor
sdem_1t25 <- 
  sdem_1t25 %>% 
  mutate(indice_marginacion=as.factor(indice_marginacion))

# Filtrar base
# Escolaridad
summary(sdem_1t25$anios_esc)
sdem_1t25 <- 
  sdem_1t25 %>% 
  filter(anios_esc<=24)

# Edad
summary(sdem_1t25$eda)
sdem_1t25 <- 
  sdem_1t25 %>% 
  filter(eda<=97)

# Tabulados básicos
# Variables numéricas
sdem_1t25 %>% 
  count(emp_ppal,wt=fac_tri) %>% 
  mutate(pct=n/sum(n)*100)

sdem_1t25 %>% 
  group_by(medica5c) %>% 
  count(emp_ppal,wt=fac_tri) %>%
  pivot_wider(names_from = emp_ppal,values_from = n, values_fill = 0) %>% 
  ungroup() %>% 
  mutate(total=rowSums(select(.,2:3),na.rm = TRUE),
         pct_informal= `Empleo informal`/total*100,
         pct_formal= `Empleo formal`/total*100)

# Horas
sdem_1t25 %>% 
  group_by(emp_ppal) %>% 
  summarise(hrs=weighted.mean(hrsocup,fac_tri))

# Ingreso mensual
sdem_1t25 %>% 
  group_by(emp_ppal) %>% 
  summarise(ingreso=weighted.mean(ingocup,fac_tri))

# Tasa de informalidad por categoria de edad
sdem_1t25 %>% 
  filter(emp_ppal=="Empleo informal") %>% 
  count(eda7c,wt=fac_tri)

sdem_1t25 %>% 
  group_by(emp_ppal) %>% 
  count(eda7c,wt=fac_tri) %>%
  pivot_wider(names_from = emp_ppal,values_from = n,values_fill = 0) %>% 
  ungroup() %>% 
  mutate(
    total = `Empleo informal` + `Empleo formal`,
    pct_informal = `Empleo informal` / total * 100,
    pct_formal   = `Empleo formal` / total * 100)

# Informalidad de mujeres por número de hijos
sdem_1t25 %>%
  count(hij5c, emp_ppal, wt = fac_tri) %>%
  filter(hij5c != "No aplica", hij5c != "No especificado") %>%
  group_by(hij5c) %>%
  mutate(
    total = sum(n),
    pct = n / total * 100
  ) %>%
  ungroup()

# Años de escolaridad
sdem_1t25 %>% 
  count(emp_ppal,anios_esc,wt=fac_tri) %>% 
  group_by(anios_esc) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  ungroup()

sdem_1t25 %>% 
  count(emp_ppal,anios_esc,wt=fac_tri) %>% 
  group_by(anios_esc) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  ungroup() %>% 
  filter(emp_ppal=="Empleo informal") %>% 
  ggplot(aes(x=anios_esc,y=pct))+
  geom_line()+
  theme_bw()

# Etiquetas
sdem_1t25$emp_ppal <- droplevels(sdem_1t25$emp_ppal)

# Modificación
sdem_1t25 <- 
  sdem_1t25 %>% 
  mutate(empleo=if_else(emp_ppal=="Empleo formal",1,0),
         eda2=eda*eda)
sdem_1t25
unique(sdem_1t25$rama)

# Modelo logit
options(scipen = 5)
probit <- 
  glm(empleo~sex+eda+eda2+anios_esc+indice_marginacion,
    weights = fac_tri,
    data = sdem_1t25,
    family = binomial(link = "probit"))
summary(probit)

library(margins)
mfx <- 
  margins(probit,data=sdem_1t25)
summary(mfx)

# Extraer las deviance del modelo
null_dev <- probit$null.deviance
resid_dev <- probit$deviance

# Calcular log-verosimilitudes
ll_null <- -null_dev / 2
ll_model <- -resid_dev / 2

# Calcular R² de McFadden
R2_McFadden <- 1 - (ll_model / ll_null)
R2_McFadden

library(pROC)
roc_obj <- roc(response = sdem_1t25$empleo,
               predictor = predict(probit, type = "response"),
               weights = sdem_1t25$fac_tri,
               quiet = TRUE)
plot(roc_obj, print.thres = TRUE)

# Mejor umbral según el índice de Youden
opt <- 
  coords(roc_obj, x = "best", 
         best.method = "youden", 
         ret = c("threshold", "sensitivity", "specificity"))
opt

# Diseño de encuesta
library(survey)
options(survey.lonely.psu = "adjust")  # Por si hay estratos con 1 PSU
sdem_1t25 <- 
  sdem_1t25 %>% 
  mutate(bandera=1)
disenio_enoe <- svydesign(
  ids = ~upm,
  strata = ~est_d_tri,
  weights = ~fac_tri,
  data = sdem_1t25,
  nest = TRUE)

# Tabulados
# Ingreso
svymean(~ingocup,design = disenio_enoe)
svyby(~ingocup,~sex,design = disenio_enoe,FUN = svymean)
confint(svyby(~ingocup,~sex,design = disenio_enoe,FUN = svymean),level=0.90)

# Tipo de empleo
svytable(~emp_ppal,design = disenio_enoe)
svytable(~emp_ppal,design = disenio_enoe,Ntotal=TRUE)*100

# Regresion
probit_svy <- 
  svyglm(empleo~sex+eda+eda2+anios_esc+indice_marginacion,
         design = disenio_enoe,
         family = binomial(link = "probit"))
summary(probit_svy)

# disenio_enoe$variables$indice_marginacion <- relevel(disenio_enoe$variables$indice_marginacion,
#                                                      ref = "Muy bajo")

# Obtener efectos marginales aproximados a partir del modelo
mfx_svy <- 
  margins(probit_svy,design=disenio_enoe)
summary(mfx_svy)

# FIN