library(tidyverse)
library(ggplot2)
library(readxl)

### SECCION 1: NACIDOS VIVOS ARGENTINA

#LECTURA DE DATOS
nac05<-read.csv("datos/nacweb05.csv") %>% 
  mutate(ANIO=2005)
nac06<-read.csv("datos/nacweb06.csv")%>% 
  mutate(ANIO=2006)
nac07<-read.csv("datos/nacweb07.csv")%>% 
  mutate(ANIO=2007)
nac08<-read.csv("datos/nacweb08.csv")%>% 
  mutate(ANIO=2008)
nac09<-read.csv("datos/nacweb09.csv")%>% 
  mutate(ANIO=2009)
nac10<-read.csv("datos/nacweb10.csv")%>% 
  mutate(ANIO=2010)
nac11<-read.csv("datos/nacweb11.csv")%>% 
  mutate(ANIO=2011)
nac12<-read.csv("datos/nacweb12.csv")%>% 
  mutate(ANIO=2012)
nac13<-read.csv("datos/nacweb13.csv")%>% 
  mutate(ANIO=2013)
nac14<-read.csv("datos/nacweb14.csv")%>% 
  mutate(ANIO=2014)
nac15<-read.csv("datos/nacweb15.csv")%>% 
  mutate(ANIO=2015)
nac16<-read.csv("datos/nacweb16.csv")%>% 
  mutate(ANIO=2016)
nac17<-read.csv("datos/nacweb17.csv")%>% 
  mutate(ANIO=2017)
nac18<-read.csv("datos/nacweb18.csv")%>% 
  mutate(ANIO=2018)
nac19<-read.csv("datos/nacweb19.csv")%>% 
  mutate(ANIO=2019)
nac20<-read.csv("datos/nacweb20.csv",sep = ";")%>% 
  mutate(ANIO=2020)%>% 
  rename("PROVRES"="ï..PROVRES")
nac21<-read.csv("datos/nacweb21.csv",sep =";")%>% 
  mutate(ANIO=2021) %>% 
  rename("PROVRES"="ï..PROVRES")
nac22<-read.csv("datos/nacweb22.csv",sep = ";")%>% 
  mutate(ANIO=2022) %>% 
  rename("PROVRES"="ï..PROVRES")

cod_prov <- data.frame(
  PROVRES = c("2", "6", "10", "14", "18", "22", "26", "30", "34", "38", 
             "42", "46", "50", "54", "58", "62", "66", "70", "74", "78", 
             "82", "86", "90", "94", "98", "99"),
  PROVINCIA = c("CABA", "Buenos Aires", "Catamarca", "Córdoba", 
            "Corrientes", "Chaco", "Chubut", "Entre Ríos", "Formosa", "Jujuy", 
            "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro", 
            "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", 
            "Santiago del Estero", "Tucumán", "Tierra del Fuego", "Otro país", 
            "Lugar no especificado")
) %>% 
mutate(PROVRES=as.integer(PROVRES))


#cCOMPILACION DE DATOS
Acumulada<-rbind(nac05,nac06)
Acumulada<-rbind(Acumulada,nac07)
Acumulada<-rbind(Acumulada,nac08)
Acumulada<-rbind(Acumulada,nac09)
Acumulada<-rbind(Acumulada,nac10)
Acumulada<-rbind(Acumulada,nac11)
Acumulada<-rbind(Acumulada,nac12)
Acumulada<-rbind(Acumulada,nac13)
Acumulada<-rbind(Acumulada,nac14)
Acumulada<-rbind(Acumulada,nac15)
Acumulada<-rbind(Acumulada,nac16)
Acumulada<-rbind(Acumulada,nac17)
Acumulada<-rbind(Acumulada,nac18)
Acumulada<-rbind(Acumulada,nac19)
Acumulada<-rbind(Acumulada,nac20)
Acumulada<-rbind(Acumulada,nac21)
todocrudo<-rbind(Acumulada,nac22)

todo<-todocrudo %>% 
  left_join(cod_prov, by="PROVRES") %>% 
  select(-TIPPARTO,-ITIEMGEST,-PROVRES)
  

NAC_ANUALES_ARG<-todo %>%
  group_by(ANIO) %>% 
  summarise(TOTAL=sum(CUENTA))

NAC_ANUALES_PROV<-todo %>%
  group_by(PROVINCIA,ANIO) %>% 
  summarise(TOTAL=sum(CUENTA)) 

ggplot(NAC_ANUALES_PROV, aes(x = ANIO, y = TOTAL, color = PROVINCIA, group = PROVINCIA)) +
  geom_line(aes(size = ifelse(PROVINCIA %in% c("Misiones", "CABA","Buenos Aires"), 1.5, 0.5))) +  
  scale_size_identity() +  # Mantiene los tamaños especificados
  labs(title = "Gráfico de Múltiples Líneas",
       x = "Tiempo",
       y = "Valor",
       color = "Variables") +
  theme_minimal()+
  guides(color = guide_legend(ncol = 1))

### SECCION 2: NATALIDAD ARGENTINA
#Def: nacimientos cada mil habitantes en un tiempo determinado

NAT <- read.csv("datos/NATALIDADDEIS2000-2022.csv") %>% 
  rename("ANIO" = "indice_tiempo") %>% 
  mutate(ANIO = as.integer(substr(ANIO, 1, 4))) %>%
  rename_with(~ c("ANIO", "totalARG", "CABA", "Buenos Aires", "Catamarca", "Córdoba",  
                  "Corrientes", "Chaco", "Chubut", "Entre Ríos", "Formosa", "Jujuy", 
                  "La Pampa", "La Rioja", "Mendoza", "Misiones", "Neuquén", "Río Negro", 
                  "Salta", "San Juan", "San Luis", "Santa Cruz", "Santa Fe", 
                  "Santiago del Estero", "Tucumán", "Tierra del Fuego"), 
              .cols = everything())

# Reestructurar el dataframe de formato "wide" a "long"
NAT_long <- NAT %>%
  pivot_longer(-ANIO, names_to = "PROVINCIA", values_to = "valor")

# Crear el gráfico de líneas
ggplot(NAT_long, aes(x = ANIO, y = valor, color = PROVINCIA, group = PROVINCIA)) +
  geom_line(aes(size = ifelse(PROVINCIA %in% c("Misiones","CABA", "totalARG"), 0.7, 0.5))) +
  labs(title = "Gráfico de Múltiples Líneas",
       x = "Tiempo",
       y = "Valor",
       color = "Variables") +
  theme_minimal()

ggplot(NAT_long, aes(x = ANIO, y = valor, color = PROVINCIA, group = PROVINCIA)) +
  geom_line(aes(size = ifelse(PROVINCIA %in% c("Misiones", "CABA", "totalARG"), 1.5, 0.5))) +  
  scale_size_identity() +  # Mantiene los tamaños especificados
  labs(title = "Gráfico de Múltiples Líneas",
       x = "Tiempo",
       y = "Valor",
       color = "Variables") +
  theme_minimal()+
  guides(color = guide_legend(ncol = 1))

#MINMOS Y MAXIMOS
           #AÑO 2000

#MINIMO
NAT_long %>% 
  filter(ANIO==2000) %>% 
  filter(valor== min(valor))

#MAXIMO
NAT_long %>% 
  filter(ANIO==2000) %>% 
  filter(valor== max(valor))

           #AÑO 2022
#MINIMO
NAT_long %>% 
  filter(ANIO==2022) %>% 
  filter(valor== min(valor))

#MAXIMO
NAT_long %>% 
  filter(ANIO==2022) %>% 
  filter(valor== max(valor))





### SECCION 3: FECUNDIDAD Banco mundial
#def: La tasa de fertilidad o fecundidad total representa el número de hijos que tendría una mujer si viviera hasta el final de sus años fértiles
#y tuviera hijos de acuerdo con las tasas de fertilidad específicas por edad del año especificado.

FECBM <- read_xls("datos/fecundidadBM.xls") %>% 
  select(-"Indicator Code",-"Country Code", -"Indicator Name")

SeleccionFECBM <-FECBM %>% 
  filter(`Country Name`%in% c("Argentina","Brasil","Bolivia","México","Alemania","Estados Unidos","Corea, República de", "Federación de Rusia","Mundo"))

# Reestructurar el dataframe de formato "wide" a "long"
seleccion_long <- SeleccionFECBM %>%
  pivot_longer(-`Country Name`, names_to = "ANIO", values_to = "FEC") %>% 
  mutate(ANIO= as.integer(ANIO),
         "Country Name"= as.factor(`Country Name`)) %>% 
  filter(ANIO != 2023)


valor_arg_2022 <- seleccion_long %>%
  filter(`Country Name` == "Argentina" & ANIO == 2022) %>%
  pull(FEC)

ggplot(seleccion_long, aes(x = ANIO, y = FEC, color = `Country Name`, group = `Country Name`)) +
  geom_line(aes(size = ifelse(`Country Name` %in% c("Argentina", "Mundo"), 1.5, 0.5))) +
  geom_hline(yintercept = valor_arg_2022, linetype = "dashed", color = "red") +  # Línea horizontal de guía
  labs(title = "Gráfico de Múltiples Líneas",
       x = "Tiempo",
       y = "Valor",
       color = "País") +
  scale_x_continuous(breaks = seq(min(seleccion_long$ANIO), max(seleccion_long$ANIO), by = 10)) +  # Años de 10 en 10
  scale_y_continuous(breaks = seq(floor(min(seleccion_long$FEC)), ceiling(max(seleccion_long$FEC)), by = 0.25)) +  # Incrementos de 0.25 en el eje y
  scale_size_identity() +  # Mantiene los tamaños especificados
  theme_minimal() +
  geom_text(aes(x = 2022, y = valor_arg_2022, label = round(valor_arg_2022, 2)), 
            vjust = -1, color = "red")  # Añadir texto al lado de la línea
