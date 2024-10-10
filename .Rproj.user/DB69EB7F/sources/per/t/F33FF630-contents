library(tidyverse)
library(ggplot2)

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

### NATALIDAD

NAT<- read.csv("datos/NATALIDADDEIS2000-2022.csv") %>% 
  rename("ANIO"="indice_tiempo") %>% 
  mutate(ANIO=as.integer(substr(ANIO,1,4)))


# Reestructurar el dataframe de formato "wide" a "long"
NAT_long <- NAT %>%
  pivot_longer(-ANIO, names_to = "PROVINCIA", values_to = "valor")

# Crear el gráfico de líneas
ggplot(NAT_long, aes(x = ANIO, y = valor, color = PROVINCIA, group = PROVINCIA)) +
  geom_line() +
  labs(title = "Gráfico de Múltiples Líneas",
       x = "Tiempo",
       y = "Valor",
       color = "Variables") +
  theme_minimal()


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



