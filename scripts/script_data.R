#library(feather)
library(dplyr)
library(sf)
library(hrbrthemes)
library(fst)
#library(here)
#library(leaflet)
#library(plotly)


####################### DATA

notas <-read.csv("~/projects/panel/features_engineering/ALL_SCHOOLS_v2.csv") 

Snotas <-read.csv("~/projects/panel/features_engineering/ALL_STUDENTS_v2_graphs.csv")

mungeo <- read.csv("~/data/geodata/mun_micro_meso.csv")

ufgeo <- ("~/data/geodata/uf.json") %>%
  st_read() 

mesogeo <- ("~/data/geodata/mesorregiao.json") %>%
  st_read() 

## Latitude Longitude Municipios e Codigos Meso e Microrregioes

geo_notas <- left_join(notas, mungeo, by = c("CO_MUNICIPIO" = 'codigo_ibge'))

Sgeo_notas <- left_join(Snotas, mungeo, by = c("CO_MUNICIPIO" = 'codigo_ibge'))


#microgeo<- ("~/data/geodata/microrregiao.json") %>%
#st_read() 


#----------------------------------------------------1ª Página -----------------------------------------------------------
######################### UF
ufnotas <-geo_notas %>% 
  group_by(CO_UF) %>%
  summarise(
    NU_NOTA_CH_MEAN = mean(NU_NOTA_CH),
    NU_NOTA_CN_MEAN = mean(NU_NOTA_CN),
    NU_NOTA_LC_MEAN = mean(NU_NOTA_LC),
    NU_NOTA_MT_MEAN = mean(NU_NOTA_MT),
    NU_NOTA_RED_MEAN = mean(NU_NOTA_REDACAO),
    NOTA_GERAL_MEAN = mean(NU_NOTA_GERAL),
    QUANTIDADE = n(),
    DP = sd(NU_NOTA_GERAL)) %>%
  ungroup() 

ufnotas$CO_UF<- as.factor(ufnotas$CO_UF)

ufnotas <- left_join(ufgeo, ufnotas, by = c("GEOCODIGO" = "CO_UF"))
#ufnotas <- st_set_geometry(ufnotas, NULL)

uf_years<-geo_notas %>%
  group_by(CO_UF, CO_ANO, IN_TP_ESCOLA) %>%
  summarise(
    NU_NOTA_CH_MEAN = mean(NU_NOTA_CH),
    NU_NOTA_CN_MEAN = mean(NU_NOTA_CN),
    NU_NOTA_LC_MEAN = mean(NU_NOTA_LC),
    NU_NOTA_MT_MEAN = mean(NU_NOTA_MT),
    NU_NOTA_RED_MEAN = mean(NU_NOTA_REDACAO),
    NOTA_GERAL_MEAN = mean(NU_NOTA_GERAL),
    DP_NOTA_GERAL = sd(NU_NOTA_GERAL),
    QUANTIDADE =  n(),
    TITULACAO = mean(TITULACAO),
    FEQ_INFRA_NENHUMA = mean(IN_INFRA_NENHUMA),
    FEQ_INFRA_BASICA = mean(IN_INFRA_BASICA),
    FEQ_INFRA_ELEMENTAR = mean(IN_INFRA_ELEMENTAR),
    FEQ_INFRA_ADEQUADA = mean(IN_INFRA_ADEQUADA),
    FEQ_INFRA_AVANCADA = mean(IN_INFRA_AVANCADA)) %>%
  ungroup()

uf_years$CO_UF<- as.factor(uf_years$CO_UF)

uf_years <- left_join(ufgeo, uf_years, by = c("GEOCODIGO" = "CO_UF"))
uf_years <-st_set_geometry(uf_years, NULL)

# #################### CITY

munnotas <- geo_notas %>%
  group_by(CO_MUNICIPIO)%>%
  summarise(
    NU_NOTA_CH_MEAN = mean(NU_NOTA_CH),
    NU_NOTA_CN_MEAN = mean(NU_NOTA_CN),
    NU_NOTA_LC_MEAN = mean(NU_NOTA_LC),
    NU_NOTA_MT_MEAN = mean(NU_NOTA_MT),
    NU_NOTA_RED_MEAN = mean(NU_NOTA_REDACAO),
    NOTA_GERAL_MEAN = mean(NU_NOTA_GERAL),
    DP_NOTA_GERAL = sd(NU_NOTA_GERAL),
    QUANTIDADE =  n()) %>%
  ungroup()

munnotas <- left_join(munnotas, mungeo, by =c('CO_MUNICIPIO'='codigo_ibge'))

mun_years <- geo_notas %>%
  group_by(CO_MUNICIPIO, CO_ANO, IN_TP_ESCOLA)%>%
  summarise(
    NU_NOTA_CH_MEAN = mean(NU_NOTA_CH),
    NU_NOTA_CN_MEAN = mean(NU_NOTA_CN),
    NU_NOTA_LC_MEAN = mean(NU_NOTA_LC),
    NU_NOTA_MT_MEAN = mean(NU_NOTA_MT),
    NU_NOTA_RED_MEAN = mean(NU_NOTA_REDACAO),
    NOTA_GERAL_MEAN = mean(NU_NOTA_GERAL),
    DP_NOTA_GERAL = sd(NU_NOTA_GERAL),
    QUANTIDADE =  n(),
    TITULACAO = mean(TITULACAO),
    FEQ_INFRA_NENHUMA = mean(IN_INFRA_NENHUMA),
    FEQ_INFRA_BASICA = mean(IN_INFRA_BASICA),
    FEQ_INFRA_ELEMENTAR = mean(IN_INFRA_ELEMENTAR),
    FEQ_INFRA_ADEQUADA = mean(IN_INFRA_ADEQUADA),
    FEQ_INFRA_AVANCADA = mean(IN_INFRA_AVANCADA)) %>%
  ungroup()

mun_years <- left_join(mun_years, mungeo, by =c('CO_MUNICIPIO'='codigo_ibge'))




######################### MESO
mesonotas <-geo_notas %>% 
  group_by(cod_uf_meso) %>%
  summarise(
    NU_NOTA_CH_MEAN = mean(NU_NOTA_CH),
    NU_NOTA_CN_MEAN = mean(NU_NOTA_CN),
    NU_NOTA_LC_MEAN = mean(NU_NOTA_LC),
    NU_NOTA_MT_MEAN = mean(NU_NOTA_MT),
    NU_NOTA_RED_MEAN = mean(NU_NOTA_REDACAO),
    NOTA_GERAL_MEAN = mean(NU_NOTA_GERAL),
    QUANTIDADE = n(),
    DP_NOTA_GERAL = sd(NU_NOTA_GERAL)) %>%
  ungroup() 

mesonotas$cod_uf_meso <- as.factor(mesonotas$cod_uf_meso)
mesonotas <- left_join(mesogeo, mesonotas,by =c('GEOCODIGO'= 'cod_uf_meso'))
#mesonotas<-st_set_geometry(mesonotas, NULL)


meso_years <-geo_notas %>% 
  group_by(cod_uf_meso,CO_ANO, IN_TP_ESCOLA) %>%
  summarise(
    NU_NOTA_CH_MEAN = mean(NU_NOTA_CH),
    NU_NOTA_CN_MEAN = mean(NU_NOTA_CN),
    NU_NOTA_LC_MEAN = mean(NU_NOTA_LC),
    NU_NOTA_MT_MEAN = mean(NU_NOTA_MT),
    NU_NOTA_RED_MEAN = mean(NU_NOTA_REDACAO),
    NOTA_GERAL_MEAN = mean(NU_NOTA_GERAL),
    DP_NOTA_GERAL = sd(NU_NOTA_GERAL),
    QUANTIDADE =  n(),
    TITULACAO = mean(TITULACAO),
    FEQ_INFRA_NENHUMA = mean(IN_INFRA_NENHUMA),
    FEQ_INFRA_BASICA = mean(IN_INFRA_BASICA),
    FEQ_INFRA_ELEMENTAR = mean(IN_INFRA_ELEMENTAR),
    FEQ_INFRA_ADEQUADA = mean(IN_INFRA_ADEQUADA),
    FEQ_INFRA_AVANCADA = mean(IN_INFRA_AVANCADA)) %>%
  ungroup() 

meso_years$cod_uf_meso <- as.factor(meso_years$cod_uf_meso)
meso_years <- left_join(mesogeo, meso_years,by =c('GEOCODIGO'= 'cod_uf_meso'))
meso_years<-st_set_geometry(meso_years, NULL)

## Save 
#write.fst(ufnotas, "ufnotas.fst")
save(ufnotas, file = "ufnotas.Rdata")

write.fst(munnotas, "munnotas.fst")
#write.fst(mesonotas, "mesonotas.fst")
save(mesonotas, file = "mesonotas.Rdata")

write.fst(uf_years, "uf_years.fst")
write.fst(mun_years, "mun_years.fst")
write.fst(meso_years, "meso_years.fst")
write.fst(Sgeo_notas, "Sgeo_notas.fst")
write.fst(geo_notas, "geo_notas.fst")



