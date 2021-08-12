#library(feather)
library(tidyverse)
library(sf)
library(hrbrthemes)
#library(here)
#library(leaflet)
#library(plotly)


####################### DATA

notas <-read.csv("~/projects/panel/features_engeneering/ALL_SCHOOLS_v2.csv") 

Snotas <-read.csv("~/projects/panel/features_engeneering/ALL_STUDENTS_v2_graphs.csv")

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
 
 
 
#----------------------------------------------------2ª Página -----------------------------------------------------------
###  over years
dim<-c(quo(TP_COR_RACA), quo(RENDA_MENSAL), quo(EDU_PAI), quo(EDU_MAE), quo(TP_SEXO)) 
dimensions_nacional <- list()
for (i in seq_along(dim)) {

var<- as.character(dim[i])
var<- str_sub(var, 2)


df<-Sgeo_notas%>%
   dplyr::select(NU_NOTA_GERAL, CO_ANO, CO_UF, TP_COR_RACA, CO_DEPENDENCIA_ADM, RENDA_MENSAL, 
                 TP_SEXO, NU_NOTA_GERAL, EDU_PAI, EDU_MAE)%>%
   group_by(CO_ANO)%>%
   mutate(QUARTILE = ntile(NU_NOTA_GERAL, 4))%>% ungroup()%>% 
   group_by(CO_ANO, !!dim[[i]])%>%
   mutate(
     N_DIM = n())%>%
   ungroup()%>%filter(QUARTILE ==4)%>% mutate( i = row_number(), dummy = 1)%>% 
   spread('CO_DEPENDENCIA_ADM', 'dummy', fill = 0)%>% dplyr::select(-i)%>% 
   group_by(CO_ANO, !!dim[[i]])%>%
   summarise(
     N_DIM_Q = n(),
     FEDERAL = sum( `Federal`)/N_DIM_Q,
     ESTADUAL_MUNICIPAL = sum(`Municipal+Estadual`)/N_DIM_Q,
     PRIVADA = sum(`Privada`)/N_DIM_Q,
     FRACAO = N_DIM_Q/N_DIM)%>%
   ungroup()%>%distinct()
 
dimensions_nacional[[var]] <- df
}

#----------------------------------------------------3ª Página ----------------------------------------------------------- 

geo_notas_nacional<-geo_notas%>% 
  dplyr::select(NU_NOTA_GERAL, CO_ANO, CO_DEPENDENCIA_ADM, TITULACAO, IN_INFRA_NENHUMA, IN_INFRA_ELEMENTAR, 
          IN_INFRA_BASICA, IN_INFRA_ADEQUADA, IN_INFRA_AVANCADA)
 
geo_notas_nacional<- geo_notas_nacional%>%
   group_by(CO_ANO)%>%
   mutate(QUARTILE = ntile(NU_NOTA_GERAL, 4))%>% ungroup()%>%gather(INFRA, QUANTIDADE, 5:9)%>% 
   group_by(CO_ANO, INFRA)%>%
   mutate(
     N_DIM = sum(QUANTIDADE))%>%
   ungroup()


geo_notas_nacional<-geo_notas_nacional%>%
  filter(QUARTILE ==4)%>% dplyr::select(-QUARTILE)%>%
  group_by(CO_ANO, INFRA, CO_DEPENDENCIA_ADM)%>%
  mutate(FRACAO = sum(QUANTIDADE))%>%ungroup()%>%
  group_by(CO_ANO, INFRA)%>%
  mutate(
     N_DIM_Q = sum(QUANTIDADE),
     PADRAO = (N_DIM_Q *0.25)/N_DIM,
     PROBABILIDADE = N_DIM_Q/N_DIM,
     FRACAO = FRACAO,
     TITULACAO = mean(TITULACAO))%>%ungroup()%>%replace(is.na(.), 0)

geo_notas_nacional$PADRAO<- if_else(geo_notas_nacional$PADRAO  > 1, 1, geo_notas_nacional$PADRAO)
  
geo_notas_nacional<-geo_notas_nacional%>%dplyr::select(CO_ANO, INFRA, PROBABILIDADE, PADRAO, N_DIM, N_DIM_Q, CO_DEPENDENCIA_ADM, FRACAO)%>%distinct()

geo_notas_nacional<-geo_notas_nacional%>%pivot_wider(names_from = "CO_DEPENDENCIA_ADM", values_from = "FRACAO")


geo_notas_nacional<-geo_notas_nacional%>%
  mutate(
    FEDERAL = `1`/N_DIM_Q,
    ESTADUAL = `2`/N_DIM_Q,
    MUNICIPAL = `3`/N_DIM_Q,
    PRIVADA = `4`/N_DIM_Q)%>%
  replace(is.na(.), 0)

#----------------------------------------

#--------------------------------------- 5º Página
ia_group_analysis <- read_csv("~/projects/panel/features_engeneering/RESULTS_GROUP_ANALYSIS.csv")


# fig<- ia_group_analysis%>%group_by(year, group)%>%summarise(AUC = mean(auc))%>%ungroup()
# 
# fig<- fig%>% pivot_wider(names_from = "tp_escola", values_from = "AUC")
# fig
# plot_ly(fig)%>%
#   add_lines(x= ~year, y= ~AUC, color = ~group, type= 'scatter', mode= 'markers+lines')

################# SAVE DATA

save(ufnotas, file = "~/projects/panel/data/ufnotas.Rdata")
save(munnotas, file = "~/projects/panel/data/munnotas.Rdata")

save(uf_years, file = "~/projects/panel/data/uf_years.Rdata")
save(mun_years, file = "~/projects/panel/data/mun_years.Rdata")

save(mesonotas, file = "~/projects/panel/data/mesonotas.Rdata")
save(meso_years, file = "~/projects/panel/data/meso_years.Rdata")

#save(infra_years, file = "~/projects/panel/data/infra_years.Rdata")

save(Sgeo_notas, file = "~/projects/panel/data/Sgeo_notas.Rdata")
save(geo_notas, file = "~/projects/panel/data/geo_notas.Rdata")

save(geo_notas_nacional, file = "~/projects/panel/data/geo_notas_nacional.Rdata")

save(dimensions_nacional, file = "~/projects/panel/data/dimensions_nacional.Rdata")

save(ia_group_analysis, file = "~/projects/panel/data/ia_group_analysis.Rdata")


#

graf_edm<- geo_notas%>%mutate(CO_DEPENDENCIA_ADM = 
                                if_else(geo_notas$CO_DEPENDENCIA_ADM!=4, 'Public', 'Private'))%>%
  filter(CO_DEPENDENCIA_ADM=='Public')
graf_edm<- graf_edm%>% group_by(CO_ANO)%>%
  summarise(
    N = n(),
    NU_NOTA_GERAL = mean(NU_NOTA_GERAL),
    None = mean(IN_INFRA_NENHUMA),
    Elementary = mean(IN_INFRA_ELEMENTAR),
    Basic = mean(IN_INFRA_BASICA),
    Adequate = mean(IN_INFRA_ADEQUADA),
    Advanced = mean(IN_INFRA_AVANCADA)
  )%>%ungroup()%>%
  gather("Infrastructure", "Freq", Basic, Elementary, 
         Adequate,Advanced, None )


ggplot(graf_edm, aes(fill=Infrastructure, y=Freq, x=CO_ANO)) + 
  geom_bar(position="stack", stat="identity")+
  scale_fill_viridis(discrete = T, option = 'cividis')+
  theme_ipsum() +
  xlab("Census years")+
  ylab("Fraction")

graf_edm
glimpse(ia_group_analysis)


fig52<- ia_group_analysis%>%group_by(year, tp_escola, group)%>%
  summarise(AUC = mean(auc))%>%ungroup()%>%filter(tp_escola =='Municipal+Estadual')


fig52$group<-if_else(fig52$group=="HYPOTHESIS", "EDUCATION", )
fig52<-fig52 %>%
  mutate(group = case_when(
    group == "FULL" ~ "All features",
    group == "HYPOTHESIS" ~ "Education",
    group == "INFRA" ~"Infrastructure",
    group =="SOCIO" ~"SEC",
    TRUE ~ group
  ))

fig52<- fig52%>% pivot_wider(names_from = "tp_escola", values_from = "AUC")
plot_ly(fig52, x= ~year, y= ~`Municipal+Estadual`, color = ~group, type= 'scatter', 
        mode= 'markers+lines')%>%layout(yaxis = list(title = 'AUC - Logistic Regression'))
  
  ggplot( aes(x=year, y=`Municipal+Estadual`)) +
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
    theme_ipsum() +
    ggtitle("Evolution of bitcoin price")
  

p31<-geo_notas%>% group_by(CO_ANO, IN_TP_ESCOLA)%>%
    summarise(
      S_GRADUACAO = sum(NU_GRADUACAO),
      S_ESPECIALIZACAO = sum(NU_ESPECIALIZACAO),
      S_MESTRADO = sum(NU_MESTRADO),
      S_DOUTORADO = sum(NU_DOUTORADO),
      N = S_GRADUACAO+S_ESPECIALIZACAO+S_MESTRADO+S_DOUTORADO,
      GRADUACAO = S_GRADUACAO/N,
      ESPECIALIZACAO = S_ESPECIALIZACAO/N,
      MESTRADO = S_MESTRADO/N,
      DOUTORADO = S_DOUTORADO/N)%>%
    ungroup()%>%
    gather("DIME", "FREQ", GRADUACAO, ESPECIALIZACAO,
           MESTRADO,DOUTORADO)



p31 <- ggplot(p31, aes(x = IN_TP_ESCOLA, y = FREQ, fill = DIME, text = paste( N, 'Escolas',
                                                                              '<br>', round(FREQ, 2)*100, '%', '=', DIME))) + scale_fill_viridis(discrete = T, option = 'viridis')+
  xlab("") + ylab("") + theme(legend.title =element_blank(),legend.key.size =unit(1.2, "lines"))+ 
  geom_bar(stat = 'identity', position = 'stack') + theme(axis.text.x = element_text(size=7, angle=28),
                                                          axis.text.y = element_text(size=7)) + 
  facet_grid(~ CO_ANO) + 
  theme(strip.text.x = element_text(size = 8))

p31
glimpse(p31)


ggplotly(p31, tooltip = "text")%>%
  layout(hovermode = "x", legend = list(font = list(size=7)))

input =1

case_when(
  input == 1 ~ filter = c(1,2,3,4),
  input == 2 ~ filter = 1)



df23<- Sgeo_notas%>%filter(case_when(
  input == 1 ~ CO_DEPENDENCIA_ADM %in% c("Municipal+Estadual", "Privada"),
  input == 2 ~ CO_DEPENDENCIA_ADM %in%"Municipal+Estadual"
))%>% dplyr::select(NU_NOTA_GERAL, CO_ANO, CO_UF, TP_COR_RACA, CO_DEPENDENCIA_ADM, RENDA_MENSAL, TP_SEXO, NU_NOTA_GERAL, EDU_PAI, EDU_MAE)
group_cols <- c("CO_ANO", "TP_COR_RACA")

df23<-df23%>%
  group_by(CO_ANO)%>%
  mutate(QUARTILE = ntile(NU_NOTA_GERAL, 4))%>% ungroup()
df23<-df23%>% 
  group_by(!!!syms(group_cols))%>%
  mutate(
    N_DIM = n())%>%
  ungroup()

df23<-df23%>%filter(QUARTILE ==4)


df23<-cbind(df23, dummy( df23$CO_DEPENDENCIA_ADM, drop = FALSE, sep= '_'))

df23<- df23 %>%rename('Federal' = 12, 'Municipal_Estadual' = 13, 'Privada'=14)


df23 %>% group_by(!!!syms(group_cols))%>%
  summarise(
    N_DIM_Q = n(),
    FEDERAL = sum(Federal)/N_DIM_Q,
    ESTADUAL_MUNICIPAL = sum(Municipal_Estadual)/N_DIM_Q,
    PRIVADA = sum(Privada)/N_DIM_Q,
    FRACAO = N_DIM_Q/N_DIM)%>%
  ungroup()%>%distinct()


df13<- geo_notas%>%filter(CO_UF == 11)

filter ='6'


df13<- geo_notas%>%filter(
  case_when(
    filter == '1' ~ IN_TP_ESCOLA %in% c("Municipal+Estadual", "Federal","Privada"),
    filter == '2' ~ IN_TP_ESCOLA %in% "Privada",
    filter == '3' ~ IN_TP_ESCOLA %in% c("Municipal+Estadual", "Federal"),
    filter == '4' ~ IN_TP_ESCOLA %in% "Municipal+Estadual",
    filter == '5' ~ IN_TP_ESCOLA %in% "Federal",
    filter == '6' ~ IN_TP_ESCOLA %in% c("Federal", "Privada")
  ))%>%
  dplyr::select(NU_NOTA_GERAL, CO_ANO, IN_TP_ESCOLA, TITULACAO, IN_INFRA_NENHUMA, IN_INFRA_ELEMENTAR, 
         IN_INFRA_BASICA, IN_INFRA_ADEQUADA, IN_INFRA_AVANCADA)


df13<- df13%>%
  group_by(CO_ANO)%>%
  mutate(QUARTILE = ntile(NU_NOTA_GERAL, 4))%>% ungroup()%>%gather(INFRA, IN_INFRA, 5:9)%>% 
  group_by(CO_ANO, INFRA)%>%
  mutate(
    N_DIM = sum(IN_INFRA))%>%ungroup()%>%
  filter(QUARTILE ==4)%>%
  group_by(CO_ANO, INFRA, IN_TP_ESCOLA)%>%
  mutate(FRACAO = sum(IN_INFRA))%>%ungroup()%>%
  group_by(CO_ANO, INFRA)%>%
  mutate(
    N_DIM_Q = sum(IN_INFRA),
    PROBABILIDADE = N_DIM_Q/N_DIM)%>%ungroup()
         
df13<-df13%>%dplyr::select(CO_ANO, INFRA, PROBABILIDADE, N_DIM, N_DIM_Q, IN_TP_ESCOLA, FRACAO)%>%distinct()    
df13<-df13 %>% pivot_wider(names_from = "IN_TP_ESCOLA", values_from = "FRACAO")%>%
  
df13%>%
  mutate(Privada = case_when( 'Privada' %in% names(df13) ~ Privada/N_DIM_Q))%>%
  mutate(`Municipal+Estadual`= case_when('Municipal+Estadual' %in% names(df13) ~ `Municipal+Estadual`/N_DIM_Q))%>%
  mutate(Federal = case_when('Federal' %in% names(df13) ~ Federal/N_DIM_Q))
    


    case_when'`Municipal+Estadual`' %in% names(df13) ~ `Municipal+Estadual`/N_DIM_Q),
    case_when( 'Federal' %in% names(df13) ~ Federal/N_DIM_Q))


