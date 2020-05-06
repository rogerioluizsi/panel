#library(feather)
library(tidyverse)
library(sf)
#library(here)
#library(leaflet)
#library(plotly)


####################### DATA

notas %>%
  select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))


mungeo$codigo_ibge %>% n_distinct()

geo_notas$CO_MUNICIPIO %>% n_distinct()

geo_notas%>% tally()

notas <-read.csv("~/projects/panel/features_engeneering/ALL_YEARS.csv") 
mungeo <- read_csv("~/data/geodata/mun_micro_meso.csv") %>% 
  select (codigo_ibge, cod_uf_meso, cod_meso_micro, latitude, longitude)

## Latitude Longitude Municipios e Codigos Meso w Microrregioes

geo_notas <- left_join(notas, mungeo, by = c("CO_MUNICIPIO" = 'codigo_ibge'))


ufgeo <- ("~/data/geodata/uf.json") %>%
  st_read() 

mesogeo <- ("~/data/geodata/mesorregiao.json") %>%
  st_read() 

#microgeo<- ("~/data/geodata/microrregiao.json") %>%
  #st_read() 



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
  
ufnotas$CO_UF<- as_factor(ufnotas$CO_UF)

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
    FEQ_INFRA_ELEMENTAR = sum(IN_INFRA_ELEMENTAR)/ QUANTIDADE,
    FEQ_INFRA_BASICA = sum(IN_INFRA_BASICA)/QUANTIDADE,
    FEQ_INFRA_ADEQUADA = sum(IN_INFRA_ADEQUADA)/QUANTIDADE) %>%
  ungroup()

uf_years$CO_UF<- as_factor(uf_years$CO_UF)

uf_years <- left_join(ufgeo, uf_years, by = c("GEOCODIGO" = "CO_UF")) %>%

uf_years<-  uf_years %>% select(UF_05, )


library(plotly)

data<- uf_years %>% filter (UF_05 =='MG')
fig <- plot_ly(x = sort(data$CO_ANO), y = data$NOTA_GERAL_MEAN,  type = 'scatter', mode = 'lines',
               legendgroup = 'group1', name = 'Zone 1 - Tree 1')
fig <- fig %>% add_lines(y = data$IN_TP_ESCOLA == 'PUBLICA', legendgroup = 'GROUP1', name = 'ESTADUAL + MUNICIPAL')
fig <- fig %>% add_lines(y = data$IN_TP_ESCOLA == 'PRIVADA', legendgroup = 'GROUP1', name = 'PRIVADA')
fig <- fig %>% aadd_lines(y = data$IN_TP_ESCOLA == 'FEDERAL', legendgroup = 'GROUP1', name = 'FEDERAL')
fig
fig <- fig %>% add_trace(y = ~Tree5, legendgroup = 'group1', name = 'Zone 1 - Tree 3')

plot_ly(x = sort(data$CO_ANO), y = data$NOTA_GERAL_MEAN) %>%
  add_lines(linetype = data$IN_TP_ESCOLA)


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
     FEQ_INFRA_ELEMENTAR = sum(IN_INFRA_ELEMENTAR)/ QUANTIDADE,
     FEQ_INFRA_BASICA = sum(IN_INFRA_BASICA)/QUANTIDADE,
     FEQ_INFRA_ADEQUADA = sum(IN_INFRA_ADEQUADA)/QUANTIDADE) %>%
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
 
 mesonotas$cod_uf_meso <- as_factor(mesonotas$cod_uf_meso)
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
     FEQ_INFRA_ELEMENTAR = sum(IN_INFRA_ELEMENTAR)/ QUANTIDADE,
     FEQ_INFRA_BASICA = sum(IN_INFRA_BASICA)/QUANTIDADE,
     FEQ_INFRA_ADEQUADA = sum(IN_INFRA_ADEQUADA)/QUANTIDADE) %>%
   ungroup() 
 
 meso_years$cod_uf_meso <- as_factor(meso_years$cod_uf_meso)
 meso_years <- left_join(mesogeo, meso_years,by =c('GEOCODIGO'= 'cod_uf_meso'))
 
################# SAVE DATA
 

save(ufnotas, file = "~/projects/panel/data/ufnotas.Rdata")
save(munnotas, file = "~/projects/panel/data/munnotas.Rdata")

save(uf_years, file = "~/projects/panel/data/uf_years.Rdata")
save(mun_years, file = "~/projects/panel/data/mun_years.Rdata")

save(mesonotas, file = "~/projects/panel/data/mesonotas.Rdata")
save(meso_years, file = "~/projects/panel/data/meso_years.Rdata")

class(ufnotas)


#_______ test

df2 <- uf_years %>% filter(UF_05 == 'MG') %>% st_set_geometry( NULL)






plot_ly(x = sort(df1$CO_ANO), y = df1$NOTA_GERAL_MEAN) %>%  
  add_lines(color = df1$IN_TP_ESCOLA, alpha = 0.5, line = list( width = 4, dash = 'dot'), 
            legendgroup = 'group1', name = list(list('1', '2', '3')))%>%
  add_trace(data=df2, x = ~CO_ANO, y = ~NOTA_GERAL_MEAN, type="scatter", mode = "lines", 
            color = ~IN_TP_ESCOLA,legendgroup = 'group2')


df1
p <- plot_ly()
for (i in sort(unique(df1$IN_TP_ESCOLA),decreasing = TRUE)) {
  p <- add_trace(p, 
                 data = df1[df1$IN_TP_ESCOLA==i,], 
                 y = ~NOTA_GERAL_MEAN, 
                 x = ~CO_ANO, 
                 type = 'scatter', 
                 mode = 'lines',
                 name = paste0(i, "", '(NACIONAL)'),
                 opacity = 0.8,
                 line = list( width = 2, dash = 'dot'),
                 legendgroup = 'group1'
                 )
}

p %>% add_trace(data=df2, x = ~CO_ANO, y = ~NOTA_GERAL_MEAN, type="scatter", mode = "lines", 
                color = ~IN_TP_ESCOLA,legendgroup = 'group2')





df1<-uf_years %>% group_by(CO_ANO, IN_TP_ESCOLA)%>% st_set_geometry( NULL) %>% summarise(NOTA_GERAL_MEAN = mean(NOTA_GERAL_MEAN)) %>%ungroup()


plot_ly() %>% 
  add_trace(data=df1, x = ~CO_ANO, y = ~NOTA_GERAL_MEAN, type= 'scatter', mode='lines', color = ~IN_TP_ESCOLA, 
            legendgroup = 'group1', showlegend= TRUE) %>% 
  add_trace(data=df2, x = ~CO_ANO, y = ~NOTA_GERAL_MEAN, type="scatter", mode = "lines", 
            color = ~IN_TP_ESCOLA,legendgroup = 'group2') 
fig  
df1 %>% plot_ly(x = ~CO_ANO, y= ~NOTA_GERAL_MEAN, type =  'scatter', mode= 'lines' ,color = ~IN_TP_ESCOLA)

data %>% group_by(CO_ANO, IN_TP_ESCOLA) %>% 
  summarise(NOTA_GERAL_MEAN = mean(NOTA_GERAL_MEAN))%>% 
  plot_ly( x= ~CO_ANO, y= ~NOTA_GERAL_MEAN) %>% add_lines(linetype = ~IN_TP_ESCOLA)

paste('Internet ', y_internet[12], '%')
data 


data %>%
  group_by(CO_DEPENDENCIA_ADM, CO_ANO) %>%
  summarise(NU_NOTA_GERAL = mean(NU_NOTA_GERAL))  %>% add_lines(linetype = data$IN_TP_ESCOLA)

data %>%
  group_by(CO_DEPENDENCIA_ADM, CO_ANO) %>%
  summarise(NOTA_GERAL_MEAN) %>%
  ggplot(aes(x=CO_ANO, y=NOTA_GERAL_MEAN, group =as_factor(CO_DEPENDENCIA_ADM), color=as_factor(CO_DEPENDENCIA_ADM))) + 
  geom_line()+ theme_minimal() +
  ggtitle("NOTA GERAL AO LONGO DOS ANOS POR TIPO DE ESCOLA") 

fig <- plot_ly(
  type = 'scatter',  
  x = data$CO_ANO,
  y = data$NOTA_GERAL_MEAN,
  group = as_factor(data$CO_DEPENDENCIA_ADM),
  color = as_factor(data$CO_DEPENDENCIA_ADM),
  text = paste("Competências: ", rownames(data$CO_DEPENDENCIA_ADM),
               "<br>CH: ", data$NU_NOTA_CH_MEAN,
               "<br>LC: ", data$NU_NOTA_LC_MEAN,
               "<br>MT: ", data$NU_NOTA_MT_MEAN,
               "<br>CN: ", data$NU_NOTA_CN_MEAN,
               "<br>RED.: ", data$NU_NOTA_RED_MEAN),

  
  
  hoverinfo = 'text',
  mode = 'lines+markers'
  
  
)

fig
summary(data)



UF <- "MG"  #get_data()
df3<- geo_notas %>% filter(cod_uf_meso == 3110)

geo_notas %>% filter( CO_ANO== 2018)



df3 %>% 
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
    FEQ_INFRA_ELEMENTAR = sum(IN_INFRA_ELEMENTAR)/ QUANTIDADE,
    FEQ_INFRA_BASICA = sum(IN_INFRA_BASICA)/QUANTIDADE,
    FEQ_INFRA_ADEQUADA = sum(IN_INFRA_ADEQUADA)/QUANTIDADE) %>%
  ungroup() 

summary(df3)

fig2 <- plot_ly(
  type = 'scatter',  
  x = df3$CO_ANO,
  y = df3$FEQ_INFRA_ELEMENTAR,
  group = as_factor(df3$IN_TP_ESCOLA),
  color = as_factor(df3$IN_TP_ESCOLA),
  #text = paste("N: ", df3$QUANTIDADE),
  hoverinfo = 'text',
  mode = 'lines+markers')

fig2


teste <-mun_years %>% filter(COD_MUNICIPIO == 3106200)
library(plotly)
plot_ly(
  type = 'scatter',  
  x = sort(teste$CO_ANO),
  y = teste$NOTA_GERAL_MEAN,
  group = as_factor(teste$IN_TP_ESCOLA),
  color = as_factor(teste$IN_TP_ESCOLA),
  text = paste("N: ", teste$QUANTIDADE,
               "<br>CH: ", teste$NU_NOTA_CH_MEAN,
               "<br>LC: ", teste$NU_NOTA_LC_MEAN,
               "<br>MT: ", teste$NU_NOTA_MT_MEAN,
               "<br>CN: ", teste$NU_NOTA_CN_MEAN,
               "<br>RED.: ", teste$NU_NOTA_RED_MEAN,
               "<br>MÉDIA GERAL.: ", teste$NOTA_GERAL_MEAN),
  
  
  
  hoverinfo = 'text',
  mode = 'lines+markers'
  
  
)  
glimpse(mungeo)
mungeo %>% filter(cod_municipio==4216057)
filter(mungeo$longitude == 'NA')
summary(mungeo$longitude)
library(brazilmaps)



municipios1 <- read_csv("data/geodata/municipios.csv") %>% select(cod_municipio, cod_meso, nome_meso, cod_micro, nome_micro, nome_uf)
municipios2 <- read_csv("data/geodata/Municipios-Brasileiros-master/csv/municipios.csv")

municipios <- left_join(municipios2, municipios1, by = c("codigo_ibge" = "cod_municipio")) 


glimpse(municipios)

municipios<- municipios %>% unite(cod_uf_meso,  codigo_uf, cod_meso, sep = "", remove = FALSE)%>% 
  unite(cod_meso_micro, cod_meso, cod_micro, sep = "", remove = FALSE)

write.csv(municipios, "~/data/geodata/mun_micro_meso.csv",row.names = FALSE )


municipios$codigo_ibge %>% n_distinct()

geo_notas$COD_MUNICIPIO %>% n_distinct()
