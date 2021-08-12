library(broom)
library(tidyr)
library(dplyr)
#library(pscl)
library(pROC)
#library(plyr)
library(caret)
library(stringr)
library(sf)
library(DescTools)
library(Jmisc)
options(tibble.print_max = Inf)
set.seed(123)
#Ler Base de Dados
notas <-read.csv("~/projects/panel/features_engeneering/ALL_SCHOOLS_v2.csv")
notas<- notas%>%filter(IN_TP_ESCOLA== 'Municipal+Estadual')
uf <- ("~/data/geodata/uf.json") %>%
  st_read()%>%select(UF_05, GEOCODIGO)
st_geometry(uf) <- NULL 

notas$CO_UF<- as.factor(notas$CO_UF)
notas<- left_join(notas, uf, by = c("CO_UF" = "GEOCODIGO"))

### Functions 
build_target <- function(df){
  df<- df%>%mutate(
    TARGET = ntile(NU_NOTA_GERAL, 4))
  df$TARGET<- if_else(df$TARGET == 4, 1, 0)
  df<- df%>%dplyr::select(-NU_NOTA_GERAL)
  print(paste0(round(mean(df$TARGET)*100, 1), '%', ' upper quartil'))
  return(df)
}
drop_col_hight_mode<- function(df){
  
  for (i in colnames(df)){
    #print(i)
    if (i != "CO_ANO") {
      prop_mode<- sort(-table(df[[i]]))[1]/nrow(df)*(-1) # get mode value
        if (prop_mode > 0.9){
          df<- df%>%dplyr::select(-i)
            #print(i)
        }
    }
  }
  return(df)
}

## Only schools elementary infrastructure schools
df1<- notas%>%filter(IN_AGUA_INEXISTENTE == 0 & IN_ESGOTO_INEXISTENTE ==0 & IN_ENERGIA_INEXISTENTE == 0)
#df1<-notas%>%filter(IN_INFRA_ELEMENTAR==1)

## regularizing some quantitify variables that are binary in some years   
df1$QT_EQUIP_COPIADAORA <- if_else(df1$QT_EQUIP_COPIADORA>0, 1, 0)
df1$QT_EQUIP_DVD <- if_else(df1$QT_EQUIP_DVD>0, 1, 0) 
df1$QT_EQUIP_IMPRESSORA <- if_else(df1$QT_EQUIP_IMPRESSORA>0, 1, 0) 
df1$QT_EQUIP_TV <- if_else(df1$QT_EQUIP_TV>0, 1, 0) 


## Select features 
df1<- df1%>%dplyr::select('CO_ESCOLA',
                          'IN_LABORATORIO_CIENCIAS',
                          'IN_SALA_ATENDIMENTO_ESPECIAL',
                          'IN_BIBLIOTECA',
                          'IN_SALA_LEITURA',
                          'IN_BANHEIRO',
                          'IN_BANHEIRO_PNE',
                          'QT_SALAS_UTILIZADAS',
                          'QT_EQUIP_TV',
                          'QT_EQUIP_DVD',
                          'QT_EQUIP_COPIADORA',
                          'QT_EQUIP_IMPRESSORA',
                          'QT_COMP_ALUNO',
                          'IN_BANDA_LARGA',
                          'QT_FUNCIONARIOS',
                          'IN_ALIMENTACAO',
                          'IN_COMUM_MEDIO_MEDIO',
                          'IN_COMUM_MEDIO_INTEGRADO',
                          'IN_COMUM_MEDIO_NORMAL',
                          'IN_SALA_PROFESSOR',
                          'IN_COZINHA',
                          'IN_EQUIP_PARABOLICA',
                          'IN_QUADRA_ESPORTES',
                          'IN_ATIV_COMPLEMENTAR',
                          'EDU_PAI',
                          'EDU_MAE', 
                          'TITULACAO',
                          'RENDA_PERCAPITA', 
                          "TP_COR_RACA_1.0","TP_COR_RACA_2.0","TP_COR_RACA_3.0","TP_COR_RACA_4.0","TP_COR_RACA_5.0",
                          'QT_MATRICULAS', 
                          'TITULACAO', 
                          'NU_CIENCIA_NATUREZA','NU_CIENCIAS_HUMANAS', 'NU_LINGUAGENS_CODIGOS', 'NU_MATEMATICA',
                          'NU_ESCOLAS', 
                          'NU_LICENCIADOS', 
                          'IN_FORM_DOCENTE',
                          'NU_IDADE', 
                          'DIVERSIDADE',
                          'NU_NOTA_GERAL','CO_ANO', 'IN_TP_ESCOLA', 'UF_05'
)

df1<- fastDummies::dummy_cols(df1, select_columns = "UF_05")
df1<- df1%>%select(-c(UF_05, UF_05_SC))


result_models<- tibble("term" = character(),"estimate" = double(), 
                       "std.error"= double(),"statistic"= double(),
                       "p.value"= double(), "year"= integer(),
                       "auc"=double(), "list_years" = character(), 
                       "tp_escola"=character(), "nrow_temp"=double())
years <- c()
freq=11
for (i in unique(df1$CO_ANO)){
  year<- i
  print(i)
  if (i!=max(df1$CO_ANO)){
    df_ano<-df1%>%filter(!(CO_ANO %in% years))
    for (tp_escola in unique(df_ano$IN_TP_ESCOLA)){
      df_escola <- df_ano%>%group_by(CO_ESCOLA)%>%mutate(n = n())%>%ungroup%>%
        filter(n==freq)
      nrow_temp<- nrow(df_escola)/freq
      print(years)
      #print(nrow_temp)
      list_years<-paste(head(unique(df_escola$CO_ANO), n=1), tail(unique(df_escola$CO_ANO), n=1), sep ="-")
      #DEMEAN
      temp_dummies<- df_escola %>%select(starts_with('UF_05_'))
      temp_remain <- df_escola%>%select(-starts_with('UF_05_'))
      temp_remain<-temp_remain%>%select(-c(CO_ANO, IN_TP_ESCOLA, n))
      temp_remain<-temp_remain%>%group_by(CO_ESCOLA)%>%
        group_modify(~as_tibble(demean(.x)))%>%ungroup()%>%select(-CO_ESCOLA)
      df_escola<- cbind(temp_remain, temp_dummies)
      #print(colnames(df_escola))
      
      #df_escola<-df_escola%>%select(-c("CO_ANO", "CO_ESCOLA", "n"))
      
      #Target
      df_escola<- build_target(df_escola)
      df_escola<-drop_col_hight_mode(df_escola)
      #print(names(temp))
      
      train_control <- trainControl(method = "cv", number = 10,
                                    classProbs = TRUE, summary=twoClassSummary)
      
      
      set.seed(123)
      #df_tp_sch$TARGET<- as.factor(df_tp_sch$TARGET)
      df_escola$TARGET<- if_else(df_escola$TARGET==1, "yes", "no")
      #print("model")
      model <-caret::train(TARGET ~ .,
                        data = df_escola,
                        trControl = train_control,
                        method = "glm",
                        metric="ROC",
                        family=binomial(logit))
      
    
     
      #Coeficientes e p-values para data frame
      table_model<- tidy(model$finalModel)
      table_model <-arrange(table_model, desc(estimate))
      table_model<-filter(table_model, p.value < 0.05)
      
      
      #Avaliacao
      auc<- getTrainPerf(model)[,1]
      temp_eval<- tibble( table_model, year, auc, list_years,tp_escola, nrow_temp)
      result_models<- add_row(result_models, temp_eval)
      temp_eval<- tibble()
      years<- years%>%append(i)
      freq<- freq-1
    }  
    print(paste('done'))
  }
}
result_models_fixed_effects<- result_models%>%group_by(term)%>%mutate(n=n())

save(result_models_fixed_effects, file = "~/projects/panel/paper_I/fixed_effects_outputs.Rdata")  
load("~/projects/panel/paper_I/fixed_effects_outputs.Rdata") 
