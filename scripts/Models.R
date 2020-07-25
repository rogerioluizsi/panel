library(broom)
library(tidyr)
library(dplyr)
#library(pscl)
#library(ROCR)
#library(plyr)
library(caret)
options(tibble.print_max = Inf)
set.seed(5)
#Ler Base de Dados
notas <-read.csv("~/projects/panel/features_engeneering/ALL_SCHOOLS.csv")
#notas<- notas%>%filter(IN_TP_ESCOLA== 'Muncipal+Estadual')
### Functions 

clip_tail<- function(df) {
  for (i in seq_along(df)) {
    if (is.numeric(i) & (length(i%>%unique()> 2))){ 
      df[[i]]<- Winsorize(i,  probs = c(0.025, 0.975), na.rm = FALSE, type = 7)
    }
  }
  return(df)
}  

drop_col_hight_mode<- function(df){
  for (i in colnames(df)){
    prop_mode<- sort(-table(df[[i]]))[1]/nrow(df)*(-1) # get mode value
    if (prop_mode > 0.9){
      df<- df%>%dplyr::select(-i)
      print(i)
    }
  }
  return(df)
}

build_target <- function(df){
  df<- df%>%mutate(
    TARGET = ntile(NU_NOTA_GERAL, 4))
  df$TARGET<- if_else(df$TARGET == 4, 1, 0)
  df<- df%>%dplyr::select(-NU_NOTA_GERAL)
  print(paste0(round(mean(df$TARGET)*100, 1), '%', ' upper quartil'))
  return(df)
}


## Only schools elementary infrastructure schools
df1<- notas%>%filter(IN_AGUA_INEXISTENTE == 0 & IN_ESGOTO_INEXISTENTE ==0 & IN_ENERGIA_INEXISTENTE == 0)

## regularizing some quantitify variables that are binary in some years   
df1$QT_EQUIP_COPIADAORA <- if_else(df1$QT_EQUIP_COPIADORA>0, 1, 0)
df1$QT_EQUIP_DVD <- if_else(df1$QT_EQUIP_DVD>0, 1, 0) 
df1$QT_EQUIP_IMPRESSORA <- if_else(df1$QT_EQUIP_IMPRESSORA>0, 1, 0) 
df1$QT_EQUIP_TV <- if_else(df1$QT_EQUIP_TV>0, 1, 0) 


## Select features 
df1<- df1%>%dplyr::select('IN_LABORATORIO_INFORMATICA',
'IN_LABORATORIO_CIENCIAS',
'IN_SALA_ATENDIMENTO_ESPECIAL',
'IN_BIBLIOTECA',
'IN_SALA_LEITURA',
'IN_BANHEIRO_FORA_PREDIO',
'IN_BANHEIRO_PNE',
'IN_DEPENDENCIAS_PNE',
'QT_SALAS_UTILIZADAS',
'QT_EQUIP_TV',
'QT_EQUIP_DVD',
'QT_EQUIP_COPIADORA',
'QT_EQUIP_IMPRESSORA',
'QT_COMP_ALUNO',
'IN_BANDA_LARGA',
'QT_FUNCIONARIOS',
'IN_ALIMENTACAO',
'EDU_PAI',
'EDU_MAE', 
'TITULACAO',
'QT_PESSOAS_CASA', 
'RENDA_MENSAL', 
'TP_COR_RACA',
'NU_NOTA_GERAL',
'IN_TP_ESCOLA',
'CO_ANO')

result_models<- tibble("term" = character(),"estimate" = double(), "std.error"= double(),"statistic"= double(),"p.value"= double(),
              "acc"=double(), "auc"=double(), "year"=integer(), "tp_escola"=character())

for (year in unique(df1$CO_ANO)){
   df_y<- df1%>%filter(CO_ANO == year)

   for (tp_escola in unique(df1$IN_TP_ESCOLA)){
     df_tp_sch <-df_y%>%filter(IN_TP_ESCOLA == tp_escola)
     df_tp_sch<- df_tp_sch%>%dplyr::select(-IN_TP_ESCOLA, - CO_ANO)
     
     df_tp_sch<-drop_col_hight_mode(df_tp_sch) 
     df_tp_sch<- build_target(df_tp_sch)
     
     trainRowNumbers <- createDataPartition(df_tp_sch$TARGET, p=0.25, list=FALSE)
     train <- df_tp_sch[trainRowNumbers,]
     test <- df_tp_sch[-trainRowNumbers,]
     
     #clip long tail subset train
     #train<- clip_tail(train)
     
     #scale train and tests from train metrics
     pp = preProcess(train, method = "range")
     train<- predict(pp, train)
     test<- predict(pp, test)
     
     ConjVazio = glm(TARGET ~1, family = binomial(logit), data = train)
     ConjCheio = glm(TARGET ~., family = binomial(logit), data = train) 
     StepModel <- step(ConjVazio, scope =list(lower=ConjVazio, upper=ConjCheio),direction = "both", trace = -1) 
     
     #Coeficientes e p-values para data frame
     table_model<- tidy(StepModel)
     
     table_model <-arrange(table_model, desc(estimate))
     table_model<-filter(table_model, p.value < 0.05)
     
     
     #selected features 
     selected_features_model<-tidy(anova(StepModel)) 
     selected_features_model<- selected_features_model%>% pull(term)
     selected_features_model<- selected_features_model[(-1)]
     
     #update test features 
     TARGET<- test[['TARGET']]
     test<-test %>% dplyr::select(selected_features_model)
     test <-cbind(test, TARGET)
     
     #Avaliacao
     print(paste("aqui"))
     PredicaoResp <- predict(StepModel, newdata = test, type = "response")
     
     PredicaoClass <- if_else(PredicaoResp > 0.5,1,0)
     MisClasificError <- mean(PredicaoClass !=test$TARGET)
     #Acuracia
     acc<- 1-MisClasificError
     
     PredicaoResp <- predict(StepModel, newdata = test, type = "response")
     roc_obj <- roc(test$TARGET, PredicaoResp)
     auc<- roc_obj$auc[1]
     #tpr<- roc_obj$sensitivities
     #fpr<- roc_obj$specificities
     temp<- tibble( table_model, acc, auc, year, tp_escola)
     result_models<- add_row(result_models, temp)
     temp<- tibble()
   }
print(paste('done'))
}

save(result_models, file = "~/projects/panel/data/result_models.Rdata")

