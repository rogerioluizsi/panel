library(broom)
library(randomForest)
require(caTools)
library(tidyr)
library(dplyr)
#library(pscl)
library(pROC)
#library(plyr)
library(caret)
library(stringr)
library(DescTools)
library(doMC)
library(tibble)


registerDoMC(cores = 5)
options(tibble.print_max = Inf)
set.seed(123)
#Ler Base de Dados
notas <-read.csv("~/projects/panel/features_engeneering/ALL_SCHOOLS_v2.csv")
#notas<- notas%>%filter(CO_DEPENDENCIA_ADM!= 4)
#notas<- notas%>%filter(IN_TP_ESCOLA== 'Municipal+Estadual')


### Functions 
clip_tail<- function(df) {
  for (i in colnames(df)) {
    print(i)
    if (is.numeric(df[[i]]) & (length(df[[i]]%>%unique())> 2)){ 
      print(paste("_______"))
      print(max(df[[i]]))
      print(min(df[[i]]))
      print(sd(df[[i]]))
      df[[i]]<- Winsorize(df[[i]],  probs = c(0.025, 0.975), na.rm = FALSE, type = 7)
      print(max(df[[i]]))
      print(min(df[[i]]))
      print(sd(df[[i]]))
      print(paste("_______"))
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


## Only schools with elementary infrastructure schools
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
                          'NU_NOTA_GERAL','CO_ANO', 'IN_TP_ESCOLA'
)

#Table for outputs
result_models<- tibble("term" = character(),"estimate" = double(), "std.error"= double(),"statistic"= double(),"p.value"= double(),
                         "auc"=double(), "year"=integer(), "tp_escola"=character(), "model"=character())

#test <- df1[0,]
#test<-test%>%select(-IN_TP_ESCOLA)%>%add_column(TARGET = NA)
#just test
#df1<-df1%>%filter(CO_ANO == 2009)

for (year in unique(df1$CO_ANO)){
  df_y<- df1%>%filter(CO_ANO == year)
  print(year)
  print(paste('-_____________-__________'))
  
  for (tp_escola in unique(df1$IN_TP_ESCOLA)){
    df_tp_sch <-df_y%>%filter(IN_TP_ESCOLA == tp_escola)
    print(tp_escola)
    print(paste( '-_____________-__________'))
    df_tp_sch<- df_tp_sch%>%dplyr::select(-IN_TP_ESCOLA, - CO_ANO)
    
   
    #Drop columns with mode higer 90%
    df_tp_sch<-drop_col_hight_mode(df_tp_sch) 
    #build Target
    df_tp_sch<- build_target(df_tp_sch)
    
    #build test and train data
    #trainRowNumbers <- createDataPartition(df_tp_sch$TARGET, p=0.75, list=FALSE)
    #test <- df_tp_sch[-trainRowNumbers,]
    #df_tp_sch<- df_tp_sch[trainRowNumbers,]
    
    #clip long tail subset train
    df_tp_sch<- clip_tail(df_tp_sch)
    
    
    train_control <- trainControl(method = "cv", number = 10, 
                                  classProbs = TRUE, summary=twoClassSummary)
    #print("train")
    #scale train and tests from train metrics
    set.seed(123)
    pp = preProcess(df_tp_sch, method = "range")
    df_tp_sch<- predict(pp, df_tp_sch)
    #test<- predict(pp, test)
    
    set.seed(123)
    #df_tp_sch$TARGET<- as.factor(df_tp_sch$TARGET)
    df_tp_sch$TARGET<- if_else(df_tp_sch$TARGET==1, "yes", "no")
    #print("model")
    rl <-caret::train(TARGET ~ .,
                   data = df_tp_sch,
                   trControl = train_control,
                   method = "glm",
                   metric="ROC",
                   family=binomial(logit))
    print(paste('RL done'))
    df_tp_sch$TARGET<- as.factor(df_tp_sch$TARGET)
    rf <-randomForest(TARGET ~., 
                      data = df_tp_sch, 
                      importance=TRUE 
                      ) 
   
    print(paste('rf done'))
    
    #Coeficientes e p-values para data frame
    table_rl<- tidy(rl$finalModel)
    table_rl <-arrange(table_rl, desc(estimate))
    table_rl<-filter(table_rl, p.value < 0.05)
    
    table_rf<- data.frame(importance(rf, type=2))
    table_rf$term<- row.names(table_rf)
    table_rf<- table_rf%>%rename(estimate = MeanDecreaseGini)
    
    #Avaliacao
    model = "LogisticRegression"
    auc<- getTrainPerf(rl)[,1]
    
    #new data
    #prob_rl <- predict(rl$finalModel, newdata = test, type = "response")
    #pred_rl <- if_else(prob_rl > 0.5,1,0)
    #roc_rl <- roc(test$TARGET, pred_rl)
    #auc_eval<- roc_rl$auc[1]
    
    #save
    temp<- tibble( table_rl, auc,year, tp_escola, model)
    result_models<- add_row(result_models, temp)
    
    
    model = "RandomForest"
    votes<- as.vector(rf$votes[,2])
    roc_obj <- roc(df_tp_sch$TARGET, votes)
    auc<- roc_obj$auc[1]
    
    #new data
    #test$TARGET<-if_else(test$TARGET=='yes', 1, 0)
    #pred_rf <- predict(rf, newdata = test, type = "response")
    #pred_rf<-if_else(pred_rf=='yes', 1, 0)
    #roc_rf <- roc(test$TARGET, pred_rf)
    #auc_eval<- roc_rf$auc[1]
    
    temp<- tibble( table_rf, auc, year, tp_escola, model)
    result_models<- add_row(result_models, temp)
    
    temp<- tibble()
  }
  print(paste('done'))
}

result_models<- result_models%>%group_by(term, model, tp_escola)%>%mutate(n=n())
save(result_models, file = "~/projects/panel/paper_I/multi_year_outputs.Rdata")

load("~/projects/panel/paper_I/multi_year_outputs.Rdata")



