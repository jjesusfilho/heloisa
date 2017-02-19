library(sparklyr)
library(dplyr)
library(rsparkling)
library(ggplot2)
options(rsparkling.sparklingwater.version = '1.6.7')
sc <- spark_connect(master = "local")

base<-readRDS("base_completa_para_regressao.rds")

base_tbl<-copy_to(sc,base,"base",overwrite = T)


mod_dataset <- base_tbl %>%
  sdf_register("mod_dataset")
summarise(mod_dataset, n())




partitions <- mod_dataset %>% sdf_partition(training = 0.9, test = 0.1, seed = 42)
partitions$training

training <- as_h2o_frame(sc, partitions$training)
test <- as_h2o_frame(sc, partitions$test)

base$gravidadeBaixa<-ifelse(base$gravidade=="baixa","sim","não")
base$gravidadeAlta<-ifelse(base$gravidade=="alta","sim","não")
base$gravidadeMedia<-ifelse(base$gravidade=="media","sim","não")

training$decisao<-as.factor(training$decisao)
training$bom_comportamento<-as.factor(training$bom_comportamento)
training$orgao_julgador<-as.factor(training$orgao_julgador)
training$criminologico<-as.factor(training$criminologico)
training$falta<-as.factor(training$falta)
training$agravante<-as.factor(training$agravante)
training$gravidade<-as.factor(training$gravidade)

training$gravidadeBaixa<-as.factor(training$gravidadeBaixa)
training$gravidadeAlta<-as.factor(training$gravidadeAlta)
training$gravidadeMedia<-as.factor(training$gravidadeMedia)




test$decisao<-as.factor(test$decisao)
test$bom_comportamento<-as.factor(test$bom_comportamento)
test$orgao_julgador<-as.factor(test$orgao_julgador)
test$criminologico<-as.factor(test$criminologico)
test$falta<-as.factor(test$falta)
test$agravante<-as.factor(test$agravante)
test$gravidade<-as.factor(test$gravidade)

test$gravidadeBaixa<-as.factor(test$gravidadeBaixa)
test$gravidadeAlta<-as.factor(test$gravidadeAlta)
test$gravidadeMedia<-as.factor(test$gravidadeMedia)


mod.glm<-h2o.glm(x = names(base)[-1],
                 family = "binomial",
                 y ="decisao",
                 training_frame = training,
                 validation_frame = test,
                # balance_classes = T, # opcion para balanceo de clases
                 nfolds = 5)

h2o.confusionMatrix(mod.glm,test)


mod.rf <- h2o.randomForest(x = names(base)[-1],
                           y ="decisao",
                           training_frame = training,
                           validation_frame = test,
                           balance_classes = T, # opcion para balanceo de clases
                            nfolds = 5,
                           stopping_rounds = 5,
                           stopping_metric = "AUC")

summary(mod.rf)


h2o.varimp_plot(mod.rf) 

h2o.gainsLift(mod.rf, valid = TRUE)

preds <-  h2o.predict(mod.rf, test)

h2o.confusionMatrix(mod.rf,test)


mod.gbm<-h2o.gbm(x = names(base)[-1],
                                    y ="decisao",
                                    training_frame = training,
                                    validation_frame = test,
                                    balance_classes = T, # opcion para balanceo de clases
                                    nfolds = 10)

summary(mod.gbm)

h2o.confusionMatrix(mod.gbm,test)


mod.deep<-h2o.deeplearning(x = names(base)[-1],
                 y ="decisao",
                 training_frame = training,
                 validation_frame = test,
                 balance_classes = T, # opcion para balanceo de clases
                 nfolds = 10)


h2o.confusionMatrix(mod.deep,test)

