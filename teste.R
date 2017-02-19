
library(lavaan)
library(semPlot)

## Imporando os dados

df<-readRDS(gzcon(url("http://cloud.josejesus.info/index.php/s/Ah0NFz3nuTdRyba/download")))



modelo<-"autonomo =~ v41+v19+v20+v18+v17
dialogal =~ v6+v5+v25+v4+v9
disciplinar =~ v2+v12+v15
punitivo =~ v3+v7+v10+v14+v11+v8\nautonomo + dialogal + disciplinar + punitivo ~ v43 + v44 + v45 + v46 + v47"




## Rodando com numéricas
fit <- cfa(modelo, data = df)

## Este plota

semPaths(fit)

## Rodando com os valores ordenados
fit2<-cfa(modelo,data=data,ordered=names(data)[1:42])

## Este não plota
semPaths(fit2)

