setwd("~/Documents/tese/tese")
library(lavaan)
library(semPlot)
library(RColorBrewer)

#Importa os dados 
data<-readRDS("data.rds")
#Lê transforma em números
data[1:42]<-as.numeric(unlist(data[1:42]))

# Dá nomes simples (retirar jogo da velha se for usar)
names(data)<-paste0("v",1:47)

#Importar dicionario
dic<-readRDS("dicionario_grupo.rds")

# Selecionar os indicadores que estão com declarações em sentido oposto às demais

opostos<-as.numeric(rownames(na.omit(dic[dic$sinal=="n",-2])))
# 
data[opostos]<-7-unlist(data[opostos])

data.cov<-cov(data[1:42])


l<-"autonomo =~ v41+v19+v20+v18+v17
      dialogal =~ v6+v5+v25+v4+v9
      disciplinar =~ v2+v12+v15
      punitivo =~ v3+v7+v10+v14+v11+v8\nautonomo + dialogal + disciplinar + punitivo ~ v43 + v44 + v45 + v46 + v47"


pal3<-brewer.pal(5,"Set3")
pal2<-brewer.pal(4,"Set2")
pal1<-brewer.pal(4,"Set1")


nodeW<-c(rep(2.5,19),rep(1.2,5),rep(1,4))
nodeH<-c(rep(2.2,19),rep(1,5),rep(1,4))

label.size<-c(rep(1.2,19),rep(1,5),rep(1,4))
labels<-c(dic2$descricao,c("autonomo","dialogal","disciplinar","punitivo"))

colors<-c(rep(pal2[1],5),rep(pal2[2],5),rep(pal2[3],3),rep(pal2[4],6),pal3,pal2)

semPaths(l,nodeLabels=labels,
         layout="tree2",
         nCharNodes = 0,
         curve=F,
         rotation=2,
         residuals=F,
         exoVar = F,
         exoCov=F,
         groups=c("man","lat"),
         sizeMan=5,
         sizeMan2 = 2.5,
         sizeLat = 3,
         sizeLat2 = 3,
         color=colors,
         vTrans=50,
         levels=c(2,3,4,7),
         #layoutSplit = T,
         #measurementLayout=T,
         #subRes = 2,
         #subscale2=10,
         #cardinal=T,
         node.width=nodeW,
         node.height=nodeH,
         normalize=T,
         label.cex=label.size,
         label.prop=.8,
         label.scale=T,
         height=20,
         width=15,
         mar=c(1,5,1,5),
         filetype="svg",
         filename="grafico2"
)




fit <- cfa(L, data = data) 


semPaths(l,nodeLabels=labels,
         layout="tree2",
         nCharNodes = 0,
         curve=F,
         rotation=2,
         residuals=F,
         exoVar = F,
         exoCov=F,
         groups=c("man","lat"),
         sizeMan=5,
         sizeMan2 = 2.5,
         sizeLat = 3,
         sizeLat2 = 3,
         color=colors,
         vTrans=50,
         levels=c(2,3,4,7),
         node.width=nodeW,
         node.height=nodeH,
         normalize=T,
         label.cex=label.size,
         label.prop=.8,
         label.scale=T,
         height=20,
         width=15,
         mar=c(1,5,1,5),
         filetype="svg",
         filename="graficoFit"
)





summary(fit,standardized=T,rsquare=T,fit.measures=T)

### Verificando os coeficientes e intervalos de confiança

parameterestimates(fit,standardized=T)

### Verificando a matriz de covariância ajustada

fitted(fit)

### Verificando o resíduo

residuals(fit)

semPaths(fit, intercepts=F,residuals=F,thresholds=F,nCharNodes=0,whatLabels="std")


fit2 <- cfa(L, sample.cov=a1.1,sample.nobs=73) # mesma coisa, mas com 

semPaths(fit, intercepts=F,residuals=F,thresholds=F,nCharNodes=0,whatLabels="std")

semPaths(fit2, intercepts=F,residuals=F,thresholds=F,nCharNodes=0,whatLabels="std")





