
library(lavann)
library(semPlot)

## Importa a classificação das perguntas (indicadores) agrapados pelas variáveis latentes que os explica
dic<-import("dic_likert.xlsx")
dic[12,3:5]<-t(c("disc",2,"p"))

a1<-datalikert
a1[1:42]<-as.numeric(unlist(a1[1:42]))
names(a1)<-paste0("v",1:47)

s<-as.numeric(rownames(na.omit(dic[dic$sinal=="n",-2])))

a1[s]<-7-unlist(a1[s])

a1.1<-cov(a1[1:42])

L <-"autonomia =~ v17+v18+v19+v20
      dialogo =~ v4+v5+v6+v9+v25
      disciplina =~ v2+v15+v12
      punitivo =~ v3+v7+v8+v10+v11+v14\nautonomia + dialogo + disciplina + punitivo ~ v43 + v44 + v45 + v46 + v47"


pal3<-brewer.pal(5,"Set3")
pal2<-brewer.pal(4,"Set2")
pal1<-brewer.pal(4,"Set1")

L1 <-"Negocial =~ Q1+Q2+Q3+Q4
Disciplinar =~ Q5+Q6+Q7+Q8
Punitivo =~ Q9+Q10+Q11+Q12\nNegocial + Disciplinar + Punitivo ~ idade + sexo + capital + tempo+segur"


semPaths(L,layout="tree2",nCharNodes = 0,curve=F,rotation=1,residuals=T,exoVar = F,exoCov=F,groups=c("man","lat"),intercepts=T,color=list(lat=pal1,man=pal2))

model.Lavaan <- "f1 =~ y1 + y2 + y3\nf2 =~ y4 + y5 + y6\nf1 + f2 ~ x1 + x2 + x3 "
ggsave()

library("lavaan")
fit <- cfa(L, data = a1.1) 

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
