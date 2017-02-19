
library(DiagrammeR)

grViz("
digraph circo{
graph [layout = circo]
node[shape=circle
      style=filled,
      color=black,
      fillcolor=Ghostwhite]
Conservadorismo;Punição;Reprodução;Reincidência
Conservadorismo->Punição;Punição->Reprodução;Reprodução->Reincidência;Reincidência->Conservadorismo
}
")

df<-readRDS("~/Documents/tese/tese/execucao/base_completa_para_regressao.rds")
library(plyr)
library(ggplot2)
library(cowplot)

df1<-readRDS("~/Documents/tese/tese/execucao/base_completa_para_regressao.rds")

library(plyr)
library(ggplot2)
library(cowplot)

df_agr<-ddply(df1,.(agravante,orgao_julgador,decisao),summarize,f=length(decisao))

ggplot(df_agr,aes(x=orgao_julgador,y=f,fill=decisao))+
  geom_bar(stat="identity",position="dodge",colour="black")+
  scale_fill_manual(values=c("red","darkgreen"),name="Decisão:")+
  #geom_text(aes(x=agravante,y=freq,label=freq),position=position_dodge(.9),vjust=-.5)+
  facet_grid(~agravante)+
  coord_flip()+
  theme(strip.background = element_rect(fill="lightblue",colour="black"),
        legend.position="bottom")+
  labs(x="Câmara Criminal",y="Número de decisões")

ggsave("~/Desktop/TESE/figure/gg_decisao_execucao.pdf",width=10,height=5)
