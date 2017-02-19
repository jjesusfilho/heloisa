library(tidyverse)
library(cowplot)
ggplot(letalidade,aes(x=ano,y=mortes))+
  geom_line(color="blue")+
  scale_x_continuous(breaks=1983:2014)+
  geom_text(aes(label=mortes),vjust=-.5,hjust=.5,size=2)+
  labs(caption="Fonte: Tereza Caldeira e Secretaria de Segurança Pública de São Paulo")+
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle=45,vjust=.5)

  )
ggsave("~/Desktop/TESE/figure/civis.pdf",width=10,height=6)


ddd<-data.frame(A=gl(2,8,labels=c("m","f")),B=gl(2,8,labels=c("y","n")),C=gl(2,8,labels=c("out","in")))

