works_with_R("3.0.2",ggplot2="0.9.3.1",tikzDevice="0.6.3",scales="0.2.3")
options(tikzDocumentDeclaration="\\documentclass[11pt]{article}",
        tikzMetricsDictionary="tikzMetrics")
## Show AFT losses.
library(survival)
data(ovarian)
ovarian$i <- 1:nrow(ovarian)
patients <- c(4,5)
t.min <- 10
t.max <- 2000
n.points <- 200
t.grid <- sort(c(#0,
            seq(t.min,t.max,l=n.points),
            2^seq(log2(t.min),log2(t.max),l=n.points)))
line.df <- data.frame()
## loss function for median of a log-logistic distribution.
LL.median <- function(di,ti,t.hat,b=1){
  stopifnot(length(di)==1)
  stopifnot(length(ti)==1)
  stopifnot(length(b)==1)
  stopifnot(di %in% c(1,0))
  stopifnot(ti > 0)
  stopifnot(t.hat > 0)
  stopifnot(b > 0)
  surv.term <- log(1+(ti/t.hat)^b)
  if(di == 0){
    surv.term
  }else{
    2*surv.term - log(b/t.hat) - (b-1)*log(ti/t.hat)-log(4*ti/b) # normalized to 0 at minimum.
  }
}
lognormal.mean <- function(di, ti, t.hat, sigma){
  stopifnot(length(di)==1)
  scaled.diff <- log(ti/t.hat)/sigma
  if(di==1){
    scaled.diff^2/2
  }else{
    -log(1-pnorm(scaled.diff))
  }
}
## Parkes time intervals for each patient.
for(i in patients){
  di <- ovarian[i,"fustat"]
  ti <- ovarian[i,"futime"]
  funs <- list(LogLogistic.2=function(t.hat){
    LL.median(di, ti, t.hat, 2)
  },LogNormal.1=function(t.hat){
    lognormal.mean(di, ti, t.hat, 1)
  })
  for(model in names(funs)){
    fun <- funs[[model]]
    cost <- fun(t.grid)
    line.df <- rbind(line.df,{
      data.frame(time=t.grid,cost,model,i)
    })
  }
}
lab.df <- ovarian[patients,]
lab.df$label <- c("survival greater\nthan this time",
                  "died at\nthis time")
library(grid)
p <- ggplot()+
  geom_line(aes(log2(time),cost,group=model,colour=model,linetype=model),
            data=line.df,size=1.5)+
  geom_point(aes(log2(futime)),y=0,data=ovarian[patients,])+
  geom_segment(aes(log2(futime),xend=ifelse(fustat==0,log2(t.max),NA)),
               y=0,yend=0,data=ovarian[patients,])+
  geom_text(aes(log2(futime),label=label),y=7,hjust=0.5,data=lab.df)+
  geom_segment(aes(log2(futime),xend=log2(futime)),y=5,yend=2,data=lab.df,
               arrow=arrow())+
  facet_grid(.~i,scales="free",labeller=function(var,val){
    if(var=="i")sprintf("patient $i=%d$",val) else as.character(val)
  })+
  theme_bw()+
  theme(panel.margin=unit(0,"cm"))+
  ylab("loss function $L_i( \\hat y)$")+
  xlab("predicted survival time $\\hat y$ in days ($\\log_2$ scale)")+
  scale_linetype_manual(values=c(LogNormal.1="dashed",LogLogistic.2="solid"))+
  scale_x_continuous(breaks=seq(4,10,by=3),minor_breaks=seq(3,11,by=1),
                     labels=function(x){
    sprintf("$%d = 2^{%d}$",2^x,x)
  })

tikz("figure-loss.tex",h=2.1,w=7)
print(p)
dev.off()

