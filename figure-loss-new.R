works_with_R(
  "3.6.3",
  data.table="1.12.8",
  ggplot2="3.3.0",
  tikzDevice="0.12.3")
options(tikzDocumentDeclaration="\\documentclass[11pt]{article}
\\usepackage{amsmath,amssymb}",
        tikzMetricsDictionary="tikzMetrics")

##from ?pnorm
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
dist.list <- list(
  censored=list(
    normal=pnorm,
    logistic=plogis,
    extreme=function(z)1-exp(-exp(z))),
  uncensored=list(
    normal=function(z)exp(-z^2 / 2)/sqrt(2*pi),
    logistic=function(z)exp(z)/(1+exp(z))^2,
    extreme=function(z)exp(z)*exp(-exp(z))))
log.data.list <- list(
  censored=c(lower=-2.5, upper=3.5),
  uncensored=c(uncensored=-1.2))
lik.fun.list <- list(
  censored=function(dt)dt[, F.or.f_upper-F.or.f_lower],
  uncensored=function(dt)dt[, F.or.f_uncensored/s/exp(label_uncensored)])
s <- 1
loss.dt.list <- list()
label.dt.list <- list()
for(label.type in names(log.data.list)){
  y <- log.data.list[[label.type]]
  label.dt.list[[label.type]] <- data.table(
    label.type, label=y, label.name=names(y))
  d.list <- dist.list[[label.type]]
  lik.fun <- lik.fun.list[[label.type]]
  pred.vec <- seq(-10, 10, by=0.1)
  grid.dt <- data.table(expand.grid(
    label.name=names(y),
    prediction=pred.vec))
  grid.dt[, label := y[label.name] ]
  grid.dt[, z := (label-prediction)/s]
  for(dist.name in names(d.list)){
    dist.fun <- d.list[[dist.name]]
    grid.dt[, F.or.f := dist.fun(z)]
    grid.wide <- dcast(
      grid.dt,
      prediction ~ label.name,
      value.var=c("F.or.f", "label"))
    grid.wide[, lik := lik.fun(.SD)]
    grid.wide[, loss := -log(lik)]
    grid.wide[, min0.loss := loss-min(loss)]
    loss.dt.list[[paste(label.type, dist.name)]] <- data.table(
      label.type, dist.name,
      grid.wide[, .(prediction, loss, min0.loss)])
  }
}
loss.dt <- do.call(rbind, loss.dt.list)
min.dt <- loss.dt[, .SD[which.min(min0.loss)], by=.(label.type, dist.name)]
label.dt <- do.call(rbind, label.dt.list)
label.tex.prefix <- c(
  uncensored="",
  lower="\\underline",
  upper="\\overline")
label.dt[, label.tex := sprintf(
  "$ \\log %s y = %.1f $",
  label.tex.prefix[label.name],
  label)]
label.dt[, y := -1]
label.dt[, point := "label"]
label.dt[, hjust := ifelse(label.name=="upper", 0, 1)]
label.dt[, offset := ifelse(label.name=="upper", 0.7, -0.7)]
min.dt[, point := "min loss"]
p <- ggplot()+
  theme_bw()+
  theme(panel.spacing=grid::unit(0, "lines"))+
  facet_grid(. ~ label.type, labeller=label_both)+
  geom_line(aes(
    prediction, min0.loss, color=dist.name),
    size=1,
    data=loss.dt)+
  geom_point(aes(
    label, y, shape=point),
    data=label.dt)+
  geom_text(aes(
    label+offset, y, label=label.tex, hjust=hjust),
    vjust=-0.1,
    data=label.dt)+
  coord_cartesian(ylim=c(NA, 10))+
  geom_point(aes(
    prediction, min0.loss,
    color=dist.name,
    size=dist.name,
    shape=point),
    data=min.dt)+
  scale_size_discrete("Distribution")+
  scale_shape_manual(values=c("min loss"=1, label=19))+
  scale_color_brewer(
    "Distribution",
    type="qual", guide=guide_legend(
      override.aes=list(shape=1)))+
  scale_x_continuous(
    "Predicted value $\\hat y$")+
  scale_y_continuous(
    "Loss $\\ell_{\\text{AFT}}(y, \\hat y)$\n(negative log likelihood)")
tikz("figure-loss-new.tex",h=2.1,w=7, standAlone = TRUE)
print(p)
dev.off()
system("pdflatex figure-loss-new")

