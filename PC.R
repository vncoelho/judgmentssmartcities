
rm(list=ls())
library(ggplot2)
data<-read.table("calibration",header=T)
data<-read.table("calibrationWithSameElements",header=T)


data$AVG<- data$NSOL/data$WEIGHTS
data$WEIGHTS <- cut(data$WEIGHTS, 
                  breaks=c(1000, 3000, 5000, 7000, 10000), include.lowest=TRUE)
data$WEIGHTS<-as.factor(data$WEIGHTS)


data$FACTS <- cut(data$FACTS, 
                breaks=c(1, 10, 15, 20, 30, 50, 70, 100), include.lowest=TRUE)
data$FACTS<-as.factor(data$FACTS)

data$CITIZENS <- cut(data$CITIZENS, 
                  breaks=c(10, 20, 30, 50, 70, 90, 110), include.lowest=TRUE)
data$CITIZENS<-as.factor(data$CITIZENS)

data$CARAC <- cut(data$CARAC, 
                     breaks=c(1, 3, 5, 7, 10, 15, 20,25, 30), include.lowest=TRUE)
data$CARAC<-as.factor(data$CARAC)




pdf("interaction_GGPLOT_NSOL_FACTS_WEIGHTS.pdf",width=10,height=7) # comment to open plot in R
ggplot(data = data,
       aes(x = FACTS , y=NSOL,colour = WEIGHTS, group=WEIGHTS)) +
  stat_summary(fun.y=mean, geom="line", size = 1.3)+
  # stat_summary(fun.y=min, geom="point",shape=2)+
  # stat_summary(fun.y=max, geom="point",shape=3)+
  stat_summary(fun.y=sd, geom="line",linetype="dashed")+
  # coord_cartesian(ylim=c(3, 8))+
  labs(x = "Número de fatos",
       y= "Porcentagem de soluções não-dominadas",
       colour= "Quantidade de 
       Possibilidas analisadas
       com pesos aleatórios")
dev.off()


pdf("interaction_GGPLOT_NSOL_CITIZENS_WEIGHTS.pdf",width=10,height=7) # comment to open plot in R
ggplot(data = data,
       aes(x = CITIZENS , y=NSOL,colour = WEIGHTS, group=WEIGHTS)) +
  stat_summary(fun.y=mean, geom="line", size = 1.3)+
  # stat_summary(fun.y=min, geom="point",shape=2)+
  # stat_summary(fun.y=max, geom="point",shape=3)+
  stat_summary(fun.y=sd, geom="line",linetype="dashed")+
  # coord_cartesian(ylim=c(3, 8))+
  labs(x = "Número de cidadãos",
       y= "Porcentagem de soluções não-dominadas",
       colour= "Quantidade de 
       Possibilidas analisadas
       com pesos aleatórios")
dev.off()

pdf("interaction_GGPLOT_NSOL_CARAC_WEIGHTS.pdf",width=10,height=7) # comment to open plot in R
ggplot(data = data,
       aes(x = CARAC , y=NSOL,colour = WEIGHTS, group=WEIGHTS)) +
  stat_summary(fun.y=mean, geom="line", size = 1.3)+
  # stat_summary(fun.y=min, geom="point",shape=2)+
  # stat_summary(fun.y=max, geom="point",shape=3)+
  stat_summary(fun.y=sd, geom="line",linetype="dashed")+
  # coord_cartesian(ylim=c(3, 8))+
  labs(x = "Número de características",
       y= "Porcentagem de soluções não-dominadas",
       colour= "Quantidade de 
       Possibilidas analisadas
       com pesos aleatórios")
dev.off()

ggplot(data = data,
       aes(x = FACTS , y=AVG,colour = WEIGHTS, group=WEIGHTS)) +
  stat_summary(fun.y=mean, geom="line", size = 1.3)+
  # stat_summary(fun.y=min, geom="point",shape=2)+
  # stat_summary(fun.y=max, geom="point",shape=3)+
  stat_summary(fun.y=sd, geom="line",linetype="dashed")+
  # coord_cartesian(ylim=c(3, 8))+
  labs(x = "Número de características",
       y= "Porcentagem de soluções não-dominadas",
       colour= "Quantidade de 
       Possibilidas analisadas
       com pesos aleatórios")



ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(1, 2, 3, 4)], col = 1 + (0:149)%/%50)
parcoord(data, col=rainbow(length(data[,1])), var.label=TRUE)

library(MASS)
library(ggplot2)

data<-read.table("./Data/data4",header=T)
datascaled <- as.data.frame(lapply(data, ggplot2:::rescale01))
datascaled$model <- rownames(data)
datamelted <- reshape2::melt(datascaled)

pdf("./Plots/parallelCoordMOFMStockMarket.pdf",width=15,height=10) # comment to open plot in R
parcoord(data[c("MAPEINV", "RMSE", "MAPE", "MAPEINV", "WMAPE", "SMAPE", "MMAPE")], col=rainbow(length(data[,1])), var.label=TRUE)
dev.off() # comment this if you commented the pdf command.


coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

pdf("polarCoordMOHFMStockMarket.pdf",width=15,height=10) # comment to open plot in R
ggplot(datamelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model),  size = 1) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  guides(color = guide_legend(ncol=4)) +
  xlab("") + ylab("") + 
  coord_polar()
dev.off() # comment this if you commented the pdf command.

pdf("radarCoordMOHFMStockMarket.pdf",width=15,height=10) # comment to open plot in R
ggplot(datamelted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = model, color = model), fill = NA, size = 0.6, show.legend = FALSE) +
  #geom_line(aes(group = model, color = model), size = 0.8) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=3)) +
  coord_radar()
dev.off() # comment this if you commented the pdf command.


mtcarsscaled <- as.data.frame(lapply(mtcars, ggplot2:::rescale01))
mtcarsscaled$model <- rownames(mtcars)
mtcarsmelted <- reshape2::melt(mtcarsscaled)
library(ggplot2)
ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_path(aes(group = model, color = model),  size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_polar()



ggplot(mtcarsmelted, aes(x = variable, y = value)) +
  geom_polygon(aes(group = model, color = model), fill = NA, size = 2, show.legend = FALSE) +
  geom_line(aes(group = model, color = model), size = 2) +
  theme(strip.text.x = element_text(size = rel(0.8)),
        axis.text.x = element_text(size = rel(0.8)),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  xlab("") + ylab("") +
  guides(color = guide_legend(ncol=2)) +
  coord_radar()




ir <- rbind(iris3[,,1], iris3[,,2], iris3[,,3])
parcoord(log(ir)[, c(3, 4, 2, 1)], col = 1 + (0:149)%/%50)


cardeaths <- Seatbelts[,"DriversKilled"]
cardeaths & lt;- data.frame(data[,1], data[,5], data[,6],data[,8])
colnames(cardeaths) &lt;- c("DriversKilled", "DistanceDriven", "PriceofGas", 
                            "SeatbeltLaw")

cardeaths &lt;- data.frame(Seatbelts[,1], Seatbelts[,5], Seatbelts[,6],Seatbelts[,8])

colnames(cardeaths) &lt;- 
  
  c("DriversKilled", "DistanceDriven", "PriceofGas", "SeatbeltLaw")

library(reshape2)
library(parcoord)
library(ggparallel)

ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)


data(mtcars)
ggparallel(list("gear", "cyl"), data=data)


ggparallel(list("gear", "cyl"), data=mtcars, method="hammock", ratio=0.25)
require(RColorBrewer)
require(ggplot2)
cols <- c(brewer.pal(4, "Reds")[-1], brewer.pal(4, "Blues")[-1])
ggparallel(list("gear", "cyl"), ratio=0.2, data=mtcars,
           method="hammock", text.angle=0) +
  scale_fill_manual(values=cols) + scale_colour_manual(values=cols) +
  theme_bw()

data<-read.table("data",header=T)
is.numeric(data$error1)
data$error1<-as.numeric(data$error1)
data$error2<-as.numeric(data$error2)
data$error3<-as.numeric(data$error3)
data$error4<-as.numeric(data$error4)
data$error5<-as.numeric(data$error5)

ggparallel(names(data), order=0, data) 

+
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")


ggparallel(names(titanic)[c(1,4,2,3)], order=0, titanic) +
  scale_fill_brewer(palette="Paired", guide="none") +
  scale_colour_brewer(palette="Paired", guide="none")

ggparallel(list("gear", "cyl"), data=mtcars, method="adj.angle",
           ratio=2)

data <- data

data(genes)
cols <- c(rep("grey80", 24), brewer.pal("YlOrRd", n = 9))
genes$chrom <- factor(genes$chrom, levels=c(paste("chr", 1:22, sep=""), "chrX", "chrY"))
ggparallel(list("path", "chrom"), text.offset=c(0.03, 0,-0.03),
           data = genes,  width=0.1, order=c(1,0), text.angle=0,
           color="white",
           factorlevels =  c(sapply(unique(genes$chrom), as.character),
                             unique(genes$path))) +
  scale_fill_manual(values = cols, guide="none") +
  scale_colour_manual(values = cols, guide="none") +
  coord_flip()
