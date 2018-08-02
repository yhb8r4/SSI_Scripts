#README:
###
###To view threadplots according to basis set, make sure to make the directory adjustments to directory. 
###

library(ggplot2)
library(gridExtra)
library(base)
library(scales)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#make adjustments to directories here
read.csv('~/Documents/SSI-R/Larger2.csv', header=T) -> L
read.csv('~/Documents/SSI-R/Smaller.csv', header=T) -> S
read.csv('~/Documents/SSI-R/Mixed2.csv', header=T) -> M
read.csv('~/Documents/SSI-R/MixedScreen22.csv', header=T) -> M2
read.csv('~/Documents/SSI-R/Mega.csv', header=T) -> Mg
read.csv('~/Documents/SSI-R/MegaNoCT.csv', header=T) -> MgN
read.csv('~/Documents/SSI-R/Mixed0.3.csv', header=T) -> M0.3
read.csv('~/Documents/SSI-R/B_w_CT.csv', header=T) -> B
read.csv('~/Documents/SSI-R/B_no_CT.csv', header=T) -> BN
read.csv('~/Documents/SSI-R/B_0.3_CT.csv', header=T) -> B_0.3_CT
read.csv('~/Documents/SSI-R/B_0.3_NoCT.csv', header=T) -> B_0.3_noCT

gen.basisplots <- function(x) {
  #nm <- assign(paste(deparse(substitute(x)),"Total", sep=""),deparse(substitute(f)), envir= .GlobalEnv)
  nm <- deparse(substitute(x))
  tMAD <- mean(abs(x$EFPTotal-x$SAPTTotal))
  eMAD <- mean(abs(x$EFPElec-x$SAPTElec))
  exMAD <- mean(abs(x$EFPExch-x$SAPTExch))
  pMAD <- mean(abs(x$EFPPolar-x$SAPTPolar))
  dMAD <- mean(abs(x$EFPDisp-x$SAPTDisp))
  guidelines <- c(-10,-5,-2, 0, 2,5,10)
  
  p <- ggplot(x, aes(x=total_rdiff, y=3)) 
  #p <- p + geom_vline(aes(xintercept=total_rdiff,color=dom_inter), size=1)
  p <- p + geom_segment(aes(x=total_rdiff, xend=total_rdiff, y=1, yend=0, color = dom_inter), size=1)
  p <- p + scale_colour_gradientn(colours=jet.colors(7), guide=FALSE)
  #p <- p + geom_vline(xintercept=tMAD, color ="black", size=5)
  p <- p + geom_segment(x=tMAD, y=3 , xend = tMAD, yend=-3, color = "black", size =5, lineend = "butt")
  p <- p + geom_vline(xintercept=guidelines, color ="grey", size=1.5)
  p <- p + scale_x_continuous(limits=c(-10,10))
  p <- p + theme(panel.background=element_rect(fill='white'), 
                 axis.ticks=element_blank(), 
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title=element_blank())
  
  q <- ggplot(x, aes(x=elec_rdiff, y=1)) #geom_vline(xinterception=totalavg, color ="black", size=2)
  # q <- q + geom_vline(aes(xintercept=elec_rdiff,color=dom_inter), size=1)
  q <- q + geom_segment(aes(x=elec_rdiff, xend=elec_rdiff, y=1, yend=0, color = dom_inter), size=1)
  q <- q + scale_colour_gradientn(colours=jet.colors(7), guide=FALSE)
  # q <- q + geom_vline(xintercept=eMAD, color ="black", size=8)
  q <- q + geom_segment(x=eMAD, y=3 , xend = eMAD, yend=-3, color = "black", size =5, lineend = "butt")
  q <- q + geom_vline(xintercept=guidelines, color ="grey", size=1.5)
  q <- q + scale_x_continuous(limits=c(-10,10))
  q <- q + theme(panel.background=element_rect(fill='white'), 
                 axis.ticks=element_blank(), 
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title=element_blank())
  
  
  t <- ggplot(x, aes(x=exch_rdiff, y=1)) #geom_vline(xinterception=totalavg, color ="black", size=2)
  #t <- t + geom_vline(aes(xintercept=exch_rdiff,color=dom_inter), size=1)
  t <- t + geom_segment(aes(x=exch_rdiff, xend=exch_rdiff, y=1, yend=0, color = dom_inter), size=1)
  t <- t + scale_colour_gradientn(colours=jet.colors(7), guide=FALSE)
  #t <- t + geom_vline(xintercept=exMAD, color ="black", size=8)
  t <- t + geom_segment(x=exMAD, y=3 , xend = exMAD, yend=-3, color = "black", size =5, lineend = "butt")
  t <- t + geom_vline(xintercept=guidelines, color ="grey", size=1.5)
  t <- t + scale_x_continuous(limits=c(-10,10))
  t <- t + theme(panel.background=element_rect(fill='white'), 
                 axis.ticks=element_blank(), 
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title=element_blank())
  
  
  o <- ggplot(x, aes(x=pol_rdiff, y=1)) #geom_vline(xinterception=totalavg, color ="black", size=2)
  #o <- o + geom_vline(aes(xintercept=pol_rdiff,color=dom_inter), size=1)
  o <- o + geom_segment(aes(x=pol_rdiff, xend=pol_rdiff, y=1, yend=0, color = dom_inter), size=1)
  o <- o + scale_colour_gradientn(colours=jet.colors(7), guide=FALSE)
  #o <- o + geom_vline(xintercept=pMAD, color ="black", size=8)
  o <- o + geom_segment(x=pMAD, y=3 , xend = pMAD, yend=-3, color = "black", size =5, lineend = "butt")
  o <- o + geom_vline(xintercept=guidelines, color ="grey", size=1.5)
  o <- o + scale_x_continuous(limits=c(-10,10))
  o <- o + theme(panel.background=element_rect(fill='white'), 
                 axis.ticks=element_blank(), 
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title=element_blank())
  
  k <- ggplot(x, aes(x=disp_rdiff, y=1)) #geom_vline(xinterception=totalavg, color ="black", size=2)
  #k <- k + geom_vline(aes(xintercept=disp_rdiff,color=dom_inter), size=1)
  k <- k + geom_segment(aes(x=disp_rdiff, xend=disp_rdiff, y=1, yend=0, color = dom_inter), size=1)
  k <- k + scale_colour_gradientn(colours=jet.colors(7), guide=FALSE)
  #k <- k + geom_vline(xintercept=dMAD, color ="black", size=8)
  k <- k + geom_segment(x=dMAD, y=3 , xend = dMAD, yend=-3, color = "black", size =5, lineend = "butt")
  k <- k + geom_vline(xintercept=guidelines, color ="grey", size=1.5)
  k <- k + scale_x_continuous(limits=c(-10,10))
  k <- k + theme(panel.background=element_rect(fill='white'), 
                 axis.ticks=element_blank(), 
                 axis.text.x=element_blank(),
                 axis.text.y=element_blank(),
                 axis.title=element_blank())
  
  
  stacked <- grid.arrange(p, q, t, o, k, ncol=1)
  print(stacked)
  return(stacked)
}

gen.columns <- function(x) {
  x$dom_inter = with(x,abs(x$EFPElec)/(abs(x$EFPElec)+abs(x$EFPDisp)))
  x$total_rdiff = with(x,(x$EFPTotal-x$SAPTTotal))
  x$elec_rdiff = with(x,(x$EFPElec-x$SAPTElec))
  x$exch_rdiff = with(x,(x$EFPExch-x$SAPTExch))
  x$pol_rdiff = with(x,(x$EFPPolar-x$SAPTPolar))
  x$disp_rdiff = with(x,(x$EFPDisp-x$SAPTDisp))
  
  gen.basisplots(x)
}

gen.componentsplots <- function(x) {
  for (i in x){
    x$dom_inter = with(x,abs(x$EFPElec)/(abs(x$EFPElec)+abs(x$EFPDisp)))
    x$total_rdiff = with(x,(x$EFPTotal-x$SAPTTotal)/x$SAPTTotal)
    x$elec_rdiff = with(x,(x$EFPElec-x$SAPTElec)/x$SAPTElec)
    x$exch_rdiff = with(x,(x$EFPExch-x$SAPTExch)/x$SAPTExch)
    x$pol_rdiff = with(x,(x$EFPPolar-x$SAPTPolar)/x$SAPTPolar)
    x$disp_rdiff = with(x,(x$EFPDisp-x$SAPTDisp)/x$SAPTDisp)
    
    nm <- deparse(substitute(x))
    tMAD <- mean(x$EFPTotal-x$SAPTTotal)
    eMAD <- mean(x$EFPElec-x$SAPTElec)
    exMAD <- mean(x$EFPExch-x$SAPTExch)
    pMAD <- mean(x$EFPPolar-x$SAPTPolar)
    dMAD <- mean(x$EFPDisp-x$SAPTDisp)
  }
}

gen.threadplots <- function(x){
  gen.columns (x)
  ggsave(filename="L", plot=stacked, device="pdf")
}
