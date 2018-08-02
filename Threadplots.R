library(ggplot2)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

read.csv('~/Documents/SSI-R/Larger2.csv', header=T) -> L

jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
L$Dom <- abs(L$EFPElec)/(abs(L$EFPElec)+abs(L$EFPDisp))

L$TotalError <- L$EFPTotal - L$SAPTTotal
LTotalAverage <- mean(L$TotalError)

L$ElecError <- L$EFPElec - L$SAPTElec
LElecAverage <- mean(L$ElecError)

L$ExchError <- L$EFPExch - L$SAPTExch
LExchAverage <- mean(L$ExchError)

L$PolarError <- L$EFPPolar - L$SAPTPolar
LPolarAverage <- mean(L$PolarError)

L$DispError <- L$EFPDisp - L$SAPTDisp
LDispAverage <- mean(L$DispError)

#guide_legend(title="Elec Dom", keyheight=3)
#ggplot(L, aes(x=Error, y=1))+scale_x_continuous(limits=c(-2.5,1)) + geom_vline(aes(xintercept=Error, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + geom_vline(xintercept=Average, color='black', size = 2)
LTotal <- ggplot(L, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7)) + labs(x=" ", y="Total") + geom_vline(xintercept=LTotalAverage, color='black', size = 2) + ggtitle("Large") + theme(plot.title = element_text( face="bold", size=20))
LElec <- ggplot(L, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=LElecAverage, color='black', size = 2)
LExch <- ggplot(L, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=LExchAverage, color='black', size = 2)
LPolar <- ggplot(L, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=LPolarAverage, color='black', size = 2)
LDisp <- ggplot(L, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=LDispAverage, color='black', size = 2)
LBasis <- grid.arrange(LTotal, LElec, LExch, LPolar, LDisp, ncol=1)

read.csv('~/Documents/SSI-R/Smaller.csv', header=T) -> S

S$Dom <- abs(S$EFPElec)/(abs(S$EFPElec)+abs(S$EFPDisp))

S$TotalError <- S$EFPTotal - S$SAPTTotal
STotalAverage <- mean(S$TotalError)

S$ElecError <- S$EFPElec - S$SAPTElec
SElecAverage <- mean(S$ElecError)

S$ExchError <- S$EFPExch - S$SAPTExch
SExchAverage <- mean(S$ExchError)

S$PolarError <- S$EFPPolar - S$SAPTPolar
SPolarAverage <- mean(S$PolarError)

S$DispError <- S$EFPDisp - S$SAPTDisp
SDispAverage <- mean(S$DispError)


#ggplot(L, aes(x=Error, y=1))+scale_x_continuous(limits=c(-2.5,1)) + geom_vline(aes(xintercept=Error, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + geom_vline(xintercept=Average, color='black', size = 2)

STotal <- ggplot(S, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=STotalAverage, color='black', size = 2) ## +ggtitle("Small") + theme(plot.title = element_text( face="bold", size=20))
SElec <- ggplot(S, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=SElecAverage, color='black', size = 2)
SExch <- ggplot(S, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=SExchAverage, color='black', size = 2)
SPolar <- ggplot(S, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=SPolarAverage, color='black', size = 2)
SDisp <- ggplot(S, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=SDispAverage, color='black', size = 2)
SBasis <- grid.arrange(STotal, SElec, SExch, SPolar, SDisp, ncol=1)

#############################MIXED#############################
read.csv('~/Documents/SSI-R/Mixed2.csv', header=T) -> M

M$Dom <- abs(M$EFPElec)/(abs(M$EFPElec)+abs(M$EFPDisp))

M$TotalError <- M$EFPTotal - M$SAPTTotal
MTotalAverage <- mean(M$TotalError)

M$ElecError <- M$EFPElec - M$SAPTElec
MElecAverage <- mean(M$ElecError)

M$ExchError <- M$EFPExch - M$SAPTExch
MExchAverage <- mean(M$ExchError)

M$PolarError <- M$EFPPolar - M$SAPTPolar
MPolarAverage <- mean(M$PolarError)

M$DispError <- M$EFPDisp - M$SAPTDisp
MDispAverage <- mean(M$DispError)


MTotal <- ggplot(M, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=MTotalAverage, color='black', size = 2)## ggtitle("Mixed") + theme(plot.title = element_text( face="bold", size=20))
MElec <- ggplot(M, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=MElecAverage, color='black', size = 2)
MExch <- ggplot(M, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=MExchAverage, color='black', size = 2)
MPolar <- ggplot(M, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=MPolarAverage, color='black', size = 2)
MDisp <- ggplot(M, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=MDispAverage, color='black', size = 2)
MBasis <- grid.arrange(MTotal, MElec, MExch, MPolar, MDisp, ncol=1)

######Mixed2#######
read.csv('~/Documents/SSI-R/MixedScreen22.csv', header=T) -> M2

M2$Dom <- abs(M2$EFPElec)/(abs(M2$EFPElec)+abs(M2$EFPDisp))

M2$TotalError <- M2$EFPTotal - M2$SAPTTotal
M2TotalAverage <- mean(M2$TotalError)

M2$ElecError <- M2$EFPElec - M2$SAPTElec
M2ElecAverage <- mean(M2$ElecError)

M2$ExchError <- M2$EFPExch - M2$SAPTExch
M2ExchAverage <- mean(M2$ExchError)

M2$PolarError <- M2$EFPPolar - M2$SAPTPolar
M2PolarAverage <- mean(M2$PolarError)

M2$DispError <- M2$EFPDisp - M2$SAPTDisp
M2DispAverage <- mean(M2$DispError)


M2Total <- ggplot(M2, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=M2TotalAverage, color='black', size = 2)##+ ggtitle("Mixed w/ Screens") + theme(plot.title = element_text( face="bold", size=20))
M2Elec <- ggplot(M2, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=M2ElecAverage, color='black', size = 2)
M2Exch <- ggplot(M2, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=M2ExchAverage, color='black', size = 2)
M2Polar <- ggplot(M2, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=M2PolarAverage, color='black', size = 2)
M2Disp <- ggplot(M2, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=M2DispAverage, color='black', size = 2)
M2Basis <- grid.arrange(M2Total, M2Elec, M2Exch, M2Polar, M2Disp, ncol=1)

###########Mega#########
read.csv('~/Documents/SSI-R/Mega.csv', header=T) -> Mg
Mg$Dom <- abs(Mg$EFPElec)/(abs(Mg$EFPElec)+abs(Mg$EFPDisp))

Mg$TotalError <- Mg$EFPTotal - Mg$SAPTTotal
MgTotalAverage <- mean(Mg$TotalError)

Mg$ElecError <- Mg$EFPElec - Mg$SAPTElec
MgElecAverage <- mean(Mg$ElecError)

Mg$ExchError <- Mg$EFPExch - Mg$SAPTExch
MgExchAverage <- mean(Mg$ExchError)

Mg$PolarError <- Mg$EFPPolar - Mg$SAPTPolar
MgPolarAverage <- mean(Mg$PolarError)

Mg$DispError <- Mg$EFPDisp - Mg$SAPTDisp
MgDispAverage <- mean(Mg$DispError)


MgTotal <- ggplot(Mg, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=MgTotalAverage, color='black', size = 2)##+ ggtitle("Mega") + theme(plot.title = element_text( face="bold", size=20))
MgElec <- ggplot(Mg, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=MgElecAverage, color='black', size = 2)
MgExch <- ggplot(Mg, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=MgExchAverage, color='black', size = 2)
MgPolar <- ggplot(Mg, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=MgPolarAverage, color='black', size = 2)
MgDisp <- ggplot(Mg, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=MgDispAverage, color='black', size = 2)
MgBasis <- grid.arrange(MgTotal, MgElec, MgExch, MgPolar, MgDisp, ncol=1)

########MegaNoCT########

read.csv('~/Documents/SSI-R/MegaNoCT.csv', header=T) -> MgN
MgN$Dom <- abs(MgN$EFPElec)/(abs(MgN$EFPElec)+abs(MgN$EFPDisp))

MgN$TotalError <- MgN$EFPTotal - MgN$SAPTTotal
MgNTotalAverage <- mean(MgN$TotalError)

MgN$ElecError <- MgN$EFPElec - MgN$SAPTElec
MgNElecAverage <- mean(MgN$ElecError)

MgN$ExchError <- MgN$EFPExch - MgN$SAPTExch
MgNExchAverage <- mean(MgN$ExchError)

MgN$PolarError <- MgN$EFPPolar - MgN$SAPTPolar
MgNPolarAverage <- mean(MgN$PolarError)

MgN$DispError <- MgN$EFPDisp - MgN$SAPTDisp
MgNDispAverage <- mean(MgN$DispError)


MgNTotal <- ggplot(MgN, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=MgNTotalAverage, color='black', size = 2)##+ ggtitle("Mega no CT Basis") + theme(plot.title = element_text( face="bold", size=20))
MgNElec <- ggplot(MgN, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=MgNElecAverage, color='black', size = 2)
MgNExch <- ggplot(MgN, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=MgNExchAverage, color='black', size = 2)
MgNPolar <- ggplot(MgN, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=MgNPolarAverage, color='black', size = 2)
MgNDisp <- ggplot(MgN, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=MgNDispAverage, color='black', size = 2)
MgNBasis <- grid.arrange(MgNTotal, MgNElec, MgNExch, MgNPolar, MgNDisp, ncol=1)

########Mega0.3#######

read.csv('~/Documents/SSI-R/Mixed0.3.csv', header=T) -> M0.3
M0.3$Dom <- abs(M0.3$EFPElec)/(abs(M0.3$EFPElec)+abs(M0.3$EFPDisp))

M0.3$TotalError <- M0.3$EFPTotal - M0.3$SAPTTotal
M0.3TotalAverage <- mean(M0.3$TotalError)

M0.3$ElecError <- M0.3$EFPElec - M0.3$SAPTElec
M0.3ElecAverage <- mean(M0.3$ElecError)

M0.3$ExchError <- M0.3$EFPExch - M0.3$SAPTExch
M0.3ExchAverage <- mean(M0.3$ExchError)

M0.3$PolarError <- M0.3$EFPPolar - M0.3$SAPTPolar
M0.3PolarAverage <- mean(M0.3$PolarError)

M0.3$DispError <- M0.3$EFPDisp - M0.3$SAPTDisp
M0.3DispAverage <- mean(M0.3$DispError)


M0.3Total <- ggplot(M0.3, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=M0.3TotalAverage, color='black', size = 2) ##+ ggtitle("Mixed w/ Pol 0.3") + theme(plot.title = element_text( face="bold", size=20))
M0.3Elec <- ggplot(M0.3, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=M0.3ElecAverage, color='black', size = 2)
M0.3Exch <- ggplot(M0.3, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=M0.3ExchAverage, color='black', size = 2)
M0.3Polar <- ggplot(M0.3, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=M0.3PolarAverage, color='black', size = 2)
M0.3Disp <- ggplot(M0.3, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=M0.3DispAverage, color='black', size = 2)
M0.3Basis <- grid.arrange(M0.3Total, M0.3Elec, M0.3Exch, M0.3Polar, M0.3Disp, ncol=1)

###########B_w_CT######

read.csv('~/Documents/SSI-R/B_w_CT.csv', header=T) -> B

B$Dom <- abs(B$EFPElec)/(abs(B$EFPElec)+abs(B$EFPDisp))

B$TotalError <- B$EFPTotal - B$SAPTTotal
BTotalAverage <- mean(B$TotalError)

B$ElecError <- B$EFPElec - B$SAPTElec
BElecAverage <- mean(B$ElecError)

B$ExchError <- B$EFPExch - B$SAPTExch
BExchAverage <- mean(B$ExchError)

B$PolarError <- B$EFPPolar - B$SAPTPolar
BPolarAverage <- mean(B$PolarError)

B$DispError <- B$EFPDisp - B$SAPTDisp
BDispAverage <- mean(B$DispError)


BTotal <- ggplot(B, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=BTotalAverage, color='black', size = 2)##+ ggtitle("Best") + theme(plot.title = element_text( face="bold", size=20))
BElec <- ggplot(B, aes(x=B$ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=BElecAverage, color='black', size = 2)
BExch <- ggplot(B, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=BExchAverage, color='black', size = 2)
BPolar <- ggplot(B, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=BPolarAverage, color='black', size = 2)
BDisp <- ggplot(B, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=BDispAverage, color='black', size = 2)
BBasis <- grid.arrange(BTotal, BExch, BPolar, BDisp, ncol=1)

######B_w_no_CT#######

read.csv('~/Documents/SSI-R/B_no_CT.csv', header=T) -> BN

BN$Dom <- abs(BN$EFPElec)/(abs(BN$EFPElec)+abs(BN$EFPDisp))

BN$TotalError <- BN$EFPTotal - BN$SAPTTotal
BNTotalAverage <- mean(BN$TotalError)

BN$ElecError <- BN$EFPElec - BN$SAPTElec
BNElecAverage <- mean(BN$ElecError)

BN$ExchError <- BN$EFPExch - BN$SAPTExch
BNExchAverage <- mean(BN$ExchError)

BN$PolarError <- BN$EFPPolar - BN$SAPTPolar
BNPolarAverage <- mean(BN$PolarError)

BN$DispError <- BN$EFPDisp - BN$SAPTDisp
BNDispAverage <- mean(BN$DispError)


BNTotal <- ggplot(BN, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=BNTotalAverage, color='black', size = 2)##+ ggtitle("Best no CT") + theme(plot.title = element_text( face="bold", size=20))
BNElec <- ggplot(BN, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=BNElecAverage, color='black', size = 2)
BNExch <- ggplot(BN, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=BNExchAverage, color='black', size = 2)
BNPolar <- ggplot(BN, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=BNPolarAverage, color='black', size = 2)
BNDisp <- ggplot(BN, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=BNDispAverage, color='black', size = 2)
BNBasis <- multiplot(BNTotal, BNElec, BNExch, BNPolar, BNDisp)

####### B_0.3_CT######

read.csv('~/Documents/SSI-R/B_0.3_CT.csv', header=T) -> B_0.3_CT

B_0.3_CT$Dom <- abs(B_0.3_CT$EFPElec)/(abs(B_0.3_CT$EFPElec)+abs(B_0.3_CT$EFPDisp))

B_0.3_CT$TotalError <- B_0.3_CT$EFPTotal - B_0.3_CT$SAPTTotal
B_0.3_CTTotalAverage <- mean(B_0.3_CT$TotalError)

B_0.3_CT$ElecError <- B_0.3_CT$EFPElec - B_0.3_CT$SAPTElec
B_0.3_CTElecAverage <- mean(B_0.3_CT$ElecError)

B_0.3_CT$ExchError <- B_0.3_CT$EFPExch - B_0.3_CT$SAPTExch
B_0.3_CTExchAverage <- mean(B_0.3_CT$ExchError)

B_0.3_CT$PolarError <- B_0.3_CT$EFPPolar - B_0.3_CT$SAPTPolar
B_0.3_CTPolarAverage <- mean(B_0.3_CT$PolarError)

B_0.3_CT$DispError <- B_0.3_CT$EFPDisp - B_0.3_CT$SAPTDisp
B_0.3_CTDispAverage <- mean(B_0.3_CT$DispError)


B_0.3_CTTotal <- ggplot(B_0.3_CT, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=B_0.3_CTTotalAverage, color='black', size = 2)##+ ggtitle("Best 0.3 Pol") + theme(plot.title = element_text( face="bold", size=20))
B_0.3_CTElec <- ggplot(B_0.3_CT, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=B_0.3_CTElecAverage, color='black', size = 2)
B_0.3_CTExch <- ggplot(B_0.3_CT, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=B_0.3_CTExchAverage, color='black', size = 2)
B_0.3_CTPolar <- ggplot(B_0.3_CT, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=B_0.3_CTPolarAverage, color='black', size = 2)
B_0.3_CTDisp <- ggplot(B_0.3_CT, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=B_0.3_CTDispAverage, color='black', size = 2)
B_0.3_CTBasis <- grid.arrange(B_0.3_CTTotal, B_0.3_CTElec, B_0.3_CTExch, B_0.3_CTPolar, B_0.3_CTDisp, ncol=1)
#######B_0.3_NoCT####

read.csv('~/Documents/SSI-R/B_0.3_NoCT.csv', header=T) -> B_0.3_noCT


B_0.3_noCT$Dom <- abs(B_0.3_noCT$EFPElec)/(abs(B_0.3_noCT$EFPElec)+abs(B_0.3_noCT$EFPDisp))

B_0.3_noCT$TotalError <- B_0.3_noCT$EFPTotal - B_0.3_noCT$SAPTTotal
B_0.3_noCTTotalAverage <- mean(B_0.3_noCT$TotalError)

B_0.3_noCT$ElecError <- B_0.3_noCT$EFPElec - B_0.3_noCT$SAPTElec
B_0.3_noCTElecAverage <- mean(B_0.3_noCT$ElecError)

B_0.3_noCT$ExchError <- B_0.3_noCT$EFPExch - B_0.3_noCT$SAPTExch
B_0.3_noCTExchAverage <- mean(B_0.3_noCT$ExchError)

B_0.3_noCT$PolarError <- B_0.3_noCT$EFPPolar - B_0.3_noCT$SAPTPolar
B_0.3_noCTPolarAverage <- mean(B_0.3_noCT$PolarError)

B_0.3_noCT$DispError <- B_0.3_noCT$EFPDisp - B_0.3_noCT$SAPTDisp
B_0.3_noCTDispAverage <- mean(B_0.3_noCT$DispError)


B_0.3_noCTTotal <- ggplot(B_0.3_noCT, aes(x=TotalError, y=1))+ geom_vline(aes(xintercept=TotalError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Total") + geom_vline(xintercept=B_0.3_noCTTotalAverage, color='black', size = 2)##+ ggtitle("Best 0.3 Pol no CT") + theme(plot.title = element_text( face="bold", size=20))
B_0.3_noCTElec <- ggplot(B_0.3_noCT, aes(x=ElecError, y=1))+ geom_vline(aes(xintercept=ElecError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Elec") + geom_vline(xintercept=B_0.3_noCTElecAverage, color='black', size = 2)
B_0.3_noCTExch <- ggplot(B_0.3_noCT, aes(x=ExchError, y=1))+ geom_vline(aes(xintercept=ExchError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Exch") + geom_vline(xintercept=B_0.3_noCTExchAverage, color='black', size = 2)
B_0.3_noCTPolar <- ggplot(B_0.3_noCT, aes(x=PolarError, y=1))+ geom_vline(aes(xintercept=PolarError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Polar") + geom_vline(xintercept=B_0.3_noCTPolarAverage, color='black', size = 2)
B_0.3_noCTDisp <- ggplot(B_0.3_noCT, aes(x=DispError, y=1))+ geom_vline(aes(xintercept=DispError, color=Dom), size=0.5) +scale_colour_gradientn(colours=jet.colors(7), guide=FALSE) + labs(x=" ", y="Disp") + geom_vline(xintercept=B_0.3_noCTDispAverage, color='black', size = 2)
B_0.3_noCTBasis <- grid.arrange(B_0.3_noCTTotal, B_0.3_noCTElec, B_0.3_noCTExch, B_0.3_noCTPolar, B_0.3_noCTDisp, ncol=1)
