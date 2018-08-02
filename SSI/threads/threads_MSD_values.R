library(ggplot2)
library(gridExtra)
library(base)
library(scales)
library(formattable)

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

gen.mad <- function(x){
charged <-c("cationic", "anionic")
nonpolar <- c("aliphatic", "aryl")
polar    <- c("polar")

charged_idx <- which((x$Type1 == charged) & (x$Type2 == charged))
nonpolar_idx <- which((x$Type1 == nonpolar) & (x$Type2 == nonpolar))
polar_idx <- which((x$Type1==polar) & (x$Type2==polar))

Total_ionic <- mean(x$EFPTotal[charged_idx]-x$SAPTTotal[charged_idx])
Total_nonpolar <- mean(x$EFPTotal[nonpolar_idx]-x$SAPTTotal[nonpolar_idx])
Total_polar <- mean(x$EFPTotal[polar_idx]-x$SAPTTotal[polar_idx])
Total_total <- mean(x$EFPTotal-x$SAPTTotal)

Elec_ionic <- mean(x$EFPElec[charged_idx]-x$SAPTElec[charged_idx])
Elec_nonpolar <- mean(x$EFPElec[nonpolar_idx]-x$SAPTElec[nonpolar_idx])
Elec_polar <- mean(x$EFPElec[polar_idx]-x$SAPTElec[polar_idx])
Elec_total <- mean(x$EFPElec-x$SAPTElec)

Exch_ionic <- mean(x$EFPExch[charged_idx]-x$SAPTExch[charged_idx])
Exch_nonpolar <- mean(x$EFPExch[nonpolar_idx]-x$SAPTExch[nonpolar_idx])
Exch_polar <- mean(x$EFPExch[polar_idx]-x$SAPTExch[polar_idx])
Exch_total <- mean(x$EFPExch-x$SAPTExch)

Polar_ionic <- mean(x$EFPPolar[charged_idx]-x$SAPTPolar[charged_idx])
Polar_nonpolar <- mean(x$EFPPolar[nonpolar_idx]-x$SAPTPolar[nonpolar_idx])
Polar_polar <- mean(x$EFPPolar[polar_idx]-x$SAPTPolar[polar_idx])
Polar_total <- mean(x$EFPPolar-x$SAPTPolar)

Disp_ionic <- mean(x$EFPDisp[charged_idx]-x$SAPTDisp[charged_idx])
Disp_nonpolar <- mean(x$EFPDisp[nonpolar_idx]-x$SAPTDisp[nonpolar_idx])
Disp_polar <- mean(x$EFPDisp[polar_idx]-x$SAPTDisp[polar_idx])
Disp_total <- mean(x$EFPDisp-x$SAPTDisp)

totals <- c(Total_ionic, Total_polar, Total_nonpolar, Total_total)
elecs <- c(Elec_ionic, Elec_polar, Elec_nonpolar, Elec_total)
exchs <- c(Exch_ionic, Exch_polar, Exch_nonpolar, Exch_total)
polars <- c(Polar_ionic, Polar_polar, Polar_nonpolar, Polar_total)
disp <- c(Disp_ionic, Disp_polar, Disp_nonpolar, Disp_total)

round(rbind(totals,elecs,exchs,polars,disp), digits=2)
}