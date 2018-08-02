
library(gridExtra)
library("grid")
library("ggplot2")
library("lattice")
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
#you need to change this: 
load_data<-function(x){
  b_ct<-'/Users/yhb8r4/Documents/SSI-Data/B_ct_hie.dat.error'
  b<-'/Users/yhb8r4/Documents/SSI-Data/B_hie.dat.error'
  medium<-'/Users/yhb8r4/Documents/SSI-Data/Medium_hie.dat.error'
  sapt<-'/Users/yhb8r4/Documents/SSI-Data/SAPT0_hie.dat.error'
  smb_ct<-'/Users/yhb8r4/Documents/SSI-Data/SMB_ct_hie.dat.error'
  smb_ct_ps<-'/Users/yhb8r4/Documents/SSI-Data/SMB_ct_ps_hie.dat.error'
  smb<-'/Users/yhb8r4/Documents/SSI-Data/SMB_hie.dat.error'
  smb_ps<-'/Users/yhb8r4/Documents/SSI-Data/SMB_ps_hie.dat.error'
  sm_es<-'/Users/yhb8r4/Documents/SSI-Data/Sm_es_hie.dat.error'
  sm<-'/Users/yhb8r4/Documents/SSI-Data/Sm_hie.dat.error'
  sm_ps<-'/Users/yhb8r4/Documents/SSI-Data/Sm_ps_hie.dat.error'
  small<-'/Users/yhb8r4/Documents/SSI-Data/Small_hie.dat.error'
  
  bf<<-read.table(b, header=TRUE, sep="")
  b_ctf<<-read.table(b_ct, header=TRUE, sep="")
  mediumf<<-read.table(medium, header=TRUE, sep="")
  smbf<<-read.table(smb, header=TRUE, sep="")
  smb_ctf<<-read.table(smb_ct, header=TRUE, sep="")
  smb_ct_psf<<-read.table(smb_ct_ps, header=TRUE, sep="")
  smb_psf<<-read.table(smb_ps, header=TRUE, sep="")
  smf<<-read.table(sm, header=TRUE, sep="")
  sm_esf<<-read.table(sm_es, header=TRUE, sep="")
  sm_psf<<-read.table(sm_ps, header=TRUE, sep="")
  smallf<<-read.table(small, header=TRUE, sep="")
  saptf<<-read.table(sapt, header=TRUE, sep="")
}

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

calc_mad<-function(Basis,EFPComp, SAPTComp){
  return(mean(abs(Basis[,EFPComp]-Basis[,SAPTComp])))
}
calc_msd<-function(Basis,EFPComp, SAPTComp){
  return(mean(Basis[,EFPComp]-Basis[,SAPTComp]))
}
calc_rel_error<-function(Basis,EFPComp, SAPTComp){
  return(c(Basis[,EFPComp]-Basis[,SAPTComp]))
}
calc_dom_force<-function(Basis){
  c(abs(Basis$EFPElec)/(abs(Basis$EFPElec)+abs(Basis$EFPDisp)))
}

gen_rel_component_mad<-function(Basis,EFPComp, SAPTComp){
  efp<-deparse(substitute(EFPComp))
  sapt<-deparse(substitute(SAPTComp))
  
  x<-calc_rel_error(Basis,efp,sapt)
  y=1
  z<-calc_dom_force(Basis)
  
  average<-calc_mad(Basis,efp,sapt)
  
  
  df<-data.frame(x,z)
  colnames(df)<-c("errors","dom_force")
  
  plot<-ggplot(df,aes(x=errors,y=1)) + 
    scale_x_continuous(limits=c(-10,10))+
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    geom_vline(aes(xintercept=errors,color=dom_force),size=0.5)+
    scale_colour_gradientn(colours=jet.colors(7), guide=FALSE)+
    geom_vline(xintercept=average, color='black', size=2)+
    theme(panel.background = element_blank())
  
  return(plot)
}
gen_rel_component_msd<-function(Basis,EFPComp, SAPTComp){
  efp<-deparse(substitute(EFPComp))
  sapt<-deparse(substitute(SAPTComp))
  
  x<-calc_rel_error(Basis,efp,sapt)
  y=1
  z<-calc_dom_force(Basis)
  
  average<-calc_msd(Basis,efp,sapt)
  
  
  df<-data.frame(x,z)
  colnames(df)<-c("errors","dom_force")
  
  plot<-ggplot(df,aes(x=errors,y=1)) + 
    scale_x_continuous(limits=c(-10,10))+
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank())+
    geom_vline(aes(xintercept=errors,color=dom_force),size=0.5)+
    scale_colour_gradientn(colours=jet.colors(7), guide=FALSE)+
    geom_vline(xintercept=average, color='black', size=2)+
    theme(panel.background = element_blank())
  
  return(plot)
}
gen_rel_blocks_elec<-function(a,b,c,d,e,f){
  a_plot<-gen_rel_component_mad(a,EFPElec, SAPTElec)
  b_plot<-gen_rel_component_mad(b,EFPElec, SAPTElec)
  c_plot<-gen_rel_component_mad(c,EFPElec, SAPTElec)
  d_plot<-gen_rel_component_mad(d,EFPElec, SAPTElec)
  e_plot<-gen_rel_component_mad(e,EFPElec, SAPTElec)
  f_plot<-gen_rel_component_mad(f,EFPElec, SAPTElec) 
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,ncol=1)
  return(bloc)
}
gen_rel_blocks_elec_msd<-function(a,b,c,d,e,f){
  a_plot<-gen_rel_component_msd(a,EFPElec, SAPTElec)
  b_plot<-gen_rel_component_msd(b,EFPElec, SAPTElec)
  c_plot<-gen_rel_component_msd(c,EFPElec, SAPTElec)
  d_plot<-gen_rel_component_msd(d,EFPElec, SAPTElec)
  e_plot<-gen_rel_component_msd(e,EFPElec, SAPTElec)
  f_plot<-gen_rel_component_msd(f,EFPElec, SAPTElec) 
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,ncol=1)
  return(bloc)
}
gen_rel_blocks_exch<-function(a,b,c){
  a_plot<-gen_rel_component_mad(a,EFPExch, SAPTExch)
  b_plot<-gen_rel_component_mad(b,EFPExch, SAPTExch)
  c_plot<-gen_rel_component_mad(c,EFPExch, SAPTExch)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,ncol=1)
  return(bloc)
}
gen_rel_blocks_exch_msd<-function(a,b,c,d){
  a_plot<-gen_rel_component_msd(a,EFPExch, SAPTExch)
  b_plot<-gen_rel_component_msd(b,EFPExch, SAPTExch)
  c_plot<-gen_rel_component_msd(c,EFPExch, SAPTExch)
  d_plot<-gen_rel_component_msd(d,EFPExch, SAPTExch)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,ncol=1)
  return(bloc)
}
gen_rel_blocks_polar<-function(a,b,c,d,e,f,g,h,i,j){
  a_plot<-gen_rel_component_mad(a,EFPPolar, SAPTPolar)
  b_plot<-gen_rel_component_mad(b,EFPPolar, SAPTPolar)
  c_plot<-gen_rel_component_mad(c,EFPPolar, SAPTPolar)
  d_plot<-gen_rel_component_mad(d,EFPPolar, SAPTPolar)
  e_plot<-gen_rel_component_mad(e,EFPPolar, SAPTPolar)
  f_plot<-gen_rel_component_mad(f,EFPPolar, SAPTPolar)
  g_plot<-gen_rel_component_mad(g,EFPPolar, SAPTPolar)
  h_plot<-gen_rel_component_mad(h,EFPPolar, SAPTPolar)
  i_plot<-gen_rel_component_mad(i,EFPPolar, SAPTPolar)
  j_plot<-gen_rel_component_mad(j,EFPPolar, SAPTPolar)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,g_plot,h_plot,i_plot,j_plot,ncol=1,padding=1.0)
  return(bloc)
}
gen_rel_blocks_polar_msd<-function(a,b,c){
  a_plot<-gen_rel_component_msd(a,EFPPolar, SAPTPolar)
  b_plot<-gen_rel_component_msd(b,EFPPolar, SAPTPolar)
  c_plot<-gen_rel_component_msd(c,EFPPolar, SAPTPolar)
  d_plot<-gen_rel_component_msd(d,EFPPolar, SAPTPolar)
  e_plot<-gen_rel_component_msd(e,EFPPolar, SAPTPolar)
  f_plot<-gen_rel_component_msd(f,EFPPolar, SAPTPolar)
  g_plot<-gen_rel_component_msd(g,EFPPolar, SAPTPolar)
  h_plot<-gen_rel_component_msd(h,EFPPolar, SAPTPolar)
  i_plot<-gen_rel_component_msd(i,EFPPolar, SAPTPolar)
  j_plot<-gen_rel_component_msd(j,EFPPolar, SAPTPolar)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,g_plot,h_plot,i_plot,j_plot,ncol=1,padding=0.2)
  return(bloc)
}
gen_rel_blocks_Disp<-function(a,b,c,d,e,f){
  a_plot<-gen_rel_component_mad(a,EFPDisp, SAPTDisp)
  b_plot<-gen_rel_component_mad(b,EFPDisp, SAPTDisp)
  c_plot<-gen_rel_component_mad(c,EFPDisp, SAPTDisp)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,ncol=1)
  return(bloc)
}
gen_rel_blocks_Disp_msd<-function(a,b,c,d,e,f){
  a_plot<-gen_rel_component_msd(a,EFPDisp, SAPTDisp)
  b_plot<-gen_rel_component_msd(b,EFPDisp, SAPTDisp)
  c_plot<-gen_rel_component_msd(c,EFPDisp, SAPTDisp)
  d_plot<-gen_rel_component_msd(d,EFPDisp, SAPTDisp)
  e_plot<-gen_rel_component_msd(e,EFPDisp, SAPTDisp)
  f_plot<-gen_rel_component_msd(f,EFPDisp, SAPTDisp)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,e_plot,ncol=1)
  return(bloc)
}
gen_rel_blocks_total<-function(a,b,c,d,e,f,g,h,i,j,k){
  a_plot<-gen_rel_component_mad(a,EFPTotal, SAPTTotal)
  b_plot<-gen_rel_component_mad(b,EFPTotal, SAPTTotal)
  c_plot<-gen_rel_component_mad(c,EFPTotal, SAPTTotal)
  d_plot<-gen_rel_component_mad(d,EFPTotal, SAPTTotal)
  e_plot<-gen_rel_component_mad(e,EFPTotal, SAPTTotal)
  f_plot<-gen_rel_component_mad(f,EFPTotal, SAPTTotal)
  g_plot<-gen_rel_component_mad(g,EFPTotal, SAPTTotal)
  h_plot<-gen_rel_component_mad(h,EFPTotal, SAPTTotal)
  i_plot<-gen_rel_component_mad(i,EFPTotal, SAPTTotal)
  j_plot<-gen_rel_component_mad(j,EFPTotal, SAPTTotal)
  k_plot<-gen_rel_component_mad(k,EFPTotal, SAPTTotal)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,g_plot,h_plot,i_plot,j_plot,k_plot, ncol=1) +
    

  return(bloc)
}
gen_rel_blocks_total_msd<-function(a,b,c,d,e,f,g,h,i,j,k){
  a_plot<-gen_rel_component_msd(a,EFPTotal, SAPTTotal)
  b_plot<-gen_rel_component_msd(b,EFPTotal, SAPTTotal)
  c_plot<-gen_rel_component_msd(c,EFPTotal, SAPTTotal)
  d_plot<-gen_rel_component_msd(d,EFPTotal, SAPTTotal)
  e_plot<-gen_rel_component_msd(e,EFPTotal, SAPTTotal)
  f_plot<-gen_rel_component_msd(f,EFPTotal, SAPTTotal)
  g_plot<-gen_rel_component_msd(g,EFPTotal, SAPTTotal)
  h_plot<-gen_rel_component_msd(h,EFPTotal, SAPTTotal)
  i_plot<-gen_rel_component_msd(i,EFPTotal, SAPTTotal)
  j_plot<-gen_rel_component_msd(j,EFPTotal, SAPTTotal)
  k_plot<-gen_rel_component_msd(k,EFPTotal, SAPTTotal)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,g_plot,h_plot,i_plot,j_plot,k_plot, ncol=1)
  return(bloc)
}

#gen_rel_blocks_elec(L,S,M,Mg,B)


# generate MAD
gen.msd <- function(x){
  charged <-c("cationic", "anionic")
  nonpolar <- c("aliphatic", "aryl")
  polar    <- c("polar")
  
  Total_ionic <- mean((x$EFPTotal[charged_idx]-x$SAPTTotal[charged_idx]))
  Total_nonpolar <- mean((x$EFPTotal[nonpolar_idx]-x$SAPTTotal[nonpolar_idx]))
  Total_polar <- mean((x$EFPTotal[polar_idx]-x$SAPTTotal[polar_idx]))
  Total_total <- mean((x$EFPTotal-x$SAPTTotal))
  
  Elec_ionic <- mean((x$EFPElec[charged_idx]-x$SAPTElec[charged_idx]))
  Elec_nonpolar <- mean((x$EFPElec[nonpolar_idx]-x$SAPTElec[nonpolar_idx]))
  Elec_polar <- mean((x$EFPElec[polar_idx]-x$SAPTElec[polar_idx]))
  Elec_total <- mean((x$EFPElec-x$SAPTElec))
  
  Exch_ionic <- mean((x$EFPExch[charged_idx]-x$SAPTExch[charged_idx]))
  Exch_nonpolar <- mean((x$EFPExch[nonpolar_idx]-x$SAPTExch[nonpolar_idx]))
  Exch_polar <- mean((x$EFPExch[polar_idx]-x$SAPTExch[polar_idx]))
  Exch_total <- mean((x$EFPExch-x$SAPTExch))
  
  Polar_ionic <- mean((x$EFPPolar[charged_idx]-x$SAPTPolar[charged_idx]))
  Polar_nonpolar <- mean((x$EFPPolar[nonpolar_idx]-x$SAPTPolar[nonpolar_idx]))
  Polar_polar <- mean((x$EFPPolar[polar_idx]-x$SAPTPolar[polar_idx]))
  Polar_total <- mean((x$EFPPolar-x$SAPTPolar))
  
  Disp_ionic <- mean((x$EFPDisp[charged_idx]-x$SAPTDisp[charged_idx]))
  Disp_nonpolar <- mean((x$EFPDisp[nonpolar_idx]-x$SAPTDisp[nonpolar_idx]))
  Disp_polar <- mean((x$EFPDisp[polar_idx]-x$SAPTDisp[polar_idx]))
  Disp_total <- mean((x$EFPDisp-x$SAPTDisp))
  
  totals <- c(Total_ionic, Total_polar, Total_nonpolar, Total_total)
  elecs <- c(Elec_ionic, Elec_polar, Elec_nonpolar, Elec_total)
  exchs <- c(Exch_ionic, Exch_polar, Exch_nonpolar, Exch_total)
  polars <- c(Polar_ionic, Polar_polar, Polar_nonpolar, Polar_total)
  disp <- c(Disp_ionic, Disp_polar, Disp_nonpolar, Disp_total)
  
  round(rbind(totals,elecs,exchs,polars,disp), digits=2)
}

gen.mad <- function(x){
  charged <-c("cationic", "anionic")
  nonpolar <- c("aliphatic", "aryl")
  polar    <- c("polar")
  
  charged_idx <- which((x$T1 == charged) & (x$T2 == charged))
  nonpolar_idx <- which((x$T1 == nonpolar) & (x$T2 == nonpolar))
  polar_idx <- which((x$T1==polar) & (x$T2==polar))
  
  Total_ionic <- mean(abs(x$EFPTotal[charged_idx]-x$SAPTTotal[charged_idx]))
  Total_nonpolar <- mean(abs(x$EFPTotal[nonpolar_idx]-x$SAPTTotal[nonpolar_idx]))
  Total_polar <- mean(abs(x$EFPTotal[polar_idx]-x$SAPTTotal[polar_idx]))
  Total_total <- mean(abs(x$EFPTotal-x$SAPTTotal))
  
  Elec_ionic <- mean(abs(x$EFPElec[charged_idx]-x$SAPTElec[charged_idx]))
  Elec_nonpolar <- mean(abs(x$EFPElec[nonpolar_idx]-x$SAPTElec[nonpolar_idx]))
  Elec_polar <- mean(abs(x$EFPElec[polar_idx]-x$SAPTElec[polar_idx]))
  Elec_total <- mean(abs(x$EFPElec-x$SAPTElec))
  
  Exch_ionic <- mean(abs(x$EFPExch[charged_idx]-x$SAPTExch[charged_idx]))
  Exch_nonpolar <- mean(abs(x$EFPExch[nonpolar_idx]-x$SAPTExch[nonpolar_idx]))
  Exch_polar <- mean(abs(x$EFPExch[polar_idx]-x$SAPTExch[polar_idx]))
  Exch_total <- mean(abs(x$EFPExch-x$SAPTExch))
  
  Polar_ionic <- mean(abs(x$EFPPolar[charged_idx]-x$SAPTPolar[charged_idx]))
  Polar_nonpolar <- mean(abs(x$EFPPolar[nonpolar_idx]-x$SAPTPolar[nonpolar_idx]))
  Polar_polar <- mean(abs(x$EFPPolar[polar_idx]-x$SAPTPolar[polar_idx]))
  Polar_total <- mean(abs(x$EFPPolar-x$SAPTPolar))
  
  Disp_ionic <- mean(abs(x$EFPDisp[charged_idx]-x$SAPTDisp[charged_idx]))
  Disp_nonpolar <- mean(abs(x$EFPDisp[nonpolar_idx]-x$SAPTDisp[nonpolar_idx]))
  Disp_polar <- mean(abs(x$EFPDisp[polar_idx]-x$SAPTDisp[polar_idx]))
  Disp_total <- mean(abs(x$EFPDisp-x$SAPTDisp))
  
  totals <- c(Total_ionic, Total_polar, Total_nonpolar, Total_total)
  elecs <- c(Elec_ionic, Elec_polar, Elec_nonpolar, Elec_total)
  exchs <- c(Exch_ionic, Exch_polar, Exch_nonpolar, Exch_total)
  polars <- c(Polar_ionic, Polar_polar, Polar_nonpolar, Polar_total)
  disp <- c(Disp_ionic, Disp_polar, Disp_nonpolar, Disp_total)
  
  round(rbind(totals,elecs,exchs,polars,disp), digits=2)
}

