library(ggplot2)
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

load_data<-function(x){
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
  
  #Larger2 = Medium
  #Smaller = Small
  #Mixed2 = SM
  #MixedScreen22 = SM_es
  #Mixed0.3.csv = Sm_ps
  #Mg = Big with CT
  #MgN = Big 
  #B_w_ct = SMB w/ CT
  #B_w_noCT = SMB 
  #B_0.3_CT = SMB w/ CT w/ pol screens
  #B_0.3_noCT = SMB w/ pol screens
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
    geom_vline(xintercept=average, color='black', size=2)
  
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
    geom_vline(xintercept=average, color='black', size=2)
  
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
gen_rel_blocks_exch<-function(a,b,c,d){
  a_plot<-gen_rel_component_mad(a,EFPExch, SAPTExch)
  b_plot<-gen_rel_component_mad(b,EFPExch, SAPTExch)
  c_plot<-gen_rel_component_mad(c,EFPExch, SAPTExch)
  d_plot<-gen_rel_component_mad(d,EFPExch, SAPTExch)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,ncol=1)
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
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,g_plot,h_plot,i_plot,j_plot,ncol=1,padding=0.2)
  return(bloc)
}
gen_rel_blocks_polar_msd<-function(a,b,c,d,e,f,g,h,i,j){
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
  d_plot<-gen_rel_component_mad(d,EFPDisp, SAPTDisp)
  e_plot<-gen_rel_component_mad(e,EFPDisp, SAPTDisp)
  f_plot<-gen_rel_component_mad(f,EFPDisp, SAPTDisp)
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,e_plot,ncol=1)
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
  bloc <-grid.arrange(a_plot,b_plot,c_plot,d_plot,e_plot,f_plot,g_plot,h_plot,i_plot,j_plot,k_plot, ncol=1)
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

