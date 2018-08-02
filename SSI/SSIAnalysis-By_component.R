get_type<-function(L){
  #charged
  charged_index<<-c(
    which(paste(L$T1,L$T2)=="cationic cationic"),
    which(paste(L$T1,L$T2)=="cationic anionic"),
    which(paste(L$T1,L$T2)=="anionic cationic"),
    which(paste(L$T1,L$T2)=="anionic anionic")
  )
  
  #polar
  polar_index<<-c(
    which(paste(L$T1,L$T2)=="cationic polar"),
    which(paste(L$T1,L$T2)=="anionic polar"),
    which(paste(L$T1,L$T2)=="polar polar"),
    which(paste(L$T1,L$T2)=="polar aliphatic"),
    which(paste(L$T1,L$T2)=="polar aryl")
  )
  
  #nonpolar block
  
  nonpolar_index<<-c(
    which(paste(L$T1,L$T2)=="aliphatic aliphatic"),
    which(paste(L$T1,L$T2)=="aliphatic aryl"),
    which(paste(L$T1,L$T2)=="aryl aryl")
  )
  total_index<<-c(seq(length(L$T1)))
}


basis_component_type_mad<-function(Basis,EFPComponent, SAPTComponent, type){
  get_type(Basis)
  mean(abs(Basis[,EFPComponent][type] - Basis[,SAPTComponent][type]))
}

basis_component_type_average_relative_mad<-function(Basis,EFPComponent, SAPTComponent, type){
  get_type(Basis)
  mean(abs((Basis[,EFPComponent][type] - Basis[,SAPTComponent][type])/abs(Basis[,SAPTComponent][type])))
}
basis_component_type_msd<-function(Basis,EFPComponent, SAPTComponent, type){
  get_type(Basis)
  mean(Basis[,EFPComponent][type] - Basis[,SAPTComponent][type])
}
basis_component_type_average_relative_msd<-function(Basis,EFPComponent, SAPTComponent, type){
  get_type(Basis)
  mean((Basis[,EFPComponent][type] - Basis[,SAPTComponent][type])/Basis[,SAPTComponent][type])
}
basis_component_type_sd<-function(Basis,EFPComponent, SAPTComponent, type){
  get_type(Basis)
  sd(Basis[,EFPComponent][type] - Basis[,SAPTComponent][type])
}

#Generate MADMSDSD block

gen_mad_msd_std_block<-function(L, efp_comp, sapt_comp) {
  efp<-deparse(substitute(efp_comp))
  sapt<-deparse(substitute(sapt_comp))
  
  MAD<-c(basis_component_type_mad(L, efp, sapt, charged_index),
         basis_component_type_mad(L, efp, sapt, polar_i-ndex),
         basis_component_type_mad(L, efp, sapt, nonpolar_index),
         basis_component_type_mad(L, efp, sapt, total_index)
  )
  
  MSD<-c(basis_component_type_msd(L, efp, sapt, charged_index),
         basis_component_type_msd(L, efp, sapt, polar_index),
         basis_component_type_msd(L, efp, sapt, nonpolar_index),
         basis_component_type_msd(L, efp, sapt, total_index)
  )
  SD <-basis_component_type_sd(L, efp, sapt, total_index)
  
  round(c(MAD,MSD,SD),2)
}
gen_relative_mad_msd_std_block<-function(L, efp_comp, sapt_comp) {
  efp<-deparse(substitute(efp_comp))
  sapt<-deparse(substitute(sapt_comp))
  
  MAD<-c(basis_component_type_average_relative_mad(L, efp, sapt, charged_index),
         basis_component_type_average_relative_mad(L, efp, sapt, polar_index),
         basis_component_type_average_relative_mad(L, efp, sapt, nonpolar_index),
         basis_component_type_average_relative_mad(L, efp, sapt, total_index)
  )
  
  MSD<-c(basis_component_type_average_relative_msd(L, efp, sapt, charged_index),
         basis_component_type_average_relative_msd(L, efp, sapt, polar_index),
         basis_component_type_average_relative_msd(L, efp, sapt, nonpolar_index),
         basis_component_type_average_relative_msd(L, efp, sapt, total_index)
  )
  SD <-basis_component_type_sd(L, efp, sapt, total_index)
  
  round(c(MAD*100,MSD*100,SD),2)
}


#generate Electric

rbind(
  gen_mad_msd_std_block(L, EFPElec,SAPTElec),
  gen_mad_msd_std_block(S, EFPElec,SAPTElec),
  gen_mad_msd_std_block(M, EFPElec,SAPTElec),
  gen_mad_msd_std_block(M2, EFPElec,SAPTElec),
  gen_mad_msd_std_block(Mg, EFPElec,SAPTElec),
  gen_mad_msd_std_block(MgN, EFPElec,SAPTElec),
  gen_mad_msd_std_block(M0.3, EFPElec,SAPTElec),
  gen_mad_msd_std_block(B, EFPElec,SAPTElec),
  gen_mad_msd_std_block(BN, EFPElec,SAPTElec),
  gen_mad_msd_std_block(B_0.3_CT, EFPElec,SAPTElec),
  gen_mad_msd_std_block(B_0.3_noCT, EFPElec,SAPTElec)
)

# gen_rel_mad_msd_std_block_elec(mediumf,smallf, smf,sm_esf,bf,smbf)
gen_rel_mad_msd_std_block_elec<-function(M,S,SM,Sm_es,B,SMB){
  table<-rbind(
    gen_relative_mad_msd_std_block(M, EFPElec, SAPTElec), 
    gen_relative_mad_msd_std_block(S, EFPElec, SAPTElec), 
    gen_relative_mad_msd_std_block(SM, EFPElec, SAPTElec), 
    gen_relative_mad_msd_std_block(Sm_es, EFPElec, SAPTElec), 
    gen_relative_mad_msd_std_block(B, EFPElec, SAPTElec),
    gen_relative_mad_msd_std_block(SMB, EFPElec, SAPTElec)
  )
  rownames(table) <- c("M", "S", "SM","SM_es","B", "SMB")
  colnames(table) <- c("MAD Ionic", "MAD Polar", "MAD NP", "MAD Total", "MSDIonic", "MSDPol", "MSDNP", "MSDTotal", "STD")
  table 
}
#gen_rel_mad_msd_std_block_exch(mediumf,smallf, bf)
gen_rel_mad_msd_std_block_exch<-function(M,S,B){
  table<-rbind(
    gen_relative_mad_msd_std_block(M, EFPExch, SAPTExch), 
    gen_relative_mad_msd_std_block(S, EFPExch, SAPTExch), 
    gen_relative_mad_msd_std_block(B, EFPExch, SAPTExch) 
  )
  rownames(table) <- c("M", "S","B")
  colnames(table) <- c("MAD Ionic", "MAD Polar", "MAD NP", "MAD Total", "MSDIonic", "MSDPol", "MSDNP", "MSDTotal", "STD")
  table 
}

#gen_rel_mad_msd_std_block_polar(mediumf,smallf,smf,b_ctf, bf, sm_psf, smb_ctf, smbf, smb_ct_psf, smb_psf)
gen_rel_mad_msd_std_block_polar<-function(M,S,SM,B_ct, B, SM_ps, SMB_ct, SMB, SMB_ps_ct, SMB_ps){
  table<-rbind(
    gen_relative_mad_msd_std_block(M, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(S, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(SM, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(B_ct, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(B, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(SM_ps, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(SMB_ct, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(SMB, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(SMB_ps_ct, EFPPolar,SAPTPolar),
    gen_relative_mad_msd_std_block(SMB_ps, EFPPolar,SAPTPolar)
  )
  rownames(table) <- c("M", "S", "SM","B_ct", "B", "SM_ps", "SMB_ct", "SMB", "SMB_ps_ct", "SMB_ps")
  colnames(table) <- c("MAD Ionic", "MAD Polar", "MAD NP", "MAD Total", "MSDIonic", "MSDPol", "MSDNP", "MSDTotal", "STD")
  table 
}
# gen_rel_mad_msd_std_block_disp(mediumf,smallf,smf,bf)
gen_rel_mad_msd_std_block_disp<-function(M,S,SM,B){
  table<-rbind(
    gen_relative_mad_msd_std_block(M, EFPDisp, SAPTDisp), 
    gen_relative_mad_msd_std_block(S, EFPDisp, SAPTDisp), 
    gen_relative_mad_msd_std_block(SM, EFPDisp, SAPTDisp), 
    gen_relative_mad_msd_std_block(B, EFPDisp, SAPTDisp) 
  )
  rownames(table) <- c("M", "S", "SM","B")
  colnames(table) <- c("MAD Ionic", "MAD Polar", "MAD NP", "MAD Total", "MSDIonic", "MSDPol", "MSDNP", "MSDTotal", "STD")
  table 
}
#gen_rel_mad_msd_std_block_total(mediumf,smallf,smf, sm_esf, b_ctf, bf, sm_psf, smb_ctf, smbf, smb_ct_psf, smb_psf)
#
gen_rel_mad_msd_std_block_total<-function(M,S,SM,SM_es, B,B_ct, SM_ps, SMB_ct, SMB, SMB_ps_ct, SMB_ps){
  table<-rbind(
    gen_relative_mad_msd_std_block(M, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(S, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(SM, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(SM_es, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(B, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(B_ct, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(SM_ps, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(SMB_ct, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(SMB, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(SMB_ps_ct, EFPTotal,SAPTTotal),
    gen_relative_mad_msd_std_block(SMB_ps, EFPTotal,SAPTTotal)
  )
  rownames(table) <- c("M", "S", "SM","SM_es", "B_ct", "B", "SM_ps", "SMB_ct", "SMB", "SMB_ps_ct", "SMB_ps")
  colnames(table) <- c("MAD Ionic", "MAD Polar", "MAD NP", "MAD Total", "MSDIonic", "MSDPol", "MSDNP", "MSDTotal", "STD")
  table 
}



#Generate Exchange
rbind(
  gen_mad_msd_std_block(L, EFPExch,SAPTExch),
  gen_mad_msd_std_block(S, EFPExch,SAPTExch),
  gen_mad_msd_std_block(M, EFPExch,SAPTExch),
  gen_mad_msd_std_block(M2, EFPExch,SAPTExch),
  gen_mad_msd_std_block(Mg, EFPExch,SAPTExch),
  gen_mad_msd_std_block(MgN, EFPExch,SAPTExch),
  gen_mad_msd_std_block(M0.3, EFPExch,SAPTExch),
  gen_mad_msd_std_block(B, EFPExch,SAPTExch),
  gen_mad_msd_std_block(BN, EFPExch,SAPTExch),
  gen_mad_msd_std_block(B_0.3_CT, EFPExch,SAPTExch),
  gen_mad_msd_std_block(B_0.3_noCT, EFPExch,SAPTExch)
)

#Generate Polar
rbind(
  gen_mad_msd_std_block(L, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(S, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(M, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(M2, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(Mg, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(MgN, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(M0.3, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(B, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(BN, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(B_0.3_CT, EFPPolar,SAPTPolar),
  gen_mad_msd_std_block(B_0.3_noCT, EFPPolar,SAPTPolar)
)

#Generate Dispersion
rbind(
  gen_mad_msd_std_block(L, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(S, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(M, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(M2, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(Mg, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(MgN, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(M0.3, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(B, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(BN, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(B_0.3_CT, EFPDisp,SAPTDisp),
  gen_mad_msd_std_block(B_0.3_noCT, EFPDisp,SAPTDisp)
)

#Generate Total
rbind(
  gen_mad_msd_std_block(L, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(S, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(M, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(M2, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(Mg, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(MgN, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(M0.3, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(B, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(BN, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(B_0.3_CT, EFPTotal,SAPTTotal),
  gen_mad_msd_std_block(B_0.3_noCT, EFPTotal,SAPTTotal)
)