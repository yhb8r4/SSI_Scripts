#you need to change this the pathway to the csv files: 
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

gen_msd_types<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean(file[,"EFPTotal"][cationic_cationic]-file[,"SAPTTotal"][cationic_cationic]),
    mean(file[,"EFPTotal"][cationic_anionic]-file[,"SAPTTotal"][cationic_anionic]),
    mean(file[,"EFPTotal"][cationic_polar]-file[,"SAPTTotal"][cationic_polar]),
    mean(file[,"EFPTotal"][cationic_thiol]-file[,"SAPTTotal"][cationic_thiol]),
    mean(file[,"EFPTotal"][cationic_aliphatic]-file[,"SAPTTotal"][cationic_aliphatic]),
    mean(file[,"EFPTotal"][cationic_aryl]-file[,"SAPTTotal"][cationic_aryl]),
    mean(file[,"EFPTotal"][anionic_anionic]-file[,"SAPTTotal"][anionic_anionic]),
    mean(file[,"EFPTotal"][anionic_polar]-file[,"SAPTTotal"][anionic_polar]),
    mean(file[,"EFPTotal"][anionic_thiol]-file[,"SAPTTotal"][anionic_thiol]),
    mean(file[,"EFPTotal"][anionic_aliphatic]-file[,"SAPTTotal"][anionic_aliphatic]),
    mean(file[,"EFPTotal"][anionic_aryl]-file[,"SAPTTotal"][anionic_aryl]),
    mean(file[,"EFPTotal"][polar_polar]-file[,"SAPTTotal"][polar_polar]),
    mean(file[,"EFPTotal"][polar_thiol]-file[,"SAPTTotal"][polar_thiol]),
    mean(file[,"EFPTotal"][polar_aliphatic]-file[,"SAPTTotal"][polar_aliphatic]),
    mean(file[,"EFPTotal"][polar_aryl]-file[,"SAPTTotal"][polar_aryl]),
    mean(file[,"EFPTotal"][thiol_thiol]-file[,"SAPTTotal"][thiol_thiol]),
    mean(file[,"EFPTotal"][thiol_aliphatic]-file[,"SAPTTotal"][thiol_aliphatic]),
    mean(file[,"EFPTotal"][thiol_aryl]-file[,"SAPTTotal"][thiol_aryl]),
    mean(file[,"EFPTotal"][aliphatic_aliphatic]-file[,"SAPTTotal"][aliphatic_aliphatic]),
    mean(file[,"EFPTotal"][aliphatic_aryl]-file[,"SAPTTotal"][aliphatic_aryl]),
    mean(file[,"EFPTotal"][aryl_aryl]-file[,"SAPTTotal"][aryl_aryl]),
    mean(file[,"EFPTotal"]-file[,"SAPTTotal"])
  ))
  
  df
  
}

gen_mad_types<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs(file[,"EFPTotal"][cationic_cationic]-file[,"SAPTTotal"][cationic_cationic])),
    mean(abs(file[,"EFPTotal"][cationic_anionic]-file[,"SAPTTotal"][cationic_anionic])),
    mean(abs(file[,"EFPTotal"][cationic_polar]-file[,"SAPTTotal"][cationic_polar])),
    mean(abs(file[,"EFPTotal"][cationic_thiol]-file[,"SAPTTotal"][cationic_thiol])),
    mean(abs(file[,"EFPTotal"][cationic_aliphatic]-file[,"SAPTTotal"][cationic_aliphatic])),
    mean(abs(file[,"EFPTotal"][cationic_aryl]-file[,"SAPTTotal"][cationic_aryl])),
    mean(abs(file[,"EFPTotal"][anionic_anionic]-file[,"SAPTTotal"][anionic_anionic])),
    mean(abs(file[,"EFPTotal"][anionic_polar]-file[,"SAPTTotal"][anionic_polar])),
    mean(abs(file[,"EFPTotal"][anionic_thiol]-file[,"SAPTTotal"][anionic_thiol])),
    mean(abs(file[,"EFPTotal"][anionic_aliphatic]-file[,"SAPTTotal"][anionic_aliphatic])),
    mean(abs(file[,"EFPTotal"][anionic_aryl]-file[,"SAPTTotal"][anionic_aryl])),
    mean(abs(file[,"EFPTotal"][polar_polar]-file[,"SAPTTotal"][polar_polar])),
    mean(abs(file[,"EFPTotal"][polar_thiol]-file[,"SAPTTotal"][polar_thiol])),
    mean(abs(file[,"EFPTotal"][polar_aliphatic]-file[,"SAPTTotal"][polar_aliphatic])),
    mean(abs(file[,"EFPTotal"][polar_aryl]-file[,"SAPTTotal"][polar_aryl])),
    mean(abs(file[,"EFPTotal"][thiol_thiol]-file[,"SAPTTotal"][thiol_thiol])),
    mean(abs(file[,"EFPTotal"][thiol_aliphatic]-file[,"SAPTTotal"][thiol_aliphatic])),
    mean(abs(file[,"EFPTotal"][thiol_aryl]-file[,"SAPTTotal"][thiol_aryl])),
    mean(abs(file[,"EFPTotal"][aliphatic_aliphatic]-file[,"SAPTTotal"][aliphatic_aliphatic])),
    mean(abs(file[,"EFPTotal"][aliphatic_aryl]-file[,"SAPTTotal"][aliphatic_aryl])),
    mean(abs(file[,"EFPTotal"][aryl_aryl]-file[,"SAPTTotal"][aryl_aryl])),
    mean(abs(file[,"EFPTotal"]-file[,"SAPTTotal"]))
  ))
  
  df
  
}
gen_relative_mad_types<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs((file[,"EFPTotal"][cationic_cationic]-file[,"SAPTTotal"][cationic_cationic])/file[,"SAPTTotal"][cationic_cationic])),
    mean(abs((file[,"EFPTotal"][cationic_anionic]-file[,"SAPTTotal"][cationic_anionic])/file[,"SAPTTotal"][cationic_anionic])),
    mean(abs((file[,"EFPTotal"][cationic_polar]-file[,"SAPTTotal"][cationic_polar])),
    mean(abs((file[,"EFPTotal"][cationic_thiol]-file[,"SAPTTotal"][cationic_thiol]))/file[,"SAPTTotal"][cationic_thiol])),
    mean(abs((file[,"EFPTotal"][cationic_aliphatic]-file[,"SAPTTotal"][cationic_aliphatic])/file[,"SAPTTotal"][cationic_aliphatic])),
    mean(abs((file[,"EFPTotal"][cationic_aryl]-file[,"SAPTTotal"][cationic_aryl])/file[,"SAPTTotal"][cationic_aryl])),
    mean(abs((file[,"EFPTotal"][anionic_anionic]-file[,"SAPTTotal"][anionic_anionic])/file[,"SAPTTotal"][anionic_anionic])),
    mean(abs((file[,"EFPTotal"][anionic_polar]-file[,"SAPTTotal"][anionic_polar])/file[,"SAPTTotal"][anionic_polar])),
    mean(abs((file[,"EFPTotal"][anionic_thiol]-file[,"SAPTTotal"][anionic_thiol])/file[,"SAPTTotal"][anionic_thiol])),
    mean(abs((file[,"EFPTotal"][anionic_aliphatic]-file[,"SAPTTotal"][anionic_aliphatic])/file[,"SAPTTotal"][anionic_aliphatic])),
    mean(abs((file[,"EFPTotal"][anionic_aryl]-file[,"SAPTTotal"][anionic_aryl])/file[,"SAPTTotal"][anionic_aryl])),
    mean(abs((file[,"EFPTotal"][polar_polar]-file[,"SAPTTotal"][polar_polar])/file[,"SAPTTotal"][polar_polar])),
    mean(abs((file[,"EFPTotal"][polar_thiol]-file[,"SAPTTotal"][polar_thiol])/file[,"SAPTTotal"][polar_thiol])),
    mean(abs((file[,"EFPTotal"][polar_aliphatic]-file[,"SAPTTotal"][polar_aliphatic])/file[,"SAPTTotal"][polar_aliphatic])),
    mean(abs((file[,"EFPTotal"][polar_aryl]-file[,"SAPTTotal"][polar_aryl])/file[,"SAPTTotal"][polar_aryl])),
    mean(abs((file[,"EFPTotal"][thiol_thiol]-file[,"SAPTTotal"][thiol_thiol])/file[,"SAPTTotal"][thiol_thiol])),
    mean(abs((file[,"EFPTotal"][thiol_aliphatic]-file[,"SAPTTotal"][thiol_aliphatic])/file[,"SAPTTotal"][thiol_aliphatic])),
    mean(abs((file[,"EFPTotal"][thiol_aryl]-file[,"SAPTTotal"][thiol_aryl])/file[,"SAPTTotal"][thiol_aryl])),
    mean(abs((file[,"EFPTotal"][aliphatic_aliphatic]-file[,"SAPTTotal"][aliphatic_aliphatic])/file[,"SAPTTotal"][aliphatic_aliphatic])),
    mean(abs((file[,"EFPTotal"][aliphatic_aryl]-file[,"SAPTTotal"][aliphatic_aryl])/file[,"SAPTTotal"][aliphatic_aryl])),
    mean(abs((file[,"EFPTotal"][aryl_aryl]-file[,"SAPTTotal"][aryl_aryl])/file[,"SAPTTotal"][aryl_aryl])),
    mean(abs((file[,"EFPTotal"]-file[,"SAPTTotal"])/file[,"SAPTTotal"]))
         ))
    
    df
    
}

gen_relative_mad_types_sapt<-function(basis){
  cationic_cationic <- c(which(paste(basis$T1,basis$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(basis$T1,basis$T2)=="cationic anionic"),   which(paste(basis$T1,basis$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(basis$T1,basis$T2)=="cationic polar"),     which(paste(basis$T1,basis$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(basis$T1,basis$T2)=="cationic thiol"),     which(paste(basis$T1,basis$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(basis$T1,basis$T2)=="cationic aliphatic"), which(paste(basis$T1,basis$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(basis$T1,basis$T2)=="cationic aryl"),      which(paste(basis$T1,basis$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(basis$T1,basis$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(basis$T1,basis$T2)=="anionic polar"),      which(paste(basis$T1,basis$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(basis$T1,basis$T2)=="anionic thiol"),      which(paste(basis$T1,basis$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(basis$T1,basis$T2)=="anionic aliphatic"),  which(paste(basis$T1,basis$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(basis$T1,basis$T2)=="anionic aryl"),       which(paste(basis$T1,basis$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(basis$T1,basis$T2)=="polar polar"))
  polar_thiol <- c(which(paste(basis$T1,basis$T2)=="polar thiol"),        which(paste(basis$T1,basis$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(basis$T1,basis$T2)=="polar aliphatic"),    which(paste(basis$T1,basis$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(basis$T1,basis$T2)=="polar aryl"),         which(paste(basis$T1,basis$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(basis$T1,basis$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(basis$T1,basis$T2)=="thiol aliphatic"),    which(paste(basis$T1,basis$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(basis$T1,basis$T2)=="thiol aryl"),         which(paste(basis$T1,basis$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(basis$T1,basis$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(basis$T1,basis$T2)=="aliphatic aryl"),     which(paste(basis$T1,basis$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(basis$T1,basis$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs((basis[,"SAPTTotal"][cationic_cationic]-basis[,"CCSDTotal"][cationic_cationic])/basis[,"CCSDTotal"][cationic_cationic])),
    mean(abs((basis[,"SAPTTotal"][cationic_anionic]-basis[,"CCSDTotal"][cationic_anionic])/basis[,"CCSDTotal"][cationic_anionic])),
    mean(abs((basis[,"SAPTTotal"][cationic_polar]-basis[,"CCSDTotal"][cationic_polar]))),
    mean(abs((basis[,"SAPTTotal"][cationic_thiol]-basis[,"CCSDTotal"][cationic_thiol])/basis[,"CCSDTotal"][cationic_thiol])),
    mean(abs((basis[,"SAPTTotal"][cationic_aliphatic]-basis[,"CCSDTotal"][cationic_aliphatic])/basis[,"CCSDTotal"][cationic_aliphatic])),
    mean(abs((basis[,"SAPTTotal"][cationic_aryl]-basis[,"CCSDTotal"][cationic_aryl])/basis[,"CCSDTotal"][cationic_aryl])),
    mean(abs((basis[,"SAPTTotal"][anionic_anionic]-basis[,"CCSDTotal"][anionic_anionic])/basis[,"CCSDTotal"][anionic_anionic])),
    mean(abs((basis[,"SAPTTotal"][anionic_polar]-basis[,"CCSDTotal"][anionic_polar])/basis[,"CCSDTotal"][anionic_polar])),
    mean(abs((basis[,"SAPTTotal"][anionic_thiol]-basis[,"CCSDTotal"][anionic_thiol])/basis[,"CCSDTotal"][anionic_thiol])),
    mean(abs((basis[,"SAPTTotal"][anionic_aliphatic]-basis[,"CCSDTotal"][anionic_aliphatic])/basis[,"CCSDTotal"][anionic_aliphatic])),
    mean(abs((basis[,"SAPTTotal"][anionic_aryl]-basis[,"CCSDTotal"][anionic_aryl])/basis[,"CCSDTotal"][anionic_aryl])),
    mean(abs((basis[,"SAPTTotal"][polar_polar]-basis[,"CCSDTotal"][polar_polar])/basis[,"CCSDTotal"][polar_polar])),
    mean(abs((basis[,"SAPTTotal"][polar_thiol]-basis[,"CCSDTotal"][polar_thiol])/basis[,"CCSDTotal"][polar_thiol])),
    mean(abs((basis[,"SAPTTotal"][polar_aliphatic]-basis[,"CCSDTotal"][polar_aliphatic])/basis[,"CCSDTotal"][polar_aliphatic])),
    mean(abs((basis[,"SAPTTotal"][polar_aryl]-basis[,"CCSDTotal"][polar_aryl])/basis[,"CCSDTotal"][polar_aryl])),
    mean(abs((basis[,"SAPTTotal"][thiol_thiol]-basis[,"CCSDTotal"][thiol_thiol])/basis[,"CCSDTotal"][thiol_thiol])),
    mean(abs((basis[,"SAPTTotal"][thiol_aliphatic]-basis[,"CCSDTotal"][thiol_aliphatic])/basis[,"CCSDTotal"][thiol_aliphatic])),
    mean(abs((basis[,"SAPTTotal"][thiol_aryl]-basis[,"CCSDTotal"][thiol_aryl])/basis[,"CCSDTotal"][thiol_aryl])),
    mean(abs((basis[,"SAPTTotal"][aliphatic_aliphatic]-basis[,"CCSDTotal"][aliphatic_aliphatic])/basis[,"CCSDTotal"][aliphatic_aliphatic])),
    mean(abs((basis[,"SAPTTotal"][aliphatic_aryl]-basis[,"CCSDTotal"][aliphatic_aryl])/basis[,"CCSDTotal"][aliphatic_aryl])),
    mean(abs((basis[,"SAPTTotal"][aryl_aryl]-basis[,"CCSDTotal"][aryl_aryl])/basis[,"CCSDTotal"][aryl_aryl])),
    mean(abs((basis[,"SAPTTotal"]-basis[,"CCSDTotal"])/basis[,"CCSDTotal"]))
  ))
  
  df
}
gen_relative_msd_types<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean((file[,"EFPTotal"][cationic_cationic]-file[,"SAPTTotal"][cationic_cationic])/file[,"SAPTTotal"][cationic_cationic]),
    mean((file[,"EFPTotal"][cationic_anionic]-file[,"SAPTTotal"][cationic_anionic])/file[,"SAPTTotal"][cationic_anionic]),
    mean((file[,"EFPTotal"][cationic_polar]-file[,"SAPTTotal"][cationic_polar])/file[,"SAPTTotal"][cationic_polar]),
    mean((file[,"EFPTotal"][cationic_thiol]-file[,"SAPTTotal"][cationic_thiol])/file[,"SAPTTotal"][cationic_thiol]),
    mean((file[,"EFPTotal"][cationic_aliphatic]-file[,"SAPTTotal"][cationic_aliphatic])/file[,"SAPTTotal"][cationic_aliphatic]),
    mean((file[,"EFPTotal"][cationic_aryl]-file[,"SAPTTotal"][cationic_aryl])/file[,"SAPTTotal"][cationic_aryl]),
    mean((file[,"EFPTotal"][anionic_anionic]-file[,"SAPTTotal"][anionic_anionic])/file[,"SAPTTotal"][anionic_anionic]),
    mean((file[,"EFPTotal"][anionic_polar]-file[,"SAPTTotal"][anionic_polar])/file[,"SAPTTotal"][anionic_polar]),
    mean((file[,"EFPTotal"][anionic_thiol]-file[,"SAPTTotal"][anionic_thiol])/file[,"SAPTTotal"][anionic_thiol]),
    mean((file[,"EFPTotal"][anionic_aliphatic]-file[,"SAPTTotal"][anionic_aliphatic])/file[,"SAPTTotal"][anionic_aliphatic]),
    mean((file[,"EFPTotal"][anionic_aryl]-file[,"SAPTTotal"][anionic_aryl])/file[,"SAPTTotal"][anionic_aryl]),
    mean((file[,"EFPTotal"][polar_polar]-file[,"SAPTTotal"][polar_polar])/file[,"SAPTTotal"][polar_polar]),
    mean((file[,"EFPTotal"][polar_thiol]-file[,"SAPTTotal"][polar_thiol])/file[,"SAPTTotal"][polar_thiol]),
    mean((file[,"EFPTotal"][polar_aliphatic]-file[,"SAPTTotal"][polar_aliphatic])/file[,"SAPTTotal"][polar_aliphatic]),
    mean((file[,"EFPTotal"][polar_aryl]-file[,"SAPTTotal"][polar_aryl])/file[,"SAPTTotal"][polar_aryl]),
    mean((file[,"EFPTotal"][thiol_thiol]-file[,"SAPTTotal"][thiol_thiol])/file[,"SAPTTotal"][thiol_thiol]),
    mean((file[,"EFPTotal"][thiol_aliphatic]-file[,"SAPTTotal"][thiol_aliphatic])/file[,"SAPTTotal"][thiol_aliphatic]),
    mean((file[,"EFPTotal"][thiol_aryl]-file[,"SAPTTotal"][thiol_aryl])/file[,"SAPTTotal"][thiol_aryl]),
    mean((file[,"EFPTotal"][aliphatic_aliphatic]-file[,"SAPTTotal"][aliphatic_aliphatic])/file[,"SAPTTotal"][aliphatic_aliphatic]),
    mean((file[,"EFPTotal"][aliphatic_aryl]-file[,"SAPTTotal"][aliphatic_aryl])/file[,"SAPTTotal"][aliphatic_aryl]),
    mean((file[,"EFPTotal"][aryl_aryl]-file[,"SAPTTotal"][aryl_aryl])/file[,"SAPTTotal"][aryl_aryl]),
    mean((file[,"EFPTotal"]-file[,"SAPTTotal"])/file[,"SAPTTotal"])
  ))
  df
}

gen_types_block_msd<-function(a,b,c,d,e,f,g,h,i,j,k){
  msd<-round(cbind(
    gen_msd_types(a),
    gen_msd_types(b),
    gen_msd_types(c),
    gen_msd_types(d),
    gen_msd_types(e),
    gen_msd_types(f),
    gen_msd_types(g),
    gen_msd_types(h),
    gen_msd_types(i),
    gen_msd_types(j),
    gen_msd_types(k)
  ),2)
  msd
  
}
gen_types_block_mad<-function(a,b,c,d,e,f,g,h,i,j,k){
  mad<-round(cbind(
    gen_mad_types(a),
    gen_mad_types(b),
    gen_mad_types(c),
    gen_mad_types(d),
    gen_mad_types(e),
    gen_mad_types(f),
    gen_mad_types(g),
    gen_mad_types(h),
    gen_mad_types(i),
    gen_mad_types(j),
    gen_mad_types(k)
  ),2)
  mad
  
}]

gen_types_block_relative_mad<-function(a,b,c,d,e,f,g,h,i,j,k){
  mad<-round(cbind(
    gen_relative_mad_types(a),
    gen_relative_mad_types(b),
    gen_relative_mad_types(c),
    gen_relative_mad_types(d),
    gen_relative_mad_types(e),
    gen_relative_mad_types(f),
    gen_relative_mad_types(g),
    gen_relative_mad_types(h),
    gen_relative_mad_types(i),
    gen_relative_mad_types(j),
    gen_relative_mad_types(k)
  ),2)
  mad
}
gen_types_block_relative_mad<-function(a,b,c,d,e,f,g,h,i,j,k){
  mad<-round(cbind(
    gen_relative_mad_types(a),
    gen_relative_mad_types(b),
    gen_relative_mad_types(c),
    gen_relative_mad_types(d),
    gen_relative_mad_types(e),
    gen_relative_mad_types(f),
    gen_relative_mad_types(g),
    gen_relative_mad_types(h),
    gen_relative_mad_types(i),
    gen_relative_mad_types(j),
    gen_relative_mad_types(k)
  ),2)
  mad
}

gen_freq_types<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    length(cationic_cationic),
    length(cationic_anionic),
    length(cationic_polar),
    length(cationic_thiol),
    length(cationic_aliphatic),
    length(cationic_aryl),
    length(anionic_anionic),
    length(anionic_polar),
    length(anionic_thiol),
    length(anionic_aliphatic),
    length(anionic_aryl),
    length(polar_polar),
    length(polar_thiol),
    length(polar_aliphatic),
    length(polar_aryl),
    length(thiol_thiol),
    length(thiol_aliphatic),
    length(thiol_aryl),
    length(aliphatic_aliphatic),
    length(aliphatic_aryl),
    length(aryl_aryl),
    nrow(file)
  ))
  
  df
}
gen_freq_block<-function(a,b,c,d,e,f,g,h,i,j,k){
  table<-cbind(gen_freq_types(a),
               gen_freq_types(b),
               gen_freq_types(c),
               gen_freq_types(d),
               gen_freq_types(e),
               gen_freq_types(f),
               gen_freq_types(g),
               gen_freq_types(h),
               gen_freq_types(i),
               gen_freq_types(j),
               gen_freq_types(k)
  )
  average_freq<-matrix(rowMeans(table))
  average_freq/3362
  #cbind(table,(average_freq/3362)*100)
}

gen_range_of_ccsd_type<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  higher_range<-matrix(cbind(
    max(file[,"CCSDTotal"][cationic_cationic]),
    max(file[,"CCSDTotal"][cationic_anionic]),
    max(file[,"CCSDTotal"][cationic_polar]),
    max(file[,"CCSDTotal"][cationic_thiol]),
    max(file[,"CCSDTotal"][cationic_aliphatic]),
    max(file[,"CCSDTotal"][cationic_aryl]),
    max(file[,"CCSDTotal"][anionic_anionic]),
    max(file[,"CCSDTotal"][anionic_polar]),
    max(file[,"CCSDTotal"][anionic_thiol]),
    max(file[,"CCSDTotal"][anionic_aliphatic]),
    max(file[,"CCSDTotal"][anionic_aryl]),
    max(file[,"CCSDTotal"][polar_polar]),
    max(file[,"CCSDTotal"][polar_thiol]),
    max(file[,"CCSDTotal"][polar_aliphatic]),
    max(file[,"CCSDTotal"][polar_aryl]),
    max(file[,"CCSDTotal"][thiol_thiol]),
    max(file[,"CCSDTotal"][thiol_aliphatic]),
    max(file[,"CCSDTotal"][thiol_aryl]),
    max(file[,"CCSDTotal"][aliphatic_aliphatic]),
    max(file[,"CCSDTotal"][aliphatic_aryl]),
    max(file[,"CCSDTotal"][aryl_aryl]),
    max(file[,"CCSDTotal"]))
  )
  
  lower_range<-matrix(cbind(
    min(file[,"CCSDTotal"][cationic_cationic]),
    min(file[,"CCSDTotal"][cationic_anionic]),
    min(file[,"CCSDTotal"][cationic_polar]),
    min(file[,"CCSDTotal"][cationic_thiol]),
    min(file[,"CCSDTotal"][cationic_aliphatic]),
    min(file[,"CCSDTotal"][cationic_aryl]),
    min(file[,"CCSDTotal"][anionic_anionic]),
    min(file[,"CCSDTotal"][anionic_polar]),
    min(file[,"CCSDTotal"][anionic_thiol]),
    min(file[,"CCSDTotal"][anionic_aliphatic]),
    min(file[,"CCSDTotal"][anionic_aryl]),
    min(file[,"CCSDTotal"][polar_polar]),
    min(file[,"CCSDTotal"][polar_thiol]),
    min(file[,"CCSDTotal"][polar_aliphatic]),
    min(file[,"CCSDTotal"][polar_aryl]),
    min(file[,"CCSDTotal"][thiol_thiol]),
    min(file[,"CCSDTotal"][thiol_aliphatic]),
    min(file[,"CCSDTotal"][thiol_aryl]),
    min(file[,"CCSDTotal"][aliphatic_aliphatic]),
    min(file[,"CCSDTotal"][aliphatic_aryl]),
    min(file[,"CCSDTotal"][aryl_aryl]),
    min(file[,"CCSDTotal"]))
  )
  matrix(higher_range-lower_range)
}
gen_average_of_ccsd_type<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  matrix(cbind(
    mean(file[,"CCSDTotal"][cationic_cationic]),
    mean(file[,"CCSDTotal"][cationic_anionic]),
    mean(file[,"CCSDTotal"][cationic_polar]),
    mean(file[,"CCSDTotal"][cationic_thiol]),
    mean(file[,"CCSDTotal"][cationic_aliphatic]),
    mean(file[,"CCSDTotal"][cationic_aryl]),
    mean(file[,"CCSDTotal"][anionic_anionic]),
    mean(file[,"CCSDTotal"][anionic_polar]),
    mean(file[,"CCSDTotal"][anionic_thiol]),
    mean(file[,"CCSDTotal"][anionic_aliphatic]),
    mean(file[,"CCSDTotal"][anionic_aryl]),
    mean(file[,"CCSDTotal"][polar_polar]),
    mean(file[,"CCSDTotal"][polar_thiol]),
    mean(file[,"CCSDTotal"][polar_aliphatic]),
    mean(file[,"CCSDTotal"][polar_aryl]),
    mean(file[,"CCSDTotal"][thiol_thiol]),
    mean(file[,"CCSDTotal"][thiol_aliphatic]),
    mean(file[,"CCSDTotal"][thiol_aryl]),
    mean(file[,"CCSDTotal"][aliphatic_aliphatic]),
    mean(file[,"CCSDTotal"][aliphatic_aryl]),
    mean(file[,"CCSDTotal"][aryl_aryl]),
    mean(file[,"CCSDTotal"])
  ))
}
return_range<-function(file,index,min_ideal,max_ideal){
  values<-abs(file[,"V4"][index]-file[,"V5"][index]/file[,"V5"][index])
  range<-max(values)-min(values)
  
  scaled<-(min_ideal*(1 - (values-min(values))/range) + max_ideal*(values-min(values)/range))
  scaled
  #mean(scaled)
  #  scaled<-min_ideal*(1 - (values-min(values))/range) + max_ideal*((x-min(values))/range)
  #  average_scaled <- mean(scaled)
}

ccsd_error_plot <- function(x) {
  
  p<-ggplot(x)
  p<- p + geom_line(data=x, aes(x=abs(V14),y=abs(V14)))
  p<- p + geom_point(data=x, aes(x=abs(V14),y=abs(V4), color=c(abs(V6)/(abs(V6)+abs(V12)))))
  p<- p + geom_point(data=x, aes(x=abs(V14),y=abs(V5), color=c(abs(V7)/(abs(V7)+abs(V13)))), shape=5)+
    scale_colour_gradientn(colours=jet.colors(7))
  
  p<- p + xlab("CCSD Energy kcal/mol")
  p<- p + ylab("CCSD Energy kcal/mol")
  #p<- p + xlim(45, 120)
  #p<- p + ylim(45,120)
  print(p)
}
#index_type(file=basis set calculation, x=type1, y=type2)
index_type <- function(file,x,y){
  a<-paste(x,y)
  b<-paste(y,x)
  index<-c(which(paste(file$T1,file$T2)==a),which(paste(file$T1,file$T2)==b))
  table<-file[index,]
  table
}
ccsd_error_plot2 <- function(file,x,y) {
  library(grid)
  #library(tidyverse)
  temp<-index_type(file,x,y)
  
  p<-ggplot(temp)
  p<- p + theme(axis.text.x = element_text(size = 30))
  p<- p + theme(axis.text.y = element_text(size = 30))
  p<- p + theme(axis.title.x = element_text(size = 100, face="bold"))
  p<- p + theme(axis.title.y = element_text(size = 100, face="bold"))
  p<- p + theme_bw()
  p<- p + geom_line(data=temp, aes(x=CCSDTotal,y=CCSDTotal), show.legend=FALSE)
#  p<- p + geom_point(data=temp, aes(x=CCSDTotal,y=EFPTotal, color=c(abs(SAPTElec)/(abs(SAPTElec)+abs(SAPTDisp))), size=10),show.legend=FALSE)
#  p<- p + geom_point(data=temp, aes(x=CCSDTotal,y=SAPTTotal, color=c(abs(SAPTElec)/(abs(SAPTElec)+abs(SAPTDisp))), size=10), shape=5, show.legend=FALSE)
  p<- p +  scale_colour_gradientn(colours=jet.colors(7))
#  p<- p + xlab("Reference CCSD Energy (kcal/mol)")
#  p<- p + ylab("Method Energy (kcal/mol)")
#  p<- p + labs(color="Energy Component")
  
#  r <- paste("R=", corr_error_vs_ccsd(file, x, y))
#  grob <- grobTree(textGrob(r, x=0.1,  y=0.95, hjust=0,
#                           gp=gpar(col="red", fontsize=10, fontface="italic")))
#  p<- p + annotation_custom(grob)
  
  #p<- p + xlim(-5, 1)
  #p<- p + ylim(-8, 1)
  p
}

#I use this query in order to find which dimers fall in what range. 
#index_type(smallf,"aryl", "aryl")[which(index_type(smallf,"aryl", "aryl")["EFPTotal"] > 0 & index_type(smallf,"aryl", "aryl")["CCSDTotal"] > 5 ),]

corr_error_vs_ccsd <- function(data,t1,t2){
  table <- index_type(data,t1,t2 )
  
  CCSD <- table[,"CCSDTotal"]
  EFP <- table[,"EFPTotal"]
  
  cor(EFP, CCSD)
}
corr_error_vs_ccsd_sapt <- function(data,t1,t2){
  table <- index_type(data,t1,t2 )
  
  CCSD <- table[,"CCSDTotal"]
  SAPT <- table[,"SAPTTotal"]
  
  cor(SAPT, CCSD)
}

corr_error_vs_dom_force <- function(data,t1,t2){
  table <- index_type(data,t1,t2 )
  
  CCSD <- table[,"CCSDTotal"]
  EFP <- table[,"EFPTotal"]
  
  ELEC<-table[,"EFPElec"]
  DISP<-table[,"EFPDisp"]
  v8
  DOM <- ELEC/(DISP+ELEC)
  
  cor(CCSD-EFP, DOM)
}

gen_relative_mad_types_CCSD<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs((file[,"EFPTotal"][cationic_cationic]-file[,"SAPTTotal"][cationic_cationic])/file[,"SAPTTotal"][cationic_cationic])),
    mean(abs((file[,"EFPTotal"][cationic_anionic]-file[,"SAPTTotal"][cationic_anionic])/file[,"SAPTTotal"][cationic_anionic])),
    mean(abs((file[,"EFPTotal"][cationic_polar]-file[,"SAPTTotal"][cationic_polar])),
    mean(abs((file[,"EFPTotal"][cationic_thiol]-file[,"SAPTTotal"][cationic_thiol]))/file[,"SAPTTotal"][cationic_thiol])),
    mean(abs((file[,"EFPTotal"][cationic_aliphatic]-file[,"SAPTTotal"][cationic_aliphatic])/file[,"SAPTTotal"][cationic_aliphatic])),
    mean(abs((file[,"EFPTotal"][cationic_aryl]-file[,"SAPTTotal"][cationic_aryl])/file[,"SAPTTotal"][cationic_aryl])),
    mean(abs((file[,"EFPTotal"][anionic_anionic]-file[,"SAPTTotal"][anionic_anionic])/file[,"SAPTTotal"][anionic_anionic])),
    mean(abs((file[,"EFPTotal"][anionic_polar]-file[,"SAPTTotal"][anionic_polar])/file[,"SAPTTotal"][anionic_polar])),
    mean(abs((file[,"EFPTotal"][anionic_thiol]-file[,"SAPTTotal"][anionic_thiol])/file[,"SAPTTotal"][anionic_thiol])),
    mean(abs((file[,"EFPTotal"][anionic_aliphatic]-file[,"SAPTTotal"][anionic_aliphatic])/file[,"SAPTTotal"][anionic_aliphatic])),
    mean(abs((file[,"EFPTotal"][anionic_aryl]-file[,"SAPTTotal"][anionic_aryl])/file[,"SAPTTotal"][anionic_aryl])),
    mean(abs((file[,"EFPTotal"][polar_polar]-file[,"SAPTTotal"][polar_polar])/file[,"SAPTTotal"][polar_polar])),
    mean(abs((file[,"EFPTotal"][polar_thiol]-file[,"SAPTTotal"][polar_thiol])/file[,"SAPTTotal"][polar_thiol])),
    mean(abs((file[,"EFPTotal"][polar_aliphatic]-file[,"SAPTTotal"][polar_aliphatic])/file[,"SAPTTotal"][polar_aliphatic])),
    mean(abs((file[,"EFPTotal"][polar_aryl]-file[,"SAPTTotal"][polar_aryl])/file[,"SAPTTotal"][polar_aryl])),
    mean(abs((file[,"EFPTotal"][thiol_thiol]-file[,"SAPTTotal"][thiol_thiol])/file[,"SAPTTotal"][thiol_thiol])),
    mean(abs((file[,"EFPTotal"][thiol_aliphatic]-file[,"SAPTTotal"][thiol_aliphatic])/file[,"SAPTTotal"][thiol_aliphatic])),
    mean(abs((file[,"EFPTotal"][thiol_aryl]-file[,"SAPTTotal"][thiol_aryl])/file[,"SAPTTotal"][thiol_aryl])),
    mean(abs((file[,"EFPTotal"][aliphatic_aliphatic]-file[,"SAPTTotal"][aliphatic_aliphatic])/file[,"SAPTTotal"][aliphatic_aliphatic])),
    mean(abs((file[,"EFPTotal"][aliphatic_aryl]-file[,"SAPTTotal"][aliphatic_aryl])/file[,"SAPTTotal"][aliphatic_aryl])),
    mean(abs((file[,"EFPTotal"][aryl_aryl]-file[,"SAPTTotal"][aryl_aryl])/file[,"SAPTTotal"][aryl_aryl])),
    mean(abs((file[,"EFPTotal"]-file[,"SAPTTotal"])/file[,"SAPTTotal"]))
  ))
  
  df
  
}

gen_relative_mad_types_CCSD<-function(file){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs((file[,"EFPTotal"][cationic_cationic]-file[,"CCSDTotal"][cationic_cationic])/file[,"CCSDTotal"][cationic_cationic])),
    mean(abs((file[,"EFPTotal"][cationic_anionic]-file[,"CCSDTotal"][cationic_anionic])/file[,"CCSDTotal"][cationic_anionic])),
    mean(abs((file[,"EFPTotal"][cationic_polar]-file[,"CCSDTotal"][cationic_polar])),
         mean(abs((file[,"EFPTotal"][cationic_thiol]-file[,"CCSDTotal"][cationic_thiol]))/file[,"CCSDTotal"][cationic_thiol])),
    mean(abs((file[,"EFPTotal"][cationic_aliphatic]-file[,"CCSDTotal"][cationic_aliphatic])/file[,"CCSDTotal"][cationic_aliphatic])),
    mean(abs((file[,"EFPTotal"][cationic_aryl]-file[,"CCSDTotal"][cationic_aryl])/file[,"CCSDTotal"][cationic_aryl])),
    mean(abs((file[,"EFPTotal"][anionic_anionic]-file[,"CCSDTotal"][anionic_anionic])/file[,"CCSDTotal"][anionic_anionic])),
    mean(abs((file[,"EFPTotal"][anionic_polar]-file[,"CCSDTotal"][anionic_polar])/file[,"CCSDTotal"][anionic_polar])),
    mean(abs((file[,"EFPTotal"][anionic_thiol]-file[,"CCSDTotal"][anionic_thiol])/file[,"CCSDTotal"][anionic_thiol])),
    mean(abs((file[,"EFPTotal"][anionic_aliphatic]-file[,"CCSDTotal"][anionic_aliphatic])/file[,"CCSDTotal"][anionic_aliphatic])),
    mean(abs((file[,"EFPTotal"][anionic_aryl]-file[,"CCSDTotal"][anionic_aryl])/file[,"CCSDTotal"][anionic_aryl])),
    mean(abs((file[,"EFPTotal"][polar_polar]-file[,"CCSDTotal"][polar_polar])/file[,"CCSDTotal"][polar_polar])),
    mean(abs((file[,"EFPTotal"][polar_thiol]-file[,"CCSDTotal"][polar_thiol])/file[,"CCSDTotal"][polar_thiol])),
    mean(abs((file[,"EFPTotal"][polar_aliphatic]-file[,"CCSDTotal"][polar_aliphatic])/file[,"CCSDTotal"][polar_aliphatic])),
    mean(abs((file[,"EFPTotal"][polar_aryl]-file[,"CCSDTotal"][polar_aryl])/file[,"CCSDTotal"][polar_aryl])),
    mean(abs((file[,"EFPTotal"][thiol_thiol]-file[,"CCSDTotal"][thiol_thiol])/file[,"CCSDTotal"][thiol_thiol])),
    mean(abs((file[,"EFPTotal"][thiol_aliphatic]-file[,"CCSDTotal"][thiol_aliphatic])/file[,"CCSDTotal"][thiol_aliphatic])),
    mean(abs((file[,"EFPTotal"][thiol_aryl]-file[,"CCSDTotal"][thiol_aryl])/file[,"CCSDTotal"][thiol_aryl])),
    mean(abs((file[,"EFPTotal"][aliphatic_aliphatic]-file[,"CCSDTotal"][aliphatic_aliphatic])/file[,"CCSDTotal"][aliphatic_aliphatic])),
    mean(abs((file[,"EFPTotal"][aliphatic_aryl]-file[,"CCSDTotal"][aliphatic_aryl])/file[,"CCSDTotal"][aliphatic_aryl])),
    mean(abs((file[,"EFPTotal"][aryl_aryl]-file[,"CCSDTotal"][aryl_aryl])/file[,"CCSDTotal"][aryl_aryl])),
    mean(abs((file[,"EFPTotal"]-file[,"CCSDTotal"])/file[,"CCSDTotal"]))
  ))
  
  df*100

}

gen_corr_types<-function(basis){
  cationic_cationic <- c(which(paste(file$T1,file$T2)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$T1,file$T2)=="cationic anionic"),   which(paste(file$T1,file$T2)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$T1,file$T2)=="cationic polar"),     which(paste(file$T1,file$T2)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$T1,file$T2)=="cationic thiol"),     which(paste(file$T1,file$T2)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$T1,file$T2)=="cationic aliphatic"), which(paste(file$T1,file$T2)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$T1,file$T2)=="cationic aryl"),      which(paste(file$T1,file$T2)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$T1,file$T2)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$T1,file$T2)=="anionic polar"),      which(paste(file$T1,file$T2)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$T1,file$T2)=="anionic thiol"),      which(paste(file$T1,file$T2)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$T1,file$T2)=="anionic aliphatic"),  which(paste(file$T1,file$T2)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$T1,file$T2)=="anionic aryl"),       which(paste(file$T1,file$T2)=="aryl anionic"))
  polar_polar <- c(which(paste(file$T1,file$T2)=="polar polar"))
  polar_thiol <- c(which(paste(file$T1,file$T2)=="polar thiol"),        which(paste(file$T1,file$T2)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$T1,file$T2)=="polar aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$T1,file$T2)=="polar aryl"),         which(paste(file$T1,file$T2)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$T1,file$T2)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$T1,file$T2)=="thiol aliphatic"),    which(paste(file$T1,file$T2)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$T1,file$T2)=="thiol aryl"),         which(paste(file$T1,file$T2)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$T1,file$T2)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$T1,file$T2)=="aliphatic aryl"),     which(paste(file$T1,file$T2)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$T1,file$T2)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs((file[,"EFPTotal"][cationic_cationic]-file[,"CCSDTotal"][cationic_cationic])/file[,"CCSDTotal"][cationic_cationic])),
    mean(abs((file[,"EFPTotal"][cationic_anionic]-file[,"CCSDTotal"][cationic_anionic])/file[,"CCSDTotal"][cationic_anionic])),
    mean(abs((file[,"EFPTotal"][cationic_polar]-file[,"CCSDTotal"][cationic_polar])),
    mean(abs((file[,"EFPTotal"][cationic_thiol]-file[,"CCSDTotal"][cationic_thiol]))/file[,"CCSDTotal"][cationic_thiol])),
    mean(abs((file[,"EFPTotal"][cationic_aliphatic]-file[,"CCSDTotal"][cationic_aliphatic])/file[,"CCSDTotal"][cationic_aliphatic])),
    mean(abs((file[,"EFPTotal"][cationic_aryl]-file[,"CCSDTotal"][cationic_aryl])/file[,"CCSDTotal"][cationic_aryl])),
    mean(abs((file[,"EFPTotal"][anionic_anionic]-file[,"CCSDTotal"][anionic_anionic])/file[,"CCSDTotal"][anionic_anionic])),
    mean(abs((file[,"EFPTotal"][anionic_polar]-file[,"CCSDTotal"][anionic_polar])/file[,"CCSDTotal"][anionic_polar])),
    mean(abs((file[,"EFPTotal"][anionic_thiol]-file[,"CCSDTotal"][anionic_thiol])/file[,"CCSDTotal"][anionic_thiol])),
    mean(abs((file[,"EFPTotal"][anionic_aliphatic]-file[,"CCSDTotal"][anionic_aliphatic])/file[,"CCSDTotal"][anionic_aliphatic])),
    mean(abs((file[,"EFPTotal"][anionic_aryl]-file[,"CCSDTotal"][anionic_aryl])/file[,"CCSDTotal"][anionic_aryl])),
    mean(abs((file[,"EFPTotal"][polar_polar]-file[,"CCSDTotal"][polar_polar])/file[,"CCSDTotal"][polar_polar])),
    mean(abs((file[,"EFPTotal"][polar_thiol]-file[,"CCSDTotal"][polar_thiol])/file[,"CCSDTotal"][polar_thiol])),
    mean(abs((file[,"EFPTotal"][polar_aliphatic]-file[,"CCSDTotal"][polar_aliphatic])/file[,"CCSDTotal"][polar_aliphatic])),
    mean(abs((file[,"EFPTotal"][polar_aryl]-file[,"CCSDTotal"][polar_aryl])/file[,"CCSDTotal"][polar_aryl])),
    mean(abs((file[,"EFPTotal"][thiol_thiol]-file[,"CCSDTotal"][thiol_thiol])/file[,"CCSDTotal"][thiol_thiol])),
    mean(abs((file[,"EFPTotal"][thiol_aliphatic]-file[,"CCSDTotal"][thiol_aliphatic])/file[,"CCSDTotal"][thiol_aliphatic])),
    mean(abs((file[,"EFPTotal"][thiol_aryl]-file[,"CCSDTotal"][thiol_aryl])/file[,"CCSDTotal"][thiol_aryl])),
    mean(abs((file[,"EFPTotal"][aliphatic_aliphatic]-file[,"CCSDTotal"][aliphatic_aliphatic])/file[,"CCSDTotal"][aliphatic_aliphatic])),
    mean(abs((file[,"EFPTotal"][aliphatic_aryl]-file[,"CCSDTotal"][aliphatic_aryl])/file[,"CCSDTotal"][aliphatic_aryl])),
    mean(abs((file[,"EFPTotal"][aryl_aryl]-file[,"CCSDTotal"][aryl_aryl])/file[,"CCSDTotal"][aryl_aryl])),
    mean(abs((file[,"EFPTotal"]-file[,"CCSDTotal"])/file[,"CCSDTotal"]))
  ))
  
  df*100
}

save_plots<-function(file,x,y){
  setwd('/Users/yhb8r4/Documents/ssi_types_figures/')
  
  title<-paste("efp",x,y,sep='_')
  title <- paste(title,".png",sep='')
  
  png(filename=title,
      units = 'in',
      width=35,
      height=10,
      res = 300,
      pointsize=30)
  
  efp_sapt_type_plot(file,x,y)
  dev.off()
}

efp_type_plot <- function(file,x,y){
  temp<-index_type(file,x,y)
  
  p<-ggplot(temp)
  p<- p + geom_line(data=temp, aes(x=CCSDTotal,y=CCSDTotal), show.legend=FALSE)
  p<- p + geom_point(data=temp, aes(x=CCSDTotal,y=EFPTotal, color=c(abs(SAPTElec)/(abs(SAPTElec)+abs(SAPTDisp))), size=10),show.legend=FALSE)
  p<- p +  scale_colour_gradientn(colours=jet.colors(7))
  p<- p + theme_bw()
  p<- p + xlab("Reference Total Energy (kcal/mol)")
  p<- p + ylab("Total Energy (kcal/mol)")
  p<- p + labs(color="Energy Component")
  p<- p + theme(axis.text.x = element_text(size = 15))
  p<- p + theme(axis.text.y = element_text(size = 15))
  p<- p + theme(axis.title.x = element_text(size = 30 ))
  p<- p + theme(axis.title.y = element_text(size = 30 ))
  
  r <- paste("R=", round(corr_error_vs_ccsd(file, x, y),4))
    grob <- grobTree(textGrob(r, x=0.2,  y=0.85, hjust=0,
                             gp=gpar(col="red", fontsize=15, fontface="italic")))
  efp_total <- grobTree(textGrob("EFP", x=0.1,  y=0.90, hjust=0,
                                 gp=gpar(col="red", fontsize=40, fontface="italic")))
  efp_msd_graphic <- grotTree(textGrob(efp_))
  p<- p + annotation_custom(grob)
  p<- p + annotation_custom(efp_total)
  print(p)
}

efp_sapt_type_plot <- function(file,x,y){
  temp<-index_type(file,x,y)
  y_max_val <- max(max(temp$EFPTotal),max(temp$SAPTTotal))
  x_min_val <- min(min(temp$EFPTotal),min(temp$SAPTTotal))
  
  mad_efp <- paste("MAE=",round(mean(abs(temp$EFPTotal-temp$CCSDTotal)),2))
  mad_sapt <- paste("MAE=",round(mean(abs(temp$SAPTTotal-temp$CCSDTotal)),2))
  
  msd_efp <- paste("MSE=",round(mean(temp$EFPTotal-temp$CCSDTotal),2))
  msd_sapt <- paste("MSE=",round(mean(temp$SAPTTotal-temp$CCSDTotal),2))
  
  rel_error_efp <- paste("Rel % Error=",round(mean(abs(temp$EFPTotal-temp$CCSDTotal)/abs(temp$CCSDTotal))*100,2))
  rel_error_sapt <- paste("Rel % Error=", round(mean(abs(temp$SAPTTotal-temp$CCSDTotal)/abs(temp$CCSDTotal))*100,2))
  
  rel_error_efp
  
  p<-ggplot(temp)
  p<- p + geom_line(data=temp, aes(x=CCSDTotal,y=CCSDTotal), show.legend=FALSE)
  p<- p + geom_point(data=temp, aes(x=CCSDTotal,y=EFPTotal, color=c(abs(SAPTElec)/(abs(SAPTElec)+abs(SAPTDisp))), size=10),show.legend=FALSE)
  p<- p +  scale_colour_gradientn(colours=jet.colors(7))
  p<- p + theme_bw()
  p<- p + xlab("Reference Total Energy (kcal/mol)")
  p<- p + ylab("Total Energy (kcal/mol)")
  p<- p + labs(color="Energy Component")
  p<- p + ylim(x_min_val, y_max_val)
  p<- p + theme(axis.text.x = element_text(size = 45))
  p<- p + theme(axis.text.y = element_text(size = 45))
 # p<- p + theme(axis.title.x = element_text(size = 30 ))
  p<- p + theme(axis.title.x = element_text(size=53))
  p<- p + theme(axis.title.y = element_text(size = 53 ))
  
  r <- paste("R=", round(corr_error_vs_ccsd(file, x, y),4))
  grob <- grobTree(textGrob(r, x=0.1,  y=0.50, hjust=0,
                            gp=gpar(col="grey", fontsize=40, fontface="bold")))
  efp_total <- grobTree(textGrob("EFP", x=0.05,  y=0.90, hjust=0,
                                 gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  efp_mad <- grobTree(textGrob(mad_efp, x=0.1,  y=0.80, hjust=0,
                                 gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  efp_msd <- grobTree(textGrob(msd_efp, x=0.1,  y=0.70, hjust=0,
                               gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  efp_rel_error <- grobTree(textGrob(rel_error_efp, x=0.1,  y=0.60, hjust=0,
                               gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  p<- p + annotation_custom(grob)
  p<- p + annotation_custom(efp_total)
  p<- p + annotation_custom(efp_mad)
  p<- p + annotation_custom(efp_msd)
  p<- p + annotation_custom(efp_rel_error)
  
  q<-ggplot(temp)
  q<- q + geom_line(data=temp, aes(x=CCSDTotal,y=CCSDTotal), show.legend=FALSE)
  q<- q + geom_point(data=temp, aes(x=CCSDTotal,y=SAPTTotal, color=c(abs(SAPTElec)/(abs(SAPTElec)+abs(SAPTDisp))), size=10), shape=15, show.legend=FALSE)
  q<- q +  scale_colour_gradientn(colours=jet.colors(7))
  q<- q + theme_bw()
  q<- q + xlab("Reference Total Energy (kcal/mol)")
  #q<- q + ylab("Total Energy (kcal/mol)")
  q<- q + labs(color="Energy Component")
  q<- q + theme(axis.text.x = element_text(size = 45))
  q<- q + theme(axis.text.y = element_blank())
  #q<- q + theme(axis.title.x = element_text(size = 30 ))
  q<- q + theme(axis.title.x = element_text(size=53))
  q<- q + theme(axis.title.y = element_blank())
  q<- q + ylim(x_min_val, y_max_val)
  
  rq <- paste("R=", round(corr_error_vs_ccsd_sapt(file, x, y),4))
  grob <- grobTree(textGrob(rq, x=0.1,  y=0.50, hjust=0,
                            gp=gpar(col="grey", fontsize=40, fontface="bold")))
  SAPT_total <- grobTree(textGrob("SAPT", x=0.05,  y=0.90, hjust=0,
                                 gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  
  sapt_mad <- grobTree(textGrob(mad_sapt, x=0.1,  y=0.80, hjust=0,
                               gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  sapt_msd <- grobTree(textGrob(msd_sapt, x=0.1,  y=0.70, hjust=0,
                               gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  sapt_rel_error <- grobTree(textGrob(rel_error_sapt, x=0.1,  y=0.60, hjust=0,
                                     gp=gpar(col=rgb(.70,.70,.70), fontsize=40, fontface="bold")))
  
  q<- q + annotation_custom(grob)
  q<- q + annotation_custom(SAPT_total)
  q<- q + annotation_custom(sapt_mad)
  q<- q + annotation_custom(sapt_msd)
  q<- q + annotation_custom(sapt_rel_error)
  
  interaction <- toupper(paste(x,y,sep='-'))
  interaction_title <- grobTree(textGrob(interaction, x=0.95,  y=0.075, just=1,
                                  gp=gpar(col="black", fontsize=40, fontface="bold")))
  q<- q + annotation_custom(interaction_title)
  efp_sapt_plot <- grid.arrange(p,q, ncol=2)
  print(efp_sapt_plot)
}

get_filtered <- function(file, x, y, numeric){
  temp <- as.data.frame(index_type(file, x, y))
  
  EFPTotal <- deparse(EFPTotal)
  subset(temp, EFPTotal  )[c("file", "A1", "A2", "T1", "T2", "EFPTotal", "SAPTTotal", "SAPTElec", "SAPTDisp")]
}

