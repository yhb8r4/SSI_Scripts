file <-read.table('/Users/ybui/Documents/CCSDdata/newanalysis/B.dat.errors', header=FALSE, sep="")


load_data<-function(x){
  
  b<-'/Users/ybui/Documents/CCSDdata/newanalysis/B.dat.errors'
  b_ct<-'/Users/ybui/Documents/CCSDdata/newanalysis/B_ct.dat.errors'
  medium<-'/Users/ybui/Documents/CCSDdata/newanalysis/Medium.dat.errors'
  smb<-'/Users/ybui/Documents/CCSDdata/newanalysis/SMB.dat.errors'
  smb_ct<-'/Users/ybui/Documents/CCSDdata/newanalysis/SMB_ct.dat.errors'
  smb_ct_ps<-'/Users/ybui/Documents/CCSDdata/newanalysis/SMB_ct_ps.dat.errors'
  smb_ps<-'/Users/ybui/Documents/CCSDdata/newanalysis/SMB_ps.dat.errors'
  sm<-'/Users/ybui/Documents/CCSDdata/newanalysis/Sm.dat.errors'
  sm_es<-'/Users/ybui/Documents/CCSDdata/newanalysis/Sm_es.dat.errors'
  sm_ps<-'/Users/ybui/Documents/CCSDdata/newanalysis/Sm_ps.dat.errors'
  small<-'/Users/ybui/Documents/CCSDdata/newanalysis/Small.dat.errors'
  
bf<-read.table(b, header=FALSE, sep="")
b_ctf<-read.table(b_ct, header=FALSE, sep="")
mediumf<-read.table(medium, header=FALSE, sep="")
smbf<-read.table(smb, header=FALSE, sep="")
smb_ctf<-read.table(smb_ct, header=FALSE, sep="")
smb_ct_psf<-read.table(smb_ct_ps, header=FALSE, sep="")
smb_psf<-read.table(smb_ps, header=FALSE, sep="")
smf<-read.table(sm, header=FALSE, sep="")
sm_esf<-read.table(sm_es, header=FALSE, sep="")
sm_psf<-read.table(sm_ps, header=FALSE, sep="")
smallf<-read.table(small, header=FALSE, sep="")
}
gen_msd_types<-function(file){
  cationic_cationic <- c(which(paste(file$V2,file$V3)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$V2,file$V3)=="cationic anionic"),   which(paste(file$V2,file$V3)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$V2,file$V3)=="cationic polar"),     which(paste(file$V2,file$V3)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$V2,file$V3)=="cationic thiol"),     which(paste(file$V2,file$V3)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$V2,file$V3)=="cationic aliphatic"), which(paste(file$V2,file$V3)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$V2,file$V3)=="cationic aryl"),      which(paste(file$V2,file$V3)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$V2,file$V3)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$V2,file$V3)=="anionic polar"),      which(paste(file$V2,file$V3)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$V2,file$V3)=="anionic thiol"),      which(paste(file$V2,file$V3)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$V2,file$V3)=="anionic aliphatic"),  which(paste(file$V2,file$V3)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$V2,file$V3)=="anionic aryl"),       which(paste(file$V2,file$V3)=="aryl anionic"))
  polar_polar <- c(which(paste(file$V2,file$V3)=="polar polar"))
  polar_thiol <- c(which(paste(file$V2,file$V3)=="polar thiol"),        which(paste(file$V2,file$V3)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$V2,file$V3)=="polar aliphatic"),    which(paste(file$V2,file$V3)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$V2,file$V3)=="polar aryl"),         which(paste(file$V2,file$V3)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$V2,file$V3)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$V2,file$V3)=="thiol aliphatic"),    which(paste(file$V2,file$V3)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$V2,file$V3)=="thiol aryl"),         which(paste(file$V2,file$V3)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$V2,file$V3)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$V2,file$V3)=="aliphatic aryl"),     which(paste(file$V2,file$V3)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$V2,file$V3)=="aryl aryl"))
  
  df<-matrix(c(
    mean(file[,"V4"][cationic_cationic]-file[,"V5"][cationic_cationic]),
    mean(file[,"V4"][cationic_anionic]-file[,"V5"][cationic_anionic]),
    mean(file[,"V4"][cationic_polar]-file[,"V5"][cationic_polar]),
    mean(file[,"V4"][cationic_thiol]-file[,"V5"][cationic_thiol]),
    mean(file[,"V4"][cationic_aliphatic]-file[,"V5"][cationic_aliphatic]),
    mean(file[,"V4"][cationic_aryl]-file[,"V5"][cationic_aryl]),
    mean(file[,"V4"][anionic_anionic]-file[,"V5"][anionic_anionic]),
    mean(file[,"V4"][anionic_polar]-file[,"V5"][anionic_polar]),
    mean(file[,"V4"][anionic_thiol]-file[,"V5"][anionic_thiol]),
    mean(file[,"V4"][anionic_aliphatic]-file[,"V5"][anionic_aliphatic]),
    mean(file[,"V4"][anionic_aryl]-file[,"V5"][anionic_aryl]),
    mean(file[,"V4"][polar_polar]-file[,"V5"][polar_polar]),
    mean(file[,"V4"][polar_thiol]-file[,"V5"][polar_thiol]),
    mean(file[,"V4"][polar_aliphatic]-file[,"V5"][polar_aliphatic]),
    mean(file[,"V4"][polar_aryl]-file[,"V5"][polar_aryl]),
    mean(file[,"V4"][thiol_thiol]-file[,"V5"][thiol_thiol]),
    mean(file[,"V4"][thiol_aliphatic]-file[,"V5"][thiol_aliphatic]),
    mean(file[,"V4"][thiol_aryl]-file[,"V5"][thiol_aryl]),
    mean(file[,"V4"][aliphatic_aliphatic]-file[,"V5"][aliphatic_aliphatic]),
    mean(file[,"V4"][aliphatic_aryl]-file[,"V5"][aliphatic_aryl]),
    mean(file[,"V4"][aryl_aryl]-file[,"V5"][aryl_aryl]),
    mean(file[,"V4"]-file[,"V5"])
  ))
  
  df
  
}

gen_mad_types<-function(file){
  cationic_cationic <- c(which(paste(file$V2,file$V3)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$V2,file$V3)=="cationic anionic"),   which(paste(file$V2,file$V3)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$V2,file$V3)=="cationic polar"),     which(paste(file$V2,file$V3)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$V2,file$V3)=="cationic thiol"),     which(paste(file$V2,file$V3)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$V2,file$V3)=="cationic aliphatic"), which(paste(file$V2,file$V3)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$V2,file$V3)=="cationic aryl"),      which(paste(file$V2,file$V3)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$V2,file$V3)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$V2,file$V3)=="anionic polar"),      which(paste(file$V2,file$V3)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$V2,file$V3)=="anionic thiol"),      which(paste(file$V2,file$V3)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$V2,file$V3)=="anionic aliphatic"),  which(paste(file$V2,file$V3)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$V2,file$V3)=="anionic aryl"),       which(paste(file$V2,file$V3)=="aryl anionic"))
  polar_polar <- c(which(paste(file$V2,file$V3)=="polar polar"))
  polar_thiol <- c(which(paste(file$V2,file$V3)=="polar thiol"),        which(paste(file$V2,file$V3)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$V2,file$V3)=="polar aliphatic"),    which(paste(file$V2,file$V3)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$V2,file$V3)=="polar aryl"),         which(paste(file$V2,file$V3)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$V2,file$V3)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$V2,file$V3)=="thiol aliphatic"),    which(paste(file$V2,file$V3)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$V2,file$V3)=="thiol aryl"),         which(paste(file$V2,file$V3)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$V2,file$V3)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$V2,file$V3)=="aliphatic aryl"),     which(paste(file$V2,file$V3)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$V2,file$V3)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs(file[,"V4"][cationic_cationic]-file[,"V5"][cationic_cationic])),
    mean(abs(file[,"V4"][cationic_anionic]-file[,"V5"][cationic_anionic])),
    mean(abs(file[,"V4"][cationic_polar]-file[,"V5"][cationic_polar])),
    mean(abs(file[,"V4"][cationic_thiol]-file[,"V5"][cationic_thiol])),
    mean(abs(file[,"V4"][cationic_aliphatic]-file[,"V5"][cationic_aliphatic])),
    mean(abs(file[,"V4"][cationic_aryl]-file[,"V5"][cationic_aryl])),
    mean(abs(file[,"V4"][anionic_anionic]-file[,"V5"][anionic_anionic])),
    mean(abs(file[,"V4"][anionic_polar]-file[,"V5"][anionic_polar])),
    mean(abs(file[,"V4"][anionic_thiol]-file[,"V5"][anionic_thiol])),
    mean(abs(file[,"V4"][anionic_aliphatic]-file[,"V5"][anionic_aliphatic])),
    mean(abs(file[,"V4"][anionic_aryl]-file[,"V5"][anionic_aryl])),
    mean(abs(file[,"V4"][polar_polar]-file[,"V5"][polar_polar])),
    mean(abs(file[,"V4"][polar_thiol]-file[,"V5"][polar_thiol])),
    mean(abs(file[,"V4"][polar_aliphatic]-file[,"V5"][polar_aliphatic])),
    mean(abs(file[,"V4"][polar_aryl]-file[,"V5"][polar_aryl])),
    mean(abs(file[,"V4"][thiol_thiol]-file[,"V5"][thiol_thiol])),
    mean(abs(file[,"V4"][thiol_aliphatic]-file[,"V5"][thiol_aliphatic])),
    mean(abs(file[,"V4"][thiol_aryl]-file[,"V5"][thiol_aryl])),
    mean(abs(file[,"V4"][aliphatic_aliphatic]-file[,"V5"][aliphatic_aliphatic])),
    mean(abs(file[,"V4"][aliphatic_aryl]-file[,"V5"][aliphatic_aryl])),
    mean(abs(file[,"V4"][aryl_aryl]-file[,"V5"][aryl_aryl])),
    mean(abs(file[,"V4"]-file[,"V5"]))
  ))
  
  df
  
}

gen_relative_mad_types<-function(file){
  cationic_cationic <- c(which(paste(file$V2,file$V3)=="cationic cationic"))
  cationic_anionic <- c(which(paste(file$V2,file$V3)=="cationic anionic"),   which(paste(file$V2,file$V3)=="anionic cationic"))
  cationic_polar <- c(which(paste(file$V2,file$V3)=="cationic polar"),     which(paste(file$V2,file$V3)=="polar cationic"))
  cationic_thiol <- c(which(paste(file$V2,file$V3)=="cationic thiol"),     which(paste(file$V2,file$V3)=="thiol cationic"))
  cationic_aliphatic <- c(which(paste(file$V2,file$V3)=="cationic aliphatic"), which(paste(file$V2,file$V3)=="aliphatic cationic"))
  cationic_aryl <- c(which(paste(file$V2,file$V3)=="cationic aryl"),      which(paste(file$V2,file$V3)=="aryl cationic"))
  anionic_anionic <- c(which(paste(file$V2,file$V3)=="anionic anionic"))
  anionic_polar <- c(which(paste(file$V2,file$V3)=="anionic polar"),      which(paste(file$V2,file$V3)=="polar anionic"))
  anionic_thiol <- c(which(paste(file$V2,file$V3)=="anionic thiol"),      which(paste(file$V2,file$V3)=="thiol anionic"))
  anionic_aliphatic <- c(which(paste(file$V2,file$V3)=="anionic aliphatic"),  which(paste(file$V2,file$V3)=="aliphatic anionic"))
  anionic_aryl <- c(which(paste(file$V2,file$V3)=="anionic aryl"),       which(paste(file$V2,file$V3)=="aryl anionic"))
  polar_polar <- c(which(paste(file$V2,file$V3)=="polar polar"))
  polar_thiol <- c(which(paste(file$V2,file$V3)=="polar thiol"),        which(paste(file$V2,file$V3)=="thiol polar"))
  polar_aliphatic <- c(which(paste(file$V2,file$V3)=="polar aliphatic"),    which(paste(file$V2,file$V3)=="aliphatic polar"))
  polar_aryl <- c(which(paste(file$V2,file$V3)=="polar aryl"),         which(paste(file$V2,file$V3)=="aryl polar"))
  thiol_thiol <- c(which(paste(file$V2,file$V3)=="thiol thiol"))
  thiol_aliphatic <- c(which(paste(file$V2,file$V3)=="thiol aliphatic"),    which(paste(file$V2,file$V3)=="aliphatic thiol"))
  thiol_aryl <- c(which(paste(file$V2,file$V3)=="thiol aryl"),         which(paste(file$V2,file$V3)=="aryl thiol"))
  aliphatic_aliphatic <- c(which(paste(file$V2,file$V3)=="aliphatic aliphatic"))
  aliphatic_aryl <- c(which(paste(file$V2,file$V3)=="aliphatic aryl"),     which(paste(file$V2,file$V3)=="aryl aliphatic"))
  aryl_aryl  <- c(which(paste(file$V2,file$V3)=="aryl aryl"))
  
  df<-matrix(c(
    mean(abs((file[,"V4"][cationic_cationic]-file[,"V5"][cationic_cationic])/file[,"V5"][cationic_cationic])),
    mean(abs((file[,"V4"][cationic_anionic]-file[,"V5"][cationic_anionic])/file[,"V5"][cationic_anionic])),
    mean(abs((file[,"V4"][cationic_polar]-file[,"V5"][cationic_polar])),
         mean(abs((file[,"V4"][cationic_thiol]-file[,"V5"][cationic_thiol])/file[,"V5"][cationic_thiol])),
         mean(abs((file[,"V4"][cationic_aliphatic]-file[,"V5"][cationic_aliphatic])/file[,"V5"][cationic_aliphatic])),
         mean(abs((file[,"V4"][cationic_aryl]-file[,"V5"][cationic_aryl])/file[,"V5"][cationic_aryl]),
              mean(abs((file[,"V4"][anionic_anionic]-file[,"V5"][anionic_anionic])/file[,"V5"][anionic_anionic])),
              mean(abs((file[,"V4"][anionic_polar]-file[,"V5"][anionic_polar])/file[,"V5"][anionic_polar])),
              mean(abs((file[,"V4"][anionic_thiol]-file[,"V5"][anionic_thiol])/file[,"V5"][anionic_thiol])),
              mean(abs((file[,"V4"][anionic_aliphatic]-file[,"V5"][anionic_aliphatic])/file[,"V5"][anionic_aliphatic])),
              mean(abs((file[,"V4"][anionic_aryl]-file[,"V5"][anionic_aryl])/file[,"V5"][anionic_aryl])),
              mean(abs((file[,"V4"][polar_polar]-file[,"V5"][polar_polar])/file[,"V5"][polar_polar])),
              mean(abs((file[,"V4"][polar_thiol]-file[,"V5"][polar_thiol])/file[,"V5"][polar_thiol])),
              mean(abs((file[,"V4"][polar_aliphatic]-file[,"V5"][polar_aliphatic])/file[,"V5"][polar_aliphatic])),
              mean(abs((file[,"V4"][polar_aryl]-file[,"V5"][polar_aryl])/file[,"V5"][polar_aryl])),
              mean(abs((file[,"V4"][thiol_thiol]-file[,"V5"][thiol_thiol])/file[,"V5"][thiol_thiol])),
              mean(abs((file[,"V4"][thiol_aliphatic]-file[,"V5"][thiol_aliphatic])/file[,"V5"][thiol_aliphatic])),
              mean(abs((file[,"V4"][thiol_aryl]-file[,"V5"][thiol_aryl])/file[,"V5"][thiol_aryl])),
              mean(abs((file[,"V4"][aliphatic_aliphatic]-file[,"V5"][aliphatic_aliphatic])/file[,"V5"][aliphatic_aliphatic])),
              mean(abs((file[,"V4"][aliphatic_aryl]-file[,"V5"][aliphatic_aryl])/file[,"V5"][aliphatic_aryl])),
              mean(abs((file[,"V4"][aryl_aryl]-file[,"V5"][aryl_aryl])/file[,"V5"][aryl_aryl])),
              mean(abs((file[,"V4"]-file[,"V5"])/file[,"V5"]))
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

