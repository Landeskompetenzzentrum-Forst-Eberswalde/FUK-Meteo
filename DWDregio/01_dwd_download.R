### set dwd regionalised data
### loop variables, download and describe
### save rda

# PACKAGES----------------------------------------------------------------
if(!"stringr"%in%rownames(installed.packages()))install.packages("stringr",dependencies = TRUE);library(stringr);
if(!"rstudioapi"%in%rownames(installed.packages())) install.packages("rstudioapi", dependencies = TRUE); library(rstudioapi);
if(!"sf"%in%rownames(installed.packages()))install.packages("sf",dependencies = TRUE);library(sf);
if(!"terra"%in%rownames(installed.packages()))install.packages("terra",dependencies = TRUE);library(terra);
if(!"RCurl"%in%rownames(installed.packages()))install.packages("RCurl",dependencies = TRUE);library(RCurl);
if(!"rdwd"%in%rownames(installed.packages()))install.packages("rdwd",dependencies = TRUE);library(rdwd);
# install.packages('terra', repos="https://rspatial.r-universe.dev");
# vignette("rdwd")
# browseURL("http://github.com/brry/prectemp/blob/master/Code_example.R")
# rdwd::app()

# GLOBALS ------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
### set time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); 
### set input dir
G$d_home <-dirname(aa)
G$d_in <-G$d_home
### set output dir
G$d_out <-file.path(G$d_home,"output");
G$d_out1 <-file.path(G$d_out,G$n_script);
if(!dir.exists(G$d_out1)){dir.create(G$d_out1)}
### end
print(G)


# LIST ftp sources --------------------------------------------------------

### update dwd sources
updateRdwd();
library(rdwd);

### index data1
data(metaIndex);
data(fileIndex);
data(gridIndex);

# DOWNLOAD daily ---------------------------------------------------------------------------
aa <-gridIndex;
aa <-aa[str_detect(aa,"^daily")];
aa <-aa[str_detect(aa,".pdf$")==F];
ii <-1; cc <-data.frame(matrix(NA,0,0)) 
for(ii in 1:length(aa))
{
  print(paste(ii," - ",length(aa)))
  bb <-unlist(str_split(aa[ii],"/"));
  jj <-1;
  for (jj in 1:length(bb)) {
    cc[ii,jj] <-bb[jj]
  }
}
cc <-cc[is.na(cc[,1])==F,]; 
levels(as.factor(cc[,1])); colnames(cc)[1] <-"res"; # resolution
levels(as.factor(cc[,2])); colnames(cc)[2] <-"var"; # variable group
tt <-c(1961:G$t_year); # tt <-c(2017:2020); # years of interest
ll <-c("evaporation_fao","hyras_de");
ll_nam <-c("et_fao","hyras");
ii <-2;
for(ii in 1:length(ll))
{
  if(ll_nam[ii]%in%c("et_fao"))
  {
    G$d_temp <-paste(G$d_out1,ll_nam[ii],sep="/");
    if(!dir.exists(G$d_temp)){dir.create(G$d_temp)}
    bb <-cc[cc$var%in%ll[ii],];
    bb <-bb[str_detect(bb$V3,"1.1.nc$"),];  # nc files only
    jj <-60;
    for(jj in 1:length(tt))
    {
      dd <-bb[str_detect(bb$V3,as.character(tt[jj])),]; 
      if(nrow(dd)==0){next};
      #if(nrow(dd)>1){dd <-dd[order(dd$V3),]; dd <-dd[1,]}
      ee <-str_length(dd$V3);
      ee <-paste(dd[,1],dd[,2],dd[,3],sep="/"); message(ee);
      # linux ftp problem - https://www.unix.com/unix-for-advanced-and-expert-users/68027-large-file-ftp-problem.html?s=9690da1aa1c7d6fb43d2ef78889ece56
      ff <- dataDWD(ee, base=gridbase, dir=G$d_temp, joinbf=T, read=F, progbar=T, dfargs=list(method="curl"));
      #ee <-list.files(G$d_temp); ee <-ee[str_detect(ee,as.character(tt[jj]))];
      #if(length(ee)>1){ee <-sort(ee);file.remove(file.path(G$d_temp,ee[2]));}
    }
  }
  if(ll_nam[ii]%in%c("hyras"))
  {
    G$d_temp <-paste(G$d_out1,ll_nam[ii],sep="/");
    if(!dir.exists(G$d_temp)){dir.create(G$d_temp)}
    bb <-cc[cc$var%in%ll[ii],];
    mm <-levels(as.factor(bb$V3));
    # mm <-c("air_temperature_mean","precipitation","humidity","radiation_global")
    kk <-6;
    for(kk in 1:length(mm))
    {
      G$d_temp1 <-paste(G$d_temp,mm[kk],sep="/");
      if(!dir.exists(G$d_temp1)){dir.create(G$d_temp1)}
      gg <-bb[bb$V3%in%mm[kk],];
      gg <-gg[str_detect(gg$V4,".nc"),];  # nc files only
      if(mm[kk]%in%"radiation_global"){gg <-gg[str_detect(gg$V4,"v3-1_de.nc"),]}
      if(!mm[kk]%in%"radiation_global"){gg <-gg[str_detect(gg$V4,"v6-0_de.nc"),]}
      gg <-gg[str_detect(gg$V4,"1931_2020")==F,]; 
      jj <-1;
      for(jj in 1:length(tt))
      {
        dd <-gg[str_detect(gg$V4,as.character(tt[jj])),]; 
        if(nrow(dd)==0){next};
        #if(nrow(dd)>1){dd <-dd[order(dd$V4,decreasing = T),]; dd <-dd[1,]}
        ee <-paste(dd[,1],dd[,2],dd[,3],dd[,4],sep="/"); message(ee);
        ff <- dataDWD(ee, base=gridbase, dir=G$d_temp1, joinbf=T, read=F, progbar=T, dfargs=list(method="curl"));
        #ee <-list.files(G$d_temp1); ee <-ee[str_detect(ee,as.character(tt[jj]))];
        #if(length(ee)>1){ee <-sort(ee);file.remove(file.path(G$d_temp1,ee[1]));}
      }
    }
  }
}



# DISCONNECT & CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")