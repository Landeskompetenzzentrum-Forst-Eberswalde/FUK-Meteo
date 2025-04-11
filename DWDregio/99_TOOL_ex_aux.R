### EX aux files saved by write.raster()

# LIBRARY --------------------------------------------------------------------------------
if(!"rstudioapi"%in%rownames(installed.packages())) install.packages("rstudioapi", dependencies = TRUE); library(rstudioapi);
if(!"stringr"%in%rownames(installed.packages())) install.packages("stringr", dependencies = TRUE); library(stringr);

# GLOBALS ------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); 
### input dir
G$d_home <-dirname(aa);
G$d_in <-G$d_home;
G$d_in1 <-file.path(G$d_in,"output"); list.files(G$d_in1);
G$d_in2 <-file.path(G$d_in1,"02_dwd_nc2tif"); list.files(G$d_in2);
### end
print(G)

# EX xml regio ------------------------------------------------------------------
ll <-list.files(G$d_in2);
ll <-ll[!ll%in%c("radolan")];
ii <-1;
for(ii in 1:length(ll))
{
  G$d_temp1 <-file.path(G$d_in2,ll[ii]); 
  cc <-list.files(G$d_temp1);
  if(ll[ii]%in%c("et_fao")){G$d_temp1 <-G$d_in2; cc <-"et_fao"}
  jj <-1;
  for(jj in 1:length(cc))
  {
    G$d_temp2 <-file.path(G$d_temp1,cc[jj]);
    dd <-list.files(G$d_temp2);
    kk <-1;
    for(kk in 1:length(dd))
    {
      G$d_temp3 <-file.path(G$d_temp2,dd[kk]);
      print(G$d_temp3);
      aa <-list.files(G$d_temp3 ); 
      bb <-aa[str_detect(aa,"aux.json$")]; 
      lapply(file.path(G$d_temp3,bb), function(x){file.remove(x)});
    }
  }
}


# EX xml clim ------------------------------------------------------------------
setwd(G$d_clim); ll <-list.files();
ii <-3;
for(ii in 1:length(ll))
{
  setwd(G$d_clim); setwd(ll[ii]); aa <-list.files();
  bb <-aa[str_detect(aa,"aux.xml$")]; 
  lapply(bb, function(x){file.remove(x)});
  if(ll[ii]%in%"years")
  {
    setwd(G$d_clim); setwd(ll[ii]); pp <-list.files();
    jj <-1;
    for(jj in 1:length(pp))
    {
      setwd(G$d_clim); setwd(ll[ii]); setwd(pp[jj]); aa <-list.files();
      bb <-aa[str_detect(aa,"aux.xml$")]; 
      lapply(bb, function(x){file.remove(x)});
    }
  }
}



# CLEAN ---------------------------------------------------
print(paste("script end:",Sys.time()))
print(paste("script run:",Sys.time()-G$st))
rm(list = ls());  gc()
cat("\014")