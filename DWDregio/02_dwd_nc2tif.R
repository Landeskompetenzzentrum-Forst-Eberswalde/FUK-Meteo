### show dwd regionalised data
### loop variables, download and describe
### save rda
sessionInfo()

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
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); 
### input dir
G$d_home <-dirname(aa);
G$d_in <-G$d_home;
G$d_in1 <-paste(G$d_in,"input",sep="/");
G$d_in2 <-paste(G$d_in,"output/01_dwd_download",sep="/");
### output dir
G$d_out <-paste(G$d_home,"output",sep="/");
G$d_out1 <-paste(G$d_out,G$n_script,sep="/");
if(!dir.exists(G$d_out1)){dir.create(G$d_out1)}
G$d_out <-G$d_out1;
### geo 
G$g_epsg <-25833; # karten projektion als epsg code
### end
print(G)

# READ geo ----------------------------------------------------------------------
aa <-list.files(G$d_in1); aa;
load(paste(G$d_in1,"gadm_germany_region.rda",sep="/"));
geo_ger <-gadm_germany_regions; rm("gadm_germany_regions"); st_bbox(geo_ger)
geo_ger <-st_transform(geo_ger,G$g_epsg); st_bbox(geo_ger);
geo_bb <-geo_ger[geo_ger$NAME_1%in%c("Brandenburg","Berlin"),]
plot(geo_bb[,1]); summary(geo_bb$geometry); str(geo_bb);
plot(geo_bb)


# nc2tif --------------------------------------------------------------------------------
tt <-c(1960:G$t_year); # tt <-c(2022:G$t_year); # years of interest
aa <-list.files(G$d_in2); aa;
ll <-aa[!aa%in%c("radolan")];
ii <-2;
for(ii in 1:length(ll))
  {
    G$d_temp <-paste(G$d_out1,ll[ii],sep="/")
    if(!dir.exists(G$d_temp)){dir.create(G$d_temp)}
    ### et_fao
    if(ll[ii]%in%"et_fao")
    {
      bb <-paste(G$d_in2,ll[ii],sep="/");
      cc <-list.files(bb); cc <-cc[str_detect(cc,".nc$")]; # nc files only
      dd <-unlist(str_split(cc,"_v")); dd <-dd[seq(1,length(dd),2)]
      yr0 <-str_sub(dd,-4); 
      cc <-cc[yr0%in%tt]; # year of intrest only;
      jj <-1;
      for(jj in 1:length(cc)) 
      {
        yr <-yr0[jj]; message(yr);
        ### folder
        G$d_temp1 <-paste(G$d_temp,yr,sep="/")
        if(!dir.exists(G$d_temp1)){dir.create(G$d_temp1)}
        ### read dwd data
        dd <-paste(bb,cc[jj],sep="/"); 
        ee <-readDWD(dd); 
        ### extract days and clip Brandenburg
        mm <-names(ee);
        kk <-1;
        for(kk in 1:length(mm))
        {
          print(paste(ll[ii],cc[jj],mm[kk],sep=" --- "))
          out <-paste(ll[ii],yr,kk,".tif",sep="_");
          ff <-list.files(G$d_temp1); 
          if(out%in%ff){next}; # next if already exist
          ff <-ee[[kk]]; # plot(ff); st_bbox(ff);
          ### format
          ff <-project(ff,paste0("epsg:",G$g_epsg)); # crs(ff, describe=TRUE, proj=TRUE);
          gg <-crop(ff,vect(geo_bb)); # plot(gg); lines(vect(geo_bb), lwd=2);
          hh <-mask(gg,vect(geo_bb)); # plot(hh); lines(vect(geo_bb), lwd=2);
          ### save
          writeRaster(hh, filename=file.path(G$d_temp1,out), overwrite=T);
        }
      }
    }
    ### hyras
    if(ll[ii]%in%"hyras")
    {
      aa <-paste(G$d_in2,ll[ii],sep="/");
      bb <-list.files(aa);
      pp <-1;
      for(pp in 1:length(bb))
      {
        ww <-paste(aa,bb[pp],sep="/");
        ### folder
        G$d_temp1 <-paste(G$d_temp,bb[pp],sep="/")
        if(!dir.exists(G$d_temp1)){dir.create(G$d_temp1)}
        ###
        cc <-list.files(ww); 
        cc <-cc[str_detect(cc,".nc$")]; # nc files only
        dd <-unlist(str_split(cc,"_v")); dd <-dd[seq(1,length(dd),2)]
        yr0 <-str_sub(dd,-4); 
        cc <-cc[yr0%in%tt]; # year of intrest only;
        jj <-39;
        for(jj in 1:length(cc)) 
        {
          yr <-yr0[jj]; message(yr);
          ### folder
          G$d_temp2 <-paste(G$d_temp1,yr,sep="/");
          if(!dir.exists(G$d_temp2)){dir.create(G$d_temp2)}
          ### read dwd data
          dd <-paste(ww,cc[jj],sep="/"); 
          ee <-readDWD(dd); 
          ### extract days and clip Brandenburg
          mm <-names(ee);
          kk <-265;
          for(kk in 1:length(mm))
          {
            print(paste(ll[ii],cc[jj],mm[kk],sep=" --- "));
            out <-paste(ll[ii],yr,kk,".tif",sep="_");
            ff <-list.files(G$d_temp2); 
            if(out%in%ff){next}; # next if already exist
            ff <-ee[[kk]]; # plot(ff); st_bbox(ff);
            ### format
            ff <-project(ff,paste0("epsg:",G$g_epsg)); # crs(ff, describe=TRUE, proj=TRUE);
            gg <-crop(ff,vect(geo_bb)); # plot(gg); lines(vect(geo_bb), lwd=2);
            hh <-mask(gg,vect(geo_bb)); # plot(hh); lines(vect(geo_bb), lwd=2);
            ### save
            writeRaster(hh, filename=file.path(G$d_temp2,out), overwrite=T);
          }
        }
      }
    }
    ### radolan
    if(ll[ii]%in%"radolan")
    {
      aa <-paste(G$d_in2,ll[ii],sep="/");
      bb <-list.files(aa); bb <-bb[str_detect(bb,".gz$")]
      pp <-1;
      for(pp in 1:length(bb))
      {
        ww <-paste(aa,bb[pp],sep="/");
        ee <-readDWD(ww); # plot(ee$dat)
        ### extract days and clip Brandenburg
        mm <-names(ee);
        kk <-1;
        for(kk in 1:length(mm))
        {
          print(paste(ll[ii],cc[jj],mm[kk],sep=" --- "));
          out <-paste(ll[ii],yr,kk,".tif",sep="_");
          ff <-list.files(G$d_temp2); if(out%in%ff){next};
          ff <-ee[[kk]]; # plot(ff); st_bbox(ff);
          ### format
          ff <-project(ff,paste0("epsg:",G$g_epsg)); # crs(ff, describe=TRUE, proj=TRUE);
          gg <-crop(ff,vect(geo_bb)); # plot(gg); lines(vect(geo_bb), lwd=2);
          hh <-mask(gg,vect(geo_bb)); # plot(hh); lines(vect(geo_bb), lwd=2);
          ### save
          writeRaster(hh, filename=file.path(G$d_temp2,out), overwrite=T);
        }
      }
    }
    ### end
  }
    
# DISCONNECT & CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")