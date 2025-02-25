### show dwd regionalised data
### loop variables, download and describe
### save rda
sessionInfo()
# https://doku.lrz.de/parallelization-using-r-10747291.html


# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","data.table","foreach","doSNOW","terra","sf","RCurl","rdwd")
for(ii in 1:length(ll)){if(!ll[ii]%in%rownames(installed.packages()))install.packages(ll[ii],dependencies = TRUE);library(ll[ii], character.only = TRUE)}


# GLOBALS ------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3) 
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); 
### dir
G$d_home <-dirname(aa);
G$d_in <-file.path(G$d_home,"input");
G$d_out <-file.path(G$d_home,"output");
G$d_in1 <-G$d_in;
G$d_in2 <-paste(G$d_out,"01_dwd_download",sep="/");
G$d_out1 <-file.path(G$d_out,G$n_script);
if(!dir.exists(G$d_out1)){dir.create(G$d_out1)}
### geo 
G$g_epsg <-25833; # karten projektion als epsg code
### end
print(G)

# READ geo ----------------------------------------------------------------------
aa <-list.files(G$d_in1); aa;
load(paste(G$d_in1,"00_read_geo-gadm_germany_region.rda",sep="/"));
geo_ger <-gadm_germany_regions; rm("gadm_germany_regions"); st_bbox(geo_ger)
geo_ger <-st_transform(geo_ger,G$g_epsg); st_bbox(geo_ger);
geo_bb <-geo_ger[geo_ger$NAME_1%in%"Brandenburg",]
plot(geo_bb[,1]); summary(geo_bb$geometry); str(geo_bb);
st_bbox(geo_bb)


# nc2tif --------------------------------------------------------------------------------
tt <-c(1960:G$t_year); # tt <-c(2022:G$t_year); # years of interest
aa <-list.files(G$d_in2); aa;
ll <-aa;
ii <-1;
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
        cl <- makeCluster(2, type="SOCK")
        registerDoSNOW(cl)
        mm <-names(ee); kk <-1;
        foreach(kk = c(1:length(mm)))%dopar%
        {
          print(paste(ll[ii],cc[jj],mm[kk],sep=" --- "))
          ff <-ee[[kk]]; # plot(ff); st_bbox(ff);
          ### format
          ff <-project(ff,paste0("epsg:",G$g_epsg)); # crs(ff, describe=TRUE, proj=TRUE);
          gg <-crop(ff,vect(geo_bb)); # plot(gg); lines(vect(geo_bb), lwd=2);
          hh <-mask(gg,vect(geo_bb)); # plot(hh); lines(vect(geo_bb), lwd=2);
          ### save
          out <-paste(G$d_temp1,paste(ll[ii],yr,kk,".tif",sep="_"),sep="/")
          writeRaster(hh, filename=out, overwrite=T);
        }
        stopCluster(cl)
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
        gg <-paste(aa,bb[pp],sep="/");
        ### folder
        G$d_temp1 <-paste(G$d_temp,bb[pp],sep="/")
        if(!dir.exists(G$d_temp1)){dir.create(G$d_temp1)}
        ###
        cc <-list.files(gg); cc <-cc[str_detect(cc,".nc$")]; # nc files only
        dd <-unlist(str_split(cc,"_v")); dd <-dd[seq(1,length(dd),2)]
        yr0 <-str_sub(dd,-4); 
        cc <-cc[yr0%in%tt]; # year of intrest only;
        jj <-1;
        for(jj in 1:length(cc)) 
        {
          yr <-yr0[jj]; message(yr);
          ### folder
          G$d_temp2 <-paste(G$d_temp1,yr,sep="/");
          if(!dir.exists(G$d_temp2)){dir.create(G$d_temp2)}
          ### read dwd data
          dd <-paste(gg,cc[jj],sep="/"); 
          ee <-readDWD(dd); 
          ### extract days and clip Brandenburg
          mm <-names(ee);
          kk <-1;
          for(kk in 1:length(mm))
          {
            print(paste(ll[ii],cc[jj],mm[kk],sep=" --- "))
            ff <-ee[[kk]]; # plot(ff); st_bbox(ff);
            ### format
            ff <-project(ff,paste0("epsg:",G$g_epsg)); # crs(ff, describe=TRUE, proj=TRUE);
            gg <-crop(ff,vect(geo_bb)); # plot(gg); lines(vect(geo_bb), lwd=2);
            hh <-mask(gg,vect(geo_bb)); # plot(hh); lines(vect(geo_bb), lwd=2);
            ### save
            out <-paste(G$d_temp2,paste(ll[ii],yr,kk,".tif",sep="_"),sep="/")
            writeRaster(hh, filename=out, overwrite=T);
          }
        }
      }
    }
    ### end
  }
    
# DISCONNECT & CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")