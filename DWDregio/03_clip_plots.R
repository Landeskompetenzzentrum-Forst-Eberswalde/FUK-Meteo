################################################################################
# Workflow to clip point data by coordinates
# Input:  DWD .tif
#         plot coordinates
# Output: list of tables of variables of each plot
#         .csv 
# Author: Rainer Hentschel, Rainer.Hentschel@lfb.brandenburg.de
# 02/2025
################################################################################



# PACKAGES --------------------------------------------------------------
ll <- c("rstudioapi", "tidyverse", "data.table","sp","sf","terra")
ii <- 1 # loop package install
for(ii in 1:length(ll)) {
  aa <- ll[ii]
  if(!aa %in% rownames(installed.packages())) install.packages(aa, dependencies = TRUE)
  library(aa, character.only = TRUE)
}


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
G$d_in <-file.path(G$d_home,"input");
G$d_out <-file.path(G$d_home,"output");
G$d_in1 <-G$d_in;
G$d_in2 <-paste(G$d_out,"02_dwd_nc2tif",sep="/");
G$d_out1 <-paste(G$d_out,G$n_script,sep="/")
if(!dir.exists(G$d_out1)){dir.create(G$d_out1)}
### geo 
G$g_epsg <-25833; # karten projektion als epsg code
### end
print(G)

# READ gadm_germany.rda--------------------------------------------------
aa <-list.files(G$d_in1); aa;
load(paste(G$d_in1,"00_read_geo-gadm_germany_region.rda",sep="/"));
geo_ger <-gadm_germany_regions; rm("gadm_germany_regions"); st_bbox(geo_ger)
geo_ger <-st_transform(geo_ger,G$g_epsg); st_bbox(geo_ger);
geo_bb <-geo_ger[geo_ger$NAME_1%in%"Brandenburg",]
plot(geo_bb[,1]); summary(geo_bb$geometry); str(geo_bb);
st_bbox(geo_bb);

# READ coordinates ------------------------------------------------------
out_name <-"outputfolder"; 
G$d_out2 <-paste(G$d_out1,out_name,sep="/"); 
if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};
xy <-NA;

### l2 open field
{
  out_name <-"l2_bb_openfield"
  G$d_out2 <-paste(G$d_out1,out_name,sep="/");
  if(!dir.exists(G$d_out2)){dir.create(G$d_out2)};
  ### in 
  aa <-list.files(G$d_in1); aa;
  xx <-read.table(file.path(G$d_in1,"l2_bb_coordinates.csv"),header=T,sep=";",dec=",");
  xx <-xx[xx$code_subpl%in%2,];
  xx <-xx[!xx$code_plot%in%c(1101),];
  G$nam_plots <-xx$code_plot; print(G$nam_plots);
  ### geo
  xy <-coordinates(data.frame(x=xx$long_epsg4326,y=xx$lat_epsg4326));
  xy <-SpatialPoints(xy,CRS("+init=epsg:4326 +datum=WGS84")); plot(xy);
  xy <-spTransform(xy,CRS("+init=epsg:25833 +datum=WGS84 "));
}



# TABLE variables --------------------------------------------------------------------------------
tt <-c(1960:G$t_year); # tt <-c(2022:G$t_year); # years of interest
aa <-list.files(G$d_in2); aa;
ll <-aa; 
ii <-1; jj <-1; kk <-1; pp <-1;
for(ii in 1:length(ll))
  {
    print(ll[ii]);
    ### et_fao
    if(ll[ii]%in%"et_fao")
    {
      yy <-data.frame(matrix(NA,0,c(nrow(xy)+2)));
      colnames(yy) <-c("year","day",G$nam_plots);
      G$d_in3 <-paste(G$d_in2,ll[ii],sep="/");
      cc <-list.files(G$d_in3); 
      cc <-cc[as.integer(cc)%in%tt]; # year of intrest only;
      for(jj in 1:length(cc)) 
      {
        yr <-cc[jj]; message(yr);
        dd <-list.files(file.path(G$d_in3,cc[jj]));
        dd <-dd[str_detect(dd,".tif$")];
        for(kk in 1:length(dd))
        {
          day <-unlist(str_split(dd[kk],"_")); day <-day[c(length(day)-1)];
          ee <-rast(file.path(G$d_in3,cc[jj],dd[kk])); #plot(ee); points(xy)
          ff <-extract(ee,st_as_sf(xy));
          ff <-round(ff[,2],4)
          yy[nrow(yy)+1,] <-c(yr,day,ff);
        }
      }
      ### save
      out <-file.path(G$d_out2,paste0(ll[ii],".csv"));
      write.table(yy,out,col.names = T,row.names = F,sep=";",dec=",",na="");
    }
    ### hyras
    if(ll[ii]%in%"hyras")
    {
      G$d_in3  <-paste(G$d_in2,ll[ii],sep="/");
      bb <-list.files(G$d_in3);
      for(pp in 1:length(bb))
      {
        yy <-data.frame(matrix(NA,0,c(nrow(xy)+2)));
        colnames(yy) <-c("year","day",G$nam_plots);
        G$d_in4 <-paste(G$d_in3,bb[pp],sep="/");
        G$d_out3 <-paste(G$d_out2,bb[pp],sep="/")
        if(!dir.exists(G$d_out3)){dir.create(G$d_out3)}
        cc <-list.files(G$d_in4); 
        cc <-cc[as.integer(cc)%in%tt]; # year of intrest only;
        for(jj in 1:length(cc)) 
        {
          yr <-cc[jj]; message(yr);
          dd <-list.files(file.path(G$d_in4,cc[jj]));
          dd <-dd[str_detect(dd,".tif$")]; dd <-sort(dd);
          for(kk in 1:length(dd))
          {
            day <-unlist(str_split(dd[kk],"_")); day <-day[c(length(day)-1)];
            ee <-rast(file.path(G$d_in4,cc[jj],dd[kk])); #plot(ee); points(xy)
            ff <-extract(ee,st_as_sf(xy));
            ff <-round(ff[,2],4)
            yy[nrow(yy)+1,] <-c(yr,day,ff);
          }
        }
        ### save
        out <-file.path(G$d_out2,paste0(ll[ii],"-",bb[pp],".csv"));
        write.table(yy,out,col.names = T,row.names = F,sep=";",dec=",",na="");
      }
    }
    ### end
  }
    
# DISCONNECT & CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")