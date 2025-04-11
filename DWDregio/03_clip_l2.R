# 
# read regionalized data
# clip plots 2 list
# save rda
#
# note: solar radiation ends with year 2020 (so far)

# PACKAGES --------------------------------------------------------------
ll <-c("rstudioapi","stringr","sp","sf","terra","RPostgreSQL","dotenv")
for(ii in 1:length(ll)){aa <-ll[ii];if(!aa%in%rownames(installed.packages()))install.packages(aa, dependencies = TRUE); library(aa, character.only = TRUE)}

# GLOBALS G ------------------------------------------------------------------
G <-list() ### list workfiles and facts
aa <-getActiveDocumentContext()$path
bb <-unlist(str_split(aa,"\\/"))
### name
G$n_script <-str_sub(bb[length(bb)],1,-3);
G$n_project <-bb[length(bb)-1];
G$n_plot <-str_sub(G$n_script,6,9)
### time
G$t_date <-Sys.Date() 
G$t_year <-as.integer(format(Sys.Date(),"%Y")); # G$year <-2023;
### dir
G$d_home <-dirname(aa); message(G$d_home);
G$d_in <-file.path(G$d_home,"input");  list.files(G$d_in)
G$d_in1 <-file.path(G$d_home,"output","02_dwd_nc2tif");  list.files(G$d_in1)
G$d_out <-file.path(G$d_home,"output"); list.files(G$d_out)
G$d_out1 <-file.path(G$d_out ,"rda");  list.files(G$d_out1)
### extras
G$regio_mean <-T; # country area mean 
G$forest_mean <-F; # forest area mean
  G$needle_mean <-F; G$leaf_mean <-F; # leaf/needle area mean
### end
print(G)

# ENVIRONMENT -------------------------------------------------------
load_dot_env(file =file.path(G$d_home,".env"))
E <-list();
E[["sys_env"]] <-Sys.getenv(); 
E[["session"]] <-sessionInfo();
E[["options"]] <-options();

# CONNECT FUK_PG -------------------------------------------------------------

### CONNECT
aa <-E[["sys_env"]]
host <-aa[names(aa)%in%"FUK_PG_HOST"]; port <-aa[names(aa)%in%"FUK_PG_PORT"];
user <-aa[names(aa)%in%"FUK_PG_USER"]; pw <-aa[names(aa)%in%"FUK_PG_PW"]; db <-aa[names(aa)%in%"FUK_PG_DB"]; 
pg <- dbConnect(PostgreSQL(),host=host,user=user,password=pw,port=port,dbname=db);

### SCHEMA
s1 <-"fuk";
qq <-paste("SELECT * FROM information_schema.tables WHERE table_schema ='", s1, "';", sep="");
aa <- dbSendQuery(pg, statement=qq);
bb <- fetch(aa, -1); tt <-bb$table_name; 


# load data -------------------------------------------------------------------------

### xy 
aa <-file.path(G$d_in,"l2_bb_be.csv"); list.files(G$d_in);
bb <-read.table(aa,header = T,sep=",",dec="."); 
bb <-bb[bb$code_location_mm%in%c("F"),]; bb <-bb[bb$dist<3000,];
cc <-coordinates(data.frame(x=bb$x_4326,y=bb$y_4326));
dd <-SpatialPoints(cc,CRS("+init=epsg:4326 +datum=WGS84")); 
xy <-SpatialPointsDataFrame(dd,bb); 

### regio raster
if(G$forest_mean)
{
  list.files(G$d_in);
  load(file.path(G$d_in,"gadm_germany_region.rda"));  
  gadm <-gadm_germany_regions[gadm_germany_regions$NAME_1%in%c("Brandenburg","Berlin"),]
}
### forest raster
if(G$forest_mean)
{
  list.files(G$d_in);
  dlt <-terra::rast(file.path(G$d_in,"DLT_2018_100m_Mosaic_Germany.tif"));
  dlt <- terra::project(dlt, paste0("epsg:4326"),method="near"); # crs(gadm)
  dlt <-crop(dlt,gadm); # 
  plot(dlt);# summary(as.factor(dlt$Count)); summary(dlt)
}

# clip regio ----------------------------------------------------------------
### var
list.files(G$d_in1); 
aa <-list.files(file.path(G$d_in1,"hyras")); aa;
bb <-paste("hyras",aa,sep="/");
var <-c(bb,"et_fao")
### nam
nam <-c("AT_max","AT","AT_min","RH","PR","RS","ET");
### dat
dat <-seq.Date(as.Date("01-01-1960","%d-%m-%Y"),Sys.Date(),by="day");
### list
REG <-list();
ll <-c(1961:G$t_year);
kk <-1;   # list.files(file.path(G$d_in2,var2[jj]));
for(kk in 1:length(var))
{
  REG[[nam[kk]]] <-NULL;
  xx <-data.frame(matrix(NA,0,0)); 
  ii <-1;
  for(ii in 1:length(ll))
  {
    uu <-file.path(G$d_in1,var[kk],ll[ii]);
    aa <-list.files(uu); aa <-aa[str_detect(aa,".tif$")];  
    if(length(aa)==0){message("missing year - next"); message(ll[ii]); message(nam[kk]); next;}
    aa <-aa[str_detect(aa,paste0("_",ll[ii],"_"))];
    if(length(aa)<365){message("missing dates in yr"); message(ll[ii]); message(nam[kk]);}
    bb <-seq.Date(as.Date(paste0(ll[ii],"-01-01")),as.Date(paste0(ll[ii],"-12-31")), by="day");
    bb <-data.frame(day=bb,jul=as.integer(format(bb,"%j")))
    jj <-1; 
    for(jj in 1:length(aa))
    {
      cc <-aa[jj]; print(cc);
      dd <-unlist(str_split(cc,as.character("_")));
      ee <-bb[bb$jul%in%dd[c(length(dd)-1)],]; print(ee);
      ff <-rast(file.path(uu,cc)); 
      gg <-spTransform(xy,crs(ff)); # plot(ff); plot(gg,add=T); 
      hh <-extract(ff,st_as_sf(gg)); # plot(hh)
      ### add date
      xx[c(nrow(xx)+1),] <-NA;
      xx[c(nrow(xx)),1] <-as.character(ee[,1]);
      if(jj==1){colnames(xx)[1] <-"date"};
      ### add regio_mean
      if(G$regio_mean)
      {
        xx[c(nrow(xx)),"regio_mean"] <-NA
        xx[c(nrow(xx)),"regio_mean"] <-round(global(ff, "mean", na.rm=TRUE),4);
      }
      ### add forest_mean
      if(G$forest_mean)
      {
        if(kk==1){tt <-dlt; tt = project(rr, crs(ff))}
        rr <-tt; ff <-resample(ff,rr)
        rr[rr==0] <- NA; 
        ww <-mask(ff,rr); # plot(tt)
        xx[c(nrow(xx)),"forest_mean"] <-NA; 
        xx[c(nrow(xx)),"forest_mean"] <-round(global(ww, "mean", na.rm=TRUE),4);
        if(G$needle_mean)
        {
          rr <-tt; rr[rr==1] <- NA; rr[rr==0] <- NA; 
          ww <-mask(ff,rr); 
          xx[c(nrow(xx)),"needle_mean"] <-NA; 
          xx[c(nrow(xx)),"needle_mean"] <-round(global(ww, "mean", na.rm=TRUE),4);
          
        }
        if(G$leaf_mean)
        {
          rr <-tt; rr[rr==2] <- NA; rr[rr==0] <- NA; 
          ww <-mask(ff,rr); 
          xx[c(nrow(xx)),"leaf_mean"] <-NA; 
          xx[c(nrow(xx)),"leaf_mean"] <-round(global(ww, "mean", na.rm=TRUE),4);
        }
      }
      ### add extracted
      {
        if(ii==1 & jj==1)
          {
            col_ext <-c(ncol(xx)+1):c(ncol(xx)+nrow(hh));
            xx[c(nrow(xx)),col_ext] <-NA;
            colnames(xx)[col_ext] <-xy@data$code_plot;
          }
        xx[c(nrow(xx)),col_ext] <-round(hh[,2],4);
      }
      ### clean
      rm("cc","dd","ee","ff","gg","hh"); gc();
      ### end jj
    }
    ### end ii
  }
  REG[[nam[kk]]] <-xx[order(xx$date, decreasing = F),];
  ### end kk
}

# SAVE ------------------------------------------------------------------------
out <-paste(G$n_script,".rda",sep="_");
save(REG,file = file.path(G$d_out1,out));
# load(file.path(G$d_out1,out))

# SAVE pg -------------------------------------------------------------
ll <-names(REG);
ii <-1; 
for(ii in 1:length(ll))
{
  aa <-REG[[ll[ii]]]; 
  ### write pg table
  s1 <-"fuk"
  dbGetQuery(pg,paste("SET search_path TO",s1)); 
  t1 <-tolower(paste("meteo",G$n_project,"l2",ll[ii],sep="-"));
  dbWriteTable(pg, t1,aa,overwrite=T); 
}

# CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")

