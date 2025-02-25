################################################################################
# Workflow to compare regionalization made by DWD and KSPcimBB 
# Input:  DWD TIF-files  
#         KSPclimBB ASC-files 
# Output: Boxplot of difference between both regionalization approaches
#         CSV dwd vs. ksp
# Author: Rainer Hentschel, Rainer.Hentschel@lfb.brandenburg.de
# 08/2024
################################################################################

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
G$d_in <-file.path(G$d_home,"input");
G$d_out <-file.path(G$d_home,"output");
G$d_in1 <-G$d_in;
G$d_in2 <-paste(G$d_in,"02_dwd_nc2tif",sep="/");
G$d_out1 <-paste(G$d_out,G$n_script,sep="/")
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


# DIFF days --------------------------------------------------------------------------------
aa <-list.files(G$d_in1); aa;
ksp <-"pet_fao"
ksp <-"precip"
aa <-list.files(G$d_in2); aa;
ll <-aa[aa=="et_fao"];
ll <-aa[aa=="hyras"];
ii <-1;
for(ii in 1:length(ll))
  {
    G$d_in3 <-paste(G$d_in2,ll[ii],sep="/");
    if(ll[ii]=="hyras"){G$d_in3 <-paste(G$d_in2,ll[ii],"precipitation",sep="/");}
    aa <-list.files(G$d_in3); aa;
    ### loop years
    jj <-1; yy <-NULL;
    for(jj in 1:length(aa))
    {
      G$d_in4 <-paste(G$d_in3,aa[jj],sep="/");
      bb <-list.files(G$d_in4); 
      bb <-bb[str_detect(bb,".tif$")]; 
      ### ORDER days
      {
        b0 <-unlist(str_split(bb,"_")); 
        ifelse(ll[ii]%in%c("et_fao"),b0 <-b0[seq(4,length(b0),5)],b0 <-b0[seq(3,length(b0),4)])
        bb <-bb[order(as.integer(b0))];
        cc <-unlist(str_split(bb,"_"));
        ifelse(ll[ii]%in%c("et_fao"),dd <-cc[seq(4,length(cc),5)],dd <-cc[seq(3,length(cc),4)])
        dat <-as.Date(as.integer(dd), origin = paste0(aa[jj],"-01-01"))-1; 
        dat <-dat[order(dat)];
      }
      ### LOOP days
      kk <-1; xx <-data.frame(matrix(NA,0,0))
      for(kk in 1:length(bb))
      {
        print(paste(jj," in ",length(aa)," --- ",kk," in ",length(bb)))
        cc <-rast(paste(G$d_in4,bb[kk],sep="/")); # plot(cc); crs(cc)
        G$d_in5 <-paste(G$d_in1,ksp,sep="/");
        dd <-list.files(G$d_in5); dd <-dd[str_detect(dd,as.character(dat[kk]))];
        ee <-rast(paste(G$d_in5,dd,sep="/")); # plot(ee)
        crs(ee) <-"epsg:32632";
        ff <-project(ee,"epsg:25833"); # plot(ff)
        gg <-resample(ff,cc, method="bilinear")
        ff <-cc-gg; # plot(gg)
        ### save tif
        # G$d_temp <-paste(G$d_out1,ll[ii],sep="/")
        # if(!dir.exists(G$d_temp)){dir.create(G$d_temp)}
        # out <-paste(G$d_temp,paste("diff",ll[ii],dat[kk],".tif",sep="_"),sep="/")
        # writeRaster(ff, filename=out, overwrite=T);
        ### table
        c1 <-summary(cc);
        g1 <-summary(gg);
        f1 <-summary(ff);
        hh <-unlist(str_split(bb[kk],"_"));
        xx[kk,"yr"] <-aa[jj];
        xx[kk,"doy"] <-hh[c(length(hh)-1)];
        xx[kk,"dwd"] <-as.numeric(unlist(str_split(c1[4],":"))[2]);
        xx[kk,"ksp"] <-as.numeric(unlist(str_split(g1[4],":"))[2]);
        xx[kk,"diff"] <-as.numeric(unlist(str_split(f1[4],":"))[2]);
      }
      ###
      yy <-rbind(yy,xx);
    }
    ### save
    out <-paste(G$d_out1,paste0(ksp,"_diff.csv"),sep="/");
    write.table(yy,out,col.names = T,row.names = F,sep=";",dec=",",na="");
    
    ### year
    aa <-tapply(yy$ksp,yy$yr,function(x) sum(x,na.rm=T))
    bb <-tapply(yy$dwd,yy$yr,function(x) sum(x,na.rm=T))
    cc <-data.frame(
      yr=names(aa),
      dwd=bb,
      ksp=aa
    )
    cc$diff <-cc$dwd-cc$ksp;
    cc <-cc[cc$yr!=2023,]
    out <-paste(G$d_out1,paste0(ksp,"_diff_year.csv"),sep="/");
    write.table(cc,out,col.names = T,row.names = F,sep=";",dec=",",na="");
  }

### CHECK
# out <-paste(G$d_out1,paste0(ksp,"_diff.csv"),sep="/");
# yy <-read.table(out,header=T,sep=";",dec=",")

summary(yy)
aa <-tapply(yy$ksp,yy$doy,function(x) mean(x,na.rm=T))
bb <-data.frame(doy=as.integer(names(aa)),y=aa); bb <-bb[order(bb$doy),];
plot(bb)



# PLOT mean -----------------------------------------------------------------------------------------
ll <-list.files(G$d_out1);
ll <-ll[str_detect(ll,".csv")];
ll <-ll[str_detect(ll,"year")==F]
ii <-4;
for(ii in 1:length(ll))
{
  nam <-unlist(str_split(ll[ii],"_diff.csv"))[1]
  aa <-paste(G$d_out1,ll[ii],sep="/");
  bb <-read.table(aa,header = T,sep=";",dec=",");
  ### date
  bb <-bb[bb$yr<2023,]
  oo <-levels(as.factor(bb$yr))
  ### color
  cc <-colorRampPalette(c("grey10","grey90"));
  ccc <-cc(length(oo));
  ### limits
  t0 <-1;
  t1 <-356;
  z0 <- floor(min(bb$diff,na.rm=T));
  z1 <- ceiling(max(bb$diff,na.rm=T));
  ### window
  graphics.off();
  out <-paste(G$d_out1,paste0(str_sub(ll[ii],1,-5),".png"),sep="/");
  png(out, units="mm", width=160, height=100, res=300);
  par(mar=c(3,3,2,1),mgp=c(2,1,0),lab=c(12,5,7)); # par()
  ### base
  plot(c(z0,z1)~c(t0,t1),type="l",ylim=c(z0,z1),
       ylab=nam,xlab="DOY",col="white",
       main="mittlere Abweichung");
  grid(nx = NA, ny=NULL); # par()$usr
  ### legend
  legend("top",ncol=10,c(oo,"Mittel"),col=c(ccc,"green3"),pch=16,cex=.5)
  ### years
  jj <-1;
  for(jj in 1:length(oo))
  {
    dd <-bb[bb$yr%in%oo[jj],];
    dd <-dd[order(dd$doy),]
    lines(dd$diff,col=ccc[jj])
  }
  ### mean
  ee <-tapply(bb$diff,paste0(bb$doy), mean)
  ee <-ee[order(as.integer(names(ee)))];
  lines(ee,col="green3",lwd=3)# sort needed
  ### save
  graphics.off(); 
}


# DISCONNECT & CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")
