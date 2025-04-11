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
G$d_out <-file.path(G$d_home,"output"); list.files(G$d_out)
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
bb <- fetch(aa, -1);

### TABLE
tt <-bb$table_name; 
tt <-tt[str_detect(tt,"dwdregio")];
tt <-tt[str_detect(tt,"-year")==F];
tt <-tt[str_detect(tt,"-0409")==F];


# LOOP table ------------------------------------------------------------
ll <-tt;
ii <-1; 
for(ii in 1:length(ll))
{
  print(tt[ii]);
  dbGetQuery(pg,paste("SET search_path TO",s1)); 
  aa <-dbReadTable(pg,tt[ii]); 
  aa$year <-format(as.Date(aa$date,tz=""),"%Y");
  x1 <-data.frame(year=levels(as.factor(aa$year)));
  x2 <-data.frame(year=levels(as.factor(aa$year)));
  ### LOOP colomns
  bb <-colnames(aa); bb <-bb[!bb%in%c("date","year")];
  jj <-1; 
  for(jj in 1:length(bb))
  {
    var <-unlist(str_split(tt[ii],"-")); var <-var[length(var)];
    ### YEAR
    {
      if(var%in%c("et","pr")){cc <-tapply(aa[,bb[jj]], aa$year, sum)};
      if(!var%in%c("et","pr")){cc <-tapply(aa[,bb[jj]], aa$year, mean)};
      cc <-tapply(aa[,bb[jj]], aa$year, sum);
      yy <-data.frame(year=names(cc),n=as.numeric(cc)); colnames(yy)[2] <-bb[jj];
      x1 <-merge(x1,yy,by="year",all.x=T);
    }
    ### 0409
    dd <-aa[format(as.Date(aa$date,tz=""),"%m")%in%c("04","05","06","07","08","09"),];
    if(var%in%c("et","pr")){cc <-tapply(dd[,bb[jj]], dd$year, sum)};
    if(!var%in%c("et","pr")){cc <-tapply(dd[,bb[jj]], dd$year, mean)};
    yy <-data.frame(year=names(cc),n=as.numeric(cc)); colnames(yy)[2] <-bb[jj];
    x2 <-merge(x2,yy,by="year",all.x=T);
  }
  ### SAVE
  s1 <-"fuk"; dbGetQuery(pg,paste("SET search_path TO",s1)); 
  t1 <-paste(tt[ii],"year",sep="-");
  dbWriteTable(pg,t1,x1,overwrite=T); 
  t2 <-paste(tt[ii],"0409",sep="-");
  dbWriteTable(pg,t2,x2,overwrite=T); 
}

# CLEAN ------------------------------------------------------------------------------
rm(list = ls());
cat("//014")

