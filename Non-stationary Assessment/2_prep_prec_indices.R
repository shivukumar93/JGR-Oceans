library(PCICt)
library(climdex.pcic)

is.leapyear=function(year)
{
  return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
}



date_n<-NA
year_n<-NA


for (i in 1901:2004)
{
  print(i)
  
  if(is.leapyear(i)==TRUE)
  {
    yrs =matrix(1,nrow=366,ncol=1)*i
    dts=c(1:366)
  }
  else
  {
    yrs =matrix(1,nrow=365,ncol=1)*i
    dts=c(1:365)
  }
  y<-t(yrs)
  dn<-t(dts)
  
  date_n<-cbind(date_n,dn)
  year_n<-cbind(year_n,y)
  
}
date_n<-date_n[2:37987]
year_n<-year_n[2:37987]

data1<-data[3:37988,]

RX1day<-matrix(nrow =104,ncol=357)
sdii<-matrix(nrow =104,ncol=357)
R10<-matrix(nrow =104,ncol=357)
CWD<-matrix(nrow =104,ncol=357)


for (i in 1:357)
{
  print(i)
  # importing tmin files

  pdata<-data1[,i]
  pdata[pdata<=1]<-0
  pdata<-cbind(pdata,year_n,date_n)
  colnames(pdata)<-c("V1","V2","V3")
  prec_d<-as.data.frame(pdata)
  

  t_min<-cbind(pdata,year_n,date_n)
  t_min<-as.data.frame(t_min,row.names = NULL)
  
  colnames(t_min)<-c("V1","V2","V3")
  t_max<-cbind(pdata,year_n,date_n)
  
  t_max<-as.data.frame(pdata,row.names = NULL)
  colnames(t_max)<-c("V1","V2","V3")
  
  
  
  prec.dates<-as.PCICt(do.call(paste,prec_d[,c("V2","V3")]), format="%Y %j", cal="gregorian")
  tmin.dates<-as.PCICt(do.call(paste,prec_d[,c("V2","V3")]), format="%Y %j", cal="gregorian")                      
  tmax.dates<-as.PCICt(do.call(paste,prec_d[,c("V2","V3")]), format="%Y %j", cal="gregorian")                         
  
  ci<- climdexInput.raw(prec_d[,1],prec_d[,1],prec_d[,1],tmax.dates,tmin.dates,prec.dates,base.range=c(1961,2004))
  
  
  #calculating TxOTAL 24 indices 
  
  RX1day[,i]<-apply(matrix(climdex.rx1day(ci),nrow=12,ncol = 104),2,max)
  sdii[,i]<-climdex.sdii(ci)
  R10[,i]<-climdex.r10mm(ci)
  CWD[,i]<-climdex.cwd(ci)

  
}


outfile1<-paste('G:/Lockdown/IJOC nons_revision/indices/Rx1day_1901_2004.csv',sep="")

write.csv(RX1day,outfile1)

outfile2<-paste('G:/Lockdown/IJOC nons_revision/indices/SDII_1901_2004.csv',sep="")

write.csv(sdii,outfile2)

outfile3<-paste('G:/Lockdown/IJOC nons_revision/indices/R10_1901_2004.csv',sep="")

write.csv(R10,outfile3)

outfile4<-paste('G:/Lockdown/IJOC nons_revision/indices/CWD_1901_2004.csv',sep="")

write.csv(CWD,outfile4)
