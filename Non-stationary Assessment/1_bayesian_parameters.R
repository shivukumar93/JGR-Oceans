
# DOUBLE COMBINATION 
library(extRemes)
library(readxl)
library(coda)
combination <- read.csv("combination.csv")
combination<-combination[,2:7]
best_combo<- read.csv("best_combo_prec.csv", header=TRUE)
best_combo<-best_combo[,2:1039]
p_val <- read.csv("min_p_val_prec.csv")
p_val<- p_val[,2:1039]

comb_id<-c(1039,1040,1041)
p_val<-p_val[,2:1039]
comb_id<-c(1039,1040,1041)
step1=0
step2=0
for(point in 495:1038)
{ 
  for(index in 3)
  {
    iter1<-20000
    iter2<-0
    
    filename111<-paste('new_india_prec_indice_',index,'.csv',sep="")
    data<-read.csv(filename111,header=TRUE)
    # data<-data[,2:109]
    m<-best_combo[index,point]
    print(paste('point_',point,'_index_',index))
    
    if (p_val[index,point]>0.05)
    {
      
      print(1)
      
      fitss<-fevd(data[,point],data = data)   
      fits<-fevd(data[,point],data = data,method = c("Bayesian"),iter=iter1,initial=as.list(fitss$results$par))      #<====== TO BE CORRECTED
      output<-heidel.diag(fits$results,eps=0.5,pvalue = 0.5)
      
      pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
      
      if(length(pmn)>sum(pmn))
      {
        
        while(length(pmn)>sum(pmn))
        {
          iter2=iter2+iter1;
          fits<-fevd(data[,point],data = data,method = c("Bayesian"),iter=iter2) 
          output<-heidel.diag(fits$results,eps=0.5,pvalue = 0.05)
          pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
          print(pmn)
          
          if(iter2>100000)
          {
            break
          }
        }
      }
      else
      {
        output<-heidel.diag(fits$results,eps=0.5,pvalue = 0.05)
        pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
        print(pmn)
        
      }
    }
    else
    {
      
      m<-best_combo[index,point]
      comb1<-combination[m,]
      comb1<-comb1[!is.na(comb1)]
      id_comb1<-comb_id[comb1]
      cov<-data[,id_comb1]
      
      #------------------------------------------------------------
      if (m<8)
        
      {
        #preparing dataframe
        lo_name11<-c('mu1','mu2','mu3')
        lo_name1<-lo_name11[1:length(comb1)]
        
        final_name1<-(lo_name1)
        df11<-data.frame(cov)
        df1<-setNames(df11,final_name1)
        #converting into list
        
        val1<-list(df1)[[1]]
        
        
        print(2)
        fitss<-fevd(data[,point],data = data,location.fun =~as.matrix(cov))
        
        fits<-fevd(data[,point],data,location.fun =~as.matrix(cov),method=c("Bayesian"),initial=list(fitss$results$par), iter=iter1) 
        
        output<-heidel.diag(fits$results,eps=0.5,pvalue = 0.05)
        
        pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
        
        if(length(pmn)>sum(pmn))
        {
          
          while(length(pmn)>sum(pmn))
          {
            iter2=iter2+iter1;
            if(iter2>100000)
            {
              break
            }
            print(iter2)
            fits<-fevd(data[,point],data,location.fun =~as.matrix(cov),method=c("Bayesian"),initial=list(fitss$results$par), iter=iter2) 
            output<-heidel.diag(fits$results,eps=0.5,pvalue = 0.05)
            pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
            print(pmn)
            
            
            
          }
          
        }
        else
        {
          output<-heidel.diag(fits$results,eps=0.05,pvalue = 0.05)
          pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
          print(pmn)
          
        }
        v <- make.qcov(fits, val1)
      }
      
      
      else
      {
        print(3)
        #REST ALL COMBINATION                                                   
        m<-best_combo[index,point]
        lo<-combination[m,1:3]
        lo<- lo[!is.na(lo)]
        lo_comb<-comb_id[lo]
        cov_lo<-data[,lo_comb]
        sh<-combination[m,4:6]
        sh<- sh[!is.na(sh)]
        sh_comb<-comb_id[sh]
        cov_sh<-data[,sh_comb]
        
        final_cov<-cbind(cov_lo,cov_sh)
        lo_name1<-c('mu1','mu2','mu3')
        lo_name<-lo_name1[1:length(lo)]
        sh_name1<-c('phi1','phi2','phi3')
        sh_name<-sh_name1[1:length(sh)] 
        final_name<-c(lo_name,sh_name)
        df<-data.frame(final_cov)
        df<-setNames(df,final_name)
        
        #converting into list
        
        val<-list(df)[[1]]
        
        
        
        fitss<-fevd(data[,point],data,location.fun =~as.matrix(cov_lo),scale.fun =~as.matrix(cov_sh))
        fits<-fevd(data[,point],data,location.fun =~as.matrix(cov_lo),scale.fun =~as.matrix(cov_sh),method = c("Bayesian"),initial=list(fitss$results$par),iter=iter1) #<====== TO BE CORRECTED
        output<-heidel.diag(fits$results,eps=0.05,pvalue = 0.05)
        pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
        
        pmn<-output[,4][!is.na(output[,4])]
        
        if(length(pmn)>sum(pmn))
          
        {
          while(length(pmn)>sum(pmn))
          {
            iter2=iter2+iter1
            if(iter2>100000)
            {
              break
            }
            
            print(iter2)
            fits<-fevd(data[,point],data,location.fun =~as.matrix(cov_lo),scale.fun =~as.matrix(cov_sh),method = c("Bayesian"), initial=list(fitss$results$par),iter=iter2) #<====== TO BE CORRECTED
            
            output<-heidel.diag(fits$results,eps=0.5,pvalue = 0.05)
            pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
            print(pmn)
            
            
          }
          
          
        }
        else
        {
          output<-heidel.diag(fits$results,eps=0.5,pvalue = 0.05)
          pmn<-output[1:(nrow(output)-1),4][!is.na(output[1:(nrow(output)-1),4])]
          print(pmn)
          
          
        }
        v <- make.qcov(fits, val)
      }
      
      
      
      
      
    }
    
    #FINDING BAYESIAN RETURN LEVEL WITH UNCERTAINITIES------------------------------------
    file123<-paste('prec_RL_B_pt',point,'_ind_',index,'.RData')
    #   write.csv(final_res5,file123)
    #   rm(final_res5,final5)
    
    a2=findAllMCMCpars(fits,qcov=v)
    p1<-round(0.2*nrow(a2))
    p2<-nrow(a2)
    write.csv(a2[p1:p2,],file123)
    data_1<-a2[p1:p2,]
    save(data_1, file=file123)
    rm(a2,data_1,fits,iter2,iter1)
  }
  
}




