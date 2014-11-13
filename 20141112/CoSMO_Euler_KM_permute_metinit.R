#install libraries for plotting
install.packages('gplots',repos='http://cran.fhcrc.org/')
library('gplots')


#move to working dir where files will be stored
setwd('/fh/fast/shou_w/shougroup/lab_users/Robin/Notebook/CoSMO Auxotrophs/Modeling/2014-11-11_CoSMO_Met_4_moreMetNeeded/')
#setwd('/fh/fast/shou_w/shougroup/lab_users/Robin/Notebook/CoSMO Auxotrophs/Modeling/2014-10-30_CoSMO_Met_3/')


#function called in each situtation
CoSMOMet<-function(fit_adv=1,km_adv=1,rel_more=1,alphaM=0.01)
{
  
  Rgrow=0.49
  Rdeath=0.054
  Ggrow=0.5
  Gdeath=0.018
  Bgrow=0.49
  Bdeath=0.054
  KL=1.0
  KA=0.1
  KM=0.5
  alphaL=2e-6
  alphaA=0.5e-6
  betaL=12.5e-6
  gammaA=1.5e-7
  #alphaM=.5e-8
  betaM=5e-8
  R=.5*(0.03*7e7*2)
  G=.5*(0.03*7e7*2)
  A=0
  L=0
  B=0
  M=0
  dt=0.1
  
  
  
  KM=KM*km_adv
  Bgrow=Bgrow*fit_adv
  betaM=betaM*rel_more
  outvector<-c(0,R,G,L,A,B,M)
  alphaM<-(((alphaM/1e15)*1e6)/(2/1000))
  
  for(i in 1:12000)
  {
    
    if(i==2000)
    {
      B=1
      
      
    }
    
    dROVERdt=Rgrow*(L/(KL+L))*R-Rdeath*R #[L-A+]
    dGOVERdt=Ggrow*(A/(KA+A))*G-Gdeath*G #[L+A-]
    dBOVERdt=Bgrow*((L/(KL+L)))*(M/(KM+M))*B-Bdeath*B #[L-A+M-]
    
    dLOVERdt=(((-1*alphaL*Rgrow*(L/(KL+L))*R)+(-1*alphaL*Bgrow*((L/(KL+L)))*(M/(KM+M))*B))+(betaL*Gdeath*G))
    dAOVERdt=(((-1*alphaA*Ggrow*(A/(KA+A))*G)+gammaA*R+gammaA*B))
    dMOVERdt=((-1*alphaM*Bgrow*((L/(KL+L)))*(M/(KM+M))*B)+betaM*R) #assuming only R is releasing the majority of methionine
    
    #print(A)
    
    
    
    if(i<2000)
    {
      R_new<-R+(dROVERdt*dt)
      G_new<-G+(dGOVERdt*dt)
      
      
      A_new<-A+(dAOVERdt*dt)
      L_new<-L+(dLOVERdt*dt)
      M_new<-M+(dMOVERdt*dt)
      
      R<-R_new
      G<-G_new
      A<-A_new
      L<-L_new
      M<-M_new
      
      
      temp<-c(i*dt,R,G,A,L,B,M)
      outvector<-rbind(outvector,temp)
      
    }
    
    else
    {
      R_new<-R+(dROVERdt*dt)
      G_new<-G+(dGOVERdt*dt)
      B_new<-B+(dBOVERdt*dt)
      
      
      A_new<-A+(dAOVERdt*dt)
      L_new<-L+(dLOVERdt*dt)
      M_new<-M+(dMOVERdt*dt)
      
      R<-R_new
      G<-G_new
      A<-A_new
      L<-L_new
      M<-M_new
      B<-B_new
      
      
      temp<-c(i*dt,R,G,A,L,B,M)
      outvector<-rbind(outvector,temp)
     
      
      
      
    }
    
    if(R+G+B>(.6*3.5e7*2))
    {
      
      #print('herp')
      R=R/(20)
      G=G/(20)
      B=B/(20)
      A=A/20
      L=L/20
      M=M/20
      #cat(B,'\t',M,'\t',alphaM,'\n')
      
      if(B<0.1)
      {
        B=0
        
      }
      
      
    }
    
  }
  
  frac<-(outvector[nrow(outvector),6]/(outvector[nrow(outvector),2]+outvector[nrow(outvector),6])*100)
  #frac<-1.0
  
  if(frac>1)
  {
    nam<-paste('Fit',fit_adv,'KM',km_adv,sep='_')
    print(nam)
    
    png(paste(nam,'png',sep='.'))
    matplot(x=outvector[,1],y=outvector[,c(2,3,6)],pch=20,xlab='Hours',ylab='Cells/mL',
            main=nam,col=c('darkred','darkgreen','darkblue'))
    legend(x='topleft',legend=c('Red','Green','Blue'),col=c('darkred','darkgreen','darkblue'),lwd=5)
    
    
    dev.off()
    
  }
  #else
  #{
    #print('Met Pop less that 1%')
    #continue 
    
    
    
  #}
  
 # print(head(outvector))
  return(outvector[nrow(outvector),6]/(outvector[nrow(outvector),2]+outvector[nrow(outvector),6])*100)
 
  
}

args<-commandArgs(TRUE)
print(args)

dir=args[3]
dir<-paste(dir,'/',sep='')
alphaM=as.numeric(as.character(args[1]))
print(alphaM)
print(alphaM+1)





km_range<-c(1,0.99,0.95,0.9,0.8,0.7,0.6,0.5, 0.4,0.3,0.25,0.2,0.15,0.1,0.05,0.01)
fitness_range<-c(1,1.01,1.02,1.03,1.04,1.05,1.1,1.15,1.2,1.25,1.3,1.35,1.4,1.45,1.5)
rel_range<-c(1,1.2,1.3,1.4,1.5,2,3,4,5,100)



freq_mat<-matrix(data=NA,nrow=length(km_range),ncol=length(fitness_range))
colnames(freq_mat)<-fitness_range[1:length(fitness_range)]
rownames(freq_mat)<-km_range[1:length(km_range)]

setwd(dir)
for(i in 1:length(km_range))
{
  for(j in 1:length(fitness_range))
  {
    
    
    fit_adv=fitness_range[j]
    km_adv=km_range[i]
    rel_more=1
    num<-CoSMOMet(fit_adv=fitness_range[j],km_adv=km_range[i],rel_more=rel_more,alphaM=alphaM)
    freq_mat[i,j]=num
  
  }
}


save.image('KM_permute_euler_met200.RData')
png(filename='KM_permute_euler_met200.png')
heatmap.2(freq_mat,col=bluered(500),dendrogram='none',Rowv=FALSE,Colv=FALSE,xlab='Fitness Advantage',ylab='Km Improvement Factor',notecol='black',trace='none',cellnote=round(freq_mat,digits=1),notecex=.75)


