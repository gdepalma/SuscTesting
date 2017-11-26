
basicPlotOne=function(MIC,DIA,xcens,ycens,MICBrkpt,MICXaxis,log2MIC){

  MIC1=MIC
  DIA1=DIA
  MIC[xcens==1 & MIC==max(MIC)]=max(MIC)+1
  MIC[xcens==-1 & MIC==min(MIC)]=min(MIC)-1
  DIA[ycens==1 & DIA==max(DIA)]=max(DIA)+1
  DIA[ycens==-1 & DIA==min(DIA)]=min(DIA)-1
  a1=data.frame(table(MIC,DIA))
  a1$MIC=as.numeric(as.character(a1$MIC))
  a1$DIA=as.numeric(as.character(a1$DIA))
  a1=a1[a1$Freq>0,]
  M1=MICBrkpt+.5

  ### For log_2 graphs
  if(log2MIC==TRUE){
    M1=2^(MICBrkpt+0.5)
    a1$MIC=2^a1$MIC
    MICTemp=c(min(MIC1)-1,min(MIC1):max(MIC1),max(MIC1)+1)
    MICTemp=2^MICTemp
    x=2^(min(MIC1):max(MIC1))
  }


  if(MICXaxis==TRUE && log2MIC==FALSE){
    fit=ggplot(a1,aes(MIC,DIA))+geom_text(aes(label=Freq),size=4)+
      geom_vline(xintercept=M1,lty=2,alpha=.5)+
      labs(x='MIC (Dilution Test in log(ug/mL))',y='DIA (Diffusion Test in mm)')+
      scale_x_continuous(breaks = seq(min(MIC1)-1,max(MIC1)+1,by=1),
                         labels = c(paste("<",min(MIC1),sep=''),seq(min(MIC1),max(MIC1),by=1), paste(">",max(MIC1),sep='')),
                         limits = c(min(MIC1)-1,max(MIC1)+1))+
      scale_y_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      theme_dbets()
  }
  if(MICXaxis==TRUE && log2MIC==TRUE){
    fit=ggplot(a1,aes(MIC,DIA))+geom_text(aes(label=Freq),size=4)+
      geom_vline(xintercept=M1,lty=2,alpha=.5)+
      labs(x='MIC (Dilution Test in ug/mL)',y='DIA (Diffusion Test in mm)')+
      scale_y_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_x_continuous(trans=log2_trans(),
                         limits=c(min(MICTemp),max(MICTemp)),
                         breaks=MICTemp,
                         labels=c(paste("<",min(x),sep=''),sort(unique(x)), paste(">",max(x),sep='')))+
      theme_dbets()
  }
  if(MICXaxis==FALSE && log2MIC==FALSE){
    fit=ggplot(a1,aes(DIA,MIC))+geom_text(aes(label=Freq),size=4)+
      geom_hline(yintercept=M1,lty=2,alpha=.5)+
      labs(y='MIC (Dilution Test in log(ug/mL))',x='DIA (Diffusion Test in mm)')+
      scale_y_continuous(breaks = seq(min(MIC1)-1,max(MIC1)+1,by=1),
                         labels = c(paste("<",min(MIC1),sep=''),seq(min(MIC1),max(MIC1),by=1), paste(">",max(MIC1),sep='')),
                         limits = c(min(MIC1)-1,max(MIC1)+1))+
      scale_x_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      theme_dbets()
  }
  if(MICXaxis==FALSE && log2MIC==TRUE){

    fit=ggplot(a1,aes(DIA,MIC))+geom_text(aes(label=Freq),size=4)+
      geom_hline(yintercept=M1,lty=2,alpha=.5)+
      labs(y='MIC (Dilution Test in ug/mL)',x='DIA (Diffusion Test in mm)')+
      scale_x_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_y_continuous(trans=log2_trans(),
                         limits=c(min(MICTemp),max(MICTemp)),
                         breaks=MICTemp,
                         labels=c(paste("<",min(x),sep=''),sort(unique(x)), paste(">",max(x),sep='')))+
      theme_dbets()
  }

  print(fit)
  invisible()

}


basicPlot=function(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU,MICXaxis,log2MIC){

  MICBrkptL=MICBrkptL+.5
  MICBrkptU=MICBrkptU-.5

  MIC1=MIC
  DIA1=DIA
  MIC[xcens==1 & MIC==max(MIC)]=max(MIC)+1
  MIC[xcens==-1 & MIC==min(MIC)]=min(MIC)-1
  DIA[ycens==1 & DIA==max(DIA)]=max(DIA)+1
  DIA[ycens==-1 & DIA==min(DIA)]=min(DIA)-1
  a1=data.frame(table(MIC,DIA))
  a1$MIC=as.numeric(as.character(a1$MIC))
  a1$DIA=as.numeric(as.character(a1$DIA))
  a1=a1[a1$Freq>0,]

  ### For log_2 graphs
  if(log2MIC==TRUE){
    MICBrkptL=2^MICBrkptL
    MICBrkptU=2^MICBrkptU
    a1$MIC=2^a1$MIC
    MICTemp=c(min(MIC1)-1,min(MIC1):max(MIC1),max(MIC1)+1)
    MICTemp=2^MICTemp
    x=2^(min(MIC1):max(MIC1))
  }

  if(MICXaxis==TRUE && log2MIC==FALSE){
    fit=ggplot(a1,aes(MIC,DIA))+geom_text(aes(label=Freq),size=4)+
      geom_vline(xintercept=MICBrkptL,lty=2,alpha=.5)+
      geom_vline(xintercept=MICBrkptU,lty=2,alpha=.5)+
      labs(x='MIC (Dilution Test in log(ug/mL))',y='DIA (Diffusion Test in mm)')+
      scale_x_continuous(breaks = seq(min(MIC1)-1,max(MIC1)+1,by=1),
                         labels = c(paste("<",min(MIC1),sep=''),seq(min(MIC1),max(MIC1),by=1), paste(">",max(MIC1),sep='')),
                         limits = c(min(MIC1)-1,max(MIC1)+1))+
      scale_y_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      theme_dbets()

  }
  if(MICXaxis==TRUE && log2MIC==TRUE){

    fit=ggplot(a1,aes(MIC,DIA))+geom_text(aes(label=Freq),size=4)+
      geom_vline(xintercept=MICBrkptL,lty=2,alpha=.5)+
      geom_vline(xintercept=MICBrkptU,lty=2,alpha=.5)+
      labs(x='MIC (Dilution Test in ug/mL)',y='DIA (Diffusion Test in mm)')+
      scale_y_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_x_continuous(trans=log2_trans(),
                         limits=c(min(MICTemp),max(MICTemp)),
                         breaks=MICTemp,
                         labels=c(paste("<",min(x),sep=''),sort(unique(x)), paste(">",max(x),sep='')))+
      theme_dbets()
  }
  if(MICXaxis==FALSE && log2MIC==FALSE){
    fit=ggplot(a1,aes(DIA,MIC))+geom_text(aes(label=Freq),size=4)+
      geom_hline(yintercept=MICBrkptL,lty=2,alpha=.5)+
      geom_hline(yintercept=MICBrkptU,lty=2,alpha=.5)+
      labs(y='MIC (Dilution Test in log(ug/mL))',x='DIA (Diffusion Test in mm)')+
      scale_y_continuous(breaks = seq(min(MIC1)-1,max(MIC1)+1,by=1),
                         labels = c(paste("<",min(MIC1),sep=''),seq(min(MIC1),max(MIC1),by=1), paste(">",max(MIC1),sep='')),
                         limits = c(min(MIC1)-1,max(MIC1)+1))+
      scale_x_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      theme_dbets()
  }
  if(MICXaxis==FALSE && log2MIC==TRUE){

    fit=ggplot(a1,aes(DIA,MIC))+geom_text(aes(label=Freq),size=4)+
      geom_hline(yintercept=MICBrkptL,lty=2,alpha=.5)+
      geom_hline(yintercept=MICBrkptU,lty=2,alpha=.5)+
      labs(y='MIC (Dilution Test in ug/mL)',x='DIA (Diffusion Test in mm)')+
      scale_x_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_y_continuous(trans=log2_trans(),
                         limits=c(min(MICTemp),max(MICTemp)),
                         breaks=MICTemp,
                         labels=c(paste("<",min(x),sep=''),sort(unique(x)), paste(">",max(x),sep='')))+
      theme_dbets()
  }

  plot(fit)
  invisible()
}

descriptiveStat=function(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU){


  #find information for plotting and display information
  N=length(MIC)
  withinOne=rep(0,length(MIC))
  withinOne[which(MIC>=MICBrkptL & MIC<=MICBrkptU)]=1
  numwithinOne=sum(withinOne)

  cat('--------------- Data Set Characteristics ---------------\n \n')
  cat('Number of Isolates: ',N,'\n')
  cat('Number of Observed Outside One of Intermediate Range: ',N-numwithinOne,'\n')
  cat('Number of Observed Within One of Intermediate Range: ',numwithinOne,'\n')
  cat('Number of MIC censored: ',sum(xcens!=0),'\n')
  cat('Number of DIA censored: ',sum(ycens!=0),'\n')
  invisible()
}

descriptiveStatOne=function(MIC,DIA,xcens,ycens,MICBrkpt){


  #find information for plotting and display information
  N=length(MIC)

  cat('--------------- Data Set Characteristics ---------------\n \n')
  cat('Number of Isolates: ',N,'\n')
  cat('Number Susceptible: ',sum(MIC<MICBrkpt+0.5),'\n')
  cat('Number Resistant: ',sum(MIC>MICBrkpt+0.5),'\n')
  cat('Number of MIC censored: ',sum(xcens!=0),'\n')
  cat('Number of DIA censored: ',sum(ycens!=0),'\n')
  invisible()
}
