
#calculates index score
ERBOne=function(DIABrkpt,MIC,DIA,MICBrkpt,VM,M){

  temp=max(VM,M)
  VMWeight=temp/VM; MWeight=temp/M

  ###outside one intermediate range
  NumSS=0; NumVM=0; NumRR=0; NumM=0
  n=length(MIC)
  for (i in 1:n){
    #SS
    if(MIC[i]<=MICBrkpt & DIA[i]>=DIABrkpt) NumSS=NumSS+1
    #resistant
    else if(MIC[i]>=MICBrkpt & DIA[i]<=DIABrkpt) NumRR=NumRR+1
    #R MIC S DIA
    else if(MIC[i]>=MICBrkpt & DIA[i]>=DIABrkpt) NumVM=NumVM+1
    #S MIC R DIA
    else if(MIC[i]<=MICBrkpt & DIA[i]<=DIABrkpt) NumM=NumM+1
  }

  index=VMWeight*NumVM/n+MWeight*NumM/n
  CorrectCount=sum(NumSS,NumRR)
  CorrectPerc=CorrectCount/n*100
  VMPerc=NumVM/n*100
  MPerc=NumM/n*100

  return(list(idx=index,CorrectPerc=CorrectPerc,VMPerc=VMPerc,MPerc=MPerc,
              CorrectCount=CorrectCount,VMCount=NumVM,MCount=NumM))
}


#finds optimum DIA given breakpoints M1 and M2 error rate bounded method
findBrkptsERBOne=function(MIC,DIA,VM=1,M=5,MICBrkpt){

  #find optimal
  parms=findBrkptsERBOneC(MIC,DIA,VM,M,MICBrkpt)
  DIABrkpt=parms$DIABrkpt

  #find information for plotting and display information
  N=length(MIC)

  cat('Optimal DIA Breakpoint for ERB:',DIABrkpt,'\n')
  cat('Number of Isolates: ',N,'\n')
  temp=matrix(nrow=1,ncol=3)
  parms=ERBOne(DIABrkpt,MIC,DIA,MICBrkpt,VM,M)
  cat('Index Score = ',parms$idx,'\n \n')
  cat('Count (%) \n')
  temp[1,1:3]=c(paste(parms$CorrectCount,' (',round(parms$CorrectPerc,digits=2),')',sep=''),
                paste(parms$VMCount,' (',round(parms$VMPerc,digits=2),')',sep=''),
                paste(parms$MCount,' (',round(parms$MPerc,digits=2),')',sep=''))
  temp=as.data.frame(temp)
  names(temp)=c('Agree','Very Major','Major')
  name.width <- max(sapply(names(temp), nchar))
  names(temp) <- format(names(temp), width = name.width, justify = "centre")
  print(format(temp, width = name.width, justify = "centre"),row.names=FALSE,quote=FALSE)
  if(parms$idx==0) cat('\n \n NOTE: When the index score equals 0, there can be a range of optimal DIA breakpoints.  We present the smallest breakpoint in this range.')
  return(list(DIABrkpt=DIABrkpt))
}



ERBGivenDIAOne=function(MIC,DIA,xcens,ycens,MICBrkpt,DIABrkpt,VM=1,M=5){


  N=length(MIC)

  cat('Optimal DIA Breakpoint for ERB:',DIABrkpt,'\n')
  cat('Number of Isolates: ',N,'\n')
  temp=matrix(nrow=1,ncol=3)
  parms=ERBOne(DIABrkpt,MIC,DIA,MICBrkpt,VM,M)
  cat('Index Score = ',parms$idx,'\n \n')
  cat('Count (%) \n')
  temp[1,1:3]=c(paste(parms$CorrectCount,' (',round(parms$CorrectPerc,digits=2),')',sep=''),
                paste(parms$VMCount,' (',round(parms$VMPerc,digits=2),')',sep=''),
                paste(parms$MCount,' (',round(parms$MPerc,digits=2),')',sep=''))
  temp=as.data.frame(temp)
  names(temp)=c('Agree','Very Major','Major')
  name.width <- max(sapply(names(temp), nchar))
  names(temp) <- format(names(temp), width = name.width, justify = "centre")
  print(format(temp, width = name.width, justify = "centre"),row.names=FALSE,quote=FALSE)
  invisible()
}


PlotBrkptsERB2One=function(MIC,DIA,xcens,ycens,VM,M,MICBrkpt,MICXaxis,log2MIC){


  parms=findBrkptsERBOneC(MIC,DIA,VM,M,MICBrkpt)
  DIABrkpt=parms$DIABrkpt

  fit=plotBrkPtsERBOne(MIC,DIA,xcens,ycens,MICBrkpt,DIABrkpt,MICXaxis,log2MIC)
  return(fit)
}

PlotBrkptsERBGivenOne=function(MIC,DIA,xcens,ycens,VM,M,MICBrkpt,DIABrkpt,MICXaxis,log2MIC){


  fit=plotBrkPtsERBOne(MIC,DIA,xcens,ycens,MICBrkpt,DIABrkpt,MICXaxis,log2MIC)

  return(fit)
}


#plot single scatterplot
plotBrkPtsERBOne=function(MIC,DIA,xcens,ycens,MICBrkpt,DIABrkpt,MICXaxis,log2MIC){

  MIC1=MIC
  DIA1=DIA
  MIC[xcens==1 & MIC==max(MIC)]=max(MIC)+1
  MIC[xcens==-1 & MIC==min(MIC)]=min(MIC)-1
  DIA[ycens==1 & DIA==max(DIA)]=max(DIA)+1
  DIA[ycens==-1 & DIA==min(DIA)]=min(DIA)-1

  n=length(MIC); classification=rep(NA,n)
  for (i in 1:n){
    #SS
    if(MIC[i]<=MICBrkpt & DIA[i]>=DIABrkpt) classification[i]='Correct'
    #resistant
    else if(MIC[i]>=MICBrkpt & DIA[i]<=DIABrkpt) classification[i]='Correct'
    #R MIC S DIA
    else if(MIC[i]>=MICBrkpt & DIA[i]>=DIABrkpt) classification[i]='Very Major'
    #S MIC R DIA
    else if(MIC[i]<=MICBrkpt & DIA[i]<=DIABrkpt) classification[i]='Major'
  }
  a1=data.frame(MIC,DIA,classification,stringsAsFactors=FALSE)
  a1 = a1 %>% group_by(MIC,DIA,classification) %>% summarize(Freq=n())
  a1$classification=factor(a1$classification,levels=c("Correct","Major","Very Major"))

  MICBrkpt=MICBrkpt+.5
  DIABrkpt=DIABrkpt+.5

  if(log2MIC==TRUE){
    MICBrkpt=2^MICBrkpt
    MIC2=MIC1
    a1$MIC=2^a1$MIC
    MICTemp=c(min(MIC2)-1,min(MIC2):max(MIC2),max(MIC2)+1)
    MICTemp=2^MICTemp
    x=2^(min(MIC2):max(MIC2))
  }

  if(MICXaxis==TRUE && log2MIC==FALSE){
    fit=ggplot(a1,aes(MIC,DIA))+geom_text(aes(label=Freq,color=factor(classification)),size=4)+
      geom_point(aes(group=factor(classification),color=factor(classification)),size=0)+
      geom_vline(xintercept=MICBrkpt,lty=2,alpha=.4)+
      geom_hline(yintercept=DIABrkpt,lty=2,alpha=.4)+
      labs(x='MIC (Dilution Test in log(ug/mL))',y='DIA (Diffusion Test in mm)')+
      scale_x_continuous(breaks = seq(min(MIC1)-1,max(MIC1)+1,by=1),
                         labels = c(paste("<",min(MIC1),sep=''),seq(min(MIC1),max(MIC1),by=1), paste(">",max(MIC1),sep='')),
                         limits = c(min(MIC1)-1,max(MIC1)+1))+
      scale_y_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_color_manual(values=c('Correct'='Black','Major'='#CC9900','Very Major'='red'))+theme_dbets()+
      theme(
        legend.position='top',
        legend.title=element_blank(),
        legend.key=element_rect(fill="gray95",colour="white"),
        legend.text = element_text(size = 15))+
      guides(colour = guide_legend(override.aes = list(size=5,alpha = 1)))
  }
  if(MICXaxis==TRUE && log2MIC==TRUE){


    fit=ggplot(a1,aes(MIC,DIA))+geom_text(aes(label=Freq,color=factor(classification)),size=4)+
      geom_point(aes(group=factor(classification),color=factor(classification)),size=0)+
      geom_vline(xintercept=MICBrkpt,lty=2,alpha=.4)+
      geom_hline(yintercept=DIABrkpt,lty=2,alpha=.4)+
      labs(x='MIC (Dilution Test in ug/mL)',y='DIA (Diffusion Test in mm)')+
      scale_x_continuous(trans=log2_trans(),
                         limits=c(min(MICTemp),max(MICTemp)),
                         breaks=MICTemp,
                         labels=c(paste("<",min(x),sep=''),sort(unique(x)), paste(">",max(x),sep='')))+
      scale_y_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_color_manual(values=c('Correct'='Black','Major'='#CC9900','Very Major'='red'))+theme_dbets()+
      theme(
        legend.position='top',
        legend.title=element_blank(),
        legend.key=element_rect(fill="gray95",colour="white"),
        legend.text = element_text(size = 14))+
      guides(colour = guide_legend(override.aes = list(size=3,alpha = 1)))
  }
  if(MICXaxis==FALSE && log2MIC==FALSE){
    fit=ggplot(a1,aes(DIA,MIC))+geom_text(aes(label=Freq,color=factor(classification)),size=4)+
      geom_point(aes(group=factor(classification),color=factor(classification)),size=0)+
      geom_hline(yintercept=MICBrkpt,lty=2,alpha=.4)+
      geom_vline(xintercept=DIABrkpt,lty=2,alpha=.4)+
      labs(y='MIC (Dilution Test in log(ug/mL))',x='DIA (Diffusion Test in mm)')+
      scale_y_continuous(breaks = seq(min(MIC1)-1,max(MIC1)+1,by=1),
                         labels = c(paste("<",min(MIC1),sep=''),seq(min(MIC1),max(MIC1),by=1), paste(">",max(MIC1),sep='')),
                         limits = c(min(MIC1)-1,max(MIC1)+1))+
      scale_x_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_color_manual(values=c('Correct'='Black','Major'='#CC9900','Very Major'='red'))+theme_dbets()+
      theme(
        legend.position='top',
        legend.title=element_blank(),
        legend.key=element_rect(fill="gray95",colour="white"),
        legend.text = element_text(size = 14))+
      guides(colour = guide_legend(override.aes = list(size=3,alpha = 1)))
  }
  if(MICXaxis==FALSE && log2MIC==TRUE){

    fit=ggplot(a1,aes(DIA,MIC))+geom_text(aes(label=Freq,color=factor(classification)),size=4)+
      geom_point(aes(group=factor(classification),color=factor(classification)),size=0)+
      geom_hline(yintercept=MICBrkpt,lty=2,alpha=.4)+
      geom_vline(xintercept=DIABrkpt,lty=2,alpha=.4)+
      labs(y='MIC (Dilution Test in ug/mL)',x='DIA (Diffusion Test in mm)')+
      scale_y_continuous(trans=log2_trans(),
                         limits=c(min(MICTemp),max(MICTemp)),
                         breaks=MICTemp,
                         labels=c(paste("<",min(x),sep=''),sort(unique(x)), paste(">",max(x),sep='')))+
      scale_x_continuous(breaks = seq(min(DIA1)-1,max(DIA1)+1,by=1),
                         labels = c(paste("<",min(DIA1),sep=''),seq(min(DIA1),max(DIA1),by=1), paste(">",max(DIA1),sep='')),
                         limits = c(min(DIA1)-1,max(DIA1)+1))+
      scale_color_manual(values=c('Correct'='Black','Major'='#CC9900','Very Major'='red'))+theme_dbets()+
      theme(
        legend.position='top',
        legend.title=element_blank(),
        legend.key=element_rect(fill="gray95",colour="white"),
        legend.text = element_text(size = 14))+
      guides(colour = guide_legend(override.aes = list(size=3,alpha = 1)))


  }

  return(fit)

}


bootStrapERBOne=function(MIC,DIA,MICBrkpt,VM,M,session){

  #bootstrap
  DIABrkpt<<-rep(NA,12000)
  n=length(MIC)

  withProgress(session, min=1, max=12000, {
    setProgress(message = 'Calculation in progress')
    for(i in 1:12000){
      #resample points
      if(i %% 1000==0) setProgress(value = i)
      idx=sample(seq(1,n,by=1),n,replace=T)
      MIC_sam=MIC[idx]
      DIA_sam=DIA[idx]

      #get weighted breakpoints
      parms=findBrkptsERBOneC(MIC_sam,DIA_sam,VM,M,MICBrkpt)
      DIABrkpt[i] <<- parms$DIABrkpt+.5
    }
  })


  #print results
  tab=table(DIABrkpt)
  a1=tab/sum(tab)*100
  a1=as.data.frame(a1)
  a1=a1[order(a1$Freq,decreasing=T),]
  a1=a1[a1$Freq!=0,]
  a2=a1
  a2$CumFreq=cumsum(a2$Freq)
  a2[,2]=round(a2[,2],2)
  a2[,3]=round(a2[,3],2)

  cat('Bootstrap samples = 12000 \n')
  cat('\n-------DIA Breakpoints by Confidence--------\n')
  temp=data.frame(DIABrkpt=a2[,1],Percent=a2[,2],Cumulative=a2[,3])
  temp[,1:3] = apply(temp[,1:3], 2, function(x) as.character(x));
  name.width <- max(sapply(names(temp), nchar))
  names(temp) <- format(names(temp), width = name.width, justify = "centre")
  print(format(temp, width = name.width, justify = "centre"),row.names=FALSE,quote=FALSE)

  invisible()

}







