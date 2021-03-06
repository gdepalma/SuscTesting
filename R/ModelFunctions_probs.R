

probDIAClass=function(data,MICBrkptL,MICBrkptU,DIASet1,DIASet2){

  f=data$fit
  density=data$dens

  e=sqrt(.5^2+.5^2)
  t=sqrt(1.5^2+1.5^2)

  grid=xgrid[xgrid>=min(MIC) & xgrid<=max(MIC)]
  weights=density[xgrid>=min(MIC) & xgrid<=max(MIC)]
  f=f[xgrid>=min(MIC) & xgrid<=max(MIC)]
  weights=weights/sum(weights)


  M1=MICBrkptL-.5
  M2=MICBrkptU-.5
  D1=DIASet1[1];  D2=DIASet1[2]
  if(is.na(DIASet2[1])==TRUE){
    D11=0; D22=0
  }else{
    D11=DIASet2[1];  D22=DIASet2[2]
  }

  probDIAC=rep(NA,length(grid))
  probDIAIC=rep(NA,length(grid))
  probDIAC1=rep(NA,length(grid))
  probDIAIC1=rep(NA,length(grid))
  probMIC=rep(NA,length(grid))

  # print(c(D1,D2))

  ### DIA Probs
  for(i in 1:length(grid)){
    d=f[i]
    m=grid[i]

    ### MIC
    #Susceptible
    if (m<=M1)  probMIC[i]=pnorm(MICBrkptL,m,e)
    #intermediate
    if(m>M1 & m<M2) probMIC[i]=pnorm(MICBrkptU-1,m,e)-pnorm(MICBrkptL,m,e)
    #resistant
    if(m>=M2) probMIC[i]=1-pnorm(MICBrkptU-1,m,e)

    ###DIA
    #Susceptible
    if(m<=M1){
      probDIAC[i]=1-pnorm(D2-.5,d,t)
      probDIAC1[i]=1-pnorm(D22-.5,d,t)
    #intermediate
    }else if(m>M1 && m<M2){
      probDIAC[i]=pnorm(D2-.5,d,t)-pnorm(D1+.5,d,t)
      probDIAC1[i]=pnorm(D22-.5,d,t)-pnorm(D11+.5,d,t)
    #Resistant
    }else{
      probDIAC[i]=pnorm(D1+.5,d,t)
      probDIAC1[i]=pnorm(D11+.5,d,t)
    }
    probDIAIC[i]=1-probDIAC[i]
    probDIAIC1[i]=1-probDIAC1[i]
  }

  ### Print DIA
  if(is.na(DIASet2[1])==FALSE){
    cat('DIA Breakpoints \n')
    cat('Set 1: ',DIASet1[1],', ',DIASet1[2],'\n',sep='')
    cat('Set 2: ',DIASet2[1],', ',DIASet2[2],'\n',sep='')
	  CorrectM=sum(probMIC)/length(grid)
    Correct1=sum(probDIAC)/length(grid)
    Correct2=sum(probDIAC1)/length(grid)
    cat('\nProbability Correct Classification \n')
    temp=data.frame(CorrectM,Correct1,Correct2)
    temp[,1:3]=round(temp[,1:3],digits=3)
    temp[,1:3]=as.character(temp[,1:3])
    names(temp)=c('MIC Correct','DIA 1 Correct','DIA 2 Correct')
    name.width <- max(sapply(names(temp), nchar))
    names(temp) <- format(names(temp), width = name.width, justify = "centre")
    print(format(temp, width = name.width, justify = "centre"),row.names=FALSE,quote=FALSE)

	  CorrectM=sum(probMIC*weights)
    Correct1=sum(probDIAC*weights)
    Correct2=sum(probDIAC1*weights)
    cat('\nProbability Correct Classification Weighted by Isolate Distribution \n')
    temp=data.frame(CorrectM,Correct1,Correct2)
    temp[,1:3]=round(temp[,1:3],digits=4)
    temp[,1:3]=as.character(temp[,1:3])
    names(temp)=c('MIC Correct','DIA 1 Correct','DIA 2 Correct')
    name.width <- max(sapply(names(temp), nchar))
    names(temp) <- format(names(temp), width = name.width, justify = "centre")
    print(format(temp, width = name.width, justify = "centre"),row.names=FALSE,quote=FALSE)

    a1=data.frame(grid,probDIAC,probDIAC1,probMIC,weights)
  }else{
    cat('DIA Breakpoints: ',DIASet1[1],', ',DIASet1[2],'\n',sep='')
    Correct1=sum(probDIAC)/length(grid)
	  CorrectM=sum(probMIC)/length(grid)
    cat('\nProbability Correct Classification \n')
    temp=data.frame(CorrectM,Correct1)
    temp[,1:2]=round(temp[,1:2],digits=3)
    temp[,1:2]=as.character(temp[,1:2])
    names(temp)=c('MIC Correct','DIA Correct')
    name.width <- max(sapply(names(temp), nchar))
    names(temp) <- format(names(temp), width = name.width, justify = "centre")
    print(format(temp, width = name.width, justify = "centre"),row.names=FALSE,quote=FALSE)

    Correct1=sum(probDIAC*weights)
	  CorrectM=sum(probMIC*weights)
    cat('\nProbability Correct Classification Weighted by Isolate Distribution \n')
    temp=data.frame(CorrectM,Correct1)
    temp[,1:2]=round(temp[,1:2],digits=4)
    temp[,1:2]=as.character(temp[,1:2])
    names(temp)=c('MIC Correct','DIA Correct')
    name.width <- max(sapply(names(temp), nchar))
    names(temp) <- format(names(temp), width = name.width, justify = "centre")
    print(format(temp, width = name.width, justify = "centre"),row.names=FALSE,quote=FALSE)

    a1=data.frame(grid,probDIAC,probDIAC1,probMIC,weights)
  }

  return(a1)

}



plotProbDIAClass=function(a1,M1,M2,DIASet1,DIASet2,logConvert){


  M1=M1-.5
  M2=M2-.5

  xgrid=a1$grid
  probDIAC=a1$probDIAC
  if(is.na(DIASet2[1])==FALSE)
    probDIAC1=a1$probDIAC1
  probMIC=a1$probMIC


  ### Plot
  grid1=xgrid[xgrid<M1-.02]
  grid2=xgrid[xgrid>M1+.02 & xgrid<M2-.02]
  grid3=xgrid[xgrid>M2+.02]

  probMIC1=probMIC[xgrid<M1-.02]
  probMIC2=probMIC[xgrid>M1+.02 & xgrid<M2-.02]
  probMIC3=probMIC[xgrid>M2+.02]

  ProbCorrectDIA1=probDIAC[xgrid<M1-.02]
  ProbCorrectDIA2=probDIAC[xgrid>M1+.02 & xgrid<M2-.02]
  ProbCorrectDIA3=probDIAC[xgrid>M2+.02]

  if(is.na(DIASet2[1])==FALSE){
    ProbCorrectDIA11=probDIAC1[xgrid<M1-.02]
    ProbCorrectDIA21=probDIAC1[xgrid>M1+.02 & xgrid<M2-.02]
    ProbCorrectDIA31=probDIAC1[xgrid>M2+.02]
  }


  if(is.na(DIASet2[1])==FALSE){
    a1=data.frame(grid=c(grid1,grid1,grid1),probC=c(probMIC1,ProbCorrectDIA1,ProbCorrectDIA11),
                  group=c(rep(0,length(grid1)),rep(1,length(grid1)),rep(2,length(grid1))))
    names(a1)=c('grid','Correct','Breakpoints')
    a2=data.frame(grid=c(grid2,grid2,grid2),probC=c(probMIC2,ProbCorrectDIA2,ProbCorrectDIA21),
                  group=c(rep(0,length(grid2)),rep(1,length(grid2)),rep(2,length(grid2))))
    names(a2)=c('grid','Correct','Breakpoints')
    a3=data.frame(grid=c(grid3,grid3,grid3),probC=c(probMIC3,ProbCorrectDIA3,ProbCorrectDIA31),
                  group=c(rep(0,length(grid3)),rep(1,length(grid3)),rep(2,length(grid3))))
    names(a3)=c('grid','Correct','Breakpoints')
    a1=melt(a1,id=c(1,3))
    a1[a1$Breakpoints==0,2]=paste('MIC ')
    a1[a1$Breakpoints==1,2]=paste('DIA Set 1: ',DIASet1[1],', ',DIASet1[2],sep='')
    a1[a1$Breakpoints==2,2]=paste('DIA Set 2: ',DIASet2[1],', ',DIASet2[2],sep='')
    a2=melt(a2,id=c(1,3))
    a2[a2$Breakpoints==0,2]=paste('MIC ')
    a2[a2$Breakpoints==1,2]=paste('DIA Set 1: ',DIASet1[1],', ',DIASet1[2],sep='')
    a2[a2$Breakpoints==2,2]=paste('DIA Set 2: ',DIASet2[1],', ',DIASet2[2],sep='')
    a3=melt(a3,id=c(1,3))
    a3[a3$Breakpoints==0,2]=paste('MIC ')
    a3[a3$Breakpoints==1,2]=paste('DIA Set 1: ',DIASet1[1],', ',DIASet1[2],sep='')
    a3[a3$Breakpoints==2,2]=paste('DIA Set 2: ',DIASet2[1],', ',DIASet2[2],sep='')

    a1$Breakpoints=factor(a1$Breakpoints,levels=c("MIC ",paste('DIA Set 1: ',DIASet1[1],', ',DIASet1[2],sep=''),
          paste('DIA Set 2: ',DIASet2[1],', ',DIASet2[2],sep='')))
    a2$Breakpoints=factor(a2$Breakpoints,levels=c("MIC ",paste('DIA Set 1: ',DIASet1[1],', ',DIASet1[2],sep=''),
          paste('DIA Set 2: ',DIASet2[1],', ',DIASet2[2],sep='')))
    a3$Breakpoints=factor(a3$Breakpoints,levels=c("MIC ",paste('DIA Set 1: ',DIASet1[1],', ',DIASet1[2],sep=''),
          paste('DIA Set 2: ',DIASet2[1],', ',DIASet2[2],sep='')))

    if(logConvert==TRUE){
      fit=ggplot(a1,aes(x=grid,y=value,color=Breakpoints))+geom_line()+
        geom_line(data=a2,aes(x=grid,y=value,color=Breakpoints))+
        geom_line(data=a3,aes(x=grid,y=value,color=Breakpoints))+
        geom_vline(xintercept=M1,lty=2,alpha=.5)+
        geom_vline(xintercept=M2,lty=2,alpha=.5)+
        labs(x='MIC',y='Probability',title='Probability of Correct Classification (non-weighted)')+
        theme_dbets()+
        theme(
          legend.position='bottom',
          legend.text = element_text(size = 16),
          legend.title=element_text(size=15))+
        ylim(0,1)+
        scale_x_continuous(breaks = seq(round(min(xgrid)),round(max(xgrid)),by=1),limits=c(min(MIC),max(MIC)))


    }
    if(logConvert==FALSE){
      M1=2^(M1)
      M2=2^(M2)
      a1$grid=2^a1$grid
      a2$grid=2^a2$grid
      a3$grid=2^a3$grid
      MICTemp=min(MIC):max(MIC)
      MICTemp=2^MICTemp
      fit=ggplot(a1,aes(x=grid,y=value,color=Breakpoints))+geom_line()+
        geom_line(data=a2,aes(x=grid,y=value,color=Breakpoints))+
        geom_line(data=a3,aes(x=grid,y=value,color=Breakpoints))+
        geom_vline(xintercept=M1,lty=2,alpha=.5)+
        geom_vline(xintercept=M2,lty=2,alpha=.5)+
        labs(x='MIC',y='Probability',title='Probability of Correct Classification (non-weighted)')+
        ylim(0,1)+
        scale_x_continuous(trans=log2_trans(),limits=c(min(MICTemp),max(MICTemp)),breaks=MICTemp)
        theme_dbets()+
        theme(
          legend.position='bottom',
          legend.text = element_text(size = 16),
          legend.title=element_text(size=15))
    }
  }
  if(is.na(DIASet2[1])==TRUE){
    a1=data.frame(grid=c(grid1,grid1),probC=c(probMIC1,ProbCorrectDIA1),
                  group=c(rep(0,length(grid1)),rep(1,length(grid1))))
    names(a1)=c('grid','Correct','Breakpoints')
    a2=data.frame(grid=c(grid2,grid2),probC=c(probMIC2,ProbCorrectDIA2),
                  group=c(rep(0,length(grid2)),rep(1,length(grid2))))
    names(a2)=c('grid','Correct','Breakpoints')
    a3=data.frame(grid=c(grid3,grid3),probC=c(probMIC3,ProbCorrectDIA3),
                  group=c(rep(0,length(grid3)),rep(1,length(grid3))))
    names(a3)=c('grid','Correct','Breakpoints')
    a1=melt(a1,id=c(1,3))
    a1[a1$Breakpoints==1,2]=paste('DIA: ',DIASet1[1],', ',DIASet1[2],sep='')
    a1[a1$Breakpoints==0,2]=paste('MIC ')
    a2=melt(a2,id=c(1,3))
    a2[a2$Breakpoints==1,2]=paste('DIA: ',DIASet1[1],', ',DIASet1[2],sep='')
    a2[a2$Breakpoints==0,2]=paste('MIC ')
    a3=melt(a3,id=c(1,3))
    a3[a3$Breakpoints==1,2]=paste('DIA: ',DIASet1[1],', ',DIASet1[2],sep='')
    a3[a3$Breakpoints==0,2]=paste('MIC ')

    a1$Breakpoints=factor(a1$Breakpoints,levels=c("MIC ",paste('DIA: ',DIASet1[1],', ',DIASet1[2],sep='')))
    a2$Breakpoints=factor(a2$Breakpoints,levels=c("MIC ",paste('DIA: ',DIASet1[1],', ',DIASet1[2],sep='')))
    a3$Breakpoints=factor(a3$Breakpoints,levels=c("MIC ",paste('DIA: ',DIASet1[1],', ',DIASet1[2],sep='')))

    if(logConvert==TRUE){
      fit=ggplot(a1,aes(x=grid,y=value,color=Breakpoints))+geom_line()+
        geom_line(data=a2,aes(x=grid,y=value,color=Breakpoints))+
        geom_line(data=a3,aes(x=grid,y=value,color=Breakpoints))+
        geom_vline(xintercept=M1,lty=2,alpha=.5)+
        geom_vline(xintercept=M2,lty=2,alpha=.5)+
        labs(x='MIC',y='Probability',title='Probability of Correct Classification (non-weighted)')+
        ylim(0,1)+
        scale_x_continuous(breaks = seq(round(min(xgrid)),round(max(xgrid)),by=1),limits=c(min(MIC),max(MIC)))+
        theme_dbets()+
        theme(
          legend.position='bottom',
          legend.text = element_text(size = 16),
          legend.title=element_text(size=15))
    }
    if(logConvert==FALSE){
      M1=2^(M1)
      M2=2^(M2)
      a1$grid=2^a1$grid
      a2$grid=2^a2$grid
      a3$grid=2^a3$grid
      MICTemp=min(MIC):max(MIC)
      MICTemp=2^MICTemp
      fit=ggplot(a1,aes(x=grid,y=value,color=Breakpoints))+geom_line()+
        geom_line(data=a2,aes(x=grid,y=value,color=Breakpoints))+
        geom_line(data=a3,aes(x=grid,y=value,color=Breakpoints))+
        geom_vline(xintercept=M1,lty=2,alpha=.5)+
        geom_vline(xintercept=M2,lty=2,alpha=.5)+
        labs(x='MIC',y='Probability',title='Probability of Correct Classification (non-weighted)')+
        ylim(0,1)+
        scale_x_continuous(trans=log2_trans(),
                           limits=c(min(MICTemp),max(MICTemp)),
                           breaks=MICTemp)+
        theme_dbets()+
        theme(
          legend.position='bottom',
          legend.text = element_text(size = 16),
          legend.title=element_text(size=15))
    }
  }


  return(fit)


}


