
output_graph_one_model_twoMIC=function(MICDens,gx,xobs,yobs,xcens,ycens,xgrid,MICBrkptL,MICBrkptU){

  xobs1=xobs
  yobs1=yobs
  xobs[xcens==1 & xobs==max(xobs)]=max(xobs)+1
  xobs[xcens==-1 & xobs==min(xobs)]=min(xobs)-1
  yobs[ycens==1 & yobs==max(yobs)]=max(yobs)+1
  yobs[ycens==-1 & yobs==min(yobs)]=min(yobs)-1
  a1=data.frame(table(xobs,yobs))
  a1$xobs=as.numeric(as.character(a1$xobs))
  a1$yobs=as.numeric(as.character(a1$yobs))
  a1=a1[a1$Freq>0,]

  ### MIC Density
  densDat=data_frame(xgrid,y=apply(MICDens,2,mean))
  densDat$lower=apply(MICDens,2,function(x) quantile(x,probs=c(.05)))
  densDat$upper=apply(MICDens,2,function(x) quantile(x,probs=c(.95)))


  pltDens=ggplot(densDat,aes(x=xgrid,y))+geom_line(color='darkblue')+
    geom_ribbon(aes(ymin=lower, ymax=upper),alpha=.5,fill='cyan4')+
    geom_vline(xintercept=MICBrkptL+.5,lty=2,alpha=.5)+
    geom_vline(xintercept=MICBrkptU-.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
                       limits = c(min(xobs1)-1,max(xobs1)+1))+
    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=11),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13))+
    labs(title='MIC Density',y='',x=expression(MIC~(log["2"]~ug/mL)))

  ### MIC/DIA Relationship
  gxDat=data_frame(xgrid,y=apply(gx,2,mean))
  gxDat$lower=apply(gx,2,function(x) quantile(x,probs=c(.05)))
  gxDat$upper=apply(gx,2,function(x) quantile(x,probs=c(.95)))


  pltRel=ggplot(data=a1,aes(x=xobs,y=yobs,label=Freq))+geom_text(size=3.2,color='black')+
    geom_line(data=gxDat,aes(x=xgrid,y=y),color='darkblue',inherit.aes = FALSE)+
    geom_ribbon(data=gxDat,aes(x=xgrid,ymin=lower, ymax=upper),fill='cyan4',alpha=.5,inherit.aes = FALSE)+
    geom_vline(xintercept=MICBrkptL+.5,lty=2,alpha=.5)+
    geom_vline(xintercept=MICBrkptU-.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
                       limits = c(min(xobs1)-1,max(xobs1)+1))+
    scale_y_continuous(breaks = seq(min(yobs1)-1,max(yobs1)+1,by=1),
                       labels = c(paste("<",min(yobs1),sep=''),seq(min(yobs1),max(yobs1),by=1), paste(">",max(yobs1),sep='')))+    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          legend.position='none',
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=8),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13))+
    labs(title='MIC/DIA Relationship',y='DIA (mm)',x="")+
    coord_cartesian(ylim =c(min(yobs)-1,max(yobs)+1))

  plt <- gridExtra::arrangeGrob(pltRel, pltDens,ncol=1, heights=c(5,2))

  return(plt)

}

output_graph_one_model_oneMIC=function(MICDens,gx,xobs,yobs,xcens,ycens,xgrid,MICBrkpt){


  xobs1=xobs
  yobs1=yobs
  xobs[xcens==1 & xobs==max(xobs)]=max(xobs)+1
  xobs[xcens==-1 & xobs==min(xobs)]=min(xobs)-1
  yobs[ycens==1 & yobs==max(yobs)]=max(yobs)+1
  yobs[ycens==-1 & yobs==min(yobs)]=min(yobs)-1
  a1=data.frame(table(xobs,yobs))
  a1$xobs=as.numeric(as.character(a1$xobs))
  a1$yobs=as.numeric(as.character(a1$yobs))
  a1=a1[a1$Freq>0,]


  ### MIC Density
  densDat=data_frame(xgrid,y=apply(MICDens,2,mean))
  densDat$lower=apply(MICDens,2,function(x) quantile(x,probs=c(.05)))
  densDat$upper=apply(MICDens,2,function(x) quantile(x,probs=c(.95)))


  pltDens=ggplot(densDat,aes(x=xgrid,y))+geom_line(color='darkblue')+
    geom_ribbon(aes(ymin=lower, ymax=upper),alpha=.5,fill='cyan4')+
    geom_vline(xintercept=MICBrkpt+.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
                       limits = c(min(xobs1)-1,max(xobs1)+1))+
    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=11),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13))+
    labs(title='MIC Density',y='',x=expression(MIC~(log["2"]~ug/mL)))

  ### MIC/DIA Relationship
  gxDat=data_frame(xgrid,y=apply(gx,2,mean))
  gxDat$lower=apply(gx,2,function(x) quantile(x,probs=c(.05)))
  gxDat$upper=apply(gx,2,function(x) quantile(x,probs=c(.95)))


  pltRel=ggplot(data=a1,aes(x=xobs,y=yobs,label=Freq))+geom_text(size=3.2,color='black')+
    geom_line(data=gxDat,aes(x=xgrid,y=y),color='darkblue',inherit.aes = FALSE)+
    geom_ribbon(data=gxDat,aes(x=xgrid,ymin=lower, ymax=upper),fill='cyan4',alpha=.5,inherit.aes = FALSE)+
    geom_vline(xintercept=MICBrkpt+.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')))+    scale_y_continuous(breaks = seq(min(yobs1)-1,max(yobs1)+1,by=1),
                       labels = c(paste("<",min(yobs1),sep=''),seq(min(yobs1),max(yobs1),by=1), paste(">",max(yobs1),sep='')))+    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          legend.position='none',
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=8),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13))+
    labs(title='MIC/DIA Relationship',y='DIA (mm)',x="")+
    coord_cartesian(ylim =c(min(yobs)-1,max(yobs)+1))



  plt <- gridExtra::arrangeGrob(pltRel, pltDens,ncol=1, heights=c(5,2))


  return(plt)

}


output_graph_compare_twoMIC=function(MICDens_log,gx_log,MICDens_spline,gx_spline,xobs,yobs,xcens,ycens,xgrid,MICBrkptL,MICBrkptU){

  xobs1=xobs
  yobs1=yobs
  xobs[xcens==1 & xobs==max(xobs)]=max(xobs)+1
  xobs[xcens==-1 & xobs==min(xobs)]=min(xobs)-1
  yobs[ycens==1 & yobs==max(yobs)]=max(yobs)+1
  yobs[ycens==-1 & yobs==min(yobs)]=min(yobs)-1
  a1=data.frame(table(xobs,yobs))
  a1$xobs=as.numeric(as.character(a1$xobs))
  a1$yobs=as.numeric(as.character(a1$yobs))
  a1=a1[a1$Freq>0,]

  ### MIC Density
  densDat1=data_frame(xgrid,y=apply(MICDens_log,2,mean))
  densDat1$lower=apply(MICDens_log,2,function(x) quantile(x,probs=c(.05)))
  densDat1$upper=apply(MICDens_log,2,function(x) quantile(x,probs=c(.95)))
  densDat1$group='Logistic'
  densDat2=data_frame(xgrid,y=apply(MICDens_spline,2,mean))
  densDat2$lower=apply(MICDens_spline,2,function(x) quantile(x,probs=c(.05)))
  densDat2$upper=apply(MICDens_spline,2,function(x) quantile(x,probs=c(.95)))
  densDat2$group='Spline'
  densDat=rbind(densDat1,densDat2)


  pltDens=ggplot(densDat,aes(x=xgrid,y,group=group,colour=group))+geom_line()+
    # geom_ribbon(aes(ymin=lower, ymax=upper),alpha=.5,fill='cyan4')+
    geom_vline(xintercept=MICBrkptL+.5,lty=2,alpha=.5)+
    geom_vline(xintercept=MICBrkptU-.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
                       limits = c(min(xobs1)-1,max(xobs1)+1))+
    theme_fivethirtyeight(base_size=17)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=11),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13),
          legend.title=element_blank())+
    labs(title='MIC Density',y='',x=expression(MIC~(log["2"]~ug/mL)))

  ### MIC/DIA Relationship
  gxDat1=data_frame(xgrid,y=apply(gx_log,2,mean))
  gxDat1$lower=apply(gx_log,2,function(x) quantile(x,probs=c(.05)))
  gxDat1$upper=apply(gx_log,2,function(x) quantile(x,probs=c(.95)))
  gxDat1$group='Logistic'
  gxDat2=data_frame(xgrid,y=apply(gx_spline,2,mean))
  gxDat2$lower=apply(gx_spline,2,function(x) quantile(x,probs=c(.05)))
  gxDat2$upper=apply(gx_spline,2,function(x) quantile(x,probs=c(.95)))
  gxDat2$group='Spline'
  gxDat=rbind(gxDat1,gxDat2)

  pltRel=ggplot(data=a1,aes(x=xobs,y=yobs,label=Freq))+geom_text(size=3.2,color='black')+
    geom_line(data=gxDat,aes(x=xgrid,y=y,group=group,color=group),inherit.aes = FALSE)+
    # geom_ribbon(data=gxDat,aes(x=xgrid,ymin=lower, ymax=upper),fill='cyan4',alpha=.5,inherit.aes = FALSE)+
    geom_vline(xintercept=MICBrkptL+.5,lty=2,alpha=.5)+
    geom_vline(xintercept=MICBrkptU-.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
                       limits = c(min(xobs1)-1,max(xobs1)+1))+
    scale_y_continuous(breaks = seq(min(yobs1)-1,max(yobs1)+1,by=1),
                       labels = c(paste("<",min(yobs1),sep=''),seq(min(yobs1),max(yobs1),by=1), paste(">",max(yobs1),sep='')))+
    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          legend.position='none',
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=8),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13))+
    labs(title='MIC/DIA Relationship',y='DIA (mm)',x="")+
    coord_cartesian(ylim =c(min(yobs)-1,max(yobs)+1))



  plt <- gridExtra::arrangeGrob(pltRel, pltDens,ncol=1, heights=c(5,2))

  return(plt)

}

output_graph_compare_oneMIC=function(MICDens_log,gx_log,MICDens_spline,gx_spline,MIC,DIA,xcens,ycens,xgrid,MICBrkpt){


  xobs1=xobs
  yobs1=yobs
  xobs[xcens==1 & xobs==max(xobs)]=max(xobs)+1
  xobs[xcens==-1 & xobs==min(xobs)]=min(xobs)-1
  yobs[ycens==1 & yobs==max(yobs)]=max(yobs)+1
  yobs[ycens==-1 & yobs==min(yobs)]=min(yobs)-1
  a1=data.frame(table(xobs,yobs))
  a1$xobs=as.numeric(as.character(a1$xobs))
  a1$yobs=as.numeric(as.character(a1$yobs))
  a1=a1[a1$Freq>0,]


  ### MIC Density
  densDat1=data_frame(xgrid,y=apply(MICDens_log,2,mean))
  densDat1$lower=apply(MICDens_log,2,function(x) quantile(x,probs=c(.05)))
  densDat1$upper=apply(MICDens_log,2,function(x) quantile(x,probs=c(.95)))
  densDat1$group='Logistic'
  densDat2=data_frame(xgrid,y=apply(MICDens_spline,2,mean))
  densDat2$lower=apply(MICDens_spline,2,function(x) quantile(x,probs=c(.05)))
  densDat2$upper=apply(MICDens_spline,2,function(x) quantile(x,probs=c(.95)))
  densDat2$group='Spline'
  densDat=rbind(densDat1,densDat2)

  pltDens=ggplot(densDat,aes(x=xgrid,y,group=group,colour=group))+geom_line()+
    # geom_ribbon(aes(ymin=lower, ymax=upper),alpha=.5,fill='cyan4')+
    geom_vline(xintercept=MICBrkpt+.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
                       limits = c(min(xobs1)-1,max(xobs1)+1))+
    theme_fivethirtyeight(base_size=17)+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=11),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13),
          legend.title=element_blank())+
    labs(title='MIC Density',y='',x=expression(MIC~(log["2"]~ug/mL)))


  ### MIC/DIA Relationship
  gxDat1=data_frame(xgrid,y=apply(gx_log,2,mean))
  gxDat1$lower=apply(gx_log,2,function(x) quantile(x,probs=c(.05)))
  gxDat1$upper=apply(gx_log,2,function(x) quantile(x,probs=c(.95)))
  gxDat1$group='Logistic'
  gxDat2=data_frame(xgrid,y=apply(gx_spline,2,mean))
  gxDat2$lower=apply(gx_spline,2,function(x) quantile(x,probs=c(.05)))
  gxDat2$upper=apply(gx_spline,2,function(x) quantile(x,probs=c(.95)))
  gxDat2$group='Spline'
  gxDat=rbind(gxDat1,gxDat2)

  pltRel=ggplot(data=a1,aes(x=xobs,y=yobs,label=Freq))+geom_text(size=3.2,color='black')+
    geom_line(data=gxDat,aes(x=xgrid,y=y,color=group,group=group),inherit.aes = FALSE)+
    geom_vline(xintercept=MICBrkpt+.5,lty=2,alpha=.5)+
    scale_x_continuous(breaks = seq(min(xobs1)-1,max(xobs1)+1,by=1),
                       labels = c(paste("<",min(xobs1),sep=''),seq(min(xobs1),max(xobs1),by=1), paste(">",max(xobs1),sep='')),
                       limits = c(min(xobs1)-1,max(xobs1)+1))+
    scale_y_continuous(breaks = seq(min(yobs1)-1,max(yobs1)+1,by=1),
                       labels = c(paste("<",min(yobs1),sep=''),seq(min(yobs1),max(yobs1),by=1), paste(">",max(yobs1),sep='')),
                       limits = c(min(yobs1)-1,max(yobs1)+1))+
    theme_fivethirtyeight()+
    theme(axis.line = element_line(colour = "black"),
          legend.position='none',
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text.x=element_text(size=11),
          axis.text.y=element_text(size=8),
          axis.title=element_text(size=11),
          plot.title=element_text(size=13))+
    labs(title='MIC/DIA Relationship',y='DIA (mm)',x="")


  plt <- gridExtra::arrangeGrob(pltRel, pltDens,ncol=1, heights=c(5,2))


  return(plt)

}
