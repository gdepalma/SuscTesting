theme_dbets <- function () {
  theme_fivethirtyeight() +
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(color='gray90'),
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=11),
          axis.title=element_text(size=11),
          plot.title=element_text(size=15,hjust = 0))
}

parse_data=function(a1){

  if(any(names(a1)!=c("MIC","DIA"))==TRUE) stop("File not formatted correctly.")

  xcens=rep(0,nrow(a1))
  ycens=rep(0,nrow(a1))
  MIC=a1$MIC
  DIA=a1$DIA

  xcens[grepl('>',a1$MIC)]=1
  xcens[grepl('<',a1$MIC)]=-1
  ycens[grepl('>',a1$DIA)]=1
  ycens[grepl('<',a1$DIA)]=-1

  MIC=as.numeric(MIC)
  DIA=as.numeric(DIA)

  return(list(MIC=MIC,DIA=DIA,xcens=xcens,ycens=ycens))

}
