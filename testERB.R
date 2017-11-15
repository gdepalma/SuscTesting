library(mixtools)
library(SuscTesting)
library(StanBayesianErrorsMonoModels)

nobs=400
xcens=rep(0,nobs)
ycens=rep(0,nobs)

xsig=.707; ysig=2.121
xgrid=seq(-12,12,length=1000)

popmn=c(-4.6,-2,1); popstd=c(1.1,1.5,1.5); popprob=c(.6,.2,.2)
xtrue=rnormmix(n=nobs,lambda=popprob,mu=popmn,sigma=popstd)
xobs=ceiling(xtrue+rnorm(nobs,0,xsig))

coef=c(35,1.17,.1,1.2)
mb = (2*coef[3]*coef[4])/(coef[3]+coef[4])
fx = 1/(1+exp(-mb*(coef[2]-xtrue)))
ytrue=coef[1]*(fx*exp(coef[3]*(coef[2]-xtrue))+(1-fx)*exp(coef[4]*
  (coef[2]-xtrue)))/(1+fx*exp(coef[3]*(coef[2]-xtrue))+(1-fx)*
  exp(coef[4]*(coef[2]-xtrue)))

yobs=round(ytrue+rnorm(nobs,0,ysig))

MIC = xobs
DIA = yobs
MICBrkptL=-1
MICBrkptU=1
MICBrkpt=0

### Descriptive Stats
descriptiveStat(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU)
descriptiveStatOne(MIC,DIA,xcens,ycens,MICBrkpt=0)

### Plots
basicPlotOne(MIC,DIA,xcens,ycens,MICBrkpt,MICXaxis=TRUE,log2MIC=TRUE)
basicPlot(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU,MICXaxis=TRUE,log2MIC=FALSE)


### ERB Two Breakpoints
findBrkptsERB(MIC,DIA,VM1=10,M1=10,m1=40,VM2=2,M2=2,m2=5,MICBrkptL,MICBrkptU,minWidth=4,maxWidth=20)

### ERB One Breakpoint
parms=findBrkptsERBOne(MIC,DIA,VM=1,M=5,MICBrkpt)
DIABrkpt=parms$DIABrkpt
ERBGivenDIAOne(MIC,DIA,xcens,ycens,MICBrkpt,DIABrkpt,VM=1,M=5)

