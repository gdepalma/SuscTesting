library(SuscTesting)
# library('devtools')
# install_github('gdepalma/StanBayesianErrorsMonoModels')
library(StanBayesianErrorsMonoModels)

# nobs=400
# xcens=rep(0,nobs)
# ycens=rep(0,nobs)
#
# xsig=.707; ysig=2.121
# xgrid=seq(-12,12,length=1000)
#
# popmn=c(-4.6,-2,1); popstd=c(1.1,1.5,1.5); popprob=c(.6,.2,.2)
# xtrue=rnormmix(n=nobs,lambda=popprob,mu=popmn,sigma=popstd)
# xobs=ceiling(xtrue+rnorm(nobs,0,xsig))
#
# coef=c(35,1.17,.1,1.2)
# mb = (2*coef[3]*coef[4])/(coef[3]+coef[4])
# fx = 1/(1+exp(-mb*(coef[2]-xtrue)))
# ytrue=coef[1]*(fx*exp(coef[3]*(coef[2]-xtrue))+(1-fx)*exp(coef[4]*
#   (coef[2]-xtrue)))/(1+fx*exp(coef[3]*(coef[2]-xtrue))+(1-fx)*
#   exp(coef[4]*(coef[2]-xtrue)))
#
# yobs=round(ytrue+rnorm(nobs,0,ysig))

a1 = read_csv(file='test/data1.csv')
parms=parse_data(a1)
MIC=parms$MIC; DIA=parms$DIA; xcens=parms$xcens; ycens=parms$ycens

MICBrkptL=-1
MICBrkptU=1
MICBrkpt=0
DIABrkptL=34
DIABrkptU=39
DIABrkpt=38

### Descriptive Stats
descriptiveStat(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU)
descriptiveStatOne(MIC,DIA,xcens,ycens,MICBrkpt=0)

### Plots
basicPlotOne(MIC,DIA,xcens,ycens,MICBrkpt,MICXaxis=TRUE,log2MIC=TRUE)
basicPlotOne(MIC,DIA,xcens,ycens,MICBrkpt,MICXaxis=TRUE,log2MIC=FALSE)
basicPlotOne(MIC,DIA,xcens,ycens,MICBrkpt,MICXaxis=FALSE,log2MIC=TRUE)
basicPlotOne(MIC,DIA,xcens,ycens,MICBrkpt,MICXaxis=FALSE,log2MIC=FALSE)

basicPlot(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU,MICXaxis=TRUE,log2MIC=FALSE)


### ERB Two Breakpoints
findBrkptsERB(MIC,DIA,VM1=10,M1=10,m1=40,VM2=2,M2=2,m2=5,MICBrkptL,MICBrkptU,minWidth=4,maxWidth=20)
plotBrkPtsERB(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU,DIABrkptL,DIABrkptU,MICXaxis=FALSE,log2MIC=FALSE)
ERBGivenDIA(MIC,DIA,xcens,ycens,MICBrkptL,MICBrkptU,DIABrkptL,DIABrkptU,VM1=10,M1=10,m1=40,VM2=2,M2=2,m2=5)


### ERB One Breakpoint
parms=findBrkptsERBOne(MIC,DIA,VM=1,M=5,MICBrkpt)
DIABrkpt=parms$DIABrkpt
ERBGivenDIAOne(MIC,DIA,xcens,ycens,MICBrkpt,DIABrkpt,VM=1,M=5)
plotBrkPtsERBOne(MIC,DIA,xcens,ycens,MICBrkpt,DIABrkpt,MICXaxis=TRUE,log2MIC=TRUE)

# Boot
bootData=bootStrapERB(MIC,DIA,MICBrkptL,MICBrkptU,VM1=10,M1=10,m1=40,VM2=2,M2=2,m2=5,
                      minWidth=3,maxWidth=10)
plotBootDataERB(bootData)
bootStrapERBOne(MIC,DIA,MICBrkpt)

### Model
xgrid=seq(min(MIC)-1,max(MIC)+1,length=1000)
N=length(MIC)
xcensl=rep(0,N)
xcensl[xcens==-1] = 1
xcensu=rep(0,N)
xcensu[xcens==1] = 1
ycensl=rep(0,N)
ycensl[ycens==-1] = 1
ycensu=rep(0,N)
ycensu[ycens==1] = 1
dat_sav=data.frame(xobs=MIC,yobs=DIA,xcensl,xcensu,ycensu,ycensl)
list_of_draws_logistic = stan_logistic.fit(dat_sav,xgrid,nchains=1,numIter=3000)
list_of_draws_spline = stan_spline.fit(dat_sav,xgrid,nchains=1,numIter=3000)

# Brkpts
a1=getDIABrkptsModel_two(list_of_draws_spline,xgrid,DIA,MICBrkptL,MICBrkptU)
a1
a1=getDIABrkptsModel_one(list_of_draws_logistic,xgrid,DIA,MICBrkpt)
a1

output_graph_one_model_twoMIC(list_of_draws_logistic,xgrid,dat_sav,MICBrkptL,MICBrkptU)
output_graph_one_model_twoMIC(list_of_draws_spline,xgrid,dat_sav,MICBrkptL,MICBrkptU)

output_graph_one_model_oneMIC(list_of_draws_spline,xgrid,dat_sav,MICBrkpt)





### Compare Fits
output_graph_compare_twoMIC(list_of_draws_logistic,list_of_draws_spline,xgrid,dat_sav,MICBrkptL,MICBrkptU)
output_graph_compare_oneMIC(list_of_draws_logistic,list_of_draws_spline,xgrid,dat_sav,MICBrkpt)
