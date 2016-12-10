preparePeaks<-function(mzData, # data to work with
  runAgg=runAggSummary, # function to collect peak information
  maxTime # scan interval is seconds
){
  nodes <- detectCores()-1
  cl <- makeCluster(nodes)
  registerDoParallel(cl)
  clusterEvalQ(cl, library(Matrix,xcms))
  msScan<-makeScan(mzData)
  idx<-which(mzData[[1]]$time<=maxTime)
  L<-length(idx)
  ch<-.getPeaks(mzData,1,runAgg,
                timeIdx = idx,
                msScan=msScan)
  for(i in 2:length(mzData)){
    ch<-rbind(ch,
              .getPeaks(mzData,i,runAgg,
                        timeIdx = idx,
                        msScan=msScan))
  }
  stopCluster(cl)
  fpks<-ddply(.data = ch,
                 .variables = .(indx),
                 summarize,
                 sumN=sum(N),
                 intstty=sum(Intsty),
                 sumS=sum(Sum),
                 max=max(Max),
                 accum=sum(accum),
                 accumM=max(accum))
  ch$accumTIC<-ch$accum/fpksaccum[ch$indx]
  ch$accumMax<-ch$accum/fpks$accumM[ch$indx]
  return(list(chromatogramm=ch,fpks=fpks,Length=l))
}

.getPeaks<-function( # process individual file in mzData
  mzData, # data to work with
  ind=1, # index of record to work with
  runAgg, # function to collect peak information
  timeIdx, # scan interval is seconds
  msScan
){
  i<-ind
  .uniquemz<-mzData[[i]]$mass
  .uniquetm<-mzData[[i]]$time
  eic<-mzData[[i]]$eic[timeIdx,]
  nzCol<-which(colSums(eic)>0)
  chromatogram<-adply(.data = eic[,nzCol],
                         .margins = 2,
                         .fun = runAgg,
                         .parallel = TRUE,
                         .progress = 'text')
  chromatogram<-cbind(data.frame(indx=i,
                                    mz=.uniquemz[nzCol],
                                    Intsty=as.vector(t(msScan[i,nzCol]))),
                         chromatogram[,-1])
  return(chromatogram)
}
runAggSummary<-function(.x){
  r<-rle(.x>0)
  idx<-which(r$values)
  c(N=length(idx),
    Max=max(r$lengths[idx]),
    Mean=mean(r$lengths[idx]),
    Sum=sum(r$lengths[idx]),
    accum=sum(.x))
}

