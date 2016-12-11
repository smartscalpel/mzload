loadMzData<-function(files,
                     uniquetm=seq(from=0,to=900,by=5), # set of bins in time
                     uniquemz=seq(from=100,to=1300,by=0.01), # set of bins in M/Z
                     rtCheck=200, # data shorter in seconds will be discarded
                     scCheck=200 # data shorter in scans will be discarded
){
  for(j in 1:length(files)){
    cat(paste(files[j],':'))
    xraw <- xcmsRaw(files[j])
    sel <- profRange(xraw)
    if(sel$rtrange[2]<rtChech|length(sel$scanidx)<scCheck){
      warning(paste('File ',files[j],' contains not enought data\n'))
    }else{
      cat(' read; ')
      accum<-rep(0,length(uniquemz))
      accumTS<-matrix(0,nrow=length(uniquetm),ncol = length(uniquemz))
      for (i in seq(along = sel$scanidx)) {
        scan <- as.data.table(getScan(xraw, sel$scanidx[i], sel$mzrange))
        scan$mzS<-findInterval(scan$mz,uniquemz)
        scan<-scan[,list(intS=sum(intensity),msA=mean(mz)),by=c('mzS')]
        accum[scan$mzS]<-accum[scan$mzS]+scan$intS
        if(xraw@scantime[i]<=max(uniquetm)){
          ts<-findInterval(xraw@scantime[i],uniquetm)
          accumTS[ts,scan$mzS]<-accumTS[ts,scan$mzS]+scan$intS
        }
      }
      cat('Done.\n')
      apptic<-spline(x=xraw@scantime,y=xraw@tic,method = 'nat', xout = uniquetm)$y#

      #points <- cbind(mz = uniquemz, intensity = accum/length(sel$scanidx))

      mzData[[j]]<-list(fname=files[j],
                        xraw=xraw,
                        spec=accum,#/length(sel$scanidx),
                        tic=apptic,
                        eic=Matrix(accumTS, sparse = TRUE),
                        sel=sel,
                        time=uniquetm,
                        mass=uniquemz)
      if(j%%15==0){save.image(file=sprintf('tmp%d.Rdata',j))}
    }
  }

}

makeScan<-function(mzData){
  msScan<-ldply(.data=mzData,.fun=function(.x).x$spec/sum(.x$spec))[,-1]
  names(msScan)<-paste('mz',uniquemz[-1],sep='')
  return(msScan)
}
