getRTrange<-function(mzData){
  rtrange<-ldply(.data=mzData,.fun=function(.x).x$sel$rtrange)
  names(rtrange)<-c('start','stop')
  return(rtrange)
}

getMZrange<-function(mzData){
  rtrange<-ldply(.data=mzData,.fun=function(.x).x$sel$mzrange)
  names(rtrange)<-c('start','stop')
  return(rtrange)
}

getNscans<-function(mzData){
  nscans<-ldply(.data=mzData,.fun=function(.x)length(.x$sel$scanidx))
  return(nscans)
}

getNpeaks<-function(mzData){
  nscans<-ldply(.data=mzData,.fun=function(.x)length(.x$sel$massidx))
  return(nscans)
}

