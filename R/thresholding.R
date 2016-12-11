thresholding<-function(m,threshold){
  if(is.null(dim(m))) stop('m should be a matrix')
  if(class(m)!="dgTMatrix"){
    m<-as(m,"dgTMatrix")
  }
  ind <- m@x > threshold
  m@x <- m@x[ind]
  m@i <- m@i[ind]
  m@j <- m@j[ind]
  m<-as(m,"dgCMatrix")
  return(m)
}
