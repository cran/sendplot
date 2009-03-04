

makeCharacter <- function(DF){
  
  dat = DF$dat
  dat2 = DF$dat2
  

  # update dat into character array to make writing more efficient
  cdat=array(" ",dim=dim(dat))
  ndat=rep(" ",dim(dat)[2])
  for(j in 1:(dim(dat)[2])){
    cdat[,j]=as.character(dat[,j])
    ndat[j]=names(dat)[j]
  }
  
  cdat2=array(" ",dim=dim(dat2))
  ndat2=rep(" ",dim(dat2)[2])
  for(j in 1:(dim(dat2)[2])){
    cdat2[,j]=as.character(dat2[,j])
    ndat2[j]=names(dat2)[j]
  }
  # combined information data frame and hyper link data frame
  if(dim(cdat2)[2] > 1){

    if(dim(cdat)[1] == 1){
      cdat = matrix(c(cdat, cdat2[,2:dim(cdat2)[2]]), nrow=1)
      ndat = c(ndat, ndat2[2:length(ndat2)])
    }else{
      cdat = cbind(cdat, cdat2[,2:dim(cdat2)[2]])
      ndat = c(ndat, ndat2[2:length(ndat2)])
    }
  }
  links.st = (dim(dat)[2])+1

  cDF = list()
  cDF$cdat = cdat
  cDF$ndat = ndat
  cDF$orgDatDim = dim(dat)
  cDF$orgDat2Dim = dim(dat2)
  cDF$links.st = links.st

  
  return(cDF)
  
}  
