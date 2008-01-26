#
# this function should be run with an open device
#   currently retrieves xlim/ylim of currently set plots
#


getPlotsBounds <- function(Splot){

  # take Snapshot of device, returns plotting inforamtion 
  snapShot <- .Internal(getSnapshot())
  # return all instances of .Primitive as text
  interStep = grep(snapShot[[1]], pattern=".Primitive", value=TRUE)
  # search text for plot.window, this is where plot xlim and ylim are stored
  Plot.indx = grep(interStep, pattern='plot.window')

  # set up useful objects
  #   will store for each figure 
  len = length(Plot.indx)
  xmins = rep(NA, len)
  xmaxs = rep(NA, len)
  ymins = rep(NA, len)
  ymaxs = rep(NA, len)

  # cycle through given indicies
  for(pid in 1:len){
    # store xlim and ylim values 
    xmins[pid] = snapShot[[1]][[Plot.indx[pid]]][[2]][[1]][1]
    xmaxs[pid] = snapShot[[1]][[Plot.indx[pid]]][[2]][[1]][2]
    ymins[pid] = snapShot[[1]][[Plot.indx[pid]]][[2]][[2]][1]
    ymaxs[pid] = snapShot[[1]][[Plot.indx[pid]]][[2]][[2]][2]

  }

  # make object 
  lims = list()
  lims$xmins = xmins
  lims$xmaxs = xmaxs
  lims$ymins = ymins
  lims$ymaxs = ymaxs
  Splot$plot.lims = lims
  Splot$snapShot = snapShot

  # return object
  return(Splot)
  
}
