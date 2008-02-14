#
# start to make general function
#


  
sendplot <- function(mat, plot.calls, x,y, mai.mat=NA, mai.prc=FALSE, xlim=NA, ylim=NA,
                     z=NA,
                     z.value="value",type="scatterplot", plt.extras = NA,
                     x.lbls=NA, y.lbls=NA,
                     xy.lbls=NA,
                     bound.pt = TRUE, source.plot=NA,
                     resize="800x1100",
                     ps.paper="letter",ps.width=8,ps.height=11,
                     fname.root="test",dir="./", header="v2",
                     paint=TRUE, img.prog = NA, up.left=c(288,203),low.right=c(620,940),
                     spot.radius=10
                     ){

  
  # figure out operating system 
  platform = .Platform$OS.type
  # if source.plot is not specified default to appropriate file
  #  source plot can only by png or ps 
  if(is.na(source.plot) | !(source.plot=="ps" | source.plot=="png")){
    if(platform == "unix") source.plot = "ps"
    if(platform == "windows" | platform == "mac") source.plot = "png"   
  }
    
  # set up file names
  fname.ps=paste(fname.root,".ps",sep="")
  fname.png=paste(fname.root,".png",sep="")
  
  # begin png file if flagged
  if(source.plot=="png"){
    wi = strsplit(resize, "x")[[1]][1]
    hi = strsplit(resize, "x")[[1]][2]
    png(file=fname.png, width=as.real(wi), height=as.real(hi))
  }
  # begin postscript file if flagged
  if(source.plot=="ps"){
    postscript(file=paste(dir,fname.ps,sep=""),paper=ps.paper,width=ps.width,height=ps.height,horizontal=FALSE)
  }
  
  # initiate layout
  # lcm(c())


  if(max(as.vector(mat),na.rm=TRUE)>1) nf = layout(mat, respect=TRUE)

  if(mai.prc) mai.def=par("mai")
  
  # loop over plot calls to place plots in order or 1:n in layout
  for(i in 1:length(plot.calls)){

    
    if(length(mai.mat)>1){
      # set up plot margins
      cat("setting margins \n")
      if(!mai.prc) par(mai=mai.mat[i,])
      if(mai.prc)  par(mai=mai.mat[i,]*mai.def)            
    }
    
    # add xlim and ylim arguments to first plot if plot is scatterplot
    if(i==1 & type!="image"){
      plt.call = plot.calls[i]
      xg = grep(pattern="xlim",plt.call)
      yg = grep(pattern="ylim",plt.call)
      if(length(xg)!=0 | length(yg)!=0){
        stop("xlim and ylim should not be specified in plot call for first graph. Please remove these arguments from the plot call and enter as function arguments.")
      }else{
        if(type!="image"){
          if(!exists("xlim")) xlim = range(x, na.rm=TRUE)
          if(!exists("ylim")) ylim = range(y, na.rm=TRUE) 
          ln = nchar(plt.call) 
          plot.calls[i] = paste(substr(plt.call,1,ln-1),", xlim=c(",xlim[1],",",xlim[2],"), ylim=c(",ylim[1],",",ylim[2],"))",sep="")
        }
      }
    }# end if i == 1
    
    # plot
    # evaluate plot call
    plt = eval.js(plot.calls[i])

    # add points to measure bounds
    if(i==1 & type=="scatterplot" & bound.pt){
      points(xlim[1],ylim[2], pch=22,bg="red",col="red")
      points(xlim[2],ylim[1], pch=22,bg="red",col="red")
    }
    if(i==1 & type=="image" & bound.pt){
      nx=length(x)
      xmin=x[1]-(x[2]-x[1])/2
      xmax=x[nx]+(x[nx]-x[nx-1])/2
      ny=length(y)
      ymin=y[1]-(y[2]-y[1])/2
      ymax=y[ny]+(y[ny]-y[ny-1])/2
      
      # changed to blue since default image colors are red and orange
      points(xmin,ymax, pch=22,bg="blue",col="blue", cex=2)
      points(xmax,ymin, pch=22,bg="blue",col="blue", cex=2)
    }

    # evaluate other plotting call if necessary
    # if plt.extras is NA no plotting for all plots
    if(length(plt.extras)==1){
      if(is.na(plt.extras)) plt.extras = rep(NA, length(plot.calls))
    }
    # if there are more plots than plot extras
    # specify no plotting for remaining plots
    if(i>length(plt.extras)) plt.extras = rep(NA, length(plt.extras))

    # cycle through all additional plot calls for current plot
    sub.np = length(plt.extras[[i]])
    for(sp in 1:sub.np){
      if(!is.na(plt.extras[[i]][[sp]]))
        eval.js(plt.extras[[i]][[sp]])
    }     
  }# end for loop over plot calls
  
  # turn off postscript device
  dev.off()

  
  # convert ps to png if ps was made and on unix OS
  if(source.plot=="ps" & platform=="unix"){
    system(paste("convert ",dir,fname.ps," -size 800x1100 -resize ",resize," ",dir,fname.png,sep=""))
  }
    
  # if flagged system call to open paint
  # if first time running program to find upper left and lower right corners of main plot
  if(paint){
    
    if(is.na(img.prog)){      
      if(platform=="unix") system(paste("kolourpaint ", dir,fname.png," &", sep=""))
      if(platform=="windows") system(paste("mspaint ", dir,fname.png," &", sep=""))
      if(platform=="mac") cat("automatic open is not supported in this version for MAC OS \n")
    }else{
      # some other program has been specified to open png image
      system(paste(img.prog, " ", dir, fname.png, " &", sep=""))
    }    
  }
    
  # if flagged make interactive html
  if(!bound.pt){ 
  
    # set up data frame of information
    # information in this data frame will be displayed for points in interactive window
    if(type=="scatterplot"){
      
      # estimate pixil location
      x.new = round(up.left[1] + ((x-xlim[1])/(xlim[2]-xlim[1]))*(low.right[1]-up.left[1]))
      y.new = round(up.left[2] + ((ylim[2]-y)/(ylim[2]-ylim[1]))*(low.right[2]-up.left[2]))
      
      # initiate data frame
      dat = data.frame(
        # pixil locations
        pix.x = x.new,
        pix.y =y.new
      )
    
      # x specific data
      contx = TRUE
      x.lbls = as.data.frame(x.lbls)
      if( (dim(x.lbls)[1]==1) & (dim(x.lbls)[2]==1)){
        if(is.na(x.lbls[1,1])) contx = FALSE
      }
      # if x.lbls is not NA continue
      if(contx){     
        for(i in 1:dim(x.lbls)[2]){
          if(i == 1) z.value = names(x.lbls)[i]
          eval.js(paste("dat$",names(x.lbls)[i], "=as.vector(x.lbls[,i])", sep=""))
        }      
      }
      # y specific data
      conty = TRUE
      y.lbls = as.data.frame(y.lbls)
      if( (dim(y.lbls)[1]==1) & (dim(y.lbls)[2]==1)){
        if(is.na(y.lbls[1,1])) conty = FALSE
      }
      # if y.lbls is not NA continue
      if(conty){     
        for(i in 1:dim(y.lbls)[2]){
          if((i == 1) & !contx) z.value = names(y.lbls)[i]
          eval.js(paste("dat$",names(y.lbls)[i], "=as.vector(y.lbls[,i])", sep=""))
        }
      }
      # xy -- assumes in this case that columns are different data vectors of row == nsmpls
      contxy = TRUE
      xy.lbls = as.data.frame(xy.lbls)
      if( (dim(xy.lbls)[1]==1) & (dim(xy.lbls)[2]==1)){
        if(is.na(xy.lbls[1,1])) contxy = FALSE
      }
      # if xy.lbls is not NA continue
      if(contxy){     
        for(i in 1:dim(xy.lbls)[2]){
          if((i == 1) & !contx & !conty) z.value = names(xy.lbls)[i]
          eval.js(paste("dat$",names(xy.lbls)[i], "=as.vector(xy.lbls[,i])", sep=""))
        }
      }
      # if all: x.lbls, y.lbls, and xy.lbls were NA no data to display
      # set up dummy vector with blanks 
      if(!contx & !conty & !contxy){  
        eval.js(paste("dat$", z.value, "=rep('',dim(dat)[2])",sep=""))
      }
      
    }# end if scatterplot
    
    if(type=="image"){
      
       # calculate width and height of active image
       wdth=low.right[1]-up.left[1]
       hght=low.right[2]-up.left[2]
       # get min and max x values for image      
       nx=length(x)
       xmin=x[1]-(x[2]-x[1])/2
       xmax=x[nx]+(x[nx]-x[nx-1])/2
       # calculate cuts and center of x values on image
       bndrs=c(xmin,(x[1:(nx-1)]+x[2:nx])/2,xmax)
       cntrs=(bndrs[1:(length(bndrs)-1)]+bndrs[2:length(bndrs)])/2
       # adjust 
       unit.int.wdth=cntrs-xmin
       unit.int.wdth=unit.int.wdth/(xmax-xmin)
       # calculate pixil positions
       x.image=round(unit.int.wdth*wdth+up.left[1])
       # get min and max y values for image
       ny=length(y)
       ymin=y[1]-(y[2]-y[1])/2
       ymax=y[ny]+(y[ny]-y[ny-1])/2
       # calculate cuts and centers of y values on image
       bndrs=c(ymin,(y[1:(ny-1)]+y[2:ny])/2,ymax)
       cntrs=(bndrs[1:(length(bndrs)-1)]+bndrs[2:length(bndrs)])/2
       # adjust
       unit.int.hght=cntrs-ymin
       unit.int.hght=unit.int.hght/(ymax-ymin)
       # calculate pixil positions
       y.image= round((1-unit.int.hght)*hght+up.left[2])
          
       # initiate data frame of info
       dat = data.frame(
         # pixil locations
         pix.x = as.vector(mapply(rep,x=x.image,MoreArgs=list(times=length(y.image)))),
         pix.y = rep(y.image, length(x.image))
        )
       
       # xy specific data
       eval.js(paste("dat$",z.value,"=as.vector(z)",sep=""))

       # x specific data
       cont = TRUE
       x.lbls = as.data.frame(x.lbls)
       if( (dim(x.lbls)[1]==1) & (dim(x.lbls)[2]==1)){
         if(is.na(x.lbls[1,1])) cont = FALSE
       }
       # if x.lbls is not NA continue
       if(cont){     
         for(i in 1:dim(x.lbls)[2]){
           eval.js(paste("dat$",names(x.lbls)[i], "=as.vector(mapply(rep,x=x.lbls[,i], MoreArgs=list(times=length(y.image))))", sep=""))
         }
       }
       # y specific data
       cont = TRUE
       y.lbls = as.data.frame(y.lbls)
       if( (dim(y.lbls)[1]==1) & (dim(y.lbls)[2]==1)){
         if(is.na(y.lbls[1,1])) cont = FALSE
       }
       # if y.lbls is not NA continue
       if(cont){     
         for(i in 1:dim(y.lbls)[2]){
           eval.js(paste("dat$",names(y.lbls)[i], "=rep(y.lbls[,i],length(x.image))", sep=""))
         }
       }
      
       cont = TRUE
       if(is.na(xy.lbls[1])) cont = FALSE
       # if xy.lbls is not NA continue
       if(cont){
         for(i in 1:length(xy.lbls)){
           #eval.js(paste("dat$",names(xy.lbls)[i],"=as.vector(xy.lbls[i]$",names(xy.lbls)[i],")", sep=""))
           eval.js(paste("dat$",names(xy.lbls)[i],"=as.vector(xy.lbls[[i]])", sep=""))
         }
       }
       
     }#end if image

    if(header!="v1" &  header!="v2") header="v2"
    
    # mapfile header info
    if(header=="v2") data(v2.header)
    if(header=="v1") data(v1.header)

    
    # begin html file
    sink(paste(dir,fname.root,".html",sep="")) 
    # add header information
    for(i in 1:length(sp.header)) cat(sp.header[i],fill=TRUE)
    # add data point info
    for(i in 1:(dim(dat)[1])){
      if(header=="v1"){
        ctmp=paste("<area shape=\"circle\" coords=\"",dat$pix.x[i],",",dat$pix.y[i],
          ",",spot.radius,"\" onmouseover=\"setData(\'",z.value,"&nbsp;&nbsp;:&nbsp;",
          as.character(dat[i,3]),sep="")
        
        if(dim(dat)[2]>3){
          for(j in 4:(dim(dat)[2])){
            ctmp = paste(ctmp, "<br> ",names(dat)[j],"&amp;nbsp;&amp;nbsp;:&amp;nbsp;",
              as.character(dat[i,j]),sep="")
          }
        }
        ctmp = paste(ctmp, "\')\" onMouseOut=\"clearData();\" />",sep="")          
        
      }# end if header==v1
      if(header=="v2"){
        ctmp=paste("<area shape=\"circle\" coords=\"",dat$pix.x[i],",",dat$pix.y[i],
          ",",spot.radius,"\" onmouseover=\"Tip(\'",z.value,"&nbsp;&nbsp;:&nbsp;",
          as.character(dat[i,3]),sep="")
        if(dim(dat)[2]>3){
          for(j in 4:(dim(dat)[2])){
            ctmp = paste(ctmp, "<br> ",names(dat)[j],"&amp;nbsp;&amp;nbsp;:&amp;nbsp;",
              as.character(dat[i,j]),sep="")
          }
        }
        ctmp = paste(ctmp, "\')\"  />",sep="")   
      }# end if header==v2
      
      cat(ctmp,fill=TRUE)
    }
    
    cat("</map>",fill=TRUE)
    cat("<div class=\"plot\">",fill=TRUE)
    if(header=="v1")cat("<img border=\"0\" src=\"",fname.png,"\" usemap=\"img-map\" />",sep="",fill=TRUE)
    if(header=="v2")cat("<img border=\"0\" src=\"",fname.png,"\" usemap=\"#img-map\" />",sep="",fill=TRUE)
    cat("</div>",fill=TRUE)
    cat("</body>",fill=TRUE)
    cat("</html>",fill=TRUE)
    
    sink()

  }# end if bound.pt  
  
}# end sendHeat function


