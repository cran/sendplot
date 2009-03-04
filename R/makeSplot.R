#
# function to make plots off of Splot object
#



makeSplot <- function(Splot,
                      fname.root="Splot",
                      dir="./",
                      overwriteSourcePlot = NA,
                      makeInteractive=TRUE,
                      overrideInteractive=NA,
                      Default=TRUE,
                      header="v3",
                      window.size = "800x1100", # in px
                      returnObj = FALSE,
                      getLims=FALSE  # used internally
                      ){

  # set up file names
  fname.ps=paste(dir,fname.root,".ps",sep="")
  fname.png=paste(dir,fname.root,".png",sep="")
  fname.jpeg=paste(dir,fname.root,".jpeg",sep="")
  fname.html=paste(dir,fname.root,".html",sep="")
  
  #
  # create static image
  #

  # overwriteSourcePlot is used instead of what is set in Splot object unless not acceptbale type or NA
  # if of not acceptable type 
  if( !is.na(overwriteSourcePlot) & (overwriteSourcePlot != "ps") & (overwriteSourcePlot != "png") & (overwriteSourcePlot != "jpeg") ){
    overwriteSourcePlot = NA
  }else{
    source.plot = overwriteSourcePlot
  }  
  if(is.na(overwriteSourcePlot))  source.plot = Splot$source.plot
  
  # to get bounging values, primitive does not work with postscript
  # temporarily change to png 
  if(getLims) {
    if( (source.plot != "png") & (source.plot != "jpeg")) source.plot = "png"
  }

  # begin png file if flagged
  if(source.plot=="png"){
    wi = strsplit(Splot$image.size, "x")[[1]][1]
    hi = strsplit(Splot$image.size, "x")[[1]][2]
    png(file=fname.png, width=as.real(wi), height=as.real(hi), pointsize=Splot$pointsize, res=Splot$res)
  }
  # begin jpeg file if flagged
  if(source.plot=="jpeg"){
    wi = strsplit(Splot$image.size, "x")[[1]][1]
    hi = strsplit(Splot$image.size, "x")[[1]][2]
    jpeg(file=fname.jpeg, width=as.real(wi), height=as.real(hi), pointsize=Splot$pointsize, res=Splot$res)
  }
  # begin postscript file if flagged
  if(source.plot=="ps"){
    postscript(file=paste(dir,fname.ps,sep=""),paper=Splot$ps.paper,width=Splot$ps.width,height=Splot$ps.height,horizontal=FALSE, pointsize=Splot$pointsize)
  }

      
  Splot$plot.output = list()
  
  
  # initiate layout
  if(max(as.vector(Splot$mat),na.rm=TRUE)>1) nf = layout(Splot$mat, respect=TRUE)
  if(Splot$mai.prc) mai.def=par("mai")


  # initialize getLims 
  if(is.null(Splot$plot.lims)){
    plot.lims = list()
    plot.lims$xmins = rep(NA, length(Splot$plot.calls))
    plot.lims$xmaxs = rep(NA, length(Splot$plot.calls))
    plot.lims$ymins = rep(NA, length(Splot$plot.calls))
    plot.lims$ymaxs = rep(NA, length(Splot$plot.calls))
    Splot$plot.lims = plot.lims    
  }
 
  
  # loop over plot calls to place plots in order or 1:n in layout
  for(i in 1:length(Splot$plot.calls)){
    # set up plot margin
    if(length(Splot$mai.mat)>1){
      if(!Splot$mai.prc) par(mai=Splot$mai.mat[i,])
      if(Splot$mai.prc)  par(mai=Splot$mai.mat[i,]*mai.def)            
    }
    # evaluate plot call
    plt = eval.js(Splot$plot.calls[[i]])

    if(is.null(plt)) Splot$plot.output[i] = NA
    if(!is.null(plt)){
      Splot$plot.output[i] = NA
      class(Splot$plot.output[i]) = "list"
      Splot$plot.output[[i]] = plt
    }
    
    # cycle through all additional plot calls for current plot
    sub.np = length(Splot$plot.extras[[i]])
    for(sp in 1:sub.np){
      if(!is.na(Splot$plot.extras[[i]][[sp]]))
        eval.js(Splot$plot.extras[[i]][[sp]])
    }

    
    Splot$plot.lims$xmins[i] = par()$usr[1]
    Splot$plot.lims$xmaxs[i] = par()$usr[2]
    Splot$plot.lims$ymins[i] = par()$usr[3]
    Splot$plot.lims$ymaxs[i] = par()$usr[4]
    
    
       
  }# end for loop over plot calls





  
  
  # retrieve xlim/ylim information to add bounding points 
  #if(getLims){
    #Splot = getPlotsBounds(Splot)   
  #}






  

  # end device 
  dev.off()

           ##############################################################
           ##############################################################
           #
           # problems with converting from ps to png and having 
           #   interactive work correctly
           # look into this further
           #   
           ##############################################################
           ##############################################################
      

  
  # convert ps to png if ps was made and on unix OS
  if(source.plot=="ps" & Splot$platform=="unix"){
    
    system(paste("convert -size 800x1100 ",dir,fname.ps," -resize ",Splot$image.size," ",dir,fname.png,sep=""))
  }



  #
  # interactive webpage
  #
  
  if(makeInteractive){
    
    # check for interactive plots
    if(!is.na(overrideInteractive[1])){
      if(length(overrideInteractive) != Splot$nfig){
        cat(paste("Note: overrideInteractive is not of correct length\n       Length of overrideInteractve:", length(overrideInteractive),"\n       should be equal to the number of figures:", Splot$nfig, "\n       Continuing with originally set interactive plots \n"))
        overrideInteractive = NA            
      }
      if(class(overrideInteractive) != "logical"){
        cat(paste("Note: overrideInteractive is not correct \n       Must be Logical (T/F) vector of length:", Splot$nfig, "\n       Continuing with originally set interactive plots \n"))
        overrideInteractive = NA      
      }      
    }
    # if override is set use override otherwise use stored Iflag object
    if(is.na(overrideInteractive[1])) Ifig = which(Splot$Iflag)
    if(!is.na(overrideInteractive[1])) Ifig = which(overrideInteractive)

    # if there is at least one interactive plot
    if(length(Ifig) !=0){

      # checks to make sure at least one of the labelled interactive plots
      #  does have a mapping 
      Fsum = 0
      for(f in Ifig){
        Fsum = Fsum + length(Splot$iMap[[f]])
      }
      if(Fsum != 0){

     
           ##############################################################
           ##############################################################
           #
           # eventually need to run a check and combined
           #   duplicate coordinates
           #
           # print points before regions?? 
           #   
           ##############################################################
           ##############################################################
      
    
        #
        #  combined image mapping and make .html
        #


        # load header information for html file
        if(header!="v1" &  header!="v2" & header!="v3") header="v3"
        # mapfile header info
        if(header=="v2") data(v2.header)
        if(header=="v1") data(v1.header)
        
        # begin html file
        sink(fname.html)
        # add header information
        if(header=="v3"){
          w.wi = strsplit(window.size, "x")[[1]][1]
          w.hi = strsplit(window.size, "x")[[1]][2]

          cat(paste("<html>\n<head>\n    <title>sendplot</title>\n    <style type=\"text/css\">\n        /* CSS GOES HERE */\n        .plot {border:1px solid #CCCCCC;display:block;overflow:auto;padding:5px;\n               max-width:",w.wi,"px; max-height:",w.hi,"px;}\n    </style>\n</head>\n", sep=""))

          data(v3.header)
          
        }

        sp.header=sp.header
        
        for(i in 1:length(sp.header)) cat(sp.header[i],fill=TRUE)
   
        
        # loop over labelled interactive
        # writing to file 
        for(fi in Ifig){
          
          lenList = length(Splot$iMap[[fi]])
          if(lenList != 0){
            
            for(ll in 1:lenList){

              obj = Splot$iMap[[fi]][[ll]]
              DFs = makeCharacter(obj)
              iType = Splot$iType[[fi]][ll]
              if(header=="v1") writeToHTML1(obj, DFs, iType)
              if(header=="v2" | header=="v3") writeToHTML2(obj, DFs, iType)
              
            }
          }
        }

        if(!is.null(Splot$Default)){
          if(Default){
            if(header=="v1") writeDefault1(Splot)
            if(header=="v2" | header=="v3") writeDefault2(Splot)
          }
        }
        
        cat("</map>",fill=TRUE)
        cat("<div class=\"plot\">",fill=TRUE)
        if(source.plot=="jpeg"){

          image.name.jpeg=paste(fname.root,".jpeg",sep="")
 
          if(header=="v1")cat("<img border=\"0\" src=\"",image.name.jpeg,"\" usemap=\"img-map\" />",sep="",fill=TRUE)
          if(header=="v2" | header=="v3")cat("<img border=\"0\" src=\"",image.name.jpeg,"\" usemap=\"#img-map\" />",sep="",fill=TRUE)          
        }else{

          image.name.png=paste(fname.root,".png",sep="")
           
          if(header=="v1")cat("<img border=\"0\" src=\"",image.name.png,"\" usemap=\"img-map\" />",sep="",fill=TRUE)
          if(header=="v2" | header=="v3")cat("<img border=\"0\" src=\"",image.name.png,"\" usemap=\"#img-map\" />",sep="",fill=TRUE)
        }
        cat("</div>",fill=TRUE)
        cat("</body>",fill=TRUE)
        cat("</html>",fill=TRUE)
        
        sink()

        if(header=="v1") cat("Note: hyperlinks currenly only work with header=v2 \n")

        
      }else{
        cat("Note:  Plot[s] are labelled as interactive:\n       But none have been mapped\n       Please set using makeImap \n")
      }

    }else{
      cat("Note:  No plots are designated interactive \n       Please set using makeImap \n")
    }
    
  }

  
 
  if(getLims) returnObj = TRUE
  if(returnObj) return(Splot)
  
}




