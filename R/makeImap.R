#
# acts on Splot object
#   gives a single figure interactive mapping
#



makeImap <- function(Splot,
                     figure=1,

                     xy.type=NA, # points, image.midpoints, image.boundaries, image.box, circle, rect, polygon 
                     x.pos,
                     y.pos,

                     x.right.pos=NA,  # if xy.type is rect
                     y.bottom.pos=NA, # if xy.type is rect
                     spot.radius = 5, # can be a vector, each entry in x.pos, # if xy.type=circle/points
         
                     x.labels=NA,
                     y.labels=NA,
                     xy.labels=NA,
                     x.links=NA,
                     y.links=NA,
                     xy.links=NA,
                     asLinks=NA,
                     

                     font.type="Helvetica", # 'Arial, Helvetica, sans-serif'   
                     font.color="black",  # name, or #------
                     font.size="12",      # can specify type px,pt,em, etc.
                     bg.color="#D6E3F6",  # name or #------
                     
                     
                     fname.root="Splot",
                     dir="./",

                     automap=TRUE,
                     automap.method="mode",
                     
                     bb.clr=NA,
                     bb.cex=2,  # note: if image.size value is high this should be increase inorder for mapping to work
              
                     returnVl=TRUE,
                     saveFlag=FALSE,
                     saveName="Splot.RData"

                      ){



  # first make normal static plot
  #  if necessary, retrieving xlim/ylims for all plots
  #                stores for future reference b/c limits are static
  if(is.null(Splot$plot.lims)){
    Splot = makeSplot(Splot, fname.root=fname.root, dir=dir, makeInteractive=FALSE, getLims=TRUE, overwriteSourcePlot="png")
  }else{
    makeSplot(Splot, fname.root=fname.root, dir=dir, makeInteractive=FALSE, overwriteSourcePlot="png")
  }

  # make separate file with bound points
  # need to compare to original file 
  boundFileName = paste(fname.root, "Dot", sep="")

  

  ###############################
  ###############################
  #
  #  Start setting up automapping
  #
  ###############################
  ###############################

  if(automap){
  
    detected = FALSE

    # loops over given colors, or 
    # if NA try: blue, red, black, white, green
    if(is.na(bb.clr[1])) bb.clr =  colors()[c(26,552,24,1,254)]  
    Cl.idx = 1
  
    # while mapping doesn't work 
    while(!detected){
    
      # if there are more colors to try 
      if(Cl.idx <= length(bb.clr)){

        # make file with bound points (always png)
        addBounding(Splot, figure, boundFileName=boundFileName, dir=dir, bb.clr=bb.clr[Cl.idx], bb.cex= bb.cex)
        # try to automatically find bounding points 
        boundingPt = automapPts(Splot, fname.root=fname.root, boundFileName=boundFileName, dir=dir, automap.method=automap.method)

        # if worked end loop
        if(class(boundingPt) != "try-error") detected=TRUE
        # if didn't work print error and try different color 
        if(class(boundingPt) == "try-error"){
          cat("Warning: First attempt at automapping failed \n\n")
          cat(paste(boundingPt))
          cat("\n\n         Attempting with different boundingPt color \n")
          Cl.idx = Cl.idx + 1
        }
      # if there are no more colors to try print Error   
      }else{
        detected = TRUE
        cat("ERROR: Could not map correctly \n")
      }
    }
  }



  ###############################
  ###############################
  #
  #  Now NOT automap 
  #
  ###############################
  ###############################
 
  if(!automap){

           ##############################################################
           ##############################################################
           #
           #   add in manual detection of points
           #     in case of unknown mapping errors
           #     and for windows
           #     (windows cannot use automap because auto conversion
           #       of png to tif for rtiff image comparison)
           #   
           ##############################################################
           ##############################################################
    
  }

  
  ###############################
  ###############################
  #
  #  Make figure mapping 
  #
  ###############################
  ###############################
 

  # if automap performed correctly
  if(class(boundingPt) != "try-error"){


    # get limits for specific figure
    xlim = c(Splot$plot.lims$xmins[figure], Splot$plot.lims$xmaxs[figure])
    ylim = c(Splot$plot.lims$ymins[figure], Splot$plot.lims$ymaxs[figure])

    # check how x and y values should be treated
    #    xy directly
    #    cuts as cuts as in image
    
    if(is.na(xy.type)){
      cat("Note:  xy.type not specified\n       Continuning with x.pos and y.pos as points\n")
      xy.type="points"
    }
    if( (xy.type != "points") & (xy.type != "image.midpoints") & (xy.type != "image.boundaries") & (xy.type != "image.box") &  (xy.type != "circle") &  (xy.type != "rect") &  (xy.type != "polygon") ){
      cat("Note:  xy.type is not acceptable \n       Continuning with x.pos and y.pos as points\n")
      xy.type="points"
    }


    
    # send to makeDF function for creation and alteration of data matrix entries 

    if( (xy.type=="points") | (xy.type=="circle") )  MapObj = makeScatterDF(Splot=Splot, xlim= xlim, ylim=ylim, x.pos=x.pos, y.pos=y.pos,boundingPt=boundingPt, x.labels=x.labels, y.labels=y.labels, xy.labels=xy.labels, x.links=x.links, y.links=y.links, xy.links=xy.links, asLinks=asLinks)

    if( (xy.type=="image.midpoints") | (xy.type=="image.boundaries") |  (xy.type=="image.box"))  MapObj = makeImageDF(Splot=Splot, xy.type=xy.type, xlim= xlim, ylim=ylim,x.pos=x.pos, y.pos=y.pos,boundingPt=boundingPt, x.labels=x.labels, y.labels=y.labels, xy.labels=xy.labels, x.links=x.links, y.links=y.links, xy.links=xy.links, asLinks=asLinks)

    if( xy.type=="rect")  MapObj = makeRectDF(Splot=Splot, xlim= xlim, ylim=ylim,x.left=x.pos, y.top=y.pos, x.right=x.right.pos, y.bottom=y.bottom.pos,  boundingPt=boundingPt, x.labels=x.labels, y.labels=y.labels, xy.labels=xy.labels, x.links=x.links, y.links=y.links, xy.links=xy.links, asLinks=asLinks)

    if( xy.type=="polygon" )  MapObj = makePolyDF(Splot=Splot, xlim= xlim, ylim=ylim, x.pos=x.pos, y.pos=y.pos,boundingPt=boundingPt, x.labels=x.labels, y.labels=y.labels, xy.labels=xy.labels, x.links=x.links, y.links=y.links, xy.links=xy.links, asLinks=asLinks)

    
    if(length(MapObj) != 1){

      if((xy.type=="points") | (xy.type=="circle")){
        if(length(spot.radius) == 1) spot.radius = rep(spot.radius, length(x.pos))
        if(length(spot.radius) < length(x.pos))  spot.radius = c(spot.radius, rep(5, (length(x.pos)-length(spot.radius))))
      }
      if(xy.type=="image.midpoints"){
        len = length(x.pos)*length(y.pos)
        if(length(spot.radius) == 1) spot.radius = rep(spot.radius, len)
        if(length(spot.radius) < len)  spot.radius = c(spot.radius, rep(5, (len-length(spot.radius))))
      }
      if(xy.type=="image.boundaries"){
        len = (length(x.pos)-1)*(length(y.pos)-1)
        if(length(spot.radius) == 1) spot.radius = rep(spot.radius, len)
        if(length(spot.radius) < len)  spot.radius = c(spot.radius, rep(5, (len-length(spot.radius))))
      }

      MapObj$spot.radius = spot.radius

      # add display information
      MapObj$font.type  = font.type
      MapObj$font.color = font.color
      MapObj$font.size = font.size
      MapObj$bg.color = bg.color
                   
      
      
      # add MapObj to Splot
      if(length(Splot$iMap[[figure]]) == 0){
        eval.js(paste("Splot$iMaps$Figure",figure,"$MapObj1 = MapObj", sep=""))
      }else{
        len = length(Splot$iMap[[figure]])
        eval.js(paste("Splot$iMaps$Figure",figure,"$MapObj",len+1," = MapObj", sep=""))
      }
      
    
      # add iType to Splot
      if(length(Splot$iTypes[[figure]]) == 0){
        eval.js(paste("Splot$iTypes$Figure",figure," = MapObj$xy.type", sep=""))
      }else{
        len = length(Splot$iTypes[[figure]])
        eval.js(paste("Splot$iTypes$Figure",figure,"[len+1] =MapObj$xy.type", sep=""))
                                        #c(Splot$iTypes$Figure",figure,"[[1]] ,MapObj$xy.type)", sep=""))
      }
  
  
      # update Iflag
      if(!Splot$Iflag[figure]){
        cat("Note: Figure currently not labelled as interactive \n      Changing to Interactive now \n")
        Splot$Iflag[figure]=TRUE
      }
    }


    
    
  }else{# end if automap worked if(class(boundingPt) !="try-error")

    cat("ERROR: Could not automap points \n       Mapping NOT performed \n       Please try again using different bb.cex/bb.clr settings \n       Or using manual mapping \n")
    
  }

  
  
    
  #########################################
  #########################################
  #
  #  save  mapping and save/return object
  #
  #########################################
  #########################################

  

  # save and return
  if(saveFlag) save(Splot, file=saveName, compress=TRUE)
  if(returnVl) return(Splot)


}
