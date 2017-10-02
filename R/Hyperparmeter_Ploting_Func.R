##################################
## Hyperparmeters Effects Plots ##
##################################

## Function is to Plot Hyperparameter Effect Using a HeatMap Raster/Scatter Plot
hyperplot <- function(data,x,y,z,dep.learn,lab.x,lab.y,lab.z,x.breaks,xlim,y.breaks,ylim,z.breaks){
  set.seed(101,kind = "L'Ecuyer")
  ## Load Libraries  ##
  library(ggplot2,quietly = T)
  library(colormap,quietly = T)
  library(dplyr,quietly = T)
  library(mlr,quietly = T)
  library(checkmate,quietly = T)
  
  ## Manage conditions & flags  ##
  # assertFlag(x.breaks.int)
  # assertFlag(y.breaks.int)
  
  #partial.flag = hyperpars.effect.data$partial
  # assert(checkClass(dep.learn, "Learner"), checkString(dep.learn), 
  #        checkNull(dep.learn))
  # z.flag = is.null(z)
  
  # ## Init. The Stopping critireon 
  # if (length(x) > 1 || length(y) > 1 || length(z) > 1 ) 
  #   stopf("Greater than 1 length x, y, z or facet not yet supported")
  # 
  # if (interpolate == TRUE  && is.null(dep.learn)) 
  #   stopf("Partial dependence requested but dep.learn not specified!")
  # 
  
  ## Prepare Data for Ploting ##
  
  # Filter Inputed Opt Dataframe Basd On Selected xy or xyz  Dendepening on the Problem #
  d = data[, c(x, y, z, "prop.type","dob")]

    # Creat a Grid of 10 for each axes
    # xo = seq(min(d[, x]), max(d[, x]), length.out = 10L)
    # yo = seq(min(d[, y]), max(d[, y]), length.out = 10L)
    xo = seq(xlim[[1]], xlim[[2]], length.out = 10L)
    yo = seq(ylim[[1]], ylim[[2]], length.out = 10L)
  
    grid = expand.grid(xo,yo,KEEP.OUT.ATTRS = F)
    names(grid) <- c(x,y)
    
    ## Interplote input Data ##
    # Init a regr task from the initial supplied dataset
    regr.task = makeRegrTask(id = "interp", data = data[, c(x,y,z)], target = z)
    mod = train(dep.learn, regr.task)
    #Perform Prediction on the Grid
    prediction = predict(mod, newdata = grid)
    # Merge perdictions as z column in the previeasily created Grid
    grid[, z] = prediction$data[, prediction$predict.type]
    # Add Point Status & iteration Number to the grid Data
    grid$prop.type = "Interpolated Point"
    grid$dob = NA
    # Bind the filtered Initial dataframe with grid Dataframe
    grid = base::rbind(data[, c(x, y, z, "prop.type","dob")], grid)
    # Fix grid Upper/Lower bounds to be as filtered Initial dataframe Upper/Lower Bounds
    ## only for logloss
    grid[grid[, z] > 1, z] = 1
    #grid[grid[, z] > max(d[, z]), z] = max(d[, z])
    grid[grid[, z] < min(d[, z]), z] = min(d[, z])
    
    # grid[grid[, x] < min(d[, x]), x] = min(d[, x])
    # grid[grid[, x] > max(d[, x]), x] = max(d[, x])
    # grid[grid[, y] < min(d[, z]), y] = min(d[, y])
    # grid[grid[, y] > max(d[, z]), y] = max(d[, y])
  

  ## Init.Ploting Functions
  
    heat.theme <- {list(ggplot2::theme_bw() +
                          theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
                                text = element_text(),
                                panel.grid.major = element_blank(),
                                panel.grid.minor = element_blank(),
                                panel.background = element_rect(fill = "white", colour = "black",size = 0.25,linetype = "solid"),
                                panel.border = element_rect(fill = NA,colour = "black",size = 0.25,linetype = "solid"),
                                axis.line = element_line(colour = "black",size = 0.25),
                                axis.title = element_text(face = "bold",size = rel(1)),
                                axis.title.y = element_text(face = "bold.italic",angle=90,vjust =0.2),
                                axis.title.x = element_text(face = "bold.italic",vjust = -0.2),
                                axis.text = element_text(), 
                                axis.ticks = element_line(),
                                #legend.background = element_rect(fill="lightblue",size=0.5, linetype="solid", colour ="darkblue"),
                                #legend.background = element_rect(color = "black",linetype = 1,size = 0.25),
                                #legend.box.background = element_rect(color = "black",linetype = 1,size = 0.25),
                                legend.title = element_text(face = "italic"),
                                #legend.title.align = 0.5,
                                legend.position = "bottom",
                                legend.justification = c("centre"),
                                legend.direction = "horizontal",
                                legend.box.just = "bottom",
                                #legend.box.margin = ggplot2::margin(r = 0 ,l= 0,t = 0,b = 0,unit = "pt"),
                                legend.box = "horizontal",
                                #plot.margin = margin(2.5, 2.5, 2.5, 2.5, "pt"),
                                 legend.margin = ggplot2::margin(r = 0,l = 0,t = -5.5,b = 0,unit = "pt")),
                        guides(fill = guide_colorbar(order = 2,ticks = T,barwidth = rel(12),barheight = rel(0.6),direction = "horizontal",
                                                     title.theme = element_text(size=rel(9),face="italic",angle = 0),
                                                     title.position = "top",
                                                     draw.ulim = T,draw.llim = T,title.hjust = 0.5),
                               shape=guide_legend(order = 1,title.theme = element_text(size=rel(9),face="italic",angle = 0),
                                                  override.aes = list(size=rel(3.5)),
                                                  title.position = "left",title.hjust = 0.5,
                                                  direction = "vertical"
                                                  ),
                               color=guide_legend(order = 1,title.theme = element_text(size=rel(9),face="italic",angle = 0),
                                                  override.aes = list(size=rel(3.5)),
                                                  title.position = "left",title.hjust = 0.5,
                                                  direction = "vertical"
                                                  )))
     }
    

    zlimit <- c(min(grid[grid$prop.type == "Interpolated Point", z]),max(grid[grid$prop.type == "Interpolated Point", z]))
    axis.x <- list(scale_x_continuous(expand=c(0,0),limits =c(xlim[[1]],xlim[[2]]),breaks=scales::extended_breaks(n = x.breaks)))
    axis.y <- list(scale_y_continuous(expand=c(0,0),limits =c(ylim[[1]],ylim[[2]]),breaks=scales::extended_breaks(n = y.breaks)))
    
  ### Ploting Based On user Choices ##

    ## Produce The Final HeatObject ##
    plt = ggplot(data = grid[grid$prop.type == "Interpolated Point", ],aes_string(x = x, y = y, fill = z, z = z,frame="dob")) 
    
    plt <- plt + geom_raster(interpolate = T,hjust = 0,vjust = 0) + 
      geom_point(data = grid[grid$prop.type != "Interpolated Point", ],
                 aes_string(shape = "prop.type", color = "prop.type"),fill="grey80",size=rel(1.2)) +
      colormap::scale_fill_colormap(colormap = "rainbow",reverse = F,breaks=scales::extended_breaks(n = z.breaks)) +
      axis.x + axis.y +
      scale_color_manual(labels = c("Infill Proposal","Init Design"),values = c("#fc8d62",'#56B4E9')) +
      scale_shape_manual(labels = c("Infill Proposal","Init Design"),values = c(16,17)) +
      heat.theme +
      labs(x = lab.x, y = lab.y,fill=lab.z,shape="Point proposal type",color="Point proposal type") 

  return(plt)
}