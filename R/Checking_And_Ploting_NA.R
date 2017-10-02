###################################
## Checking & Ploting NA Values ##
##################################

Na_Plot <- function(Input.Data){
  
  library(dplyr,quietly = T)
  library(ggplot2,quietly = T)
  library(reshape2,quietly = T)
  
  ## Preparing The Data For The Plotting Function ##
  Input.Data %>% ## Loading Data
    
    is.na(.) %>% ## Checking for NA. in The DataSet
    
    reshape2::melt(.) ->  Na.Pool ## Metling Prevouis Data Into a Pool
  
  gg <- ggplot(data= Na.Pool ,mapping = aes_string(x="Var2",y="Var1",fill= "value"))
  
  ## Init. Plot Limits for Clean Plots ##
  ylimit <- layer_scales(gg)$y$range$range
  
  gg <- gg + 
    geom_tile() +
    #geom_raster(hjust = 0, vjust = 0 ) +
    #facet_wrap(~Data.set, nrow =2 ,ncol =3 )' +
    scale_fill_manual( values =  c("FALSE"='#56B4E9',"TRUE"='#CC79A7') ,labels = c("Present","Missing"),
                       name = "Status",drop = F ) +
    scale_y_continuous(breaks = scales::extended_breaks(n = 10),expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    
    ggplot2::theme_light() + theme(axis.text.x  = element_text(angle=45, vjust=1,hjust =1),
                                   axis.text.y  = element_text(angle=0, vjust=0.5,hjust = 0.5),
                                   axis.line = element_line(colour = "gray50",size = 0.125),
                                   axis.ticks = element_line(colour = "gray50"),
                                   panel.grid.major = element_blank(),
                                   panel.grid.minor=element_blank(),
                                   panel.background = element_rect(size = 0.125,colour = "gray50",fill = NA),
                                   legend.position = c("bottom"),
                                   legend.justification = c("center"),
                                   legend.box.just = "right",
                                   legend.direction = "horizontal" ,
                                   axis.title = element_text(face = "bold.italic"),
                                   legend.title = element_text(face = "bold.italic")) +

    labs(x = "Variables",y = "Number Of Observations")
  
  return(gg)
} ## Plot Dataset Missing Values