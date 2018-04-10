#########################################
## Variance Inflation Factor Function ##
########################################

Vif_Plot <- function(Input.Data,form,lim,xlab,ylab){
  library(stats,quietly = T)
  library(dplyr,quietly = T)
  library(ggplot2,quietly = T)
  #library(car,quietly = T)
  
  ## Preparing Data For Ploting and Getting rid of Indisred Columns ##
  ## like df and 'GVIF' and  Rename `GVIF^(1/(2*Df))` into 'Var_Vif' ##
  
  stats::glm(formula = Formula::Formula(form), data=Input.Data,family = "binomial") %>%
    car::vif(.) %>% as.data.frame(.) %>%
    dplyr::mutate(.,Variables=rownames(.),VIF=`GVIF^(1/(2*Df))`,
                  `GVIF^(1/(2*Df))`=NULL,Df=NULL,GVIF=NULL) -> Var_Vif
  #Var_Vif$Variables[order(Var_Vif$Variables)] <- Var.Labels
  ## init. The Initial GGobject design with Desired Aesthetic ##
  gg <- ggplot(Var_Vif, aes(y=reorder(Variables, VIF), x=VIF,label=round(VIF,2)))
  
  ## Init. Plot Limits for Clean Plots ##
  xlimit <- layer_scales(gg)$x$range$range
  #"#e3e2e1" grey
  #"#a3c4dc" light bleu
  #"#E69F00" orange
  #ef8a62
  ## specify The desired Geoms Necessary For The Plot ##
  gg <- gg + 
    geom_segment(aes(x = 0, y = Variables, xend = VIF, yend = Variables),size=0.7, color = "#a3c4dc",linetype="solid") +
    geom_point(stat='identity',shape=21,color="#FC4E07",fill="#FC4E07", size=rel(7.5))  +
    geom_text(color="white", size=rel(2.5)) +
    
    ## Fix Up Axes Limits to Envlope All Data ##
    scale_x_continuous(expand=c(0,0),breaks = scales::extended_breaks(n = 5),limits =c(lim[[1]],lim[[2]]) ) +
    #scale_x_continuous(labels = Var.labels) +
    
    ## Specify The labels for Axis & The Diffrent Aesthetic Used ##
    labs(x=xlab, y=ylab) +
    
    ## Enhancing The The default Theme to be More Appealing
    ggplot2::theme_bw() +
    theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          #panel.border = element_rect(fill = NA,colour = "black",size = 0.25,linetype = "solid"),
          panel.border = element_blank() ,
          
          axis.line = element_line(colour = "#999999",linetype = "solid",size = 0.125,arrow = arrow(length = unit(0.25, "cm"))),
          #axis.line.y = element_blank(),
          
          axis.title = element_text(face = "bold.italic",size = rel(1)),
          axis.title.y = element_text(face = "bold.italic",angle=90,vjust =-0.2),
          axis.title.x = element_text(face = "bold.italic",vjust = -0.2),
          axis.text = element_text(),
          axis.ticks = element_line(colour = "#999999"),
          axis.ticks.y = element_blank(),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "gray95",color = "grey80",size = 0.0625,linetype = "solid"),
          legend.key = element_rect(fill = NA,color = NA) ## For an Empty Legend Key That blend with The Overall Legend Fill ##
          #legend.text = element_text(size = rel(0.85)),
          #legend.box.margin = ggplot2::margin(r = 0 ,l = 0,t = 0,b = 0,unit = "pt")
    ) #+
  
  ## Print The Heatmap & Return Only The GGobject of The Heatmap
  #print(gg)
  #theme(axis.text.x = element_text(angle=-45,hjust = -0.5))+
  #coord_flip()
  return(gg)
}



