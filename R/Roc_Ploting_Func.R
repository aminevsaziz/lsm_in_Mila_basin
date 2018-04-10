############################
## Roc Ploting Function ##
###########################

## Roc Ploting Function for All Learners at The Same Plot

Roc.Plot.Stacked <- function(Learners){
  library(dplyr,quietly = T)
  library(ggplot2,quietly = T)
  library(mlr,quietly = T)
  library(ggalt,quietly = T)
  ## metrics to Export Later With ROC Curves
  #Roc_Metrics <- list(fpr,tpr,acc,auc,f1,mmce,gmean,gpr,tp,tn,fp,fn,tnr,fnr)
  Roc_Metrics <- list(setAggregation(fpr, testgroup.mean),setAggregation(tpr, testgroup.mean),
                      setAggregation(auc, testgroup.mean),setAggregation(tnr, testgroup.mean),
                      setAggregation(fnr, testgroup.mean))
  
  Color_Palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73","#999999", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  
  Threshold_Perf_Data <- dplyr::bind_rows(lapply(Learners,function(x){
    mlr::generateThreshVsPerfData(x,measures = Roc_Metrics,gridsize=25L)$data %>%
      dplyr::mutate(.,Learner.id=(gsub("\\.preproc|\\.tuned|\\.dummied",replacement = "",x = x$learner.id) 
) ) }))
  
  LTP <- c("solid", "dashed", "dotted", "4C88C488", "12345678","longdash", "twodash")
  Linetype_Palette <- rep(LTP,length.out=length(Learners))
  #Size_Palette <- rep(ggplot2::rel(0.50),length.out=length(Learners))
  Shape_Palette <- rep(c(4,15,8,18,13,20),length.out=length(Learners))
  
  ## Init The Default Scale_Labels Learner.id Column in Threshold_Perf_Data DF ##
  Scale_Labels <- unique(paste(sort(Threshold_Perf_Data$Learner.id,decreasing = F)))
  
  ## Init The Default Scale_Labels_With_AUC from Learner.id  & auc columns in Threshold_Perf_Data DF ##
  Scale_Labels_With_AUC <- unique(paste(sort(Threshold_Perf_Data$Learner.id,decreasing = F),"AUC =",round(Threshold_Perf_Data[order(Threshold_Perf_Data$Learner.id,decreasing = F),"auc"],digits = 4)))
  
  gg <- ggplot(data = Threshold_Perf_Data ,
               aes(x=fpr,y=tpr,colour =Learner.id,
                   #fill=Learner.id,
                   linetype=Learner.id,
                   #shape =Learner.id,
                   group=Learner.id
                   ))
  gg <- gg + ggalt::geom_xspline(size=rel(0.5)) +
    #geom_line(size=0.7) +
    #geom_point(size=0.7) +
    geom_point(position = "jitter",size=0.6) +
    #Theme_Pub_Light +
    geom_abline(intercept = 0,slope = 1,linetype="longdash",colour = "gray75", size = 0.4) +

    ## Fix Up Axes Limits to Envlope All Data ##
    scale_x_continuous(limits=c(0,1)) +
    scale_y_continuous(limits=c(0,1)) +

    ## Override The Default Aesthetic Passed By "Learner.id" to a More Pleasing One ##
    scale_color_manual(values = Color_Palette,labels=Scale_Labels_With_AUC) +
    #scale_linetype_manual(labels=Scale_Labels_With_AUC) +
    scale_linetype_manual(values = Linetype_Palette,labels=Scale_Labels_With_AUC) +
    #scale_size_manual(values = Size_Palette,labels=Scale_Labels_With_AUC) +
    #scale_shape_manual(values = Shape_Palette,labels=Scale_Labels_With_AUC) +
    
    ## Specify The labels for Axis & The Diffrent Aesthetic Used ##
    labs(x = "False Postive Rate", y = "True Positive Rate",
         color="",shape="",linetype=""
         #fill="",size=""
         ) + 
    ggplot2::theme_bw() +
    
    ## Enhancing The The default Theme to be More Appealing
    theme(plot.title = element_text(face = "bold",size = rel(1.2), hjust = 0.5),
          text = element_text(),
          panel.grid.major = element_line(linetype=2,colour = "gray90", size = 0.0625),
          panel.grid.minor = element_line(linetype="4C88C488",colour = "gray90", size = 0.0625),
          #panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(fill = NA,colour = "black",size = 0.25,linetype = "solid"),
          axis.line = element_line(colour = "black",size = 0.25),
          #axis.title = element_text(face = "bold.italic"),
          axis.title = element_text(face = "bold.italic",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(vjust = -0.2),
          axis.text = element_text(), 
          axis.ticks = element_line(),
          legend.title = element_blank(),
          legend.background = element_rect(fill = "gray95",color = "grey80",size = 0.0625,linetype = "solid"),
          legend.key = element_rect(fill = NA,color = NA), ## For an Empty Legend Key That blend with The Overall Legend Fill ##
          #legend.text = element_text(size = rel(0.85)),
          legend.box.margin = ggplot2::margin(r = 0.125 ,l = 0.125,t = 0,b = 0,unit = "pt"),
          
          ## For Placing The Legend In the Bottom right Corner ##
          legend.position = c(.99, .01),
          legend.justification = c("right", "bottom"),
          legend.box.just = "right") +
    
    ## To Make Sure dashed Lines & Double dashed line Patterns Appears in The legend Keys ##
    guides(linetype = guide_legend(override.aes = list(size = 0.55)))
    
    
    
  
  
  
  
  
  
  
  
  
    # #scale_colour_gradient2(low="#ef8a62", mid="#ffffff", high="#999999",midpoint = 0.5) +
    # labs(x = "False Postive Rate", y = "True Positive Rate",
    #      color="",shape="",
    #      fill="",linetype="") +
    # 
    # theme(
    #   legend.position = c(.95, .05),
    #   legend.justification = c("right", "bottom"),
    #   legend.box.just = "right",
    #   panel.grid.minor = element_line(size = 0.125,linetype = 3),
    #   panel.grid.major = element_line(size = 0.25,linetype = 4),
    #   legend.background = element_rect(fill = "grey95",color = "grey80"),
    #   legend.title = element_blank())
  
}
