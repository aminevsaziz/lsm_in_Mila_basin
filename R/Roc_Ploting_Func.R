############################
## Roc Ploting Function ##
###########################

## Roc Ploting Function for Individual Rocs With Threshold Gradient

Roc.Plot.Single <- function(learner){
  Roc_Metrics <- list(fpr,tpr,acc,auc,f1,mmce,gmean,gpr,tp,tn,fp,fn,tnr,fnr) ## metrics to Export Later With ROC Curves
  Threshold_Perf_Data <- dplyr::bind_rows(lapply(learners,function(x){
    mlr::generateThreshVsPerfData(x,measures = Roc_Metrics)$data %>%
      dplyr::mutate(.,learner.id=(substring(x$learner.id,1,3)) ) }))
  
  gg <- ggplot(data = j ,aes(x=fpr,y=tpr))
  gg <- gg + geom_line(color="black",size=1.1,linetype="solid") +
    geom_point(aes(color=threshold),size=2.5,position = "jitter") +
    geom_abline(intercept = 0,slope = 1,linetype="longdash",colour = "gray75", size = 0.4) +
    scale_x_continuous("False Positive Rate", limits=c(0,1)) +
    scale_y_continuous("True Positive Rate", limits=c(0,1)) +
    #scale_colour_viridis(discrete = T) +
    
    scale_colour_gradient2(low="#ef8a62", mid="#ffffff", high="#999999",midpoint = 0.5) +
    labs(x = "False Postive Rate", y = "True Positive Rate", colour = "Cut-Off \n") +
    
    theme(legend.position=c(0.99,0),
          legend.justification=c(0.98,-0.05), ## use c(1,-0.05) for Defaults & c(0.99,-0.05) for 5X5in
          legend.direction = "vertical" ,
          legend.key.height = unit(0.4,"cm"), ## Use 0.3cm for 4X4inch Plot & 0.4cm for 5X5in
          legend.key.width = unit(0.35,"cm") ## Use 0.3cm for 4X4inch Plot & 0.3cm for 5X5in
    ) +
    theme(panel.grid.minor = element_line(size = 0.125,linetype = "solid"),
          panel.grid.major = element_line(size = 0.25))
}

## Roc Ploting Function for All Learners at The Same Plot

Roc.Plot.Stacked <- function(Learners){
  library(dplyr,quietly = T)
  library(ggplot2,quietly = T)
  library(mlr,quietly = T)
  library(ggalt,quietly = T)
  ## metrics to Export Later With ROC Curves
  Roc_Metrics <- list(fpr,tpr,acc,auc,f1,mmce,gmean,gpr,tp,tn,fp,fn,tnr,fnr)
  
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
  Scale_Labels_With_AUC <- unique(paste(sort(Threshold_Perf_Data$Learner.id,decreasing = F),"AUC =",round(Threshold_Perf_Data[order(Threshold_Perf_Data$Learner.id,decreasing = F),"auc"],digits = 3)))
  
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

## Roc Ploting Function For One Learner on The ##
## Foreground & Rest on The Background ##

Roc.Plot.Comaprison <- function(Learners,Main.Learner,Main.Learner.Color,Other.Learners.Color){
  library(dplyr,quietly = T)
  library(ggplot2,quietly = T)
  library(mlr,quietly = T)
  library(ggalt,quietly = T)
  
  ## metrics to Export Later With ROC Curves
  Roc_Metrics <- list(mlr::fpr,mlr::tpr,mlr::acc,mlr::auc,mlr::f1,mlr::mmce,mlr::gmean,mlr::gpr,mlr::tp,mlr::tn,mlr::fp,mlr::fn,mlr::tnr,mlr::fnr) 
  
  ## Generate threshold Performance Data for The ##
  ## Passed List of Learners in Learners Argument ##
  Threshold_Perf_Data <- dplyr::bind_rows(lapply(Learners,function(x){
    mlr::generateThreshVsPerfData(x,measures = Roc_Metrics,gridsize = 20L)$data %>%
      dplyr::mutate(.,Learner.id= x$learner.id )} ))
  
  ## Getting Rid of The ".tuned" or ".preproc" or Even ."dummied" Suffixes ## 
  Threshold_Perf_Data$Learner.id <- gsub("\\.preproc|\\.tuned|\\.dummied",replacement = "",x = Threshold_Perf_Data$Learner.id) 
  
  ## Init The Main_Learner_Id So to be Later Used in Alternating Color Scale Manually
  Main_Learner_Id <- gsub("\\.preproc|\\.tuned|\\.dummied",replacement = "",x = Main.Learner$learner.id) 
  
  ## Init The Default Color_Palette Equal to Length of Supplied List of Learners
  Color_Palette <- rep(Other.Learners.Color,length.out=length(Learners))
  
  ## Override The Default Color_Palette With The Desired Main.Learner.Color using ##
  ## Main_Learner_id as pattern in sorted alphabetecally uquique learners Ids to ##
  ## obtain its Index & change it in Color_Palette ##
  ## Remarque: The sorting Alphabetically was because ##
  ## ggplot2 Handle character Variables in Acending Alphabetic Order ##
  Color_Palette[grep(Main_Learner_Id,x = sort(unique(Threshold_Perf_Data$Learner.id),decreasing = F),value = F)] <- Main.Learner.Color
  
  ## Init The Default Linetype_Palette Equal to Length of Supplied List of Learners ##
  ## While The Palette is missing Solid Line Type (1) This is because we Want The Main ##
  ## Learner To be a Solid Line Type to Ensure Contrast ##
  LTP <- c("solid", "dashed", "dotted", "4C88C488", "12345678","longdash", "twodash")
  Linetype_Palette <- rep(LTP,length.out=length(Learners))
  
  ## Override The Default Linetype_Palette With a Solid Linetype ##
  ##Specifically For Main.Learner in Same Manner as Color_Palette Above ##
  Linetype_Palette[grep(Main_Learner_Id,x = sort(unique(Threshold_Perf_Data$Learner.id),decreasing = F),value = F)] <- "solid"
  
  ## Init The Default Scale_Labels Learner.id Column in Threshold_Perf_Data DF ##
  Scale_Labels <- unique(paste(sort(Threshold_Perf_Data$Learner.id,decreasing = F)))
  
  ## Init The Default Scale_Labels_With_AUC from Learner.id  & auc columns in Threshold_Perf_Data DF ##
  Scale_Labels_With_AUC <- unique(paste(sort(Threshold_Perf_Data$Learner.id,decreasing = F),"AUC =",round(Threshold_Perf_Data[order(Threshold_Perf_Data$Learner.id,decreasing = F),"auc"],digits = 3)))
  
  ## Override The Default Scale_Labels With Scale_Labels_With_AUC Only ##
  ## in The desired Main.Learner Index ## 
  Scale_Labels[grep(Main_Learner_Id,x = sort(Scale_Labels,decreasing = F),value = T)] <- Scale_Labels_With_AUC[grep(Main_Learner_Id,x = sort(Scale_Labels,decreasing = F))]
  
  ## Init The Default Size_Palette & Shape_Palette based "Learner.id" Column in Threshold_Perf_Data DF ##
  
  Size_Palette <- rep(ggplot2::rel(0.50),length.out=length(Learners))
  Shape_Palette <- rep(c(4,15,8,18,13,20),length.out=length(Learners))
  
  ## Override The Default Size_Palette & Shape_Palette With 1 & 20 Only ##
  ## in The desired Main.Learner Index ## 
  Size_Palette[grep(Main_Learner_Id,x = sort(unique(Threshold_Perf_Data$Learner.id),decreasing = F))] <- ggplot2::rel(0.8)
  Shape_Palette[grep(Main_Learner_Id,x = sort(unique(Threshold_Perf_Data$Learner.id),decreasing = F))] <- 20
  
  
  ## init. The Initial GGobject design with Desired Aesthetic ##
  gg <- ggplot(data = Threshold_Perf_Data,aes_string(x="fpr",y="tpr",linetype="Learner.id",colour="Learner.id",size="Learner.id",shape="Learner.id"))
  
  ## specify The desired Geoms Necessary For The Plot ##
  gg <- gg + ggalt::geom_xspline(spline_shape = 1) + #geom_point() +
    geom_abline(intercept = 0,slope = 1,linetype="longdash",colour = "gray80", size = 0.4) +
    
    ## Fix Up Axes Limits to Envlope All Data ##
    scale_x_continuous(limits=c(0,1)) +
    scale_y_continuous(limits=c(0,1)) +
    
    ## Override The Default Aesthetic Passed By "Learner.id" to a More Pleasing One ##
    scale_color_manual(values = Color_Palette,labels=Scale_Labels) +
    scale_linetype_manual(values = Linetype_Palette,labels=Scale_Labels) +
    scale_size_manual(values = Size_Palette,labels=Scale_Labels) +
    scale_shape_manual(values = Shape_Palette,labels=Scale_Labels) +
    
    ## Specify The labels for Axis & The Diffrent Aesthetic Used ##
    labs(x = "False Postive Rate", y = "True Positive Rate",
         color="",shape="",
         fill="",linetype="",size="") + 
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
          axis.title = element_text(face = "bold",size = rel(1)),
          axis.title.y = element_text(angle=90,vjust =2),
          axis.title.x = element_text(face = "bold",vjust = -0.2),
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
    guides(linetype = guide_legend(override.aes = list(size = 0.35)))
  
  #print(gg)
  #return(gg)
}
