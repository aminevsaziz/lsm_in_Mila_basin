######################################
## Correlation Correlogram Function ##
######################################

## Plot The Correlogram Using "ggplot2"
Cor_Ggplot <- function(Input.Data){
  library(dplyr,quietly = T)
  library(stats,quietly = T)
  library(reshape2,quietly = T)
  library(ggplot2,quietly = T)
  
  # ## Creating the Correlation Matrix
  dplyr::select_if(Samples, is.numeric) %>%
    stats::cor(., y = NULL, use = "everything",method = "pearson") -> cormat
  
  ## Helper function to reorder the correlation matrix :
  reorder_cormat <- function(cormat){
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
  }
  
  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }
  
  # Reorder the correlation matrix
  cormat <- reorder_cormat(cormat)
  upper_tri <- get_upper_tri(cormat)
  
  # Melt the correlation matrix
  melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
  
  # Create a ggheatmap
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))
  ggheatmap <- ggheatmap + geom_tile(color = "white")+
    geom_text(aes(Var2, Var1, label = round(value,2)), color = "black", size = 4) +
    scale_fill_gradient2(low = "#E69F00", high = "#999999", mid = "#ffffff", 
                         midpoint = 0, limit = c(-1,1), space = "Lab") +
    ggplot2::theme_bw()+ # minimal theme
    coord_fixed() + # Same aspect Width for Both Axes
    theme(axis.text = element_text(face = "plain"),
          axis.text.x = element_text(face = "plain",angle = 45, vjust = 0.5, hjust = 0.5),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          legend.justification = c(1, 0),
          legend.position = c(0.6, 0.7),
          legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = rel(10), barheight = rel(1),
                                 title.position = "top", title.hjust = 0.5)) +
    
    labs(fill="Pearson\nCorrelation")
  
  return(ggheatmap) ## Clean Temp Data
  
  
} 
