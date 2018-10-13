
#### Functions for wrangling

clear.labels <- function(x) {
  if(is.list(x)) {
    for(i in 1 : length(x)) class(x[[i]]) <- setdiff(class(x[[i]]), 'labelled') 
    for(i in 1 : length(x)) attr(x[[i]],"label") <- NULL
  }
  else {
    class(x) <- setdiff(class(x), "labelled")
    attr(x, "label") <- NULL
  }
  return(x)
}


###------------------- Functions for Matching -----------------------###

# Density comparison


mydensplot <- function(data, xvar, xname=xname, weight=NULL, lmts = NULL){
  data %>% 
    ggplot() +
    geom_density(aes_string(x=xvar, fill = "soep", weight=weight), alpha = 0.4) +
    xlab(xname) + 
    ylab("Dichte") + 
    theme_minimal() +
    scale_fill_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4")) +
    scale_x_continuous(limits = lmts)
}

mydensplot.sim <- function(data, xvar, xname=xname, weight=NULL, lmts = NULL){
  data %>% 
    ggplot() +
    geom_density(aes_string(x=xvar, fill = "a", weight=weight), alpha = 0.4) +
    xlab(xname) + 
    ylab("Dichte") + 
    theme_minimal() +
    scale_fill_manual("Datensatz", labels= c("A","B"),values = c("gold","turquoise4")) +
    scale_x_continuous(limits = lmts)
}

mydistplot <- function(data, xvar, xname=xname){
  data %>% 
    ggplot() +
    stat_ecdf(aes_string(x=xvar, colour = "soep"), alpha = 0.8) +
    xlab(xname) + 
    ylab("cumulative percent") + 
    theme_minimal() +
    scale_colour_manual("Datensatz", labels= c("VSKT","SOEP"),values = c("gold","turquoise4")) 
}





mydensplot.post <- function(data, xvar, xname=xname, lmts = NULL){
  data %>% 
    ggplot() + 
    geom_density(aes_string(x=xvar, fill = "vskt"), alpha = 0.4) +
    xlab(xname) + 
    ylab("Dichte") +
    theme_minimal() +
    scale_fill_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold")) +
    scale_x_continuous(limits = lmts)
}

mydensplot.post.sim <- function(data, xvar, xname=xname, lmts = NULL){
  data %>% 
    ggplot() + 
    geom_density(aes_string(x=xvar, fill = "b"), alpha = 0.4) +
    xlab(xname) + 
    ylab("Density") +
    theme_minimal() +
    scale_fill_manual("Data Set", labels= c("FUSED","B"),values = c("turquoise4","gold")) +
    scale_x_continuous(limits = lmts)
}


mydistplot.post <- function(data, xvar, xname=xname){
  data %>% 
    ggplot() +
    stat_ecdf(aes_string(x=xvar, colour = "vskt"), alpha = 0.8) +
    xlab(xname) + 
    ylab("cumulative percent") + 
    theme_minimal() +
    scale_colour_manual("Datensatz", labels= c("FUSED","VSKT"),values = c("turquoise4","gold"))

}


mydiagnostics.test <- function(var1, var2){
  
  
  # Calculate the common x and y range 
  xrng = range(c(fA.nnd$var1, vskt.mp$var1), finite=T, na.rm = T)
  yrng = range(c(fA.nnd$var2, vskt.mp$var2), finite=T, na.rm = T)
  

  
  # Calculate the 2d density estimate over the common range
  d1 = kde2d(data1$var1, data1$var2, lims=c(xrng, yrng), n=200)
  d2 = kde2d(data2$var1, data2$gbja, lims=c(xrng, yrng), n=200)
  
  # Confirm that the grid points for each density estimate are identical
  identical(d1$x, d2$x) # TRUE
  identical(d1$y, d2$y) # TRUE
  
  # Calculate the difference between the 2d density estimates
  diff12 = d1 
  diff12$z = d2$z - d1$z
  
  ## Melt data into long format
  # First, add row and column names (x and y grid values) to the z-value matrix
  rownames(diff12$z) = diff12$x
  colnames(diff12$z) = diff12$y
  
  # Now melt it to long format
  diff12.m = melt(diff12$z, id.var=rownames(diff12))
  names(diff12.m) = c("Einkommen","Geburtsjahr","z")
  
}

mydiagnostics <- function(data1, data2){
  
  # Plot difference between densities
  ggplot(diff12.m, aes_string(data1, data2, z="z", fill="z")) +
    geom_tile() +
    theme_classic() +
    stat_contour(aes(colour=..level..), binwidth=0.001) +
    scale_fill_gradient2(low="red",mid="white", high="turquoise4", midpoint=0) +
    scale_colour_gradient2(low="red", mid="white", high="turquoise4", midpoint=0) +
    coord_cartesian(xlim=xrng, ylim=yrng) +
    guides(colour=FALSE) +
    ggtitle("Einkommens-Contour-Differenzen zwischen Fused Data und VSKT")
  
  
}



eta.fcn <- function (glm.yx)
{
  # function to compute the table of eta and
  # partial eta coefficients:
  #
  # eta= SS_{effect}/SS_{total}
  # partial.eta = SS_{effect}/(SS_{effect}+SS_{error})
  #
  # starting from the ANOVA table.
  # glm.xy is an lm or glm object.
  #
  anova.yx <- anova(glm.yx)
  dev.tot <- anova.yx[1,4]
  dev.res <- deviance(glm.yx)
  anova.yx <- anova.yx[-1,-4]
  eta <- anova.yx[,2]/dev.tot
  eta.p <- anova.yx[,2]/(dev.res+anova.yx[,2])
  out <- cbind(anova.yx[,-3], eta=eta, eta.p=eta.p)
  out <- out[order(out[,"eta"], decreasing = T),]
  list( dev.tot=dev.tot, dev.res=dev.res, anova=out)
}






