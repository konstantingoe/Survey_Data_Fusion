
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

asNumeric <- function(x) as.numeric(as.character(x))
factorsNumeric <- function(d) modifyList(d, lapply(d[, sapply(d, is.factor)],   
                                                   asNumeric))


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


##### function for plotting tree in randomForest #####

to.dendrogram <- function(dfrep,rownum=1,height.increment=0.1){
  
  if(dfrep[rownum,'status'] == -1){
    rval <- list()
    
    attr(rval,"members") <- 1
    attr(rval,"height") <- 0.0
    attr(rval,"label") <- dfrep[rownum,'prediction']
    attr(rval,"leaf") <- TRUE
    
  }else{##note the change "to.dendrogram" and not "to.dendogram"
    left <- to.dendrogram(dfrep,dfrep[rownum,'left daughter'],height.increment)
    right <- to.dendrogram(dfrep,dfrep[rownum,'right daughter'],height.increment)
    rval <- list(left,right)
    
    attr(rval,"members") <- attr(left,"members") + attr(right,"members")
    attr(rval,"height") <- max(attr(left,"height"),attr(right,"height")) + height.increment
    attr(rval,"leaf") <- FALSE
    attr(rval,"edgetext") <- dfrep[rownum,'split var']
    #To add Split Point in Dendrogram
    #attr(rval,"edgetext") <- paste(dfrep[rownum,'split var'],"\n<",round(dfrep[rownum,'split point'], digits = 2),"=>", sep = " ")
  }
  
  class(rval) <- "dendrogram"
  
  return(rval)
}

## function for multiple t-test

pairedttest <- function(x,y){
  test <- t.test(x,y, paired=F)
  out <- data.frame(stat = test$statistic,
                    df   = test$parameter,
                    pval = test$p.value,
                    conl = test$conf.int[1],
                    conh = test$conf.int[2]
  )
  return(out)
}

corrtest <- function(x,y){
  test <- cocor.indep.groups(x, y, 
                     n1 = 4884, n2 =1465, data.name = c("soepcorr","fusedcorr"))
  return(test)
}

####### Hot Deck Functions #####
#distance hot deck
distancehd <- function(A=A,B=B, distfun = mahalanobis, constr = c(algorithm="hungarian", nn=1)){
  match <- NND.hotdeck(data.rec=A, data.don=B,
                       match.vars=X.mtc, 
                       don.class = donclass,
                       dist.fun = distfun,
                       rank = TRUE,
                       constrained = TRUE,
                       constr.alg = constr[1],
                       k=constr[2])
  fused <- create.fused(data.rec=A, data.don=B,
                        mtc.ids=match$mtc.ids,
                        z.vars=Z.vars)
  return(fused)
}

#random hotdeck
randomhd <- function(A=A,B=B, distfun = mahalanobis, cutdon = "rot" ){
  match <- RANDwNND.hotdeck(data.rec=A, data.don=B,
                       match.vars=X.mtc, 
                       don.class = donclass,
                       dist.fun = distfun,
                       cut.don = cutdon
                       )
  fused <- create.fused(data.rec=A, data.don=B,
                        mtc.ids=match$mtc.ids,
                        z.vars=Z.vars)
  return(fused)
}


#rank hotdeck
rankhd <- function(A=A,B=B, constr = c(algorithm="hungarian", nn=1)){
  match <- NND.hotdeck(data.rec=A, data.don=B,
                       match.vars=X.mtc, 
                       don.class = donclass,
                       var.rec = rankvar,
                       constrained = TRUE,
                       constr.alg = constr[1],
                       k=constr[2])
  fused <- create.fused(data.rec=A, data.don=B,
                        mtc.ids=match$mtc.ids,
                        z.vars=Z.vars)
  return(fused)
}
