
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
                     n1 = nrow(soep), n2 =nrow(A_k$`1`), data.name = c("soepcorr","fusedcorr"))
  return(test)
}

####### Hot Deck Functions #####
#distance hot deck
distancehd <- function(A=A,B=B, distfun = mahalanobis, constr = c(algorithm="hungarian", nn=1)){
  match1 <- NND.hotdeck(data.rec=A, data.don=B,
                       match.vars=X.mtc, 
                       don.class = donclass,
                       dist.fun = distfun,
                       rank = TRUE,
                       constrained = TRUE,
                       constr.alg = constr[1],
                       k=constr[2])
  fused1 <- create.fused(data.rec=A, data.don=B,
                        mtc.ids=match1$mtc.ids,
                        z.vars=Z.vars)
  return(fused1)
}

#random hotdeck
randomhd <- function(A=A,B=B, distfun = mahalanobis, cutdon = "rot", weight =NULL ){
  match2 <- RANDwNND.hotdeck(data.rec=A, data.don=B,
                       match.vars=X.mtc, 
                       don.class = donclass,
                       dist.fun = distfun,
                       cut.don = cutdon,
                       weight.don = weight
                       )
  fused2 <- create.fused(data.rec=A, data.don=B,
                        mtc.ids=match2$mtc.ids,
                        z.vars=Z.vars)
  return(fused2)
}


#rank hotdeck
rankhd <- function(A=A,B=B, constr = c(algorithm="hungarian", nn=1)){
  match3 <- NND.hotdeck(data.rec=A, data.don=B,
                       match.vars=X.mtc, 
                       don.class = donclass,
                       var.rec = rankvar,
                       constrained = TRUE,
                       constr.alg = constr[1],
                       k=constr[2])
  fused3 <- create.fused(data.rec=A, data.don=B,
                        mtc.ids=match3$mtc.ids,
                        z.vars=Z.vars)
  return(fused3)
}


# 3rd level: correlation matrix difference test

corrtestmat <- function(matrixA, matrixB){ 
    testmat <- matrix(NA, nrow=7, ncol=7)
        for (i in 1:nrow(testmat)) {
          for (j in 1:ncol(testmat)) {
            testmat[i,j] <- get.cocor.results(corrtest(matrixA[i,j],matrixB[i,j]))$fisher1925$p.value
          }
        }
      testmat[upper.tri(testmat, diag = T)] <- NA
    row.names(testmat) <- colnames(testmat) <- xyz.vars
  return(testmat)
}


# 2nd level: multivariate distribution test

mvartest <- function(A=A, B=B){
  mvtest <- cramer.test(as.matrix(factorsNumeric(select(A, one_of(xyz.vars)))), 
                        as.matrix(factorsNumeric(select(B, one_of(xyz.vars)))))
}

# Summary statistics function

simSumm <- function(repetitions = repetitions, data = data){ 
  summ <- lapply(select(data, -repetitions), 
                 function(x) rbind( mean = mean(x),
                                     sd = sd(x),
                                     var = var(x),
                                     minimum = min(x),
                                     maximum = max(x),
                                     s.size = length(x)))
  return(data.frame(summ))
}

simSumm2 <- function(repetitions = repetitions, data = data){ 
  summ <- lapply(select(data, -repetitions), 
                 function(x) rbind( mean = mean(x)))
  return(data.frame(summ))
}

simSumm3 <- function(data = data){ 
  summ <- lapply(select(data, -repetitions), 
                 function(x) rbind(sd = sd(x)))
  return(data.frame(summ))
}

simSumm4 <- function(repetitions = repetitions, data = data){ 
  summ <- lapply(data, 
                 function(x) rbind( mean = mean(x)))
  return(data.frame(summ))
}

simSumm5 <- function(data = data){ 
  summ <- lapply(data, 
                 function(x) rbind(sd = sd(x)))
  return(data.frame(summ))
}

nullToNA <- function(x) {
  x[sapply(x, is.null)] <- NA
  return(x)
}

#Matching and fusing monte carlo function
# for routine add "distance", "random" or "rank", for list the nesting indicators 
# and for FUN the user written matching and fusing functions

montecarlofunc <- function(routine = routine, list1 = list1, list2 =NULL, FUN=FUN) {
  if (routine == "distance"){
    llply(.progress = "text", list1, function(y) 
      llply(.progress = "text", list2, function(z) 
        llply(.progress = "text", A_k, function(x) FUN(x,
                                    B, distfun = z, constr = y))))
  }else if (routine == "random"){
    llply(.progress = "text", list1, function(y) 
      llply(.progress = "text", list2, function(z) 
        llply(.progress = "text", A_k, function(x) FUN(x,
                                    B, distfun = z, cutdon = y))))
    
  }else if (routine == "rank"){
    llply(.progress = "text", list1, function(y) 
      llply(.progress = "text", A_k, function(x) FUN(x,
                                  B, constr = y)))
  }
  
}


#### 4th level KS.function

KS.match <- function(routine = routine ,list1 = list1, list2 = NULL, out = out){
  if (routine == "distance"){
      setNames(lapply(seq_along(list1), function(w)
        setNames(lapply(seq_along(list2), function(s) 
          setNames(lapply(1:rep, function(r) 
            lapply(xzlist, function(t) ks.test(select(
              simlist$distancematch[[w]][[s]][[r]], one_of(xz.vars))[,t], ksB[,t], 
                alternative = "two.sided")[[out]])),
                  names(A_k))),names(list2))),names(list1))
    }else if (routine == "random"){
      setNames(lapply(seq_along(list1), function(w)
        setNames(lapply(seq_along(list2), function(s) 
          setNames(lapply(1:rep, function(r) 
            lapply(xzlist, function(t) ks.test(select(
              simlist$randommatch[[w]][[s]][[r]], one_of(xz.vars))[,t], ksB[,t], 
                alternative = "two.sided")[[out]])),
                 names(A_k))),names(list2))),names(list1))
    }else if (routine == "rank"){
      setNames(lapply(seq_along(list1), function(w)
        setNames(lapply(1:rep, function(r) 
          lapply(xzlist, function(t) ks.test(select(
            simlist$rankmatch[[w]][[r]], one_of(xz.vars))[,t], ksB[,t], 
              alternative = "two.sided")[[out]])),
                names(A_k))),names(list1))
    }
}


# KS aggregate function

ksaggregate <- function(routine = routine, list1 = list1, list2 = NULL){
  if (routine == "distance"){
    df <- setNames(lapply(seq_along(list1), function(w)
          setNames(lapply(seq_along(list2), function(z) 
            ldply(kslist$distanceks[[w]][[z]], data.frame, .id = "repetitions")), names(list2))),names(list1))
        
        stat1 <- setNames(lapply(seq_along(list1), function(s) 
          setNames(lapply(seq_along(list2), function(t) 
            simSumm2(data=df[[s]][[t]])), names(list2))), names(list1))
        stat1 <- do.call("rbind",(do.call("rbind", stat1)))
        stat2 <- setNames(lapply(seq_along(list1), function(s) 
          setNames(lapply(seq_along(list2), function(t) 
            simSumm3(data=df[[s]][[t]])), names(list2))), names(list1))
        stat2 <- do.call("rbind",(do.call("rbind", stat2)))
        return(rbind(stat1,stat2))
  }else if (routine == "random"){
    df <- setNames(lapply(seq_along(list1), function(w)
      setNames(lapply(seq_along(list2), function(z) 
        ldply(kslist$randomks[[w]][[z]], data.frame, .id = "repetitions")), names(list2))),names(list1))
    
    stat1 <- setNames(lapply(seq_along(list1), function(s) 
      setNames(lapply(seq_along(list2), function(t) 
        simSumm2(data=df[[s]][[t]])), names(list2))), names(list1))
    stat1 <- do.call("rbind",(do.call("rbind", stat1)))
    stat2 <- setNames(lapply(seq_along(list1), function(s) 
      setNames(lapply(seq_along(list2), function(t) 
        simSumm3(data=df[[s]][[t]])), names(list2))), names(list1))
    stat2 <- do.call("rbind",(do.call("rbind", stat2)))
    return(rbind(stat1,stat2))
  }else if (routine == "rank"){
    df <- setNames(lapply(seq_along(list1), function(w)
         ldply(kslist$rankks[[w]], data.frame, .id = "repetitions")),names(list1))
     
    stat1 <- setNames(lapply(seq_along(list1), function(s) 
        simSumm2(data=df[[s]])), names(list1))
    stat1 <- do.call("rbind",(do.call("rbind", stat1)))
    stat2 <- setNames(lapply(seq_along(list1), function(s) 
        simSumm3(data=df[[s]])), names(list1))
    stat2 <- do.call("rbind",(do.call("rbind", stat2)))
    return(rbind(stat1,stat2))
    
  }
}


#latex function

kstex <- function(data=data, routine = routine ){
  if (routine == "distance"){
        
      test0 <- apply(t(data[1:6,]), 2, round, digits = 4)
      test1 <- t(apply(apply(data[7:nrow(data),], 2, round, digits = 4), 2, function(i) paste("(", i, ")", sep="")))
      output <- NULL
      for (i in 1:nrow(test0)){
        output <- rbind(output, test0[i,])
        output <- rbind(output, test1[i,])
      }
      rownames(output) <- c("Pension", "", "Birthyear", "", "Unempben", "", "Workexp", "", "Unempexp","", "Income","")
      return(output)
  }else if (routine == "random"){
    test0 <- apply(t(data[1:8,]), 2, round, digits = 4)
    test1 <- t(apply(apply(data[9:nrow(data),], 2, round, digits = 4), 2, function(i) paste("(", i, ")", sep="")))
    output <- NULL
    for (i in 1:nrow(test0)){
      output <- rbind(output, test0[i,])
      output <- rbind(output, test1[i,])
    }
    rownames(output) <- c("Pension", "", "Birthyear", "", "Unempben", "", "Workexp", "", "Unempexp","", "Income","")
    return(output)
  }else if (routine == "rank"){
    test0 <- apply(data[1:6,], 2, round, digits = 4)
    test1 <- apply(apply(data[7:nrow(data),], 2, round, digits = 4), 2, function(i) paste("(", i, ")", sep=""))
    output <- NULL
    for (i in 1:nrow(test0)){
      output <- rbind(output, test0[i,])
      output <- rbind(output, test1[i,])
    }
    rownames(output) <- c("Pension", "", "Birthyear", "", "Unempben", "", "Workexp", "", "Unempexp","", "Income","")
    return(output)
  }
}


# Testpower function 
pvalue <- 0.05

kspower <- function(routine = routine , data=data, list1 = list1, list2 = NULL){
  if (routine == "distance"){
     agg <- setNames(lapply(seq_along(list1), function(r) setNames(lapply(seq_along(list2), function(s) lapply(1:rep, function(t) 
              sum(sum(rowSums(as.matrix(data[[r]][[s]][[t]]) > pvalue)) / nrow(as.matrix(data[[r]][[s]][[t]]))))), names(list2))),names(list1))
     output <- NULL
     for (i in 1:3){
       for (j in 1:3){
         output[i] <- sum(unlist(agg$hungarian[[i]])) /rep
         output[3+j] <- sum(unlist(agg$lpsolve[[j]])) /rep 
       }
     }
     return(round(output, digits = 4))
  }else if (routine == "random"){
    agg <- setNames(lapply(seq_along(list1), function(r) setNames(lapply(seq_along(list2), function(s) lapply(1:rep, function(t) 
            sum(sum(rowSums(as.matrix(data[[r]][[s]][[t]]) > pvalue)) / nrow(as.matrix(data[[r]][[s]][[t]]))))), names(list2))),names(list1))
    output <- NULL
    for (i in 1:4){
      for (j in 1:4){
        output[i] <- sum(unlist(agg$cutdon.rot[[i]])) /rep
        output[4+j] <- sum(unlist(agg$cutdon.min[[j]])) /rep 
      }
    }
    return(round(output, digits = 4))
  }else if (routine == "rank"){
    agg <- setNames(lapply(seq_along(list1), function(r) 
             lapply(1:rep, function(t) 
                sum(sum(rowSums(as.matrix(data[[r]][[t]]) > pvalue)) / nrow(as.matrix(data[[r]][[t]]))))),names(list1))
    output <- NULL
        output[1] <- sum(unlist(agg$hungarian)) /rep
        output[2] <- sum(unlist(agg$lpsolve)) /rep 
    return(round(output, digits = 4))
  }
}  

##### 3rd level: Correlation ####

corr.match <- function(routine = routine, list1 = list1, list2 = NULL){
  if (routine == "distance"){
        corr <- setNames(lapply(seq_along(list1), function(g) 
          setNames(lapply(seq_along(list2), function(s)
            setNames(lapply(1:rep, function(z) corrtestmat(
              corrmatfull, cor(select(simlist$distancematch[[g]][[s]][[z]], 
                one_of(xyz.vars))))),names(A_k))),names(list2))), names(list1))
        return(corr)
      
  }else if (routine == "random"){
         corr <- setNames(lapply(seq_along(list1), function(g) 
          setNames(lapply(seq_along(list2), function(s)
            setNames(lapply(1:rep, function(z) corrtestmat(
              corrmatfull, cor(select(simlist$randommatch[[g]][[s]][[z]], 
                one_of(xyz.vars))))),names(A_k))),names(list2))), names(list1))
      return(corr)
  }else if (routine == "rank"){
          corr <- setNames(lapply(seq_along(list1), function(g) 
              setNames(lapply(1:rep, function(z) corrtestmat(
                corrmatfull, cor(select(simlist$rankmatch[[g]][[z]], 
                  one_of(xyz.vars))))),names(A_k))), names(list1)) 
      return(corr)
  }
}  



correshape <- function(data=data){
  data$rows <- rownames(data)
  df <- filter(melt(data),!is.na(value))
  df <- df %>% 
    mutate(corr = paste(df$rows, "vs", df$variable))
  c <- as.data.frame(t(df$value))
  names(c) <- df$corr
  return(c)
}

montecarlocorr <- function(routine = routine, list1 = list1, list2 = NULL){
  if (routine == "distance"){
     df <-  setNames(lapply(seq_along(list1), function(z)
              setNames(lapply(seq_along(list2), function(y)
               ldply(lapply(1:rep, function(x) correshape(
                data=as.data.frame(correlationlist$distancecorr[[z]][[y]][[x]]))))), names(list2))), names(list1))
     return(df)
     
  }else if (routine == "random"){
    df <-  setNames(lapply(seq_along(list1), function(z)
            setNames(lapply(seq_along(list2), function(y)
              ldply(lapply(1:rep, function(x) correshape(
                data=as.data.frame(correlationlist$randomcorr[[z]][[y]][[x]]))))), names(list2))), names(list1))
    return(df)
    
  }else if (routine == "rank"){
    df <-  setNames(lapply(seq_along(list1), function(z)
             ldply(lapply(1:rep, function(x) correshape(
              data=as.data.frame(correlationlist$rankcorr[[z]][[x]]))))), names(list1))
    return(df)
  }
}

corraggregate <- function(routine = routine, list1 = list1, list2 = NULL){
  if (routine == "distance"){
    df <-  setNames(lapply(seq_along(list1), function(z)
      setNames(lapply(seq_along(list2), function(y)
        ldply(lapply(1:rep, function(x) correshape(
          data=as.data.frame(correlationlist$distancecorr[[z]][[y]][[x]]))))), names(list2))), names(list1))
    
    stat1 <- setNames(lapply(seq_along(list1), function(s) 
      setNames(lapply(seq_along(list2), function(t) 
        simSumm4(data=df[[s]][[t]])), names(list2))), names(list1))
    stat1 <- do.call("rbind",(do.call("rbind", stat1)))
    stat2 <- setNames(lapply(seq_along(list1), function(s) 
      setNames(lapply(seq_along(list2), function(t) 
        simSumm5(data=df[[s]][[t]])), names(list2))), names(list1))
    stat2 <- do.call("rbind",(do.call("rbind", stat2)))
    return(rbind(stat1,stat2))
  }else if (routine == "random"){
    df <-  setNames(lapply(seq_along(list1), function(z)
      setNames(lapply(seq_along(list2), function(y)
        ldply(lapply(1:rep, function(x) correshape(
          data=as.data.frame(correlationlist$randomcorr[[z]][[y]][[x]]))))), names(list2))), names(list1))
    
    stat1 <- setNames(lapply(seq_along(list1), function(s) 
      setNames(lapply(seq_along(list2), function(t) 
        simSumm4(data=df[[s]][[t]])), names(list2))), names(list1))
    stat1 <- do.call("rbind",(do.call("rbind", stat1)))
    stat2 <- setNames(lapply(seq_along(list1), function(s) 
      setNames(lapply(seq_along(list2), function(t) 
        simSumm5(data=df[[s]][[t]])), names(list2))), names(list1))
    stat2 <- do.call("rbind",(do.call("rbind", stat2)))
    return(rbind(stat1,stat2))
  }else if (routine == "rank"){
    df <-  setNames(lapply(seq_along(list1), function(z)
      ldply(lapply(1:rep, function(x) correshape(
        data=as.data.frame(correlationlist$rankcorr[[z]][[x]]))))), names(list1))
    
    stat1 <- setNames(lapply(seq_along(list1), function(s) 
      simSumm4(data=df[[s]])), names(list1))
    stat1 <- do.call("rbind",(do.call("rbind", stat1)))
    stat2 <- setNames(lapply(seq_along(list1), function(s) 
      simSumm5(data=df[[s]])), names(list1))
    stat2 <- do.call("rbind",(do.call("rbind", stat2)))
    return(rbind(stat1,stat2))
    
  }
}


corrtex <- function(data=data, routine = routine ){
  if (routine == "distance"){
    
    test0 <- apply(t(data[1:6,]), 2, round, digits = 4)
    test1 <- t(apply(apply(data[7:nrow(data),], 2, round, digits = 4), 2, function(i) paste("(", i, ")", sep="")))
    output <- NULL
    for (i in 1:nrow(test0)){
      output <- rbind(output, test0[i,])
      output <- rbind(output, test1[i,])
    }
    rownames(output) <- c("Corr(Income,Pension)", "", "Corr(Income,Birthyear)", "", "Corr(Income,Unempben", "", "Corr(Income,Workexp)","","Corr(Income,Unempexp)","", "Corr(Income,Education)", "")

    return(output)
  }else if (routine == "random"){
    test0 <- apply(t(data[1:8,]), 2, round, digits = 4)
    test1 <- t(apply(apply(data[9:nrow(data),], 2, round, digits = 4), 2, function(i) paste("(", i, ")", sep="")))
    output <- NULL
    for (i in 1:nrow(test0)){
      output <- rbind(output, test0[i,])
      output <- rbind(output, test1[i,])
    }
    rownames(output) <- c("Corr(Income,Pension)", "", "Corr(Income,Birthyear)", "", "Corr(Income,Unempben", "", "Corr(Income,Workexp)","","Corr(Income,Unempexp)","", "Corr(Income,Education)", "")
    return(output)
  }else if (routine == "rank"){
    test0 <- apply(t(data[1:21,]), 2, round, digits = 4)
    test1 <- t(apply(apply(data[22:nrow(data),], 2, round, digits = 4), 2, function(i) paste("(", i, ")", sep="")))
    output <- NULL
    for (i in 1:nrow(test0)){
      output <- rbind(output, test0[i,])
      output <- rbind(output, test1[i,])
    }
    #rownames(output) <- c("Corr(Income,Pension)", "", "Corr(Income,Birthyear)", "", "Corr(Income,Unempben", "", "Corr(Income,Workexp)","","Corr(Income,Unempexp)","", "Corr(Income,Education)", "")
    return(output)
  }
}

#### testpower correlation ####

corrpower <- function(routine = routine , data=data, list1 = list1, list2 = NULL){
  if (routine == "distance"){
    agg <- setNames(sapply(seq_along(list1), function(r) 
      setNames(sapply(seq_along(list2), 
                      function(s) sum(rowSums(as.matrix(data[[r]][[s]]) > pvalue) / ncol(as.matrix(data[[r]][[s]]))) / rep), names(list2))), names(list1))
    output <- as.vector(agg)
    return(round(output, digits = 4))
  }else if (routine == "random"){
      agg <- setNames(sapply(seq_along(list1), function(r) 
        setNames(sapply(seq_along(list2), 
                        function(s) sum(rowSums(as.matrix(data[[r]][[s]]) > pvalue) / ncol(as.matrix(data[[r]][[s]]))) / rep), names(list2))), names(list1))
      output <- as.vector(agg)
      return(round(output, digits = 4))
  }else if (routine == "rank"){
    agg2 <- setNames(sapply(seq_along(list1), 
             function(s) sum(rowSums(as.matrix(data[[s]]) > pvalue) / ncol(as.matrix(data[[s]]))) / rep), names(list1))
    return(round(agg2, digits = 4))
  }
}  

###### Level 2: Preserving the distribution #####

xyz.match <- function(routine = routine, list1 = list1, list2 = NULL){
  if (routine == "distance"){
    xyz <- setNames(llply(.progress = "tk",seq_along(list1), function(g) 
      setNames(llpply(.progres = "text",seq_along(list2), function(s)
        setNames(llply(.progress = "text",1:rep, function(z) mvartest(A=soep, 
          B=simlist$distancematch[[g]][[s]][[z]])$p.value),names(A_k))),names(list2))), names(list1))
    return(xyz)
    
  }else if (routine == "random"){
    xyz <- setNames(llply(.progress = "tk", seq_along(list1), function(g) 
      setNames(llply(.progress = "text",seq_along(list2), function(s)
        setNames(llply(.progress = "text",1:rep, function(z) mvartest(A=soep, 
          B=simlist$randommatch[[g]][[s]][[z]])$p.value),names(A_k))),names(list2))), names(list1))
    return(xyz)
  }else if (routine == "rank"){
    xyz <- setNames(llply(.progress = "tk",seq_along(distfuns1), function(g) 
        setNames(llply(.progress = "text",1:rep, function(z) mvartest(A=soep, 
            B=simlist$rankmatch[[g]][[z]])$p.value),
                 names(A_k))), names(distfuns1))
    return(xyz)
  }
}  

### convert to dataframe

xyztestdf <- function(data=data, routine=routine, list1 = list1, list2 = NULL){
  if (routine == "distance"){
    xyzdf <- do.call("cbind",
               do.call("cbind",
                lapply(seq_along(list1),
                  function(t) lapply(seq_along(list2),
                    function(s) ldply(data[[t]][[s]], .id = NULL)))))
    return(xyzdf)
  }else if (routine == "random"){
    xyzdf <- do.call("cbind",
              do.call("cbind",
               lapply(seq_along(list1),
                function(t) lapply(seq_along(list2),
                 function(s) ldply(data[[t]][[s]], .id = NULL)))))
    return(xyzdf)
  }else if (routine == "rank"){
    xyzdf <- do.call("cbind",
               lapply(seq_along(list1),
                 function(s) ldply(data[[s]], .id = NULL)))
    return(xyzdf)
  }
}







