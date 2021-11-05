reldiag = function(x,y,
                   type = "mean",      # for quantiles set type = list("quantile",alpha = 0.1) [replace 0.1 with appropriate level]
                   resampling = TRUE, n_resamples = 999, replace = TRUE, digits = 3, region_level = 0.9,
                   inset_hist = TRUE, scatter_plot = TRUE, show_decomp = TRUE,
                   lim = NULL,         # plotting region (used for xlim and ylim), e.g. lim = c(0,1)
                   main = "",xlab = "x",ylab = "y"){
  if(type[[1]] == "mean"){
    pava = function(x,y) isoreg(x,y)$yf
    score = function(x,y) mean((x-y)^2) # canonical score
    marg = base::mean
    identif = function(x,y) x-y
    score_label = "MSE "
  }
  else{
    if(type[[1]] == "quantile"){
      require(isotone)
      alpha = type[[2]]
      pava = function(x,y) gpava(x,y,solver = weighted.fractile,p = alpha)$x
      score = function(x,y) mean(2*(as.numeric(x >= y) - alpha)*(x-y))
      marg = function(x) quantile(x,alpha)
      identif = function(x,y) as.numeric(x > y) - alpha
      score_label = "QS "
    }
    else stop("type must be \"mean\" or list(\"quantile\",level)")
  }
  if(is.null(lim)){
    lim = range(x)
    lim[1] = lim[1]
    lim[2] = lim[2]
    adj_summand = c(-1,1)*max(abs(lim))*0.2
  }
  else adj_summand = 0
  
  plot(NULL,xlim = lim+adj_summand,ylim = lim+adj_summand,main = main,
       xlab = xlab,ylab = ylab)
  
  ord_x = order(x)
  x = x[ord_x]
  y = y[ord_x]
  
  x_rc = pava(x,y)
  
  res = y - x
  
  s = score(x,y)
  s_rc_ucond = score(x + marg(res),y)
  s_rc = score(x_rc,y)
  s_mg = score(marg(y),y)
  
  mcb = s - s_rc
  umcb = s - s_rc_ucond
  cmcb = s_rc_ucond - s_rc
  dsc = s_mg - s_rc
  unc = s_mg
  
  # test: mean identification zero? (t-test)
  v = identif(x,y)
  t = sqrt(length(v)) * mean(v)/sd(v)
  pval_ucond = 1 - abs(pt(t,length(v)-1) - 0.5)*2
  
  if(resampling){
    n_samples = n_resamples + 1 # total number of samples including observed sample
    low = floor(n_samples * (1-region_level)/2)
    up = n_samples - low
    pval_digits = ceiling(log(n_samples,10))

    resamples = sapply(1:n_resamples,function(i) x + sample(res,length(y),replace = TRUE)) 
    
    x_rc_resamples = apply(resamples, 2, function(y) pava(x,y))
    x_rc_resamples_sorted = apply(cbind(x_rc,x_rc_resamples),1,sort) - marg(res) # includes observed values + bias corrected (shifted by mean residual)
    
    ran_x = range(x)
    polygon(c(ran_x[1],x,ran_x[2],rev(x),ran_x[1]),
            c(ran_x[1],x_rc_resamples_sorted[up,],ran_x[2],rev(x_rc_resamples_sorted[low,]),ran_x[1]),
            border = NA,col = "lightblue1")
    points(x,x_rc_resamples_sorted[low,],type = "l",lty = 1,col = "lightblue2")
    points(x,x_rc_resamples_sorted[up,],type = "l",lty = 1,col = "lightblue2")
    box()
    
    mcb_resamples = sapply(1:n_resamples,function(i) score(x,resamples[,i]) - score(x_rc_resamples[,i],resamples[,i]))
    mcb_bounds = sort(c(mcb,mcb_resamples))[c(low,up)]
    
    rank_obs = tail(rank(c(mcb_resamples,mcb)),1)
    pval = 1 - (rank_obs - 1)/(n_resamples + 1)
    
    if(show_decomp) text(x = (lim + adj_summand)[1],y = (lim + adj_summand)[2],
                         paste0(c(score_label,"uMCB ","cMCB ","DSC ","UNC "),
                                bquote(.(format(round(c(s,umcb,cmcb,dsc,unc),digits = digits)),nsmall = digits)),
                                c("",paste0(" [","p = ", bquote(.(format(round(pval_ucond,digits = pval_digits),nsmall = pval_digits))),"]"),
                                  paste0(" [","p = ", bquote(.(format(round(pval,digits = pval_digits),nsmall = pval_digits))),"]"),
                                  "",""),
                                collapse = "\n"),
                         adj = c(0,1))
  }
  else if(show_decomp) text(x = (lim + adj_summand)[1],y = (lim + adj_summand)[2],
                            paste0(c(score_label,"uMCB ","cMCB ","DSC ","UNC "),
                                   bquote(.(format(round(c(s,umcb,cmcb,dsc,unc),digits = digits)),nsmall = digits)),
                                   c("",paste0(" [","p = ", bquote(.(format(round(pval_ucond,digits = 3),nsmall = 3))),"]"),
                                     "","",""),
                                   collapse = "\n"),
                            adj = c(0,1))
  
  abline(a = 0,b = 1,col = "grey",lty = 2)
  points(x,x_rc,type = "l")
  
  if(scatter_plot){
    points(x,y,pch = 20,col = adjustcolor("black",alpha = 0.25),cex = 0.5)
  }
  
  if(inset_hist){
    a = par("usr")
    a = c(grconvertX(a[1:2], "user", "ndc"),grconvertY(a[3:4], "user", "ndc"))
    par.old = par(fig = c(0.3*a[1] + 0.7*a[2],0.05*a[1] + 0.95*a[2],0.9*a[3] + 0.1*a[4],0.65*a[3] + 0.35*a[4]),
                  pty = "m",mar = c(1,0,0,0),mgp = c(1,0.4,0),tcl = -0.25,new = TRUE)
    plot(hist(x,breaks = 8,main = "",yaxt = "n",xlab = "",ylab = ""),add = TRUE)
    par(par.old)
  }
  
  invisible(list(x = x,y = y,res = res,x_rc = x_rc,
                 MDU = c(umcb,cmcb,mcb,dsc,unc),
                 pval_cond = if(resampling) pval else NA,
                 pval_ucond = pval_ucond,
                 mcb_bounds = if(resampling) mcb_bounds else NA))
}

# Mean Reliability Diagram:

# n = 100
# y = rnorm(n)
# x = rnorm(n,sd = 0.5)
# 
# reldiag(x,y)
# reldiag(x,y,resampling = FALSE)   # no resampling = no consistency bands
# reldiag(x,y,show_decomp = FALSE)  # without score and decomposition
# reldiag(x,y,inset_hist = FALSE)   # without inset histogram
# reldiag(x,y,scatter_plot = FALSE) # without scatter plot


# Quantile Reliability Diagram:

# n = 100
# alpha = 0.1
# y = rnorm(n)
# x = qnorm(alpha) + rnorm(n,sd = 0.5)
# 
# reldiag(x,y,type = list("quantile",alpha = alpha))
# reldiag(x,y,type = list("quantile",alpha = alpha),resampling = FALSE)   # no resampling = no consistency bands
# reldiag(x,y,type = list("quantile",alpha = alpha),show_decomp = FALSE)  # without score and decomposition
# reldiag(x,y,type = list("quantile",alpha = alpha),inset_hist = FALSE)   # without inset histogram
# reldiag(x,y,type = list("quantile",alpha = alpha),scatter_plot = FALSE) # without scatter plot


