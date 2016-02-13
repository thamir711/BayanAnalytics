## URL: http://www.portfolioprobe.com/2012/04/02/replacing-market-indices/

pp.marketdistrib <-
  function (asset.rets, index.ret = NULL, xlab = "Return", ident = c(0, 
                                                                     0), fill = TRUE, ppositive=TRUE, iqr=TRUE, clwd = 3, 
            height.id = seq(.05, by=.05, 19), ...) 
  {
    fun.copyright <- "Placed in the public domain 2012 by Burns Statistics"
    dar <- density(asset.rets)
    plot(dar, yaxt = "n", type = "n", xlab = xlab, ...)
    if (fill) {
      dxn <- dar$x < 0
      dxp <- dar$x > 0
      if (sum(dxn)) {
        polygon(c(dar$x[dxn], max(dar$x[dxn])), c(dar$y[dxn], 
                                                  0), col = "red", border = NA)
      }
      if (sum(dxp)) {
        polygon(c(dar$x[dxp], min(dar$x[dxp])), c(dar$y[dxp], 
                                                  0), col = "green2", border = NA)
      }
    }
    else {
      if (any(dar$x < 0)) {
        lines(dar$x[dar$x < 0], dar$y[dar$x < 0], lwd = clwd, 
              col = "red")
      }
      if (any(dar$x > 0)) {
        lines(dar$x[dar$x > 0], dar$y[dar$x > 0], lwd = clwd, 
              col = "green2")
      }
    }
    if (length(ident) && any(unlist(ident) > 0)) {
      anam <- names(asset.rets)
      if (!length(anam)) {
        anam <- names(drop(asset.rets))
      }
      if (!length(anam)) {
        warning("no asset names for identification")
      }
      else {
        asort <- sort(asset.rets)
        ytraw <- par("usr")[4]
        if (is.list(ident)) {
          if (length(ident) != 2) {
            stop("'ident' must have length 2 when a list")
          }
          if (!is.list(ident[[1]])) 
            ident[[1]] <- list(ident[[1]])
          for (i in seq(along = ident[[1]])) {
            items <- ident[[1]][[i]]
            if(!length(items)) next
            if (!is.numeric(items)) {
              stop("bad value for 'ident' -- when a ", 
                   "list must have length 2 and each ", "component must be a list of ", 
                   "numeric vectors")
            }
            low <- asort[items]
            text(low, ytraw * height.id[i], names(low))
          }
          if (!is.list(ident[[2]])) 
            ident[[2]] <- list(ident[[2]])
          nassets <- length(asort)
          for (i in seq(along = ident[[2]])) {
            items <- ident[[2]][[i]]
            if (!is.numeric(items)) {
              stop("bad value for 'ident' -- when a ", 
                   "list must have length 2 and each ", "component must be a list of ", 
                   "numeric vectors")
            }
            high <- asort[nassets + 1 - items]
            text(high, ytraw * height.id[i], names(high))
          }
        }
        else {
          if (ident[1] > 0) {
            low <- head(asort, ident[1])
            text(low, ytraw * height.id[1], names(low))
          }
          if (ident[2] > 0) {
            high <- tail(asort, ident[2])
            text(high, ytraw * height.id[1], names(high))
          }
        }
      }
    }
    if (length(index.ret)) {
      abline(v = index.ret)
    }
    if(ppositive) {
      fpos <- mean(asset.rets > 0)
      fneg <- mean(asset.rets < 0)
      usr <- par("usr")
      xlocn <- usr[1] + .05 * (usr[2] - usr[1])
      xlocp <- usr[1] + .95 * (usr[2] - usr[1])
      yloc <- usr[3] + .95 * (usr[4] - usr[3])
      text(xlocn, yloc, paste(round(100 * fneg), "%", sep=""),
           col="red")
      text(xlocp, yloc, paste(round(100 * fpos), "%", sep=""),
           col="green4")
    }
    if(iqr) {
      quar <- quantile(asset.rets, c(.25, .75))
      axis(1, at=quar, labels=FALSE, tck=.02, col="gold", lwd=3)
      title(sub=paste("interquartile range: ", signif(diff(quar), 3),
                      sep=""))
    }
  }
