dc_intervals <- function(dat, threshold=0.007, price="close", time="time", vol="volumeto") {
  # candidates for confirmed directional change
  dcc_up_cand <- c(FALSE, c(dat[2:nrow(dat), price]<dat[1:(nrow(dat)-1), price]*(1-threshold)))
  dcc_down_cand <- c(FALSE, c(dat[2:nrow(dat), price]>dat[1:(nrow(dat)-1), price]*(1+threshold)))
  
  # initialize new columns
  dat[,"interval"] <- NA  # 4 types of intervals: down, down_os, up, up_os (os=overshoot, NA means unknown)
  dat[,"point"] <- NA     # 3 types of points: dcc_up, dcc_down,  ep (NA means ignore)
  
  # figure out if the first change is up or down
  first_up <- which.max(dcc_up_cand) 
  first_down <- which.max(dcc_down_cand)
  
  if(first_up<first_down) {
    dat[first_up,"point"] <- "dcc_up"
    mode <- "uoi/dei"
    i <- first_up
  } else {
    dat[first_down, "point"] <- "dcc_down"
    mode <- "doi/uei"
    i <- first_down
  }
  
  # iterate till the end of the data
  while (i<nrow(dat)) {
    if(mode=="dcc_up") {
      j <- which.max(dcc_down_cand[i+1:length(dcc_down_cand)])+i
      if(!dcc_down_cand[j]) break # end of dataset
      dat[i,"point"] <- "dcc_up"
      dat[i,"interval"] <- "uoi"
      if((j-1)-(i+1)>0) { # if not changing direction immediately
        ep_idx <- which.min(dat[(i+1):(j-1), price])+i
        dat[ep_idx, "point"] <- "ep"
        dat[i:ep_idx-1, "interval"] <- "uoi"
        dat[ep_idx:j,"interval"] <- "dei"
      } 
      i <- j+1
      mode <- "dcc_down"
    } else { # mode=="dcc_down"
      j <- which.max(dcc_up_cand[i+1:length(dcc_up_cand)])+i
      if(!dcc_up_cand[j]) break # end of dataset
      dat[i,"point"] <- "dcc_down"
      dat[i,"interval"] <- "doi"
      if((j-1)-(i+1)>0) { # if not changing direction immediately
        ep_idx <- which.max(dat[(i+1):(j-1), price])+i
        dat[ep_idx, "point"] <- "ep"
        dat[i:ep_idx-1, "interval"] <- "doi"
        dat[ep_idx:j,"interval"] <- "uei"
      }
      i <- j+1
      mode <- "dcc_up"      
    }
  }
  
  dat <- dat[!is.na(dat[,"point"]),] # drop all points that are not dcc points
  return(dat)
}


# begin, end ... "YYYY-MM-DD"
plot_dc <- function(dat, dc_dat=NULL, begin=NULL, end=NULL, lwd=3, ...) {
  if(is.null(end)) end <- Sys.time()
  if(is.character(end)) end <- as.POSIXct(strptime(end, "%Y-%m-%d"))
  if(is.null(begin)) begin <- end - 180*24*3600 # 180 d from end as default
  if(is.character(begin)) begin <- as.POSIXct(strptime(begin, "%Y-%m-%d"))
  if(is.null(dc_dat)) dc_dat <- dc_intervals(dat=dat)

  plot(close ~ time, data=dat, xlim=c(begin, end), type="l", col="darkgrey", lwd=lwd, tcl=0, ...)
  
  dcc_idx <- which(dc_dat$point=="dcc_up"|dc_dat$point=="dcc_down")
  for (i in setdiff(dcc_idx,1)) {
    segments(
      x0=dc_dat[i-1,"time"], y0=dc_dat[i-1,"close"],
      x1=dc_dat[i,"time"], y1=dc_dat[i,"close"],
      col="red", lwd=lwd
    )
  }
  for (i in setdiff(dcc_idx,nrow(dc_dat))) {
    segments(
      x0=dc_dat[i,"time"], y0=dc_dat[i,"close"],
      x1=dc_dat[i+1,"time"], y1=dc_dat[i+1,"close"],
      col="darkgreen", lwd=lwd
    )
  }
  axis(1,at=dc_dat[dcc_idx, "time"],labels=FALSE)
}