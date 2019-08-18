findwords <- function(tf) {
  txt <- scan(tf, "")
  wl <- list()
  for (i in 1 : length(txt)) {
    wrd <- txt[i]
    wl[[wrd]] <- c(wl[[wrd]], i)
  }
  
  wl
}

alphawl <- function(wrdlst) {
  nms <- names(wrdlst)
  sn <- sort(nms)
  wrdlst[sn]
}