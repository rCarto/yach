#' @title Calandar Heatmap
#' @description Plot a calandar heatmap
#' @name calandarHeat
#' @param dates a vector of dates
#' @param values a vector of values
#' @param minv min value
#' @param maxv max value
#' @param colors a vector of colors
#' @param ncolors number of colors
#' @param title title of the calandar heatmap
#' @param date.form date format
#' @param l TRUE: plot legend
#' @import grDevices
#' @import graphics
#' @examples
#' days <- seq(as.Date("2016-01-1"), as.Date("2016-12-31"), "days")
#' values <- runif(length(days))
#' calendarHeat(days, values)
#' @export
calendarHeat <- function(dates,
                         values,
                         minv,
                         maxv,
                         colors,
                         ncolors=99,
                         title,
                         date.form = "%Y-%m-%d",
                         l=T) {
  # construction du jeu de données des date/valeurs/couleurs
  if (class(dates)[1] == "character" | class(dates)[1] == "factor" ) {
    dates <- strptime(dates, date.form)
  }
  min.date <- as.Date(paste(format(min(dates), "%Y"),
                            "-1-1",sep = ""))
  max.date <- as.Date(paste(format(max(dates), "%Y"),
                            "-12-31", sep = ""))
  dates.f <- data.frame(date.seq = seq(min.date, max.date, by="days"))
  caldat <- data.frame(date.seq = seq(min.date, max.date, by="days"), value = NA)
  dates <- as.Date(dates)
  caldat$value[match(dates, caldat$date.seq)] <- values
  if (missing(colors)) colors <- c("#080E5B", "#7190A1", "#B6EFB6", "#F0ABAE",
                                   "#D35C61", "#8B1713")
  cols <- colorRampPalette(colors, space = "Lab")(ncolors)
  if(missing(minv))minv <- min(caldat$value, na.rm=T)
  if(missing(maxv))maxv <- max(caldat$value, na.rm=T)
  caldat$cols <- cols[findInterval(x = caldat$value,
                                   vec = seq(minv, maxv,
                                             length.out = ncolors+1),
                                   left.open = F, all.inside = T)]

  # check leap year
  if(nrow(caldat)==366){leap <- 1}else{leap <- 0}

  # length of each month
  ml <- c(31, if(leap == 1){29}else{28}, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # first and last weekday
  firstwd <- as.numeric(format(x = caldat$date.seq[1], "%u"))
  lastwd <- as.numeric(format(x = caldat$date.seq[365+leap], "%u"))

  # fin du mois
  caldat$gm <- NA
  caldat$gm[cumsum(ml)]<- 1


  x <- c(rep(1,8-firstwd), rep(2:52, each=7), rep(53,lastwd))

  y <- c((8-firstwd):1, rep(7:1,51), 8-(1:lastwd))


  caldat <- cbind(caldat, x,y)

  day <- c("Dim.", "Sam.", "Ven.",
           "Jeu.", "Mer.", "Mar.", 'Lun.')
  mon <- unique(months(caldat$date.seq, abbreviate = T))

  plot(c(0, 53), c(0, 9), type = "n", xlab = "", ylab = "",axes = F, asp=1)
  rect(caldat$x, caldat$y, caldat$x+1, caldat$y+1,
       col = caldat$cols,
       border = "#C4C4C4",lwd=0.5)
  text(x = rep(0.8,7), y= c(1:7)+0.5, cex=0.8,adj = 1,xpd=T,
       labels = day)
  text(x = seq(2, 52, length.out = 12), y= rep(0.3,12), cex=0.8,adj = 0.5,xpd=T,
       labels = mon)

  for(i in 1:length(ml)){
    p <- caldat[cumsum(ml)[i],c("x","y")]
    if(p[2] !=1 ){
      lines(c(p[1], p[1]), c(p[2],1), lwd=1.5)
      lines(c(p[1], p[1]+1), c(p[2],p[2]), lwd=1.5)
    }
    lines(c(p[1]+1, p[1]+1), c(p[2],8), lwd=1.5)
  }

  if(caldat[caldat$date.seq==max.date,"y"]==1){mx<-54}else{mx <- 53}
  if(caldat[caldat$date.seq==max.date,"y"]==7){mxy<-1}else{mxy <- 2}

  lines(c(1, mx), c(1,1), lwd=1.5)
  lines(c(mxy, 54), c(8,8), lwd=1.5)

  p <- caldat[1,c("x","y")]
  lines(c(1, 1), c(1,p[2]+1), lwd=1.5)
  lines(c(1, 2), c(p[2]+1,p[2]+1), lwd=1.5)
  lines(c(2, 2), c(p[2]+1,8), lwd=1.5)
  if (missing(title)) title <- format(min.date, "%Y")
  text(x = 1, y= 9, labels = title, font=2, adj=0 )

  if(l){
    n.colors=99
    x <- seq(20,35,length.out = ncolors)
    y <- rep(-1,length(x))

    sp <- x[2]-x[1]

    cols2 <- colorRampPalette(cols, space = "Lab")(length(x))
    rect(xleft = x, ybottom = y, xright = x+sp,ytop = y+0.4, col = cols2, border = NA)
    text(x=seq(20,35+sp,length.out = 5), y=rep(-1.4,5), cex=0.8,adj = 0.5,
         labels=formatDistr(seq(minv,maxv,length.out = 5)))
  }

}


#' @noRd
formatDistr <- function(distr){
  for (i in 1:length(distr)){
    if ((distr[i] < 1)){
      distr[i] <- round(distr[i],2)
    }else if ((distr[i] < 10)){
      distr[i] <- round(distr[i],1)
    }else if (distr[i] < 100){
      distr[i] <- round(distr[i],0)
    }else if (distr[i] < 10000){
      distr[i] <- round(distr[i],-1)
    }else if (distr[i] < 100000){
      distr[i] <- round(distr[i],-2)
    }else if (distr[i] < 10000000){
      distr[i] <- round(distr[i],-3)
    }else if (distr[i] < 1000000000000000){
      distr[i] <- round(distr[i],-3)
    }
  }
  return((format(distr, big.mark = " ")))
}
