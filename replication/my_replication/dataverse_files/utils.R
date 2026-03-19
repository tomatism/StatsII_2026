###################
#### UTILITIES ####
###################

# Misc with na.rm=TRUE
Mean <- function(x) mean(x,na.rm=TRUE)
SD <- function(x) sd(x,na.rm=TRUE)
Sum <- function(x) sum(x,na.rm=TRUE)
Min <- function(x) min(x,na.rm=TRUE)
Max <- function(x) max(x,na.rm=TRUE)
range01 <- function(x){(x-min(x,na.rm=TRUE))/(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Function to replace values in variable
ifelse.lab <- function(vals=0, replace=NA, var="TOPBOT", data=dt, lab.start=1) {
  
  setDT(data)
  
  if.form <- quote(get(var) %in% vals)
  
  v.temp.g <- with(data, get(var))
  v.temp.g.labs <- get_labels(v.temp.g,non.labelled=TRUE,drop.unused=TRUE,values="as.prefix")
  v.temp.g.lab <- get_label(v.temp.g)
  
  v.temp.g.new <- data.table(data[, ifelse(eval(if.form),replace,get(var))])
  setnames(v.temp.g.new, var)
  
  sort(as.vector(as.matrix(unique(v.temp.g.new))))
  
  v.temp.g.labs <- v.temp.g.labs[as.numeric(gsub("\\[(\\d{1,})\\].*","\\1",v.temp.g.labs)) 
                                 %in% sort(as.vector(as.matrix(unique(v.temp.g.new))))]
  v.temp.g.labs <- gsub("\\[(\\d{1,})\\]\\s","",v.temp.g.labs)
  
  v.temp.g.new <- v.temp.g.new %>% 
    dplyr::select(var) %>%
    set_labels(var, labels=v.temp.g.labs)
  set_label(v.temp.g.new, label=v.temp.g.lab)
}

#### Colors ####

trans <- function(color="black", alpha=0.5) {
  if(alpha<0 | alpha>1) stop("alpha must be between 0 and 1")
  
  alpha = floor(255*alpha)  
  newColor = col2rgb(col=unlist(list(color)), alpha=FALSE)
  
  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }
  
  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}

#### Plotting ####

plot.a <- function(...,mar=c(3.5,3.5,0,0)) {
  par(mar=mar, mgp=c(3,.5,0), las=1, tck=-.012,
      lwd=1, cex.axis=1) #mgp=c(2,.7,0)
  plot(..., pch="",xaxt="n",yaxt="n",
       ylab="",xlab="",frame.plot=FALSE)
}

axis.a <- function(side.set=1, ...) {
  axis(side=side.set, lwd.ticks=1, ...)
}

title.a <- function(mgp.new=c(2,1,1), ...) {
  title(mgp=mgp.new, ...)
}