library(xts)
library(fts)

NROW <- 1e6
NCOL <- 20
ROLLING.WIN <- 50

raw.data.vector <- rnorm(NROW)
raw.data.vector2 <- rnorm(NROW)
raw.data.matrix <- rnorm(NROW*NCOL)
raw.dates.vector <- as.POSIXct(1:NROW,origin='1970-01-01')

fts.base <- fts(data=raw.data.vector,dates=raw.dates.vector)
xts.base <- xts(raw.data.vector,raw.dates.vector)


###########################################################
################# FTS tests
test.fts.construct <- function() {
  system.time(ans <- fts(data=raw.data.vector,dates=raw.dates.vector))
}
test.fts.cbind <- function() {
  ## roughly half the data set
  y <- fts.base[fts.base > 0]
  system.time(ans <- cbind(fts.base,y))
}
test.fts.cbind.identical <- function() {
  system.time(ans <- cbind(fts.base,fts.base))
}
test.fts.rbind <- function() {
  system.time(ans <- rbind(fts.base,fts.base))
}
test.fts.subset <- function() {

  s.date <- as.POSIXct(1e5,origin='1970-01-01')
  e.date <- as.POSIXct(2e5,origin='1970-01-01')
  dts <- dates(fts.base)

  system.time(ans <- fts.base[ dts > s.date & dts < e.date,])
}
test.fts.diff <- function() {
  ## not implemented
}
test.fts.lag <- function() {
  system.time(ans <- lag(fts.base,1))
}
test.fts.add.identical <- function() {
  system.time(ans <- fts.base + fts.base)
}
test.fts.add.not.identical <- function() {
  ## roughly half the data set
  y <- fts.base[fts.base > 0]

  system.time(ans <- fts.base + y)
}
test.fts.rolling.mean <- function() {
  system.time(ans <- moving.mean(fts.base,ROLLING.WIN))
}
test.fts.rolling.min <- function() {
  system.time(ans <- moving.min(fts.base,ROLLING.WIN))
}
test.fts.rolling.max <- function() {
  system.time(ans <- moving.max(fts.base,ROLLING.WIN))
}
test.fts.rolling.sum <- function() {
  system.time(ans <- moving.sum(fts.base,ROLLING.WIN))
}
test.fts.rolling.sd <- function() {
  system.time(ans <- moving.sd(fts.base,ROLLING.WIN))
}

###########################################################
################# XTS tests
test.xts.construct <- function() {
  system.time(ans <- .xts(raw.data.vector,raw.dates.vector))
}
test.xts.cbind <- function() {
  ## roughly half the data set
  y <- xts.base[xts.base > 0]
  system.time(ans <- cbind(xts.base,y))
}
test.xts.cbind.identical <- function() {
  system.time(ans <- cbind(xts.base,xts.base))
}
test.xts.rbind <- function() {
  system.time(ans <- rbind(xts.base,xts.base))
}
test.xts.subset <- function() {
  system.time(ans <- xts.base['19700102 034640/19700103073320'])
}
test.xts.diff <- function() {
  ## not implemented
}
test.xts.lag <- function() {
  system.time(ans <- lag(xts.base,1))
}
test.xts.add.identical <- function() {
  system.time(ans <- xts.base + xts.base)
}
test.xts.add.not.identical <- function() {
  ## roughly half the data set
  y <- xts.base[xts.base > 0]

  system.time(ans <- xts.base + y)
}
test.xts.rolling.mean <- function() {
  system.time(ans <- rollapply(xts.base,ROLLING.WIN,mean))
}
test.xts.rolling.max <- function() {
  system.time(ans <- rollapply(xts.base,ROLLING.WIN,max))
}
test.xts.rolling.min <- function() {
  system.time(ans <- rollapply(xts.base,ROLLING.WIN,min))
}
test.xts.rolling.sum <- function() {
  system.time(ans <- rollapply(xts.base,ROLLING.WIN,sum))
}
test.xts.rolling.sd <- function() {
  system.time(ans <- rollapply(xts.base,ROLLING.WIN,sd))
}


do.tests <- function(ts.class,tests,trials) {
  cat("running test suite for",ts.class,"\n")
  ans <- list()

  for(test in tests) {
    cat("doing test",ts.class,test,"\n")
    this.test.results <- list()
    tfun <- get(paste("test",ts.class,test,sep="."))
    for(i in 1:trials) {
      cat(".")
      this.test.results[[i]] <- tfun()
    }
    cat("\n")
    this.test.result <- do.call(rbind,this.test.results)

    this.ans <- rbind(apply(this.test.result,2,mean),
                      apply(this.test.result,2,sd),
                      apply(this.test.result,2,max),
                      apply(this.test.result,2,min))

    rownames(this.ans) <- c("mean","sd","max","min")

    ans[[ test ]] <- this.ans
  }
  ans
}

do.results.summary <- function(x,stat,time.type) {  
  do.call(rbind,lapply(x,"[",stat,time.type))
}

do.results.contrast <- function(ans.absolute.times) {
  colnames(ans.absolute.times) <- paste(colnames(ans.absolute.times),"time")

  ans.relative.times <- ans.absolute.times/apply(ans.absolute.times,1,min)
  colnames(ans.relative.times) <- paste(colnames(ans.relative.times),"relative.time")

  cbind(ans.absolute.times,ans.relative.times)
}

ts.tests <- c("construct",
              "cbind",
              "cbind.identical",
              "subset",
              "rbind",
              ##"diff",
              "lag",
              "add.identical",
              "add.not.identical",
              "rolling.mean",
              "rolling.max",
              "rolling.min",
              "rolling.sum",
              "rolling.sd")

ts.classes <- c("fts","xts")

ts.test.results <- lapply(ts.classes,do.tests,tests=ts.tests,trials=10)
results.summary <- do.call(cbind,lapply(ts.test.results,do.results.summary,"mean","elapsed"))
colnames(results.summary) <- ts.classes
results.summary <- do.results.contrast(results.summary)
print(results.summary)
