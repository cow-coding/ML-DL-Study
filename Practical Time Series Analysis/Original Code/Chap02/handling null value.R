install.packages("zoo")
install.packages("data.table")
require(zoo)
require(data.table)

unemp <- fread("UNRATE.csv")
unemp[, DATE := as.Date(DATE)] > setkey(unemp, DATE)

# make a dataset that random null data
rand.unemp.idx <- sample(1:nrow(unemp), .1*nrow(unemp))
rand.unemp <- unemp[-rand.unemp.idx]

## make a dataset that if unemploy ratio is higher, more data loss
high.unemp.idx <- which(unemp$UNRATE > 8)
num.to.select <- .2 * length(high.unemp.idx)

high.unemp.idx <- sample(high.unemp.idx)
bias.unemp <- unemp[-high.unemp.idx]

# make all data
all.dates <- seq(from=unemp$DATE[1], to=tail(unemp$DATE, 1), by='months')

rand.unemp = rand.unemp[J(all.dates, roll=0)]
bias.unemp = bias.unemp[J(all.dates), roll=0]
rand.unemp[, rpt := is.na(UNRATE)]
bias.unemp[, rpt := is.na(UNRATE)]

# forward fill
rand.unemp[, impute.ff := na.locf(UNRATE, na.rm=FALSE)]
bias.unemp[, impute.ff := na.locf(UNRATE, na.rm=FALSE)]

unemp[350:400, plot (DATE, UNRATE, col=1, lwd=2, type='b')]
rand.unemp[350:400, lines(DATE, impute.ff, col=2, lwd=2, lty=2)]
rand.unemp[350:400][rpt == TRUE, points(DATE, impute.ff, col=2, pch=6, cex=2)]

# moving average
rand.unemp[, impute.rm.nolookahead := rollapply(c(NA, NA, UNRATE), 3,
              function(x) {
                if (!is.na(x[3])) x[3] 
                else mean(x, na.rm=TRUE)
              })]
bias.unemp[, impute.rm.nolookahead := rollapply(c(NA, NA, UNRATE), 3,
                                                function(x) {
                                                  if (!is.na(x[3])) x[3] 
                                                  else mean(x, na.rm=TRUE)
                                                })]

## rolling mean with look ahead
rand.unemp[, impute.rm.lookahead := rollapply(c(NA, UNRATE, NA), 3,
                                      function(x){
                                        if (!is.na(x[2])) x[2]
                                        else mean(x, na.rm=TRUE)
                                      })]
bias.unemp[, impute.rm.lookahead := rollapply(c(NA, UNRATE, NA), 3,
                                              function(x){
                                                if (!is.na(x[2])) x[2]
                                                else mean(x, na.rm=TRUE)
                                              })]


unemp[49:108, plot (DATE, UNRATE, col=1, lwd=2, type='b')]
rand.unemp[49:108, lines(DATE, impute.rm.nolookahead, col=3, lwd=2, lty=2)]
rand.unemp[49:108][rpt == TRUE, points(DATE, impute.rm.nolookahead, col=3, pch=7, cex=2)]
rand.unemp[49:108, lines(DATE, impute.rm.lookahead, col=2, lwd=2, lty=4)]
rand.unemp[49:108][rpt == TRUE, points(DATE, impute.rm.lookahead, col=2, pch=6, cex=2)]

## linear interpolation
rand.unemp[, impute.li := na.approx(UNRATE)]
bias.unemp[, impute.li := na.approx(UNRATE)]

## spline interpolation
rand.unemp[, impute.sp := na.spline(UNRATE)]
bias.unemp[, impute.sp := na.spline(UNRATE)]

use.idx = 90:120
unemp[use.idx, plot(DATE, UNRATE, col=1, type='b')]
rand.unemp[use.idx, lines(DATE, impute.li, col=2, lwd=2, lty=2)]
rand.unemp[use.idx, lines(DATE, impute.sp, col=3, lwd=2, lty=3)]

sort(rand.unemp[, lapply(.SD, function(x) mean((x - unemp$UNRATE)^2, na.rm=TRUE)),
                .SDcols = c("impute.ff", "impute.rm.lookahead","impute.rm.nolookahead", "impute.li", "impute.sp")])

sort(bias.unemp[, lapply(.SD, function(x) mean((x - unemp$UNRATE)^2, na.rm=TRUE)),
                .SDcols = c("impute.ff", "impute.rm.lookahead","impute.rm.nolookahead", "impute.li", "impute.sp")])

## down sampling that seasonal
unemp[seq.int(from=1, to=nrow(unemp), by=12)]

## upsampling
# all.dates <- seq(from=unemp$DATE[1], to=tail(unemp$DATE, 1), by="months")
# rand.unemp = rand.unemp[J(all.dates), roll=0]
