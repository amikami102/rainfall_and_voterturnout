##################################################
## Duhaime & Moulton (2016) replication code    ##
## based on:                                    ##
##    Fraga & Hersh 2011 and                    ##
##    Gomez, Hansford, & Krause 2007            ##
##                                              ##
## Contact co-authors:                          ##
## Erik Duhaime: eduhaime@mit.edu               ##
## Taylor Moulton: taylor.moulton@sloan.mit.edu ##
##################################################
## R 3.0.3 GUI 1.63 Snow Leopard build (6660)   ##
## Last run on August 24, 2016                  ##
##################################################

install.packages('Zelig') ## Note: Zelig package not up-to-date with newest version of R
install.packages('lme4')

library(Zelig)
library(cem)
library(foreign)
library('ZeligMultilevel')

dm <- read.dta('DuhaimeMoulton2016.dta')

################################################################################

## Modified zelig plot.ci function for our own purposes to get the relevant data points.
## this is not a robust modification, so you may wish to not use this function
get.zelig.ci <- function (x, qi = "ev", var = NULL, ci = c(80, 95, 99.9), comp.mode) 
{
    if (length(ci) < 3) {
        ci <- rep(ci, 3)
    }
    if (length(ci) > 3) {
        ci <- ci[1:3]
    }
    ci <- sort(ci)
    if (!"pooled.sim" %in% class(x)) {
        something <- list(x = x)
        class(something) <- "pooled.sim"
        attr(something, "titles") <- x$titles
        x <- something
    }
    xmatrix <- matrix(NA, nrow = length(x), ncol = length(x[[1]]$x$data))
    for (i in 1:length(x)) {
        xmatrix[i, ] <- as.matrix(x[[i]]$x$data)
    }
    if (length(x) == 1 && is.null(var)) {
        warning("Must specify the `var` parameter when plotting the confidence interval of an unvarying model. Plotting nothing.")
        return(invisible(FALSE))
    }
    if (is.character(var)) {
        if (!(var %in% names(x[[1]]$x$data))) {
            warning("Specified variable for confidence interval plot is not in estimated model.  Plotting nothing.")
            return(invisible(FALSE))
        }
    }
    if (is.null(var)) {
        each.var <- apply(xmatrix, 2, sd)
        flag <- each.var > 0
        min.var <- min(each.var[flag])
        var.seq <- 1:ncol(xmatrix)
        position <- var.seq[each.var == min.var]
    } else {
        if (is.numeric(var)) {
            position <- var
        } else if (is.character(var)) {
            position <- grep(var, names(x[[1]]$x$data))
        }
    }
    position <- min(position)
    xseq <- xmatrix[, position]
    xname <- names(x[[1]]$x$data[position])
    ev1 <- NULL
    if (qi == "pv") {
        request <- "Predicted Values: Y|X"
        if (!is.null(x[[1]]$x1)) {
            ev1 <- simulation.matrix(x, "Predicted Values: Y|X1")
        }
    } else if (qi == "fd") {
        request <- "First Differences: E(Y|X1) - E(Y|X)"
    } else {
        request <- "Expected Values: E(Y|X)"
        if (!is.null(x[[1]]$x1)) {
            ev1 <- simulation.matrix(x, "Expected Values: E(Y|X1)")
        }
    }
    ev <- simulation.matrix(x, request)
    ci.upper <- function(x, alpha) {
        pos <- max(round((1 - (alpha/100)) * length(x)), 1)
        return(sort(x)[pos])
    }
    ci.lower <- function(x, alpha) {
        pos <- max(round((alpha/100) * length(x)), 1)
        return(sort(x)[pos])
    }
    k <- ncol(ev)
    n <- nrow(ev)
    
    form.history <- function(k, xseq, results, ci = c(80, 95, 
        99.9)) {
        history <- matrix(NA, nrow = k, ncol = 8)
        for (i in 1:k) {
            v <- c(xseq[i], median(results[, i]), ci.upper(results[, 
                i], ci[1]), ci.lower(results[, i], ci[1]), ci.upper(results[, 
                i], ci[2]), ci.lower(results[, i], ci[2]), ci.upper(results[, 
                i], ci[3]), ci.lower(results[, i], ci[3]))
            history[i, ] <- v
        }
        if (k == 1) {
            left <- c(xseq[1] - 0.5, median(results[, 1]), ci.upper(results[, 
                1], ci[1]), ci.lower(results[, 1], ci[1]), ci.upper(results[, 
                1], ci[2]), ci.lower(results[, 1], ci[2]), ci.upper(results[, 
                1], ci[3]), ci.lower(results[, 1], ci[3]))
            right <- c(xseq[1] + 0.5, median(results[, 1]), ci.upper(results[, 
                1], ci[1]), ci.lower(results[, 1], ci[1]), ci.upper(results[, 
                1], ci[2]), ci.lower(results[, 1], ci[2]), ci.upper(results[, 
                1], ci[3]), ci.lower(results[, 1], ci[3]))
            v <- c(xseq[1], median(results[, 1]), ci.upper(results[, 
                1], ci[1]), ci.lower(results[, 1], ci[1]), ci.upper(results[, 
                1], ci[2]), ci.lower(results[, 1], ci[2]), ci.upper(results[, 
                1], ci[3]), ci.lower(results[, 1], ci[3]))
            history <- rbind(left, v, right)
        }
        return(history)
    }
    history <- form.history(k, xseq, ev, ci)
    if (!is.null(ev1)) {
        history1 <- form.history(k, xseq, ev1, ci)
    } else {
        history1 <- NULL
    }
    
    uncompetitive <- as.data.frame(x=cbind(x=history[,1], lower=history[,4], upper=history[,3]))
    uncompetitive.reference <- history[1,2]
    uncompetitive$lower.rel <- uncompetitive$lower - uncompetitive.reference
    uncompetitive$upper.rel <- uncompetitive$upper - uncompetitive.reference
    
    competitive <- as.data.frame(x=cbind(x=history1[,1], lower=history1[,4], upper=history1[,3]))
    competitive.reference <- history1[1,2]
    competitive$lower.rel <- competitive$lower - competitive.reference
    competitive$upper.rel <- competitive$upper - competitive.reference
    if ( comp.mode == 'competitive' ) {
		return(competitive)		
	} else if ( comp.mode == 'uncompetitive' ) {
		return(uncompetitive)
	} else print('ERROR... comp.mode not valid.')
}

plot.zelig.ci <- function(df1, df2=NULL, df3=NULL, xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL, col=c('black'), lty=c(1))
{
	## default colors 2 and 3 if none are specified
	if(is.na(col[2])) { col[2] <- 2 }
	if(is.na(col[3])) { col[3] <- 3 }
	
	## default line segment type is 1 if not specified
	if(is.na(lty[2])) { lty[2] <- 1 }
	if(is.na(lty[3])) { lty[3] <- 1 }
	
	## find x and y limits
	if(is.null(xlim))
	{
		all.xlim <- c( floor(min(df1$x, df2$x, df3$x)) , ceiling(max(df1$x, df2$x, df3$x)) )
	} else {
		all.xlim <- xlim
	}
    if(is.null(ylim))
    {
    	all.ylim <- c( floor(min(df1$lower.rel, df2$lower.rel, df3$lower.rel)) , ceiling(max(df1$upper.rel, df2$upper.rel, df3$upper.rel)) )
    } else {
    	all.ylim <- ylim
    }
    
    par(bty = 'n')
    par( mar = c(6,6,3,3) )
    plot(x=NA, y=NA, xlim=all.xlim, ylim=all.ylim, xlab=xlab, ylab=ylab, cex.lab=1.5, cex.axis=1.5)
    ## plot the df1 99% CI vertical lines
    segments(x0=df1$x, y0=df1$lower.rel, y1=df1$upper.rel, lwd=3, col=col[1], lty=lty[1])
    ## add points to the ends of the CI
	points(x=df1$x, y=df1$lower.rel, pch=19, col=col[1])
	points(x=df1$x, y=df1$upper.rel, pch=19, col=col[1])
	
	if(!is.null(df2))
	{
		## plot the df2 99% CI vertical lines
    	segments(x0=df2$x, y0=df2$lower.rel, y1=df2$upper.rel, lwd=3, col=col[2], lty=lty[2])
    	## add points to the ends of the CI
		points(x=df2$x, y=df2$lower.rel, pch=19, col=col[2])
		points(x=df2$x, y=df2$upper.rel, pch=19, col=col[2])
	}
	if(!is.null(df3))
	{
		## plot the df3 99% CI vertical lines
    	segments(x0=df3$x, y0=df3$lower.rel, y1=df3$upper.rel, lwd=3, col=col[3], lty=lty[3])
    	## add points to the ends of the CI
		points(x=df3$x, y=df3$lower.rel, pch=19, col=col[3])
		points(x=df3$x, y=df3$upper.rel, pch=19, col=col[3])
	}
}

################################################################################

z.table1.3 <- zelig(Turnout ~ Rain + Snow + post_competitive + Rain*post_competitive + Snow*post_competitive + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + GubElection + SenElection + Turnout_Lag + avg_rain + avg_snow + Yr76 + Yr80 + Yr84 + Yr88 + Yr92 + Yr96 + Yr2000 + tag(1|FIPS_County), data=dm, model='ls.mixed')
z.table1.3.coefficients <- summary(z.table1.3)$coefficients
## Coefficients
## z.table1.3.coefficients
##                           Estimate  Std. Error      t value
## (Intercept)            7.378344811 0.387915697  19.02048531
## Rain                  -4.022067889 0.444450949  -9.04952030
## Snow                  -0.407082061 0.688634981  -0.59114345
## post_competitive       5.753615128 0.320829063  17.93358455
## ZPcntHSGrad            0.674327111 0.048646982  13.86164330
## AdjIncome              0.152430774 0.087160141   1.74885872
## PcntBlack             -0.004523429 0.002563172  -1.76477807
## FarmsPerCap           17.734121828 1.013091749  17.50495140
## Closing               -0.060297044 0.003356689 -17.96324757
## Motor                 -0.157204493 0.102002034  -1.54118979
## GubElection           -0.236021287 0.072547436  -3.25333742
## SenElection            0.201913154 0.062354162   3.23816641
## Turnout_Lag            0.803822245 0.003521686 228.24928916
## avg_rain              -0.056202473 0.636058936  -0.08836048
## avg_snow               6.036055873 0.460415404  13.11002157
## Yr76                  -0.172941216 0.141255644  -1.22431367
## Yr80                  -0.442149297 0.134889523  -3.27786240
## Yr84                  -1.050004091 0.126226259  -8.31842835
## Yr88                  -3.074209561 0.135048864 -22.76368320
## Yr92                   1.614518256 0.147127085  10.97363043
## Yr96                  -5.696969145 0.167049149 -34.10355077
## Yr2000                 1.170307878 0.166818562   7.01545358
## Rain:post_competitive  3.980126288 0.605888613   6.56907261
## Snow:post_competitive -0.012402223 0.811215614  -0.01528844
## Observations: 24,757
nrow(dm)

################################################################################

## Replication of Fraga & Hersh 2011 Figure 4:
## Simulated Outcomes, Competitive and Uncompetitive Environments

x.min <- 0
x.max <- 2
x.competition.low  <- setx(z.table1.3, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.70)
x.competition.high <- setx(z.table1.3, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.96)
s.out  <- sim(z.table1.3, x=x.competition.low, x1=x.competition.high)

## get the 99% confidence intervals for both high and low competition
aggregate.uncompetitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='uncompetitive')
aggregate.competitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='competitive')

color.competition.low <- 'gray50'
color.competition.high <- 'black'

pdf(file='dm_2016_aggregate_1972_2000.pdf', width=11, height=8.5)
## Replication of Fraga & Hersh 2011 Figure 4:
## Simulated Outcomes, Competitive and Uncompetitive Environments

## plot the 99% confidence intervals for both high and low competition
plot.zelig.ci(df1=aggregate.uncompetitive, df2=aggregate.competitive, xlab='Rainfall on Election Day (inches)', ylab='Change in County Turnout (%)', col=c(color.competition.low, color.competition.high), ylim=c(-3,1))
abline(h=0)
text(x=0.75, y=0.7,'Competitive State', font=2, cex=2, col=color.competition.high)
text(x=0.75, y=0.5,'(4% Margin of Victory)', font=1, cex=1.5, col=color.competition.high)
text(x=0.35, y=-1.3,'Uncompetitive State', font=2, cex=2, col=color.competition.low)
text(x=0.35, y=-1.5,'(30% Margin of Victory)', font=1, cex=1.5, col=color.competition.low)
dev.off()

################################################################################

## Replication of Fraga & Hersh 2011 Table 1, Model 3, but for Others
## Impact of competitiveness and inclement weather on turnout:
## ex post competitive (1972 - 2000)

z.table1.3.other.recent <- zelig(TAOTurnout ~ Rain + Snow + post_competitive + Rain*post_competitive + Snow*post_competitive + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + GubElection + SenElection + Turnout_Lag + avg_rain + avg_snow + Yr76 + Yr80 + Yr84 + Yr88 + Yr92 + Yr96 + Yr2000 + tag(1|FIPS_County), data=dm, model='ls.mixed')
z.table1.3.other.recent.coefficients <- summary(z.table1.3.other.recent)$coefficients
## Coefficients
z.table1.3.other.recent.coefficients
##                          Estimate  Std. Error     t value
## (Intercept)            1.32213185 0.193006583   6.8501904
## Rain                   2.44358295 0.205137601  11.9119213
## Snow                  -3.60072738 0.317161345 -11.3529831
## post_competitive      -2.72553081 0.151698007 -17.9668201
## ZPcntHSGrad            0.61828281 0.024689546  25.0422923
## AdjIncome             -0.21851571 0.044565458  -4.9032528
## PcntBlack             -0.02878383 0.001340938 -21.4654397
## FarmsPerCap            0.33711346 0.524749039   0.6424280
## Closing               -0.02316865 0.001693396 -13.6817654
## Motor                  0.46861157 0.047973242   9.7681863
## GubElection           -0.05204709 0.036979880  -1.4074435
## SenElection            0.01235263 0.028451882   0.4341585
## Turnout_Lag            0.04921763 0.001763424  27.9102661
## avg_rain               0.97835399 0.314556013   3.1102696
## avg_snow               0.30619501 0.228027182   1.3428005
## Yr76                   0.44740111 0.065154213   6.8668025
## Yr80                   2.66626991 0.062257150  42.8267264
## Yr84                  -0.63598902 0.058034257 -10.9588552
## Yr88                  -0.21334490 0.062241041  -3.4277206
## Yr92                  11.93205182 0.068413821 174.4099612
## Yr96                   5.20918330 0.077998777  66.7854484
## Yr2000                 1.16800681 0.077960175  14.9820958
## Rain:post_competitive -4.03676503 0.279362264 -14.4499295
## Snow:post_competitive  5.14618227 0.373650286  13.7727240

x.competition.low  <- setx(z.table1.3.other.recent, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.70)
x.competition.high <- setx(z.table1.3.other.recent, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.96)
s.out  <- sim(z.table1.3.other.recent, x=x.competition.low, x1=x.competition.high)

other.recent.uncompetitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='uncompetitive')

other.recent.competitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='competitive')

pdf(file='dm_2016_other_1972_2000.pdf', width=11, height=8.5)
## Replication of Fraga & Hersh 2011 Figure 4: (OTHER)
## Simulated Outcomes, Competitive and Uncompetitive Environments

## plot the 99% confidence intervals for both high and low competition
plot.zelig.ci(df1=other.recent.uncompetitive, df2= other.recent.competitive, xlab='Rainfall on Election Day (inches)', ylab='Change in County Turnout for Others (%)', col=c(color.competition.low, color.competition.high))
abline(h=0)
text(x=0.35, y=-1.7,'Competitive State', font=2, cex=2, col=color.competition.high)
text(x=0.35, y=-1.95,'(4% Margin of Victory)', font=1, cex=1.5, col=color.competition.high)
text(x=0.75, y=0.5,'Uncompetitive State', font=2, cex=2, col=color.competition.low)
text(x=0.75, y=0.25,'(30% Margin of Victory)', font=1, cex=1.5, col=color.competition.low)
dev.off()

## Clear out old objects:
rm(z.table1.3.other.recent)
rm(x.competition.high)
rm(x.competition.low)
rm(s.out)

################################################################################

## Replication of Fraga & Hersh 2011 Table 1, Model 3, but for Republicans
## Impact of competitiveness and inclement weather on turnout:
## ex post competitive (1972 - 2000)

z.table1.3.republican.recent <- zelig(GOPTurnout ~ Rain + Snow + post_competitive + Rain*post_competitive + Snow*post_competitive + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + GubElection + SenElection + Turnout_Lag + avg_rain + avg_snow + Yr76 + Yr80 + Yr84 + Yr88 + Yr92 + Yr96 + Yr2000 + tag(1|FIPS_County), data=dm, model='ls.mixed')
z.table1.3.republican.recent.coefficients <- summary(z.table1.3.republican.recent)$coefficients
## Coefficients
z.table1.3.republican.recent.coefficients
##                           Estimate  Std. Error      t value
## (Intercept)            14.59552781 0.574323667  25.41341867
## Rain                   -1.40694136 0.385484209  -3.64980285
## Snow                    5.00444177 0.591769724   8.45673844
## post_competitive       -4.35511363 0.312424459 -13.93973329
## ZPcntHSGrad             1.28070614 0.079523733  16.10470361
## AdjIncome               2.07753064 0.154399547  13.45554879
## PcntBlack              -0.09778135 0.006624499 -14.76056561
## FarmsPerCap            51.07709277 2.300097247  22.20649272
## Closing                 0.02108762 0.005226564   4.03470029
## Motor                  -0.58597373 0.097082176  -6.03585284
## GubElection            -0.33706941 0.122935813  -2.74183250
## SenElection             0.46741135 0.051874616   9.01040600
## Turnout_Lag             0.34392206 0.004945734  69.53913536
## avg_rain               -6.24933158 0.810644682  -7.70908848
## avg_snow               -0.01185005 0.592803744  -0.01998984
## Yr76                   -8.91628395 0.126468458 -70.50203736
## Yr80                   -4.47307588 0.126506528 -35.35845891
## Yr84                   -0.68382182 0.116482240  -5.87061009
## Yr88                   -5.43607556 0.122046313 -44.54108815
## Yr92                  -11.94432935 0.139802476 -85.43718062
## Yr96                  -11.66964105 0.161656131 -72.18805099
## Yr2000                 -2.18361594 0.163146511 -13.38438637
## Rain:post_competitive   2.72929208 0.523560732   5.21294266
## Snow:post_competitive  -6.70619916 0.697548877  -9.61394876

x.competition.low  <- setx(z.table1.3.republican.recent, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.70)
x.competition.high <- setx(z.table1.3.republican.recent, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.96)
s.out  <- sim(z.table1.3.republican.recent, x=x.competition.low, x1=x.competition.high)

republican.recent.competitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='competitive')

## Clear out old objects:
rm(z.table1.3.republican.recent)
rm(x.competition.high)
rm(x.competition.low)
rm(s.out)

################################################################################

## Replication of Fraga & Hersh 2011 Table 1, Model 3, but for Democrats
## Impact of competitiveness and inclement weather on turnout:
## ex post competitive (1972 - 2000)

z.table1.3.democrat.recent <- zelig(DemTurnout ~ Rain + Snow + post_competitive + Rain*post_competitive + Snow*post_competitive + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + GubElection + SenElection + Turnout_Lag + avg_rain + avg_snow + Yr76 + Yr80 + Yr84 + Yr88 + Yr92 + Yr96 + Yr2000 + tag(1|FIPS_County), data=dm, model='ls.mixed')
z.table1.3.democrat.recent.coefficients <- summary(z.table1.3.democrat.recent)$coefficients
## Coefficients
z.table1.3.democrat.recent.coefficients
##                           Estimate  Std. Error     t value
## (Intercept)             4.07026381 0.605660867   6.7203678
## Rain                   -6.45161793 0.404352400 -15.9554337
## Snow                   -0.87900243 0.620691117  -1.4161672
## post_competitive       13.53845469 0.327907556  41.2874131
## ZPcntHSGrad            -1.00842716 0.083894528 -12.0201779
## AdjIncome              -0.84650018 0.163043155  -5.1918781
## PcntBlack               0.09265118 0.007056338  13.1302082
## FarmsPerCap           -14.94196322 2.443517072  -6.1149412
## Closing                -0.10048193 0.005508289 -18.2419488
## Motor                  -0.11528030 0.101887257  -1.1314496
## GubElection             0.08143559 0.129663692   0.6280524
## SenElection            -0.35677319 0.054406150  -6.5575893
## Turnout_Lag             0.16683559 0.005205192  32.0517626
## avg_rain                9.11620207 0.852356072  10.6952979
## avg_snow                7.25053031 0.623363551  11.6313029
## Yr76                    7.66553083 0.132736680  57.7499064
## Yr80                    1.36311749 0.132938447  10.2537491
## Yr84                    0.18341719 0.122392169   1.4986023
## Yr88                    2.02056324 0.128143227  15.7680065
## Yr92                    0.56534152 0.146837309   3.8501218
## Yr96                    0.52303452 0.169809381   3.0801273
## Yr2000                  0.81539145 0.171407813   4.7570262
## Rain:post_competitive   6.78749373 0.549183164  12.3592531
## Snow:post_competitive   0.15661845 0.731644585   0.2140636

x.competition.low  <- setx(z.table1.3.democrat.recent, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.70)
x.competition.high <- setx(z.table1.3.democrat.recent, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.96)
s.out  <- sim(z.table1.3.democrat.recent, x=x.competition.low, x1=x.competition.high)

democrat.recent.competitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='competitive')

## Clear out old objects:
rm(z.table1.3.democrat.recent)
rm(x.competition.high)
rm(x.competition.low)
rm(s.out)

################################################################################

pdf(file='dm_2016_competitive_1972_2000.pdf', width=11, height=8.5)

plot.zelig.ci(df1=republican.recent.competitive, df2=democrat.recent.competitive, df3=other.recent.competitive, xlab='Rainfall on Election Day (inches)', ylab='Change in County Turnout (%)', col=c('red','blue','black'), lty=c(2,3,1))
abline(h=0)
text(x=0.35, y=1.3,'Republican', font=2, cex=2, col='red')
text(x=1.70, y=-1.0,'Democrat', font=2, cex=2, col='blue')
text(x=0.75, y=-1.7,'Other', font=2, cex=2, col='black')
dev.off()

################################################################################


## Now look at just 2000
dm.2000 <- dm[dm$Year == 2000, ]

z.table1.3.republican.2000 <- zelig(GOPTurnout ~ Rain + Snow + post_competitive + Rain*post_competitive + Snow*post_competitive + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + GubElection + SenElection + Turnout_Lag + avg_rain + avg_snow, data=dm.2000, model='ls')

x.competition.low  <- setx(z.table1.3.republican.2000, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.70)
x.competition.high <- setx(z.table1.3.republican.2000, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.96)
s.out  <- sim(z.table1.3.republican.2000, x=x.competition.low, x1=x.competition.high)

republican.2000.competitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='competitive')

## Clear out old objects:
rm(z.table1.3.republican.2000)
rm(x.competition.high)
rm(x.competition.low)
rm(s.out)

z.table1.3.democrat.2000 <- zelig(DemTurnout ~ Rain + Snow + post_competitive + Rain*post_competitive + Snow*post_competitive + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + GubElection + SenElection + Turnout_Lag + avg_rain + avg_snow, data=dm.2000, model='ls')

x.competition.low  <- setx(z.table1.3.democrat.2000, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.70)
x.competition.high <- setx(z.table1.3.democrat.2000, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.96)
s.out  <- sim(z.table1.3.democrat.2000, x=x.competition.low, x1=x.competition.high)

democrat.2000.competitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='competitive')

## Clear out old objects:
rm(z.table1.3.democrat.2000)
rm(x.competition.high)
rm(x.competition.low)
rm(s.out)

z.table1.3.other.2000 <- zelig(TAOTurnout ~ Rain + Snow + post_competitive + Rain*post_competitive + Snow*post_competitive + ZPcntHSGrad + AdjIncome + PcntBlack + FarmsPerCap + Closing + Motor + GubElection + SenElection + Turnout_Lag + avg_rain + avg_snow, data=dm.2000, model='ls')

x.competition.low  <- setx(z.table1.3.other.2000, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.70)
x.competition.high <- setx(z.table1.3.other.2000, Rain=seq(x.min,x.max,(x.max-x.min)/40), post_competitive=0.96)
s.out  <- sim(z.table1.3.other.2000, x=x.competition.low, x1=x.competition.high)

other.2000.competitive <- get.zelig.ci(x=s.out, qi='ev', var='Rain', ci=0.99, comp.mode='competitive')

## Clear out old objects:
rm(z.table1.3.other.2000)
rm(x.competition.high)
rm(x.competition.low)
rm(s.out)

################################################################################

pdf(file='dm_2016_competitive_2000.pdf', width=11, height=8.5)

plot.zelig.ci(df1=republican.2000.competitive, df2=democrat.2000.competitive, df3=other.2000.competitive, xlab='Rainfall on Election Day (inches)', ylab='Change in County Turnout (%)', col=c('red','blue','black'), lty=c(2,3,1))
abline(h=0)
text(x=0.35, y=-2.5,'Republican', font=2, cex=2, col='red')
text(x=0.75, y=5.0,'Democrat', font=2, cex=2, col='blue')
text(x=1.70, y=0.5,'Other', font=2, cex=2, col='black')
dev.off()