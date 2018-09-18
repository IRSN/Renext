### R code from vignette source '/home/yves/Bureau/gforgeIRSN/Renext/inst/doc/RenextGuide.Rnw'
### Encoding: UTF-8

###################################################
### code chunk number 1: options
###################################################
options(prompt = "> ", continue = "  ", width = 80, encoding = "UTF8")
l <- Sys.setlocale(category = "LC_TIME", locale="en_GB.UTF-8")
Renext.Version <- packageVersion("Renext")
library(Renext)


###################################################
### code chunk number 2: Chap_Intro.Rnw:423-425
###################################################
library(Renext)
names(Brest)


###################################################
### code chunk number 3: Chap_Intro.Rnw:432-434
###################################################
head(Brest$OTdata, n = 4)
str(Brest$OTinfo)


###################################################
### code chunk number 4: <
###################################################
End <- Brest$OTinfo$end; Start <- Brest$OTinfo$start
Dur <- as.numeric(difftime(End,  Start, units = "days"))/365.25
Dur
Dur - as.numeric(Brest$OTinfo$effDuration) 


###################################################
### code chunk number 5: summaryBrest
###################################################
class(Brest)
summary(Brest)


###################################################
### code chunk number 6: RenplotBrest
###################################################
plot(Brest)


###################################################
### code chunk number 7: missing
###################################################
head(Brest$OTmissing, n = 4)


###################################################
### code chunk number 8: labelgaronneMAX
###################################################
names(Garonne)
Garonne$MAXinfo
head(Garonne$MAXdata, n = 4)


###################################################
### code chunk number 9: RenplotGaronne
###################################################
plot(Garonne)


###################################################
### code chunk number 10: rRendata1
###################################################
set.seed(1234)
RD1 <- rRendata(MAX.effDuration = c(40, 50, 30))
plot(RD1)


###################################################
### code chunk number 11: rRendata2
###################################################
RD2 <- rRendata(effDuration = 30,
                distname.y = "GPD",
                par.y = c(scale = 1, shape = 0.1),
                OTS.effDuration = c(40, 50, 30), OTS.threshold = c(3, 4, 2))
plot(RD2)


###################################################
### code chunk number 12: exppBrest
###################################################
expplot(x = Brest$OTdata$Surge, main = "expplot for \"Brest\"")


###################################################
### code chunk number 13: weibpBrest
###################################################
weibplot(x = Brest$OTdata$Surge-30, main = "weibplot for \"Brest\" (surge - 30)")


###################################################
### code chunk number 14: ExpPlot
###################################################
library(evd); set.seed(136)
X <- rgumbel(400); X <- X[X > 0.6]           ## X is truncated Gumbel
n <- length(X); 
Zrev <- sort(X); F <- (1:n) / (n + 1)           ## distribution function
y.exp <- -log(1 - F); y.gum <- -log(-log(F))   
plot(Zrev, y.exp, col = "red3", main = "exponential plot")


###################################################
### code chunk number 15: GumPlot
###################################################
plot(Zrev, y.gum, col = "SteelBlue3", main = "Gumbel plot")


###################################################
### code chunk number 16: spGaronne
###################################################
plot(Flow ~ date, data = Garonne$OTdata, type = "h", main = "Flows > 2500 m3/s")


###################################################
### code chunk number 17: Chap_DescriptiveTools.Rnw:144-145
###################################################
subset(Garonne$OTdata, date >= as.POSIXct("1945-01-01") & date <= as.POSIXct("1950-01-01"))


###################################################
### code chunk number 18: gdGaronne
###################################################
gof.date(date = Garonne$OTdata$date)


###################################################
### code chunk number 19: ieGaronne
###################################################
ie <- interevt(date = Garonne$OTdata$date)
names(ie)
d <- ie$interevt$duration
expplot(d, main = "Exponential plot for interevents")
bt <- gofExp.test(d) 
bt


###################################################
### code chunk number 20: gdBrest
###################################################
gof.Brest  <- gof.date(date = Brest$OTdata$date, skip = Brest$OTmissing,
                       start = Brest$OTinfo$start, end = Brest$OTinfo$end)
print(names(gof.Brest))


###################################################
### code chunk number 21: Chap_DescriptiveTools.Rnw:277-278
###################################################
head(gof.Brest$noskip, n = 2)


###################################################
### code chunk number 22: gdBrest2
###################################################
gof.Brest2  <- gof.date(date = Brest$OTdata$date, 
                        skip = Brest$OTmissing, plot.type = "omit",
                        start = Brest$OTinfo$start, end = Brest$OTinfo$end)


###################################################
### code chunk number 23: gdBrest3
###################################################
gof.Brest3 <- gof.date(date = subset(Brest$OTdata, Surge > 50)$date, 
                       skip = Brest$OTmissing, plot.type = "omit",
                       start = Brest$OTinfo$start, end = Brest$OTinfo$end)
c(gof.Brest3$KS.pvalue,  gof.Brest3$effKS.pvalue)                     


###################################################
### code chunk number 24: bp40
###################################################
data(Brest.years); data(Brest.years.missing)
bp40 <- barplotRenouv(data = Brest.years, threshold = 40,
           na.block = Brest.years.missing, main = "threshold = 40 cm")


###################################################
### code chunk number 25: bp50
###################################################
bp50 <- barplotRenouv(data = Brest.years, threshold = 50,
           na.block = Brest.years.missing, main = "threshold = 50 cm")


###################################################
### code chunk number 26: Chap_DescriptiveTools.Rnw:474-476
###################################################
bp40$tests
bp50$tests


###################################################
### code chunk number 27: Chap_DescriptiveTools.Rnw:484-485
###################################################
bp50$freq


###################################################
### code chunk number 28: feGaronne
###################################################
fit.exp <- Renouv(x = Garonne$OTdata$Flow, effDuration = 65, threshold = 3000,
                  distname.y = "exponential", main = "exponential")
class(fit.exp)


###################################################
### code chunk number 29: Chap_Renouv.Rnw:45-46
###################################################
methods(class = "Renouv")


###################################################
### code chunk number 30: Chap_Renouv.Rnw:51-52
###################################################
coef(fit.exp)


###################################################
### code chunk number 31: namefitexp
###################################################
head(names(fit.exp), n = 24)


###################################################
### code chunk number 32: fwGaronne
###################################################
fit.weibull <- Renouv(x = Garonne$OTdata$Flow, effDuration = 65, threshold = 3000,
                      distname.y = "weibull", main = "Weibull")
coef(fit.weibull)
fit.weibull$sigma


###################################################
### code chunk number 33: rlGaronneOpt1
###################################################
plot(fit.weibull,  Tlim = c(1, 100), main = "return periods from 0 to 100 years")


###################################################
### code chunk number 34: rlGaronneOpt2
###################################################
plot(fit.weibull, Tlim = c(1, 100), ylim = c(3000, 10000), pct.conf = 95,
     main = "return levels and 95% limits")


###################################################
### code chunk number 35: feGaronneH
###################################################
Garonne$MAXdata$Flow


###################################################
### code chunk number 36: feGaronneH
###################################################
fit.exp.H <- Renouv(x = Garonne$OTdata$Flow,
                    effDuration = 65, threshold = 3000,
                    MAX.data = list(Garonne$MAXdata$Flow),
                    MAX.effDuration = Garonne$MAXinfo$duration,
                    distname.y = "exponential",
                    main = "Garonne data, \"exponential\" with MAXdata")


###################################################
### code chunk number 37: fwGaronneH
###################################################
fit.weib.H <- Renouv(x = Garonne$OTdata$Flow,
                     effDuration = 65, threshold = 3000,
                     MAX.data = list(Garonne$MAXdata$Flow),
                     MAX.effDuration = Garonne$MAXinfo$duration,
                     distname.y = "weibull",
                     main = "Garonne data, \"Weibull\" with MAXdata")


###################################################
### code chunk number 38: nonorth
###################################################
fit.exp.H$corr


###################################################
### code chunk number 39: Chap_Renouv.Rnw:743-752
###################################################
ST <- SandT(Garonne)
ub <- ST$thresh[2]
u0 <- ST$thresh[1]
nOT <- sum(Garonne$OTdata$Flow > ub)
nOTS <- nrow(Garonne$MAXdata)
nTOT <- nOT + nOTS
wOT <- Garonne$OTinfo$effDuration
wOTS <- Garonne$MAXinfo$duration
wTOT <- wOT + wOTS


###################################################
### code chunk number 40: fwGaronneObj
###################################################
fitWithObj <- Renouv(x = Garonne)


###################################################
### code chunk number 41: fwGaronneObj1
###################################################
fitWithObj1 <- Renouv(x = Garonne, threshold = 3000)


###################################################
### code chunk number 42: fGPDGaronne
###################################################
fit.GPD <- Renouv(x = Garonne$OTdata$Flow, effDuration = Garonne$OTinfo$effDuration, 
                  threshold = 3000, distname.y = "GPD",
                  main = "Garonne data, \"GPD\"")
coef(fit.GPD)


###################################################
### code chunk number 43: fGPDGaronneH
###################################################
fit.GPD.H <- Renouv(Garonne, threshold = 3000, distname.y = "GPD",
                    main = "Garonne data, \"GPD\" with MAXdata")
coef(fit.GPD.H)


###################################################
### code chunk number 44: Chap_Renouv.Rnw:885-889
###################################################
fit.maxlo <- Renouv(x = Garonne$OTdata$Flow,
                    effDuration = Garonne$OTinfo$effDuration, 
                    threshold = 3000, distname.y = "maxlo")
coef(fit.maxlo)


###################################################
### code chunk number 45: Chap_Renouv.Rnw:899-904
###################################################
trylomax <- try(Renouv(x = Garonne$OTdata$Flow,
                       effDuration = Garonne$OTinfo$effDuration,
                       threshold = 3000, distname.y = "lomax"))
class(trylomax)
cat(trylomax)


###################################################
### code chunk number 46: Chap_Renouv.Rnw:925-927
###################################################
fit.maxlo.H <- Renouv(Garonne, threshold = 3000, distname.y = "maxlo")
coef(fit.maxlo.H)


###################################################
### code chunk number 47: Chap_Renouv.Rnw:943-945
###################################################
predict(fit.GPD, newdata = c(100, 200))
predict(fit.maxlo, newdata = c(100, 200))


###################################################
### code chunk number 48: fixweibGaronneH
###################################################
fit.weib.fixed.H <- 
  Renouv(x = Garonne$OTdata$Flow,
         effDuration = 65, threshold = 3000,
         MAX.data = list(Garonne$MAXdata$Flow),
         MAX.effDuration = Garonne$MAXinfo$duration,
         distname.y = "weibull",
         fixed.par.y = c(shape = 1.4),
         start.par.y = c(scale = 2000),
         trace = 0,
         main = "Garonne data, \"Weibull\" with MAXdata and fixed shape")


###################################################
### code chunk number 49: nonorth
###################################################
fit.weib.fixed.H$estimate


###################################################
### code chunk number 50: fixSLTWGaronneH
###################################################
fit.SLTW.H <- 
  Renouv(x = Garonne$OTdata$Flow,
         effDuration = 65, threshold = 3000,
         MAX.data = list(Garonne$MAXdata$Flow),
         MAX.effDuration = Garonne$MAXinfo$duration,
         distname.y = "SLTW",
         fixed.par.y = c(delta = 2800, shape = 1.4),
         start.par.y = c(scale = 2000),
         main = "Garonne data, \"SLTW\" with MAXdata, delta and shape fixed")


###################################################
### code chunk number 51: nonorth
###################################################
fit.SLTW.H$cov


###################################################
### code chunk number 52: anova1
###################################################
anova(fit.exp, fit.weibull)


###################################################
### code chunk number 53: Chap_Renouv.Rnw:1177-1179
###################################################
anova(fit.exp, fit.GPD)
anova(fit.exp, fit.maxlo)


###################################################
### code chunk number 54: Chap_Renouv.Rnw:1189-1191
###################################################
anova(fit.exp.H, fit.GPD.H)
anova(fit.exp.H, fit.maxlo.H)


###################################################
### code chunk number 55: Chap_Renouv.Rnw:1230-1235
###################################################
X <- Garonne$OTdata$Flow
Y <- X[X > 3000]
c(CV2 = CV2.test(Y)$p.value, Jackson = Jackson.test(Y)$p.value)
Y <- X[X > 3300]
c(CV2 = CV2.test(Y)$p.value, Jackson = Jackson.test(Y)$p.value)


###################################################
### code chunk number 56: Chap_POTBlocks.Rnw:24-26
###################################################
head(venice, n = 3)
range(venice, na.rm = TRUE)


###################################################
### code chunk number 57: Chap_POTBlocks.Rnw:43-49
###################################################
r <- 5
MAX.data <- as.list(as.data.frame(t(venice[ , 1:r])))
MAX.data <- lapply(MAX.data, function(x) x[!is.na(x)])
MAX.effDuration <- rep(1, length(MAX.data))
head(MAX.data, n = 2)
head(unlist(lapply(MAX.data, length)))


###################################################
### code chunk number 58: Chap_POTBlocks.Rnw:61-66
###################################################
fit.GPD <- Renouv(x = NULL, 
                  MAX.data = MAX.data, MAX.effDuration = MAX.effDuration,
                  distname.y = "GPD", threshold = 66,
                  numDeriv = FALSE, trace = 0, plot = FALSE)
coef(fit.GPD)


###################################################
### code chunk number 59: venice5
###################################################
plot(fit.GPD)


###################################################
### code chunk number 60: Chap_POTBlocks.Rnw:98-99
###################################################
fit.GPD$MAX


###################################################
### code chunk number 61: Chap_POTBlocks.Rnw:141-147
###################################################
fit.GEV <- fGEV.MAX(MAX.data = MAX.data, MAX.effDuration = MAX.effDuration)
fit.GEV$estimate
require(ismev)
fit.GEVref <- rlarg.fit(venice, show = FALSE)
 
fit.GEVref$mle


###################################################
### code chunk number 62: Dunk1
###################################################
RD <- Dunkerque
OTdata <- RD$OTdata; OTmissing <- RD$OTmissing
## allow up to 50% of gap within each block, or only 5%?
MAX1 <- OT2MAX(OTdata = OTdata, OTmissing = OTmissing,
               maxMissingFrac = 0.5,
               main = "impact of the 'maxMissingFrac' formal")
MAX2 <- OT2MAX(OTdata = OTdata, OTmissing = OTmissing, dataFrames = TRUE,
               prefix = "Max", maxMissingFrac = 0.05, plot = FALSE)
lines(MAX2$MAXdata$date, MAX2$MAXdata$Surge, type = "h", col = "red", lwd = 3)
legend("topleft", lw = c(1, 3), col = c("black", "orangered"),
       legend = c("50% max", " 5% max"))


###################################################
### code chunk number 63: Dunk2
###################################################
## r largest obs for r = 4
MAX3 <- OT2MAX(OTdata, OTmissing = OTmissing, MAX.r = 4,
               maxMissingFrac = 0.9, 
               dataFrames = FALSE, trace = TRUE,
               main = "r largest with r = 4")

## restrict the period
MAX4 <- OT2MAX(OTdata, OTmissing = OTmissing, MAX.r = 4,
               start = "1962-01-01",
               end = "1990-01-01",
               maxMissingFrac = 0.9, 
               dataFrames = FALSE, trace = TRUE,
               main = "r-largest with r = 4 with given 'start' and 'end'")
## use in a block maxima analysis, as if there were no gaps.
fitDunk <- fGEV.MAX(MAX.data = MAX3$data,
                    MAX.effDuration = rep(1, length(MAX3$effDuration)))   


###################################################
### code chunk number 64: Dunk3
###################################################
 ## plot the gap rate
MAX5 <- OT2MAX(OTdata = OTdata, OTmissing = OTmissing,
                maxMissingFrac = 0.5,
                main = "probability of being in a  gap",
                plotType = "gap")


###################################################
### code chunk number 65: Dunk4
###################################################
require(lattice)
xyplot(MAX5$monthGapTS[ , c(1:3, 10:12)], type = "h", lwd = 2, ylim = c(0, 1))


###################################################
### code chunk number 66: ggaronne1
###################################################
fitG <- Renouv(Garonne, distname.y = "GPD", plot = FALSE)
## specify pch background color for MAX block #1
plot(fitG, show = list(OT = TRUE, MAX = FALSE), main = "use plot, then lines")
lines(fitG, show = list(MAX = TRUE))


###################################################
### code chunk number 67: Chap_Graphics.Rnw:79-82
###################################################
names(RLpar())
str(RLpar()$quant)
names(RLpar()$MAX)


###################################################
### code chunk number 68: Chap_Graphics.Rnw:87-89
###################################################
## display 10 names
head(names(unlist(RLpar())), n = 10)


###################################################
### code chunk number 69: Chap_Graphics.Rnw:99-101
###################################################
newPar <- RLpar("quant.col" = "azure")
unlist(newPar$quant)


###################################################
### code chunk number 70: ggaronne2
###################################################
## specify pch background colour for MAX block #1
plot(fitG, par = RLpar(MAX.block1.bg = "green", MAX.block1.pch = 24),
     main = "change symbol bg colour") 


###################################################
### code chunk number 71: Chap_Graphics.Rnw:133-135
###################################################
newPar <- RLpar("OTS.block[0-9]+.col" = "red")
newPar$OTS$block1$col


###################################################
### code chunk number 72: ggaronne3
###################################################
plot(fitG, par = RLpar("*.pch" = 25), main = "regexp for plotting characters (pch)")


###################################################
### code chunk number 73: Chap_Graphics.Rnw:162-206
###################################################
L <- RLpar()
cat("\\begin{table}\n  \\centering \\tt \\begin{tabular}{|l|l|l|l|l|}\n   \\hline")
cat("   \\multicolumn{1}{|c|}{\\textrm{\\bf level 1}} \\rule{0pt}{1em} & \n")
cat("   \\multicolumn{1}{c|}{\\textrm{\\bf level 2}} & \n")
cat("   \\multicolumn{1}{c|}{\\textrm{\\bf level 3}} & \n")
cat("   \\multicolumn{1}{c|}{\\textrm{\\bf value}} &  \n")
cat("   \\multicolumn{1}{c|}{\\textrm{\\bf full name}}\\\\")
cat("   \\hline \\hline \n")

Nb <- sapply(L, function(x) sum(sapply(x, is.list)))
for (nm in names(L)) {
    cat(sprintf("%s &  & & & \\\\", nm), "\n")
    if (Nb[nm] == 0) {
        for (nm1 in names(L[[nm]])) {
            nm3 <- paste(nm, nm1, sep = ".")
            val <- L[[nm]][[nm1]]
            if (is.numeric(val)) {
                cat(sprintf("     &  \"%s\" & & \\multicolumn{1}{r|}{%s} & %s \\\\", nm1, L[[nm]][[nm1]], nm3), "\n")
            } else {
                cat(sprintf("     &  \"%s\" & & \"%s\" & %s \\\\", nm1, L[[nm]][[nm1]], nm3), "\n")
            }
        }
    }  else {
        cat(sprintf("     &  %s &    & & \\\\", names(L[[nm]])[1] ), "\n")
        for (nm2 in names(L[[nm]][[1]])) {
            val <- L[[nm]][[1]][[nm2]]
            nm3 <- paste(nm, names(L[[nm]])[1], nm2, sep = ".")
            if (is.numeric(val)) {
                cat(sprintf("     &  & \"%s\"& \\multicolumn{1}{r|}{%s} & %s \\\\", nm2, val, nm3), "\n")
            } else {
                cat(sprintf("     &  & \"%s\" & \"%s\" & %s \\\\", nm2, val, nm3), "\n")
            }
        }
        
        cat(sprintf("     &  %s & (list)   &  &\\\\", names(L[[nm]])[2] ), "\n")
        cat(sprintf("     &  \\multicolumn{1}{c|}{\\vdots} &   &  & \\\\"), "\n")
        cat(sprintf("     &  %s & (list)   &  &\\\\", names(L[[nm]])[Nb[nm]] ), "\n")
    }
    cat("\\hline\n")
}
cat("\n   \\end{tabular}\n")
cat("\\caption{\\label{RLparTable} \\rm The \\texttt{RLpar()} hierarchical list. The hidden
  structures are similar to those shown, e.g. within \\texttt{MAX}, the \\texttt{block2} has the same structure
  as \\texttt{block1}.} \\end{table}\n")


###################################################
### code chunk number 74: ggaronne4
###################################################
RLlegend.ini()
plot(fitG, show = list(OT = TRUE, MAX = FALSE),
     main = "use plot, then lines", legend = FALSE)
lines(fitG, show = list(OT = FALSE, quant = FALSE, MAX = TRUE), legend = FALSE)
RLlegend.show()


###################################################
### code chunk number 75: gchangeu
###################################################
u <- seq(from = 2500, to = 5000, by = 500)
fit1 <- Renouv(Garonne, threshold = u[1], distname.y = "GPD", plot = FALSE)

cols <- translude(rainbow(length(u)), alpha = 0.6)

RLlegend.ini()
## plot with no lines or points.
plot(fit1,
     main = "GPD for 'Garonne'. Sensitivity of RL to the threshold u",
     show = list(quant = FALSE, OT = TRUE, conf = FALSE, MAX = TRUE),
     legend = FALSE)               
for (i in 1L:length(u)) {
    fiti <- Renouv(Garonne, threshold = u[i], distname.y = "GPD", plot = FALSE)
    lines(fiti, legend = FALSE,
          label = paste("u = ", u[i]),
          show = list(OT = FALSE, conf = FALSE, quant = TRUE, MAX = FALSE),
          par = RLpar(quant.col = cols[i]))
}
RLlegend.show()


###################################################
### code chunk number 76: ggaronne5
###################################################
fitSim <- Renouv(x = rexp(100), effDuration = 100, threshold = 0,
                 OTS.data = list("deluge" = c(1.2, 2.4, 6.2, 3.1),
                     "dryness1" = c(0.2, 0.3),
                     "dryness2" = numeric(0),
                     "dryness3" = numeric(0)),
                 OTS.effDuration = c(60, 100, 20, 30),
                 OTS.threshold = c(1.0, 0.1, 0.3, 0.1),
                 plot = FALSE)

plot(fitSim, main = "simulated data, by Block", label = "")


###################################################
### code chunk number 77: ggaronne6
###################################################
plot(fitSim, main = "simulated data", label = "", byBlockStyle = c("OTS" = FALSE))


###################################################
### code chunk number 78: ggaronne7
###################################################
RLlegend.ini()
plot(fitSim, main = "grouping blocks", label = "",
     show = list("OTS" = FALSE),              ## IMPORTANT!
     legend = FALSE)

## add dryness blocks. Note that the label is used as prefix for all elements.
lines(fitSim, label = "dyryness",
      byBlockStyle = c("OTS" = FALSE),
      show = list("quant" = FALSE, "OTS" = c(FALSE, TRUE, TRUE, TRUE)),
      par = RLpar(OTS.block1.pch = 22,
          OTS.block1.col = "red", OTS.block1.bg = "gold"),
      legend = FALSE)

## add deluge block
lines(fitSim, label = "",
      byBlockStyle = c("OTS" = TRUE),
      show = list("quant" = FALSE, "OTS" = c(TRUE, FALSE, FALSE, FALSE)),
      par = RLpar(OTS.block1.col = "SteelBlue3", bg = "darkcyan"),
      legend = FALSE)
RLlegend.show()


###################################################
### code chunk number 79: ggaronne8
###################################################
RLlegend.ini()
plot(fitSim, main = "char. in 'show'", label = "", show = list("OTS" = "dryness"))
RLlegend.show()


