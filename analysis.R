#####
# SETUP
#####

library(data.table)
library(dplyr)
library(zoo)
library(ggplot2)
library(ggpubr)
library(cowplot)
library(forecast)
library(moments)
library(tseries)
library(rugarch)

dev.off()
rm(list = ls())
#Sys.setlocale(locale = "Polish")    # recommended


#####
# INITIAL TRANSFORMATION
#####

DataAllPrices <- fread("Wig20Daily.csv")
Data <- zoo(DataAllPrices$Zamkniecie, as.Date(DataAllPrices$Data))
DataDates <- index(Data)

LogReturns <- 100*diff(log(Data))
LogReturnsValues <- as.numeric(coredata(LogReturns))

PriceNormalized <- exp(cumsum(LogReturns/100))


# Initial analysis:
anyNA(Data)

Data[1]
Data[length(Data)]

LogReturns[1]
LogReturns[length(LogReturns)]

filter(DataAllPrices, Zamkniecie < 1400)
index(LogReturns[coredata(LogReturns) == min(LogReturns)])

PriceNormalized[index(PriceNormalized) == "2008-01-09"]


#####
# PRICES AND RETURNS PLOTS
#####

DataPriceDefault <- data.table(Day = DataDates,
                               Price = coredata(Data))

PriceDefaultPlot <- ggplot(DataPriceDefault, aes(Day, Price)) +
  geom_line(color = "deepskyblue3") +
  xlab("") +
  ylab("Cena") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))


DataPriceNormalized <- data.table(Day = DataDates[2:length(DataDates)],
                                  Returns = LogReturnsValues,
                                  PriceNormalized = PriceNormalized)

PriceNormalizedPlot <- ggplot(DataPriceNormalized, aes(Day, PriceNormalized)) +
  geom_line(color = "deepskyblue3") + 
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  xlab("") +
  ylab("Cena znormalizowana") + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1))

ggarrange(PriceDefaultPlot, PriceNormalizedPlot, ncol = 1, nrow = 2)


ReturnsNormalizedPlot <- ggplot(DataPriceNormalized, aes(Day, Returns)) +
  geom_line(color = "deepskyblue3") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_minimal() +
  theme(axis.text.x=element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 0, color = "grey")
ReturnsNormalizedPlot


#####
# STATISTICS & J-B
#####

SessionDaysPerYear <- 365/as.numeric(mean(diff(DataDates)))

TableSummaryStatistics <- data.table(`Session days per year` = SessionDaysPerYear,
                                     Mean = mean(LogReturns)*SessionDaysPerYear,
                                     Deviaton = sd(LogReturns)*sqrt(SessionDaysPerYear),
                                     Minimum = min(LogReturns),
                                     Maximum = max(LogReturns),
                                     Skewness = skewness(LogReturns),
                                     Kurtosis = kurtosis(LogReturns),
                                     `Jarque-Bera test statistic` = jarque.bera.test(LogReturnsValues)$stat)
TableSummaryStatistics

TableJarqueBeraTest <- jarque.bera.test(LogReturnsValues)
TableJarqueBeraTest


#####
# DENSITY & FAT TAILS
#####

LogReturnsValuesStandardized <- (LogReturnsValues-mean(LogReturnsValues))/sd(LogReturnsValues)

LogReturnsValuesStandardizedDensityDegreesOfFreedom <- 4 + 6/(kurtosis(LogReturnsValues)-3)
LogReturnsValuesStandardizedDensityBinWidth <- 0.05


LogReturnsValuesStandardizedDensityPlot <- ggplot(data.table(LogReturnsValuesStandardized), aes(LogReturnsValuesStandardized)) +
  geom_histogram(bins = 150, color = "darkgrey", fill = "deepskyblue3") +
  theme_minimal() +
  labs(title = "Standardized emperical distribution of returns", y = "", x = "") +
  stat_function(fun = function(x) ddist("norm", x)*length(LogReturnsValuesStandardized)*LogReturnsValuesStandardizedDensityBinWidth,
                color = "red",
                size = 1) +
  stat_function(fun = function(x) ddist("std", x, shape = LogReturnsValuesStandardizedDensityDegreesOfFreedom)
                *length(LogReturnsValuesStandardized)*LogReturnsValuesStandardizedDensityBinWidth,
                color = "chartreuse3",
                size = 1)


LogReturnsValuesStandardizedFatTailsPlot <- ggplot(data.table(LogReturnsValuesStandardized), aes(x = LogReturnsValuesStandardized)) +
  geom_histogram(binwidth = LogReturnsValuesStandardizedDensityBinWidth,
                 colour = "darkgrey",
                 fill = "deepskyblue3",
                 size = 0.1) +
  stat_function(fun = function(x) ddist("norm",x)*length(LogReturnsValuesStandardized)*LogReturnsValuesStandardizedDensityBinWidth,
                color = "red",
                size = 1) +
  stat_function(fun = function(x) ddist("std", x, shape = LogReturnsValuesStandardizedDensityDegreesOfFreedom)
                *length(vLogReturnsValuesStandardized)*LogReturnsValuesStandardizedDensityBinWidth,
                color = "chartreuse3",
                size = 1) +
  theme_minimal() +
  xlim(-7, -2) +
  labs(title = "Fat tails", y = "", x = "", caption = "")

ggarrange(LogReturnsValuesStandardizedDensityPlot, LogReturnsValuesStandardizedFatTailsPlot, ncol = 1, nrow = 2)


SimulatedValuesNormalDistirbution <- seq(-10, +5, length.out = length(LogReturnsValuesStandardized))
SimulatedDensityNormalDistirbution <- dnorm(SimulatedValuesNormalDistirbution)

SimulatedValuesTStudentDistirbution <- seq(-10, +5, length.out = length(LogReturnsValuesStandardized))
SimulatedDensityTStudentDistirbution <- dt(SimulatedValuesNormalDistirbution, df = length(SimulatedValuesTStudentDistirbution) - 1)


EmpericalNormalDensityComparisonPlot <- ggplot(data.table(LogReturnsValuesStandardized = LogReturnsValuesStandardized,
                                                          SimulatedValuesNormalDistirbution = SimulatedValuesNormalDistirbution,
                                                          SimulatedDensityNormalDistirbution = SimulatedDensityNormalDistirbution),
                                               aes(LogReturnsValuesStandardized)) +
  geom_density(color = "darkgrey", fill = "deepskyblue3", alpha = 0.5) +
  geom_area(aes(SimulatedValuesNormalDistirbution, SimulatedDensityNormalDistirbution), color = "red", fill = "red", alpha = 0.2) +
  theme_minimal() +
  labs(caption = "Rozk³ad empiryczny vs. rozk³ad normalny", y = "", x = "")


EmpericalTStudentDensityComparisonPlot <- ggplot(data.table(LogReturnsValuesStandardized = LogReturnsValuesStandardized,
                                                             SimulatedValuesTStudentDistirbution = SimulatedValuesTStudentDistirbution,
                                                             SimulatedDensityTStudentDistirbution = SimulatedDensityTStudentDistirbution),
                                                 aes(LogReturnsValuesStandardized)) +
  geom_density(color = "darkgrey", fill = "deepskyblue3", alpha = 0.5) +
  geom_area(aes(SimulatedValuesTStudentDistirbution, SimulatedDensityTStudentDistirbution),
            color = "chartreuse3",
            fill = "chartreuse3",
            alpha = 0.2) +
  theme_minimal() +
  labs(caption = "Rozk³ad empiryczny vs. rozk³ad t-Studenta", y = "", x = "Standaryzowane stopy zwrotów")

ggarrange(EmpericalNormalDensityComparisonPlot, EmpericalTStudentDensityComparisonPlot, ncol = 1, nrow = 2)


#####
# QQ PLOTS
#####

QuantileCalculated <- seq(1/length(LogReturnsValuesStandardized),
                          1-1/length(LogReturnsValuesStandardized),
                          1/length(LogReturnsValuesStandardized))

QuantileEmperical <- quantile(LogReturnsValuesStandardized, QuantileCalculated)
QuantileNormalDistribution <- qnorm(QuantileCalculated)
QuantileTStudentDistriburtion <- qt(QuantileCalculated,LogReturnsValuesStandardizedDensityDegreesOfFreedom)*
  sqrt((LogReturnsValuesStandardizedDensityDegreesOfFreedom-2)/LogReturnsValuesStandardizedDensityDegreesOfFreedom)

TableQuantiles <- data.table(QuantileEmperical, QuantileNormalDistribution, QuantileTStudentDistriburtion)


QQPlotNormalEmperical <- ggplot(TableQuantiles) + 
  geom_point(aes(QuantileNormalDistribution, QuantileEmperical), size = 1.5, shape = 19, color = "red", alpha = 0.9) +
  theme_light() +
  xlim(min(TableQuantiles), max(TableQuantiles))+ylim(min(TableQuantiles), max(TableQuantiles)) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  theme_minimal() +
  labs(y = "Q empiryczny", x = "Q normalny", caption = "Empiryczny vs. normalny")


QQPlotTStudentEmperical <- ggplot(TableQuantiles) + 
  geom_point(aes(QuantileTStudentDistriburtion, QuantileEmperical), size = 1.5, shape = 19, color = "chartreuse3", alpha = 0.9) +
  theme_light() +
  xlim(min(TableQuantiles), max(TableQuantiles))+ylim(min(TableQuantiles), max(TableQuantiles)) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  theme_minimal() +
  labs(y = "Q empiryczny", x = "Q t-Studenta", caption = "Empiryczny vs. t-Studenta")


QQPlotNormalTStudent <- ggplot(TableQuantiles) + 
  geom_point(aes(QuantileNormalDistribution, QuantileTStudentDistriburtion), size = 1.5, shape = 19, color = "deepskyblue3", alpha = 0.9) +
  theme_light() +
  xlim(min(TableQuantiles), max(TableQuantiles))+ylim(min(TableQuantiles), max(TableQuantiles)) +
  geom_abline(intercept = 0, slope = 1, size = 1) +
  theme_minimal() +
  labs(y = "Q t-Studenta", x = "Q normalny")


ggdraw() +
  draw_plot(QQPlotNormalEmperical, x=0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(QQPlotTStudentEmperical, x=0.5, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(QQPlotNormalTStudent, x=0, y = 0, width = 0.5, height = 0.5) +
  draw_plot_label(label = c("Empiryczny vs. normalny", "Empiryczny vs. t-Studenta", "t-Studenta vs. normalny"),
                  size = 10,
                  x = c(0, 0.5, 0),
                  y = c(1, 1, 0.5))


#####
# VOLATILITY, ACF, PACF
#####

PlotConfidenceInterval <- qnorm((1 + 0.95)/2)/sqrt(length(LogReturnsValues))

PlotACF <- ggAcf(LogReturnsValues) +
  geom_segment(color = "deepskyblue3") +
  geom_hline(yintercept = c(PlotConfidenceInterval, -PlotConfidenceInterval), color = "red", linetype = "dashed") +
  theme_minimal() + 
  labs(title = "")

PlotPACF <- ggPacf(LogReturnsValues) +
  geom_segment(color = "deepskyblue3") +
  geom_hline(yintercept = c(PlotConfidenceInterval, -PlotConfidenceInterval), color = "red", linetype = "dashed") +
  theme_minimal() + 
  labs(title = "")

ggarrange(PlotACF, PlotPACF, ncol = 1, nrow = 2)


Box.test(LogReturnsValues, lag = 20, type = c("Ljung-Box"))  # test for auto correlation
Box.test(LogReturnsValues, lag = 12, type = c("Ljung-Box"))


# Volatility clustering:
ggAcf(LogReturnsValues^2) +
  geom_segment(color = "deepskyblue3") +
  geom_hline(yintercept = c(PlotConfidenceInterval, -PlotConfidenceInterval), color = "red", linetype = "dashed") +
  theme_minimal() + 
  labs(title = "")

Box.test(LogReturnsValues^2, lag = 20, type = c("Ljung-Box"))


# Leverage effect:
Ccf(LogReturnsValues^2, LogReturnsValues, type = "correlation", main = "")

ggCcf(LogReturnsValues^2, LogReturnsValues) +
  geom_segment(color = "deepskyblue3") +
  geom_hline(yintercept = c(PlotConfidenceInterval, -PlotConfidenceInterval), color = "red", linetype = "dashed") +
  theme_minimal() + 
  labs(title = "")



#####
# SELECT SPECIFICATION OF GARCH(P, Q)
#####

GarchSpecificationLagSelector <- function(LogReturns, Pmax = 4, Qmax = 4, crit = "SIC", dist = "norm"){
  InformationCriterionMatrix <- matrix(NA, Pmax, Qmax+1)
  for(p in 1:Pmax){
    for(q in 0:Qmax){
      
      spec = ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(p, q)), 
                        mean.model = list(armaOrder = c(0, 0), include.mean = TRUE),  
                        distribution.model = dist)
      fit = ugarchfit(data = LogReturns, spec = spec)
      
      if (crit == "AIC") {
        InformationCriterionMatrix[p, q+1] <- infocriteria(fit)[1]
        
      } else if (crit == "SIC") {
        InformationCriterionMatrix[p, q+1] <- infocriteria(fit)[2]
        
      } else {
        InformationCriterionMatrix[p, q+1] <- infocriteria(fit)[4]
      }
    }
  }
  rownames(InformationCriterionMatrix) <- paste('p=', 1:Pmax, sep = "")
  colnames(InformationCriterionMatrix) <- paste('q=', 0:Qmax, sep = "")
  return(InformationCriterionMatrix)
}


# Finding best fit: 
TableGarchSpecificationAICNormal <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "AIC", dist = "norm")     # (1, 1), 3.34 
TableGarchSpecificationAICsNormal <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "AIC", dist = "snorm")   # (1, 1), 3.336 
TableGarchSpecificationAICTStudent <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "AIC", dist = "std")    # (1, 1), 3.31
TableGarchSpecificationAICsTStudent <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "AIC", dist = "sstd")  # (1, 1), 3.309

TableGarchSpecificationSICNormal <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "SIC", dist = "norm")     # (1, 1), 3.347
TableGarchSpecificationSICsNormal <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "SIC", dist = "snorm")   # (1, 1), 3.345
TableGarchSpecificationSICTStudent <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "SIC", dist = "std")    # (1, 1), 3.319
TableGarchSpecificationSICsTStudent <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "SIC", dist = "sstd")  # (1, 1), 3.320

TableGarchSpecificationHQNormal <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "HQ", dist = "norm")       # (1, 1), 3.34
TableGarchSpecificationHQsNormal <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "HQ", dist = "snorm")     # (1, 1), 3.34
TableGarchSpecificationHQTStudent <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "HQ", dist = "std")      # (1, 1), 3.313
TableGarchSpecificationHQsTStudent <- GarchSpecificationLagSelector(LogReturns, 4, 4, crit = "HQ", dist = "sstd")    # (1, 1), 3.313

TableGarchSpecificationHQsTStudent

# Best is GARCH(1, 1):
BestGarchOrder <- c(1, 1)
BestArmaOrder <- c(0, 0)
BestModelDistribution <- "std"

Garch11Specification <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = BestGarchOrder), 
                             mean.model = list(armaOrder = BestArmaOrder, include.mean = TRUE),
                             distribution.model = BestModelDistribution)

Garch11Fit <- ugarchfit(data = LogReturns, spec = Garch11Specification)


#####
# SELECTING BEST MODEL FROM GARCH FAMILY
#####

IGarch11Specification <- ugarchspec(mean.model = list(armaOrder = BestArmaOrder, include.mean = FALSE),
                                    variance.model = list(model = "iGARCH", garchOrder = BestGarchOrder),
                                    fixed.pars = list(alpha1 = 1 - 0.94, omega = 0),
                                    distribution.model = BestModelDistribution)   # lambda set to 0.94

EGarch11Specification  <- ugarchspec(variance.model = list(model = "eGARCH", garchOrder = BestGarchOrder), 
                                     mean.model = list(armaOrder = BestArmaOrder, include.mean = TRUE),
                                     distribution.model = BestModelDistribution)

GjrGarch11Specification  <- ugarchspec(variance.model = list(model = "gjrGARCH", garchOrder = BestGarchOrder),
                                       mean.model = list(armaOrder = BestArmaOrder, include.mean = TRUE),
                                       distribution.model = BestModelDistribution)

ApGarch11Specification  <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = BestGarchOrder),
                                      mean.model = list(armaOrder = BestArmaOrder, include.mean = TRUE),
                                      distribution.model = BestModelDistribution)

IGarch11Fit <- ugarchfit(data = LogReturns, spec = IGarch11Specification)
EGarch11Fit <- ugarchfit(data = LogReturns, spec = EGarch11Specification)
GjrGarch11Fit <- ugarchfit(data = LogReturns, spec = GjrGarch11Specification)
ApGarch11Fit <- ugarchfit(data = LogReturns, spec = ApGarch11Specification)


# Selecting best model:
TableGarch11Models <- cbind(infocriteria(Garch11Fit),
                            infocriteria(IGarch11Fit),
                            infocriteria(EGarch11Fit),
                            infocriteria(GjrGarch11Fit),
                            infocriteria(ApGarch11Fit))
colnames(TableGarch11Models) <- c("GARCH", "IGARCH", "eGARCH", "gjrGARCH", "apARCH")
TableGarch11Models


# Best is EGARCH(1, 1):
BestGarchFamily <- "eGARCH"
BestGarchSpecification <- ugarchspec(variance.model = list(model = BestGarchFamily, garchOrder = BestGarchOrder),
                                     mean.model = list(armaOrder = BestArmaOrder, include.mean = TRUE),
                                     distribution.model = BestModelDistribution)
BestGarchFit <- ugarchfit(data = LogReturns, spec = BestGarchSpecification)

BestGarchFit
coef(BestGarchFit)


# Plots with best model:
BestGarchResidualsStandardized <- as.zoo(residuals(BestGarchFit)/sigma(BestGarchFit))

PlotBestGarchResidualsStandardized <- ggplot(tibble(Day = index(BestGarchResidualsStandardized),
                                                    Residuals = coredata(BestGarchResidualsStandardized)),
                                             aes(Day, Residuals)) +
  geom_line(color = "deepskyblue3") +
  xlab("") +
  ylab("") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_hline(yintercept = 0, color = "grey")


PlotACFBestGarchResidualsStandardized <- ggAcf(coredata(BestGarchResidualsStandardized)^2) +
  geom_segment(color = "deepskyblue3") +
  geom_hline(yintercept = c(PlotConfidenceInterval, -PlotConfidenceInterval), color = "red", linetype = "dashed") +
  theme_minimal() + 
  labs(title = "")

ggarrange(PlotBestGarchResidualsStandardized, PlotACFBestGarchResidualsStandardized, ncol = 2, nrow = 1)


#####
# BACKTESTING EGARCH
#####

BacktestingObservationsNumber <- 500

BestGarchRoll <- ugarchroll(BestGarchSpecification,
                            data = LogReturns,
                            n.start = length(LogReturns) - BacktestingObservationsNumber,
                            refit.winodw = "moving",
                            refit.every = 1)

LogReturnsVaR5Roll <- BestGarchRoll@forecast$VaR$`alpha(5%)`
LogReturnsVaR1Roll <- BestGarchRoll@forecast$VaR$`alpha(1%)`
LogReturnsRealized <- tail(LogReturns, BacktestingObservationsNumber)

BestGarchRollMean <- BestGarchRoll@forecast$density$Mu
BestGarchRollSigma <- BestGarchRoll@forecast$density$Sigma
BestGarchRollDegreesOfFreedom <- BestGarchRoll@forecast$density$Shape


BacktestingEsToleranceLevel <- 0.05
i <- 1
LogReturnsES5Roll <- rep(0, length(LogReturnsVaR5Roll))
for (i in 1:length(BestGarchRollDegreesOfFreedom)) {
  qf <- function(x) qdist(BestModelDistribution, p = x, shape = BestGarchRollDegreesOfFreedom[i])
  LogReturnsES5Roll[i] <- BestGarchRollMean[i] + BestGarchRollSigma[i]*
    (1/BacktestingEsToleranceLevel * integrate(qf, 0, BacktestingEsToleranceLevel)$value)
}
LogReturnsES5Roll <- zoo(LogReturnsES5Roll, tail(index(LogReturns), BacktestingObservationsNumber))


BacktestingEsToleranceLevel <- 0.01
i <- 1
LogReturnsES1Roll <- rep(0, length(LogReturnsVaR5Roll))
for (i in 1:length(BestGarchRollDegreesOfFreedom)) {
  qf <- function(x) qdist(BestModelDistribution, p = x, shape = BestGarchRollDegreesOfFreedom[i])
  LogReturnsES1Roll[i] <- BestGarchRollMean[i] + BestGarchRollSigma[i]*
    (1/BacktestingEsToleranceLevel * integrate(qf, 0, BacktestingEsToleranceLevel)$value)
}
LogReturnsES1Roll <- zoo(LogReturnsES1Roll, tail(index(LogReturns), BacktestingObservationsNumber))


TableLogReturnsBacktest <- data.table(LogReturnsRealized = LogReturnsRealized,
                                      LogReturnsES5Roll = LogReturnsES5Roll,
                                      LogReturnsES1Roll = LogReturnsES1Roll,
                                      LogReturnsVaR5Roll = LogReturnsVaR5Roll,
                                      LogReturnsVaR1Roll = LogReturnsVaR1Roll,
                                      Day = index(
                                        LogReturns[(length(LogReturns) - BacktestingObservationsNumber):(length(LogReturns) - 1)]))

PLotLogReturnsVaR5Roll <- ggplot(TableLogReturnsBacktest, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsVaR5Roll, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsVaR5Roll, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsVaR5Roll), color = "deepskyblue3") +
  labs(caption = "VaR 5%", y = "", x = "") +
  theme_light()

PlotLogReturnsVaR1Roll <- ggplot(TableLogReturnsBacktest, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsVaR1Roll, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsVaR1Roll, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsVaR1Roll), color = "deepskyblue3") +
  labs(caption = "VaR 1%", y = "", x = "") +
  theme_light()

PlotsLogReturnsVaRRoll <- ggarrange(PLotLogReturnsVaR5Roll, PlotLogReturnsVaR1Roll, ncol = 1, nrow = 2)


PlotLogReturnsES5Roll <- ggplot(TableLogReturnsBacktest, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsES5Roll, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsES5Roll, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsES5Roll), color = "deepskyblue3") +
  labs(caption = "ES 5%", y = "", x = "") +
  theme_light()

PlotLogReturnsES1Roll <- ggplot(TableLogReturnsBacktest, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsES1Roll, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsES1Roll, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsES1Roll), color = "deepskyblue3") +
  labs(caption = "ES 1%", y = "", x = "") +
  theme_light()

PlotsLogReturnsESRoll <- ggarrange(PlotLogReturnsES5Roll, PlotLogReturnsES1Roll, ncol = 1, nrow = 2)

ggarrange(PlotsLogReturnsVaRRoll, PlotsLogReturnsESRoll, ncol = 1, nrow = 2)


TableVar1Backtest <- report(BestGarchRoll)

TableVar5Backtest <- VaRTest(alpha = 0.05, actual = coredata(LogReturnsRealized), VaR = coredata(LogReturnsVaR5Roll))
TableVar5Backtest

# Bactesting ES for 5% and 1%  (McNeil and Frey test)
TableEs5Backtest <- ESTest(alpha = 0.05, LogReturnsRealized, LogReturnsES5Roll, LogReturnsVaR5Roll)
TableEs5Backtest

TableEs1Backtest <- ESTest(alpha = 0.01, LogReturnsRealized, LogReturnsES1Roll, LogReturnsVaR1Roll)
TableEs1Backtest


#####
# BACKTESTING HISTORICAL SIMULATION
#####

HistoricalSimulationForecast <- function(LogReturns,
                                         BacktestingObservationsNumber,
                                         HistoricalSimulationLength,
                                         BacktestingToleranceLevel = 0.05) {
  
  T <- length(LogReturns)
  VaR <- rep(NA, BacktestingObservationsNumber)
  ES <- rep(NA, BacktestingObservationsNumber)
  
  for (i in 1:BacktestingObservationsNumber) {
    z <- tail(LogReturns[1:(T-1-BacktestingObservationsNumber+i)], HistoricalSimulationLength)
    m <- mean(z)
    s <- sd(z)
    VaR[i] <- qnorm(BacktestingToleranceLevel)*s+m
    ES[i] <- m - s*dnorm(qnorm(BacktestingToleranceLevel))/BacktestingToleranceLevel
  }
  VaR = zoo(VaR, tail(index(LogReturns), BacktestingObservationsNumber))
  ES  = zoo(ES, tail(index(LogReturns), BacktestingObservationsNumber))
  return(list(VaR = VaR, ES = ES)) 
}
HistoricalSimulationLength <- length(LogReturns) - BacktestingObservationsNumber


BacktestingToleranceLevel <- 0.05
LogReturns5RollHistoricalSimulation <- HistoricalSimulationForecast(LogReturns,
                                                                    BacktestingObservationsNumber,
                                                                    HistoricalSimulationLength,
                                                                    BacktestingToleranceLevel)

LogReturnsES5RollHistoricalSimulation <- LogReturns5RollHistoricalSimulation$ES
LogReturnsVaR5RollHistoricalSimulation <- LogReturns5RollHistoricalSimulation$VaR


BacktestingToleranceLevel <- 0.01
LogReturns1RollHistoricalSimulation <- HistoricalSimulationForecast(LogReturns,
                                                                    BacktestingObservationsNumber,
                                                                    HistoricalSimulationLength,
                                                                    BacktestingToleranceLevel)

LogReturnsES1RollHistoricalSimulation  <- LogReturns1RollHistoricalSimulation$ES
LogReturnsVaR1RollHistoricalSimulation <- LogReturns1RollHistoricalSimulation$VaR


TableLogReturnsBacktestHistoricalSimulation <- data.table(LogReturnsRealized = LogReturnsRealized,
                                                          LogReturnsES5RollHistoricalSimulation = LogReturnsES5RollHistoricalSimulation,
                                                          LogReturnsES1RollHistoricalSimulation = LogReturnsES1RollHistoricalSimulation,
                                                          LogReturnsVaR5RollHistoricalSimulation = LogReturnsVaR5RollHistoricalSimulation,
                                                          LogReturnsVaR1RollHistoricalSimulation = LogReturnsVaR1RollHistoricalSimulation,
                                                          Day =
  index(LogReturns[(length(LogReturns) - BacktestingObservationsNumber):(length(LogReturns) - 1)]))


PLotLogReturnsVaR5RollHistoricalSimulation <- ggplot(TableLogReturnsBacktestHistoricalSimulation, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsVaR5RollHistoricalSimulation, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsVaR5RollHistoricalSimulation, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsVaR5RollHistoricalSimulation), color = "deepskyblue3") +
  labs(caption = "VaR 5%", y = "", x = "") +
  theme_light()

PlotLogReturnsVaR1RollHistoricalSimulation <- ggplot(TableLogReturnsBacktestHistoricalSimulation, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsVaR1RollHistoricalSimulation, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsVaR1RollHistoricalSimulation, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsVaR1RollHistoricalSimulation), color = "deepskyblue3") +
  labs(caption = "VaR 1%", y = "", x = "") +
  theme_light()

PlotsLogReturnsVaRRollHistoricalSimulation <- ggarrange(PLotLogReturnsVaR5RollHistoricalSimulation,
                                                        PlotLogReturnsVaR1RollHistoricalSimulation,
                                                        ncol = 1,
                                                        nrow = 2)


PlotLogReturnsES5RollHistoricalSimulation <- ggplot(TableLogReturnsBacktestHistoricalSimulation, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsES5RollHistoricalSimulation, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsES5RollHistoricalSimulation, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsES5RollHistoricalSimulation), color = "deepskyblue3") +
  labs(caption = "ES 5%", y = "", x = "") +
  theme_light()

PlotLogReturnsES1RollHistoricalSimulation <- ggplot(TableLogReturnsBacktestHistoricalSimulation, aes(Day, LogReturnsRealized)) +
  geom_point(color = ifelse(LogReturnsRealized < LogReturnsES1RollHistoricalSimulation, "red", "grey"),
             fill = ifelse(LogReturnsRealized < LogReturnsES1RollHistoricalSimulation, "red", "grey"), shape = 23) +
  geom_hline(yintercept = 0)+
  geom_line(aes(y = LogReturnsES1RollHistoricalSimulation), color = "deepskyblue3") +
  labs(caption = "ES 1%", y = "", x = "") +
  theme_light()

PlotsLogReturnsESRollHistoricalSimulation <- ggarrange(PlotLogReturnsES5RollHistoricalSimulation,
                                                       PlotLogReturnsES1RollHistoricalSimulation,
                                                       ncol = 1,
                                                       nrow = 2)

ggarrange(PlotsLogReturnsVaRRollHistoricalSimulation, PlotsLogReturnsESRollHistoricalSimulation, ncol = 1, nrow = 2)


TableVar5BacktestHistoricalSimulation <- VaRTest(alpha = 0.05,
                                                 actual = coredata(LogReturnsRealized),
                                                 VaR = coredata(LogReturnsVaR5RollHistoricalSimulation))
TableVar5BacktestHistoricalSimulation


TableVar1BacktestHistoricalSimulation <- VaRTest(alpha = 0.01,
                                                 actual = coredata(LogReturnsRealized),
                                                 VaR = coredata(LogReturnsVaR1RollHistoricalSimulation))
TableVar1BacktestHistoricalSimulation


TableEs5BacktestHistoricalSimulation <- ESTest(alpha = 0.05,
                                               LogReturnsRealized,
                                               LogReturnsES5RollHistoricalSimulation,
                                               LogReturnsVaR5RollHistoricalSimulation)
TableEs5BacktestHistoricalSimulation


TableEs1BacktestHistoricalSimulation <- ESTest(alpha = 0.01,
                                               LogReturnsRealized,
                                               LogReturnsES1RollHistoricalSimulation,
                                               LogReturnsVaR1RollHistoricalSimulation)
TableEs1BacktestHistoricalSimulation


#####
# FORECASTING WITH EGARCH(1, 1)
#####

BestGarchForecast <- ugarchforecast(BestGarchFit, data = LogReturns, n.ahead = 10)

BestGarchForecastSigma <- sigma(BestGarchForecast)
BestGarchForecastMean  <- fitted(BestGarchForecast)
BestGarchForecastDegreesOfFreedom <- BestGarchFit@fit$coef["shape"]
qf <- function(x) qdist(BestModelDistribution, p = x, shape = BestGarchForecastDegreesOfFreedom)

ForecastingEsToleranceLevel <- 0.05
LogReturnsVar5Forecast <- BestGarchForecastMean + BestGarchForecastSigma*qdist(BestModelDistribution,
                                                                               ForecastingEsToleranceLevel,
                                                                               shape = BestGarchForecastDegreesOfFreedom)
LogReturnsEs5Forecast  <- BestGarchForecastMean + BestGarchForecastSigma*
  (1/ForecastingEsToleranceLevel * integrate(qf, 0, ForecastingEsToleranceLevel)$value)  

ForecastingEsToleranceLevel <- 0.01
LogReturnsVar1Forecast <- BestGarchForecastMean + BestGarchForecastSigma*qdist(BestModelDistribution,
                                                                               ForecastingEsToleranceLevel,
                                                                               shape = BestGarchForecastDegreesOfFreedom)
LogReturnsEs1Forecast  <- BestGarchForecastMean + BestGarchForecastSigma*
  (1/ForecastingEsToleranceLevel * integrate(qf, 0, ForecastingEsToleranceLevel)$value)

cbind(LogReturnsVar5Forecast, LogReturnsVar1Forecast, LogReturnsEs5Forecast, LogReturnsEs1Forecast)[, 1]


#####
# GRAPH FOR THE 1ST PARAGRAPH
#####
TableWig20 <- data.table(Instrument = c("PKOBP", "KGHM", "PZU", "PKNORLEN", "ALLEGRO", "CDPROJEKT", "DINOPL", "PEKAO", "PGNIG", "LPP", 
                                "CYFRPLSAT", "SANPL", "PGE", "ORANGEPL", "LOTOS", "ASSECOPOL", "CCC", "TAURONPE", "JSW", "MERCATOR"),
                         Share = round(c(13.365, 12.420, 9.267, 8.874, 8.294, 6.599, 6.051, 6.027, 4.852,	4.123, 3.918, 3.409, 2.608,
                                         2.029, 1.818, 1.806, 1.717, 1.37, 0.782, 0.670), 2))

ggplot(TableWig20, aes(Instrument, Share)) +
  geom_bar(stat = "identity", fill = "deepskyblue3", color = "darkgrey") +
  geom_text(aes(label = Share), size = 3.5, vjust = -0.3) +
  theme_minimal() +
  labs(x = "Nazwa skrocona instrumentu", y = "Udzial w portfelu, %") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_y_continuous(breaks = seq(0, 15, 2.5)) +
  ylim(0, 15)
