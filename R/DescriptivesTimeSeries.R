#
# Copyright (C) 2013-2018 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

DescriptivesTimeSeriesInternal <- function(jaspResults, dataset, options) {
    jaspDescriptives::.DescriptivesTimeSeries(jaspResults, dataset, options)
}

.tsStateSpacePlot <- function(jaspResults, dataset, options, ready, position) {
  if (!options$stateSpacePlot)
    return()

  if (is.null(jaspResults[["stateSpacePlot"]])) {
    plot <- createJaspPlot(title = "State Space Plot")
    plot$dependOn(c("stateSpacePlot", "lag", "regressionType", "addSmooth", "addSmoothCI", "addSmoothCIValue", "dependentVariable"))
    plot$position <- position

    jaspResults[["stateSpacePlot"]] <- plot

    if (!ready)
      return()

    .tsFillStateSpacePlot(plot, dataset, options)
  }
}

.tsFillStateSpacePlot <- function(stateSpacePlot, dataset, options) {
  yName <- options$dependentVariable[1]
  y     <- dataset[, yName]
  yLag  <- c(rep(NA, options$lag), y[1:(length(y) - options$lag)])

  yName <- decodeColNames(yName)
  xName <- as.expression(bquote(.(yName)[t-.(options$lag)]))
  yName <- as.expression(bquote(.(yName)[t]))

  dat <- data.frame(y, yLag)
  dat <- na.omit(dat)

  # forceLinearSmooth <- options$regressionType == "linear"
  # Does not work with bquote
  p <- jaspGraphs::JASPScatterPlot(dat$yLag, dat$y,
                                   xName = xName, 
                                   yName = yName,
                                   addSmooth = options$addSmooth,
                                   addSmoothCI = options$addSmoothCI,
                                   smoothCIValue = options$addSmoothCIValue,
                                   forceLinearSmooth = options$regressionType == "linear",
                                   plotAbove = "none", plotRight = "none"
                                   )

  stateSpacePlot$plotObject <- p
  
  return()
}

.tsACF <- function(jaspResults, dataset, options, ready, position){
  if (!is.null(jaspResults[["acfContainer"]]))
    return()

  acfContainer <- createJaspContainer(title = gettext("Autocorrelation Function Plots"))
  acfContainer$dependOn(c("acfPlots", "acfCI", "acfCIValue", "dependentVariable"))
  jaspResults[["acfContainer"]] <- acfContainer
  jaspResults[["acfContainer"]]$position <- position

  if (!ready) {
    return()
  }

  if (options$acfPlots) {
    acfPlot <- createJaspPlot(title = "Autocorrelation Function")
    acfPlot$dependOn(c("acfPlot", "acfCI", "acfCIValue", "dependentVariable"))
    acfPlot$position <- 1
    acfContainer[["acfPlot"]] <- acfPlot

    .tsFillACF(acfPlot, type = "ACF", dataset, options)

    pacfPlot <- createJaspPlot(title = "Partial Autocorrelation Function")
    pacfPlot$dependOn(c("pacfPlot", "pacfCI", "pacfCIValue", "dependentVariable"))
    pacfPlot$position <- 2
    acfContainer[["pacfPlot"]] <- pacfPlot

    .tsFillACF(pacfPlot, type = "PACF", dataset, options)
  }
}

.tsFillACF <- function(plot, type, dataset, options) {
  y <- dataset[, options$dependentVariable[1]]
  y <- na.omit(y)

  yACF  <- acf(y, plot = F)
  yPACF <- pacf(y, plot = F)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(c(yACF$lag, yPACF$lag))

  p <- ggplot2::ggplot()
  if (options$acfCI) {
    clim      <- qnorm((1 + options$acfCIValue) / 2) / sqrt(yACF$n.used)
    dfSegment <- data.frame(x = min(xBreaks), xend = max(xBreaks), y = c(clim, -clim))

    p <- p +
      ggplot2::geom_segment(ggplot2::aes(x = x, xend = xend, y = y, yend = y),
                            linetype = "dashed", color = "blue", data = dfSegment)
  }

  yBreaks <- jaspGraphs::getPrettyAxisBreaks(c(yACF$acf, yPACF$acf, clim, -clim))

  if (type == "ACF")  dat <- data.frame(acf = yACF$acf, lag = yACF$lag)
  if (type == "PACF") dat <- data.frame(acf = yPACF$acf, lag = yPACF$lag)

  p <- p +
    ggplot2::geom_linerange(data = dat, ggplot2::aes(x = lag, ymin = 0, ymax = acf)) +
    ggplot2::scale_x_continuous(name = "Lag", breaks = xBreaks, limits = range(xBreaks)) +
    ggplot2::scale_y_continuous(name = type, breaks = yBreaks, limits = range(yBreaks))

  p <- jaspGraphs::themeJasp(p)

  plot$plotObject <- p
  
  return()
}

.tsPowerSpectralDensity <- function(jaspResults, dataset, options, ready, position){
  if (!options$powerSpectralDensity)
    return()

  if (is.null(jaspResults[["powerSpectralDensity"]])) {
    plot <- createJaspPlot(title = "Power Spectral Density Plot")
    plot$dependOn(c("powerSpectralDensity",
                    "detrend", "demean", 
                    "smoothing", "kernel", "m1", "m2", 
                    "taper",
                    "scaling", "noScaling", "log", "log10",
                    "dependentVariable"))
    plot$position <- position

    jaspResults[["powerSpectralDensity"]] <- plot

    if (!ready)
      return()

    .tsFillPowerSpectralDensity(plot, dataset, options)
  }
}

.tsFillPowerSpectralDensity <- function(powerSpectralDensity, dataset, options) {
  y <- dataset[, options$dependentVariable[1]]
  y <- na.omit(y)

  k <- NULL

  # crashes when modified daniell has zeros..
  if (options$smoothing)
    k <- kernel(options$kernel, c(options$m1, options$m2))
  
  yPSD <- spec.pgram(y, 
                     kernel = k,
                     taper = options$taper,
                     demean = options$demean,
                     detrend = options$detrend,
                     plot = F)

  dat <- data.frame(x = yPSD$freq, y = yPSD$spec)

  xBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$x)
  yBreaks <- jaspGraphs::getPrettyAxisBreaks(dat$y)

  p <- ggplot2::ggplot(dat, ggplot2::aes(x = x, y = y)) + jaspGraphs::geom_line() +
    ggplot2::scale_x_continuous(name = "Frequency", breaks = xBreaks) +
    ggplot2::scale_y_continuous(name = "Spectrum", breaks = yBreaks)

  if (options$scaling != "noScaling") {
    logTrans <- options$scaling
    logFunction <- function(x) exp(x)
    logLabels <- scales::math_format(e ^ .x)
    if (logTrans == "log10") {
      logFunction <- function(x) 10 ^ x
      logLabels <- scales::math_format(10 ^ .x)
    }

    p <- p + ggplot2::scale_y_continuous(trans = logTrans,
                                         breaks = scales::trans_breaks(logTrans, logFunction),
                                         labels = scales::trans_format(logTrans, logLabels))
  }
  
  p <- jaspGraphs::themeJasp(p)

  powerSpectralDensity$plotObject <- p
  
  return()
}