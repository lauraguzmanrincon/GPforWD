# FROM "WhiteDwarfs" folder

# Load and process ----
#' type: "EBVfree" or "EBVfixed"
#' Returns: list of observed period, emprirical distribution of parameters, and range of interest of parameters (relevant if uniform)
#' Notes: copied from R01.R/II. Old version stored (observablesSummary_EBVfree, observablesSummary_EBVfixed, readme) in "R01pII_06042021_observables_summary_both.RData"
getObservables <- function(type){
  
  # Measured period for interpolation (for both fixed and free)
  periodHourMeasure <- 7.13351186
  periodDayMeasure <- periodHourMeasure/24
  
  # Distribution of observed measures (for both fixed and free)
  # Teff (normal)
  teffParameters <- c(mean = 4382, sd = 326)
  teffRange <- c(min = 4382 - 326, max = 4382 + 326)
  
  # Mass (uniform)
  massParameters <- c(min = 0.23, max = 0.44)
  massRange <- massParameters
  
  # For empirical ones using observables:
  if(type == "EBVfree"){
    observables <- data.table(read.csv("Data/Observables/observables_EBVfree.txt", sep = " "))
  }else if(type == "EBVfixed"){
    observables <- data.table(read.csv("Data/Observables/observables_EBVfixed.txt", sep = " "))
  }else{
    stop("Invalid type")
  }
  
  # Abundances (emprirical)
  # Old one: logCNParameters <- c(min = -0.5 - 0.16, max = -0.5 + 0.16)
  logCNParameters <- c(mean = mean(observables$abundances.ratio), sd = sd(observables$abundances.ratio))
  logCNRange <- logCNParameters
  
  # Mdot (empirical)
  # OLD one: mdotParameters <- c(min = -9.731264295642008, max = -9.597336671464438)
  mdotParameters <- c(mean = mean(observables$log10_accretion_rate), sd = sd(observables$log10_accretion_rate))
  mdotRange <- mdotParameters
  
  # Output
  observablesSummary <- list(periodHourMeasure = periodHourMeasure, periodDayMeasure = periodDayMeasure,
                             teffParameters = teffParameters, teffRange = teffRange, massParameters = massParameters, massRange = massRange,
                             logCNParameters = logCNParameters, logCNRange = logCNRange, mdotParameters = mdotParameters, mdotRange = mdotRange,
                             observables = observables)
  return(observablesSummary)
}

#' type: "EBVfree" or "EBVfixed"
#' date: "ddmmyyyy" format of the creation day
#' File path of the form "Fixed003" must exist
#' Notes: copied and generalised from R01.R/I.
readSaveSimulations <- function(type, index, dateToday){
  if(!type %in% c("EBVfree", "EBVfixed")){
    stop("Invalid type")
  }
  
  outputFile <- paste0("Data/ProcessedData/R04Fun_", ifelse(type == "EBVfree", "Free", "Fixed"), sprintf("%03d", index), "_", dateToday, ".RData")
  if(!file.exists(outputFile)){
    # Read list of files
    filesPath <- paste0("Data/", ifelse(type == "EBVfree", "Free", "Fixed"), sprintf("%03d", index), "/")
    if(!file.exists(filesPath)){
      stop("File with simulations does not exist")
    }
    listFiles <- list.files(path = paste0(filesPath), pattern = glob2rx("Mwd*_Md*_Porb*.data")) # :0 !!!! glob2rx("Mwd*_Md*_Porb*.data")
    
    # Load files
    tableList <- vector("list", length(listFiles))
    for(ii in 1:length(listFiles)){
      cat(ii, "...")
      textFromFile <- as.double(strsplit(listFiles[ii], split = "Mwd|\\_Md|\\_Porb|\\.data")[[1]][2:4])
      
      tryCatch({
        tt1 <- data.table(read.table(paste0(filesPath, listFiles[ii]), skip = 5, header = T))
        tableList[[ii]] <- tt1[, .(period_days, log_Teff, star_mass, surface_c12, surface_n14, lg_mstar_dot_1, mwd = textFromFile[1], md = textFromFile[2], pp = textFromFile[3])]
      }, error = function(e) {cat("SKIPPED ERROR for ", listFiles[ii], ":", conditionMessage(e), "\n")})
      
    }
    
    # Create output
    tableData <- do.call("rbind", tableList)
    # Add labels
    tableData[, gr := paste0("mass=", sprintf("%.3f", md), ",Porb=", sprintf("%.3f", pp))]
    tableData[, mLabel := paste0("mass=", sprintf("%.3f", md), "M")]
    tableData[, porbLabel := paste0("Porb=", sprintf("%.3f", pp), "d")]
    # Add main transformed variables
    tableData[, logCN := log10((surface_c12/12.0107)/(surface_n14/14.0067))]
    tableData[, Teff := 10^log_Teff]
    
    # Save
    save(tableData, file = outputFile)
  }else{
    cat("The file '", outputFile, "' already exists")
  }
}

#' Stack data from simulations and remove duplicates
#' Read saved files from readSavesimulations(). Assumes uniqueness in source files
#' Notes: copied and adapted from R01.R/I.
stackSimulationsNoDup <- function(type, indices){
  # Stack datasets
  tableDataList <- vector("list", length(indices))
  for(ii in 1:length(indices)){
    index <- indices[ii]
    # Assumes uniqueness in file dates:
    outputFile <- list.files(path = "Data/ProcessedData/", pattern = glob2rx(paste0("R04Fun_", ifelse(type == "EBVfree", "Free", "Fixed"), sprintf("%03d", index), "_*.RData")))
    cat(paste0("Data/ProcessedData/", outputFile), "\n")
    load(paste0("Data/ProcessedData/", outputFile))
    tableDataList[[ii]] <- data.table(tableData, simIndex = index)
  }
  tableAll <- do.call("rbind", tableDataList)
  
  # Create table with no duplicates
  tableAll[, N := .N, .(simIndex, period_days, log_Teff, star_mass, surface_c12, surface_n14, lg_mstar_dot_1, md, pp)]
  cat("There are", tableAll[, .(size = .N), N][N > 1, size]/2, "duplicated rows...")
  tableAll[N > 1, rankDup := frank(N, ties.method = "random"), .(simIndex, period_days, log_Teff, star_mass, surface_c12, surface_n14, lg_mstar_dot_1, md, pp)]
  print(nrow(tableAll[N == 2, .N, rankDup][,.N,N]) == 1)
  tableAll[, isDup := N == 2 & rankDup == 2]
  print(sum(tableAll$isDup == T) == tableAll[, .(size = .N), N][N >1, size]/2)
  
  # Output
  tableNoDup <- tableAll[isDup == F]
  return(tableNoDup)
}

#' tableNoDup: table with simulations without duplicated rows, output from ()
#' Return: dataClose: Interpolated values of main variables (Teff, logCN, star_mass, lg_mstar_dot_1) for given period (in hours) using tableData
#' Notes: copied and adapted from R01.R/I.
interpolateValues <- function(tableNoDup, period_inHours){
  period_inDays <- period_inHours/24
  
  tableNoDup[, idGr := frank(gr, ties.method = "dense")]
  dataClose <- tableNoDup[, .(numObs = .N, fromSim = min(simIndex)), .(md, pp, gr, mLabel, porbLabel, idGr)]
  for(ii in 1:nrow(dataClose)){
    tempTable <- tableNoDup[idGr == ii]
    dataClose[idGr == ii, Teff := approx(x = tempTable[order(period_days), period_days], y = tempTable[order(period_days), Teff], xout = period_inDays)$y]
    dataClose[idGr == ii, logCN := approx(x = tempTable[order(period_days), period_days], y = tempTable[order(period_days), logCN], xout = period_inDays)$y]
    dataClose[idGr == ii, star_mass := approx(x = tempTable[order(period_days), period_days], y = tempTable[order(period_days), star_mass], xout = period_inDays)$y]
    dataClose[idGr == ii, lg_mstar_dot_1 := approx(x = tempTable[order(period_days), period_days], y = tempTable[order(period_days), lg_mstar_dot_1], xout = period_inDays)$y]
  }
  dataClose[, ":="(period_hours = 24*period_inDays, period_days = period_inDays)]
  
  return(dataClose)
}

# Create error grid ----

#' Compute error measures
#' Error 2 is the one that makes sense. Divides by sd.
#' Notes: copied from R03.R/"Compute error measures"
#' Control: 20210917 - add error measure 3, based on relative error
computeErrorMeasures <- function(dataClose, parameterList){
  list2env(parameterList, environment())
  
  # Measure 1: ?  Measure 2: standardised one Measure 3: relative error
  dataClose[, ":="(teffError = as.numeric(NA), cnError = as.numeric(NA), massError = as.numeric(NA), mdotError = as.numeric(NA))]
  for(ii in 1:nrow(dataClose)){
    #dataClose[idGr == ii, Teff]
    
    varParameters <- teffParameters
    samplesTeff <- rnorm(5000, mean = varParameters["mean"], sd = varParameters["sd"]) # NORM
    transfSamplesTeff <- abs(samplesTeff - dataClose[idGr == ii, Teff])/(abs(samplesTeff) + abs(dataClose[idGr == ii, Teff]))
    dataClose[idGr == ii, teffError := mean(transfSamplesTeff)]
    transfSamplesTeff2 <- abs(samplesTeff - dataClose[idGr == ii, Teff])/(sd(dataClose$Teff, na.rm = T))
    dataClose[idGr == ii, teffError2 := mean(transfSamplesTeff2)]
    transfSamplesTeff3 <- abs(samplesTeff - dataClose[idGr == ii, Teff])/abs(samplesTeff)
    dataClose[idGr == ii, teffError3 := mean(transfSamplesTeff3)]
    
    varParameters <- logCNParameters
    #samplesCN <- runif(5000, min = varParameters["min"], max = varParameters["max"]) # UNIF
    samplesCN <- observables$abundances.ratio
    transfSamplesCN <- abs(samplesCN - dataClose[idGr == ii, logCN])/(abs(samplesCN) + abs(dataClose[idGr == ii, logCN]))
    dataClose[idGr == ii, cnError := mean(transfSamplesCN)]
    transfSamplesCN2 <- abs(samplesCN - dataClose[idGr == ii, logCN])/(sd(dataClose$logCN, na.rm = T))
    dataClose[idGr == ii, cnError2 := mean(transfSamplesCN2)]
    transfSamplesCN3 <- abs(samplesCN - dataClose[idGr == ii, logCN])/abs(samplesCN)
    dataClose[idGr == ii, cnError3 := mean(transfSamplesCN3)]
    
    varParameters <- massParameters
    samplesMass <- runif(5000, min = varParameters["min"], max = varParameters["max"]) # UNIF
    transfSamplesMass <- abs(samplesMass - dataClose[idGr == ii, star_mass])/(abs(samplesMass) + abs(dataClose[idGr == ii, star_mass]))
    dataClose[idGr == ii, massError := mean(transfSamplesMass)]
    transfSamplesMass2 <- abs(samplesMass - dataClose[idGr == ii, star_mass])/(sd(dataClose$star_mass, na.rm = T))
    dataClose[idGr == ii, massError2 := mean(transfSamplesMass2)]
    transfSamplesMass3 <- abs(samplesMass - dataClose[idGr == ii, star_mass])/abs(samplesMass)
    dataClose[idGr == ii, massError3 := mean(transfSamplesMass3)]
    
    varParameters <- mdotParameters
    #samplesMdot <- runif(5000, min = varParameters["min"], max = varParameters["max"]) # UNIF
    samplesMdot <- observables$log10_accretion_rate
    transfSamplesMdot <- abs(samplesMdot - dataClose[idGr == ii, lg_mstar_dot_1])/(abs(samplesMdot) + abs(dataClose[idGr == ii, lg_mstar_dot_1]))
    dataClose[idGr == ii, mdotError := mean(transfSamplesMdot)]
    transfSamplesMdot2 <- abs(samplesMdot - dataClose[idGr == ii, lg_mstar_dot_1])/(sd(dataClose$lg_mstar_dot_1, na.rm = T))
    dataClose[idGr == ii, mdotError2 := mean(transfSamplesMdot2)]
    transfSamplesMdot3 <- abs(samplesMdot - dataClose[idGr == ii, lg_mstar_dot_1])/abs(samplesMdot)
    dataClose[idGr == ii, mdotError3 := mean(transfSamplesMdot3)]
  }
  dataClose[, totalError := (teffError + cnError + massError + mdotError)/ 4]
  dataClose[, totalError2 := (teffError2 + cnError2 + massError2 + mdotError2)/ 4]
}

# Examples ----
#parameterList <- getObservables(type = "EBVfree")

##readSaveSimulations(type = "EBVfree", index = 1, dateToday = "22022021") # ~3min
##readSaveSimulations(type = "EBVfixed", index = 1, dateToday = "06042021") # ~5min
##readSaveSimulations(type = "EBVfixed", index = 2, dateToday = "02072021") # ~5min

#tableNoDup <- stackSimulationsNoDup("EBVfree", 1)
#tableNoDup <- stackSimulationsNoDup("EBVfixed", 1:2)

#dataClose <- interpolateValues(tableNoDup, periodHourMeasure) # ~2sec

#dataCloseError <- computeErrorMeasures(dataClose, parameterList) # ~3sec / ~10sec

# Plots ----
#' plotAllGroups: if TRUE, plots varToPlot for every group. If FALSE, plots groupToPlot for every variable
#' variableToPlot: (if plotAllGroups == T) "Teff", "logCN", "star_mass", "lg_mstar_dot_1"
#' groupToPlot: (if plotAllGroups == F) e.g. "mass=1.000,Porb=2.400"
#' Example: plotSimulations(plotAllGroups = T, variableToPlot = "Teff", groupToPlot = NULL)
#' Example: plotSimulations(plotAllGroups = F, variableToPlot = NULL, groupToPlot = "mass=1.000,Porb=2.400")
plotSimulations <- function(plotAllGroups, variableToPlot, groupToPlot){
  # Trick to avoid repetition. If plotAllGroups, only one loop will be done
  if(plotAllGroups){
    varToPlotList <- variableToPlot
  }else{
    varToPlotList <- c("Teff", "logCN", "star_mass", "lg_mstar_dot_1")
  }
  
  # Plot limits
  teffPlotLims <- c(min = 3000, max = 9000)
  logCNPlotLims <- c(min = -3, max = 1)
  massPlotLims <- c(min = 0, max = 2)
  mdotPlotLims <- c(min = -10, max = -6.3)
  
  # Loop per variable in varToplot
  plotList <- vector("list", length(varToPlotList))
  for(ii in 1:length(varToPlotList)){
    varToPlot <- varToPlotList[ii]
    
    # Parameters for plot
    suppressWarnings(tableNoDup[, varToPlot := NULL])
    tableNoDup[, varToPlot := get(varToPlot)]
    suppressWarnings(dataClose[, varToPlot := NULL])
    dataClose[, varToPlot := get(varToPlot)]
    if(varToPlot == "Teff"){
      varRange <- c(min = parameterList$teffRange["min"], max = parameterList$teffRange["max"])
      varPlotLims <- teffPlotLims
      varAxis <- "Teff (K)"
      filePlot <- "teff"
    }else if(varToPlot == "logCN"){
      varRange <- c(min = parameterList$logCNRange["mean"] - parameterList$logCNRange["sd"], max = parameterList$logCNRange["mean"] + parameterList$logCNRange["sd"])
      varPlotLims <- logCNPlotLims
      varAxis <- "Log C/N"
      filePlot <- "logCN"
    }else if(varToPlot == "star_mass"){
      varRange <- c(min = parameterList$massRange["min"], max = parameterList$massRange["max"])
      varPlotLims <- massPlotLims
      varAxis <- "Mass (M.)"
      filePlot <- "massDot"
    }else if(varToPlot == "lg_mstar_dot_1"){
      varRange <- c(min = parameterList$mdotRange["mean"] - parameterList$mdotRange["sd"], max = parameterList$mdotRange["mean"] + parameterList$mdotRange["sd"])
      varPlotLims <- mdotPlotLims
      varAxis <- "Log M_donor"
      filePlot <- "logMdonor"
    }
    
    if(plotAllGroups){
      # Plot facet with all groups
      plotList[[ii]] <- ggplot(tableNoDup[order(md, 24*period_days)], aes(x = 24*period_days, y = varToPlot, colour = factor(md))) + theme_laura() + facet_wrap(. ~ pp) +
        geom_rect(xmin = -Inf, xmax = Inf, ymin = min(varRange), ymax = max(varRange), colour = NA, fill = "gray50", alpha = 0.5) +
        geom_vline(xintercept = parameterList$periodHourMeasure, colour = "gray50") +
        #geom_point(size = 0.5) +
        geom_path(size = 0.5) +
        geom_point(data = dataClose, size = 3, colour = "black") +
        scale_x_continuous(limits = c(0, 50), expand = c(0,0)) + scale_y_continuous(limits = varPlotLims, expand = c(0,0)) +
        labs(x = "P_orb (hr)", y = varAxis)
    }else{
      # Plot single
      dataToPlot <- tableNoDup[gr == groupToPlot]
      plotList[[ii]] <- ggplot(dataToPlot, aes(x = 24*period_days, y = varToPlot)) + theme_laura() +
        geom_rect(xmin = -Inf, xmax = Inf, ymin = min(varRange), ymax = max(varRange), colour = NA, fill = "gray50", alpha = 0.5) +
        geom_vline(xintercept = parameterList$periodHourMeasure, colour = "gray50") +
        #geom_point(size = 0.5) +
        geom_path(size = 0.5) +
        scale_x_continuous(limits = c(0, 50), expand = c(0,0)) + scale_y_continuous(limits = varPlotLims, expand = c(0,0)) +
        labs(title = paste0(varAxis, " (", groupToPlot, ")"), x = "P_orb (hr)", y = varAxis)
    }
  }
  if(plotAllGroups){
    return(plotList[[1]])
  }else{
    return(multiplot(plotList[[1]], plotList[[2]], plotList[[3]], plotList[[4]], cols = 2))
  }
  
}

# Other functions ----
insert_minor <- function(major_labs, n_breaks, n_minor){
  labs <- c(sapply(major_labs, function(x) c(sprintf('%#.1f', x), rep("", n_minor))))
  labs[1:n_breaks]
}
