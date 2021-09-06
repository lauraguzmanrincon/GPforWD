
setwd("/home/laura/Documents/PostPhD/WhiteDwarfs/")
source("/home/laura/Documents/PostPhD/JUNIPER/local_data_interventions/R_files/00_Functions.R")
source("/home/laura/Documents/PostPhD/WhiteDwarfs/R04Fun_Main.R")
allPlotsPath <- "/home/laura/Documents/PostPhD/WhiteDwarfs/"
library(ggplot2)
library(data.table)
library(ggridges)
library(viridis)
library(latex2exp)
library(dplyr) # needed for plots (case_when)


#parameterList <- getObservables(type = "EBVfree")
#parameterList <- getObservables(type = "EBVfixed")

##readSaveSimulations(type = "EBVfree", index = 1, dateToday = "22022021") # ~3min
##readSaveSimulations(type = "EBVfixed", index = 1, dateToday = "06042021") # ~5min
##readSaveSimulations(type = "EBVfixed", index = 2, dateToday = "02072021") # ~5min
##readSaveSimulations(type = "EBVfixed", index = 3, dateToday = "07072021") # ~5min

#tableNoDup <- stackSimulationsNoDup("EBVfree", 1)
#tableNoDup <- stackSimulationsNoDup("EBVfixed", 1:2)

#dataClose <- interpolateValues(tableNoDup, parameterList$periodHourMeasure) # ~2sec

#dataCloseError <- computeErrorMeasures(dataClose, parameterList) # ~3sec / ~10sec

# Load - process error for BO
parameterList <- getObservables(type = "EBVfixed")
tableNoDup <- stackSimulationsNoDup("EBVfixed", 1:3)
dataClose <- interpolateValues(tableNoDup, parameterList$periodHourMeasure) # ~2sec
dataCloseError <- computeErrorMeasures(dataClose, parameterList) # ~3sec / ~10sec

# Plot Grid
gGrid <- ggplot(dataClose, aes(x = md, y = pp, colour = totalError2)) + theme_laura() +
  geom_point(size = 10) +
  scale_x_continuous(breaks = seq(1,2,0.1), labels = insert_minor(seq(1,2,0.2), length(seq(1,2,0.1)), 1)) + # TODO watch out breaks
  scale_y_continuous(breaks = seq(2.4,3.4,0.1), labels = insert_minor(seq(2.4,3.4,0.2), length(seq(2.4,3.4,0.1)), 1)) + # TODO watch out breaks
  scale_colour_viridis(option = "B", direction = 1) +
  theme(strip.background = element_rect(fill = NA),
        legend.key = element_rect(fill = NA, colour = "gray90")) +
  labs(y = TeX("$P_{orb;i}$ (days)"), x = TeX("$M_{donor;i}$ ($M_\u0298$)"), colour = "error band") +
  guides(colour = guide_colourbar(title = "track error", title.position = "right", barheight = unit(10, "cm"), title.theme = element_text(angle = 270, hjust = 0.5)))
gGrid
#savePDF(gGrid, "Plot02072021_01_R04_EBVfixed_error3D", 8, 8)
#write.csv(x = dataClose[, .(mass = md, porb = pp, label = gr, numSimulation = fromSim, Teff, logCN, star_mass, lg_mstar_dot_1, period_hours, period_days,
#                        teffError2, cnError2, massError2, mdotError2, totalError2)], file = "Output02072021_01_R04_EBVfixed_error.csv", row.names = F)
todayIs <- format(Sys.Date(), format = "%Y%m%d")
write.csv(x = dataClose[, .(mass = md, porb = pp, label = gr, numSimulation = fromSim, Teff, logCN, star_mass, lg_mstar_dot_1, period_hours, period_days,
                            teffError2, cnError2, massError2, mdotError2, totalError2)],
          file = paste0("GPyOpt/Output_errorFromR/File", todayIs, "_R04_EBVfixed_sim1to3_errors.csv"), row.names = F)



# Individual
plotSimulations(plotAllGroups = T, variableToPlot = "Teff", groupToPlot = NULL)
plotSimulations(plotAllGroups = F, variableToPlot = NULL, groupToPlot = "mass=1.000,Porb=2.400")
plotSimulations(plotAllGroups = F, variableToPlot = NULL, groupToPlot = "mass=1.704,Porb=2.898")
plotSimulations(plotAllGroups = F, variableToPlot = NULL, groupToPlot = "mass=1.700,Porb=2.900")
#savePDF(plotSimulations(plotAllGroups = F, variableToPlot = NULL, groupToPlot = "mass=1.704,Porb=2.898"), "Plot07072021_01_R04_EBVfixed_Md1p704_Porb2p400", 10, 10)
#savePDF(plotSimulations(plotAllGroups = F, variableToPlot = NULL, groupToPlot = "mass=1.700,Porb=2.900"), "Plot07072021_01_R04_EBVfixed_Md1p700_Porb2p898", 10, 10)
plotSimulations(plotAllGroups = F, variableToPlot = NULL, groupToPlot = "mass=1.000,Porb=3.100")


# TODO
# Error sensitivity? - create other measures?

# jupyter notebook
# jupyter-lab