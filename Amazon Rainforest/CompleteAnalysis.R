setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Reading and tidying----
source('Libraries.R')
source('Functions.R')
source('TrmmLoading.R')
source('LandfluxAetLoading.R')
source('PrincetonPetLoading.R')
source('LoadCru.R')
source('ComputeSpei.R')
source('WaterDeficitTibbleCompilation.R')
source('CwdAnomalies.R')

#Plotting
source('DryMonthsAndAnnualRainfallPlotting.R')
source('GatheringAndPlottingAnomalies.R')
source('Correlation-GLK.R')
source('TemporalEvolutionOfDroughtExtent.R')
source('SpatialCorrelationAcrossIndices.R')
source('spiSpei2005.R')