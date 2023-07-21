source("R/NormMethods.R")
source("R/nS_nrmComps_plots.R")


Nano_EndogenousMatrix <- Nano_ExpressionMatrix |>
  dplyr::filter(Code.Class == "Endogenous") |>
  dplyr::select(tidyr::contains(".RCC")) |>
  as.matrix()

rownames(Nano_EndogenousMatrix) <- dplyr::filter(Nano_ExpressionMatrix, Code.Class == "Endogenous")$Name

LaneSumNormalized <- log2(
  apply(Nano_EndogenousMatrix, MARGIN = 2, FUN = function(col) {
  return(col/sum(col))
}))

RLEBoxPlot(NanostringNormalized, 'nanoString Normalized')
RLEBoxPlot(LaneSumNormalized, 'Lane Sum Normalized')
RLEBoxPlot(RUVcorrected, 'RUV Normalized')
