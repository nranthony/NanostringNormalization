source("R/NormMethods.R")
source("R/nS_nrmComps_plots.R")

########################### Molania paper data

#***** Reading Nanostring gene expression raw data
Nano_ExpressionMatrix <- read.delim('Example 1_LungCancerStudy/LungCancer_Nanostring_RawCounts.txt',
                                    stringsAsFactors = FALSE, header = TRUE, as.is = TRUE)
#***** Reading sample and clinical information
Nano_SampleInfo <- read.delim('Example 1_LungCancerStudy/LungCancer_Nanostring_SampleInformation.txt',
                              stringsAsFactors = FALSE, header = TRUE, as.is = TRUE)



########################## Per Lane

Nano_EndogenousMatrix <- Nano_ExpressionMatrix |>
  dplyr::filter(Code.Class == "Endogenous") |>
  tibble::column_to_rownames("Name") |>
  dplyr::select(tidyr::contains(".RCC")) |>
  as.matrix()

# rownames(Nano_EndogenousMatrix) <- dplyr::filter(Nano_ExpressionMatrix, Code.Class == "Endogenous")$Name

LaneSumNormalized <- log2(
  apply(Nano_EndogenousMatrix, MARGIN = 2, FUN = function(col) {
  return(col/sum(col))
}))

RLEBoxPlot(NanostringNormalized, 'nanoString Normalized')
RLEBoxPlot(LaneSumNormalized, 'Lane Sum Normalized')
RLEBoxPlot(RUVcorrected, 'RUV Normalized')



############################  meta analysis of RLE points
data_mat <- NanostringNormalized
library(ggplot2)


df <- data.frame(NanostringNormalized - matrixStats::rowMedians(NanostringNormalized))

df <- data.frame(NanostringNormalized - matrixStats::rowMedians(NanostringNormalized),
                 LaneSumNormalized - matrixStats::rowMedians(LaneSumNormalized),
                 RUVcorrected - matrixStats::rowMedians(RUVcorrected))

# create function and use to generate a long table of medians and mads for each norm type
rle_data <- data_mat - matrixStats::rowMedians(data_mat)
rle_medians <- apply(rle_data, 2, median) #|> tibble::as.tibble()
rle_mads <- apply(rle_data, 2, mad) #|> tibble::as.tibble()

# Create a histogram
histogram_plot <- ggplot(data = data, aes(x = value)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Histogram", x = "Values", y = "Frequency")
# histogram_plot

# Create a density plot
density_plot <- ggplot(data = data, aes(x = value)) +
  geom_density(fill = "green", alpha = 0.5) +
  labs(title = "Density Plot", x = "Values", y = "Density")
# density_plot

# Display both plots side by side
library(gridExtra)
grid.arrange(histogram_plot, density_plot, ncol = 2)
