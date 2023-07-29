

RLEBoxPlot <- function(data_mat, title_str) {
  #***** RLE plots - Figure 1 A - nCounter normalization
  # y_limits = c(-4.1,4.1)
  y_limits = c(-3,3)
  # y_limits = c(-8,8)
  # y_limits = c(-0.5,0.5)

  par(mar = c(6.5,6.5,2.3,0))
  boxplot(data_mat - matrixStats::rowMedians(data_mat),
          main = '',
          xlab = '',
          ylab = '',
          xaxt = 'n',
          yaxt = 'n',
          ylim = y_limits,
          outline = FALSE,
          names = FALSE,
          frame = FALSE,
          whisklty = 3,
          whisklwd = 1.5,
          staplelty = 1,
          notch = TRUE,
          boxlwd = 2,
          staplelwd = 0,
          boxcol = Color_Batches[factor(Nano_SampleInfo$Cartridges)],
          border = Color_Batches[factor(Nano_SampleInfo$Cartridges)],
          col = 'gray87')
  box(lwd = 4, bty = 'l')
  title(title_str, line = -2, cex.main =2)
  Median_Nano <- apply(data_mat - matrixStats::rowMedians(data_mat), 2, median)
  points(c(1:ncol(data_mat)), Median_Nano, col = Color_Batches[factor(Nano_SampleInfo$Cartridges)], pch = 19, cex = 1.2)
  axis(2, mgp = c(3.5, .9 ,0), lwd.ticks=6, las=1, cex.axis=3)
  mtext('RLE', 2, line = 3.5, cex = 2)
  abline(h = 0, col = 'black', lwd = 5, lty = 2)
  par(lwd = 3)
  # axis.break(2, -4.2, style = 'zigzag', brw = .02)
}

RLEDistributions <- function(data_mat, title_str) {
  rle_data <- data_mat - matrixStats::rowMedians(data_mat)
  data_medians <- apply(rle_data, 2, median)
  data_mads <- apply(rle_data, 2, mad)

}
