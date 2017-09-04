library(corrplot)
library(ggplot2)

df_t <- data.frame("x" = rnorm(20000), "y" = rexp(20000, 10))

# colored scatter
df_t$d <- densCols(  # 手动计算颜色
    df_t$x, df_t$y, colramp = colorRampPalette(rev(rainbow(10, end = 4/6)))
)
ggplot(df_t, aes(x, y)) + 
    geom_point(aes(color = d), shape = 16, alpha = I(0.5)) + 
    scale_color_identity()

# hex
ggplot(df_t, aes(x, y)) + 
    geom_hex(bins = 75) + 
    scale_fill_gradientn(colors = rev(rainbow(10, end = 4/6)))

# scatter + contour
ggplot(df_t, aes(x, y)) + 
    geom_point(shape = 16, alpha = I(0.2)) + 
    stat_density_2d(bins = 15, color = "red", size = 0.5) 

# bin2d
ggplot(df_t, aes(x, y)) + geom_bin2d(bins = 200)

# heatmap
# 最简单的pheatmap
# excel heatmap
# scatter plot的密度：从alpha开始
# geom_hex, geom_bin2d, densCols, stat_density_2d
