# 扰动点+箱线图+提琴图（jitter-box-violin plot, jbvplot），用于展示数据分布
# 
library(ggplot2)
jbvplot <- function(df.in) {
    p <- ggplot(
        df.in, aes(variable, value, color = variable)
    ) + 
        geom_violin(aes(fill = variable), alpha = I(0.1)) +  
        # 提琴图图层，透明度0.1
        geom_boxplot(alpha = I(0.8), outlier.size = NA) +
        # 箱线图图层，透明度0.8，同时去除离群值
        geom_jitter(width = 0.25, size = 1.5) + 
        # 扰动点图图层
        theme_bw() + 
        theme(
            axis.title.x = element_blank(), 
            panel.grid.minor = element_blank()
        )
    return(p)
}
# 以下是使用示例
# data(mpg)
# df.mpg <- mpg[, c("class", "hwy")]
# colnames(df.mpg) <- c("variable", "value")
# p <- jbvplot(df.in = df.mpg)
