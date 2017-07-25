# 雷达图，用于对二维数据分布进行可视化

radar_plot <- function(df.in, x.cut = 50, y.cut = 50) {
    # 雷达图
    
    # 参数
    #     x.cut：轴向坐标轴的分隔数，默认50
    #     y.cut：径向坐标轴的分隔数，默认50
    # 
    require(ggplot2)
    df.in$x <- cut(df.in$x, breaks = x.cut)
    df.in$y <- cut(df.in$y, breaks = y.cut)
    df.in <- as.data.frame(table(df.in))
    df.in[df.in == 0] <- NA
    arrow <- arrow(angle = 15, length = unit(0.15, "inches"), type = "closed")
    p <- ggplot(df.in, aes(x, y, fill = Freq)) +
        geom_tile() + 
        geom_segment(
            aes(x = 0, xend = 0, y = 0, yend = y.cut + 1), arrow = arrow
        ) +
        geom_segment(
            aes(x = 0.5, xend = 7, y = y.cut + 1, yend = y.cut + 1), 
            arrow = arrow
        ) +
        scale_fill_continuous(
            na.value = 'white', 
            high = "#132B43",  # 黑蓝色
            low = "#56B1F7",  # 淡蓝色
            trans = 'log10'
        ) +
        theme_bw() + 
        theme(
            axis.text = element_blank(), 
            panel.grid.major.x = element_blank(),
            # panel.grid.major = element_blank(), 
            panel.border = element_rect()
        ) + 
        coord_polar()  # 极坐标慢好多，好烦啊
    return(p)
}

data("diamonds")
df.d <- diamonds[, c("carat", "price")]
colnames(df.d) <- c("x", "y")
p <- radar_plot(df.d)