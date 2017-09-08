# wind rose plot with weighted width and catlog group
# 猫风玫瑰图(*･ω･q*)

library(dplyr)  # dplyr()
library(scales)  # rescale()
library(ggplot2)
library(viridis)

my_cat_wind_rose <- function (
    data, x, y, level_x = NULL, level_y = NULL, 
    width_type = "count", catlog = NULL
) {
    # TODO instruction
    
    require(dplyr)
    require(ggplot2)
    require(viridis)
    
    # args parsing ----
    colnames(data)[colnames(data) == x] <- "x"
    colnames(data)[colnames(data) == y] <- "y"
    if (is.null(level_x)) {
        if (!is.factor(data$x)) {
            data$x <- factor(data$x)
        }
    } else {
        data$x <- factor(data$x, levels = level_x)
    }
    if (is.null(level_y)) {
        if (!is.factor(data$y)) {
            data$y <- factor(data$y)
        }
    } else {
        data$y <- factor(data$y, levels = level_y)
    }
    if (!is.null(catlog)) {
        warning(
            "Just make sure level_x in same group is adjacent!", 
            immediate. = TRUE, call. = FALSE
        )
        # TODO sorting level_x to make sure group will not overlap
    }
    
    # data formatting ----
    df_count1 <- as.data.frame(
        table(data[, c("x", "y")]), responseName = "freq"
    )
    df_count2 <- as.data.frame(
        table(data[, c("x")], dnn = "x"), responseName = "count"
    )
    if (width_type == "prop") {
        df_count2$count <- df_count2$count / sum(df_count2$count)
    }
    range_bin <- c(min(df_count2$count), max(df_count2$count))
    df_count2$count <- scales::rescale(df_count2$count, c(0.4, 1))
    df_count2$x_pos <- cumsum(df_count2$count) -  # full width cum
        df_count2$count * 0.5 +  # half width each
        c(0, cumsum(rep(0.1, length(df_count2$count) - 1)))  # 0.1 as bin
    df_p <- dplyr::left_join(df_count1, df_count2, by = "x")
    
    # ggplot ----
    p <- ggplot(df_p) +
        geom_bar(
            aes(x = x_pos, y = freq, fill = y, width = count), 
            stat = "identity", position = "stack"
        ) + 
        scale_x_continuous(
            limits = c(
                0, 
                max(df_count2$x_pos) + 
                    0.5 * df_count2$count[length(df_count2$count)] + 
                    0.1
            ), 
            breaks = df_count2$x_pos, 
            labels = df_count2$x
        ) + 
        scale_fill_viridis(discrete = TRUE, option = "C") + 
        guides(fill = guide_legend(title = y)) + 
        theme_light() + 
        theme(
            # axis.ticks.length = unit(0.5, "lines"), 
            panel.grid.minor = element_blank(), 
            # panel.grid.major.x = element_line(color = "black"), 
            panel.grid.major.x = element_blank(), 
            panel.grid.major.y = element_blank()
        ) + 
        coord_polar(direction = -1) +
        labs(x = NULL, y = paste("#", y))
    if (!is.null(catlog)) {  # add catlog
        l.x <- lapply(
            names(catlog), function(cat) {
                df_sub <- df_count2[df_count2$x %in% catlog[[cat]], ]
                c(
                    df_sub$x_pos[1] - df_sub$count[1] / 2, 
                    df_sub$x_pos[nrow(df_sub)] + df_sub$count[nrow(df_sub)] / 2
                )
            }
        )
        lim_up <- ggplot_build(p)$layout$panel_ranges[[1]]$r.range[2] * 1.05
        df_seg <- data.frame(
            "group" = factor(names(catlog), levels = names(catlog)), 
            "x" = sapply(l.x, `[`, 1), 
            "xend" = sapply(l.x, `[`, 2), 
            "y" = lim_up, 
            "yend" = lim_up
        )
        p <- p + geom_segment(
            data = df_seg, mapping = aes(
                x = x, xend = xend, y = y, yend = yend, color = group
            ), size = 2, lineend = "round"
        )
    }
    p
}

# examples----
data(diamonds)
catlog <- list(
    "Beautiful" = c("D", "E", "F"), 
    "Just so-so" = c("G", "H"), 
    "Unacceptable" = c("I", "J")
)
p <- my_cat_wind_rose(
    diamonds, "color", "cut", # level_x = rev(levels(diamonds$color)), 
    catlog = catlog
)
ggsave("temp.pdf", plot = p)

