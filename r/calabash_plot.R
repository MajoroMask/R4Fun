# bubble plot with group
# 糖福禄图
# TODO ain't no function made yet, just a example

library(tidyverse)
library(magrittr)
library(viridis)
library(ggrepel)

df_ori <- read.table(
    "./data/go.tab", sep = "\t", header = T, 
    check.names = F, stringsAsFactors = F
)
df_ori$cat <- sub("GOTERM_", "", df_ori$cat)
list_label <- lapply(
    unique(df_ori[, "cat"]), function(cat) {
        df_sub <- df_ori[df_ori[, "cat"] == cat, ]
        selected <- df_sub$desc[
            df_sub$bonferroni <= sort(df_sub$bonferroni)[3] |
                df_sub$count >= sort(df_sub$count, decreasing = T)[3]
        ]
        df_sub[df_sub$desc %in% selected, ]
    }
)
df_label <- do.call(rbind, list_label)

p1 <- ggplot(df_ori) + 
    geom_vline(xintercept = -log2(0.05), linetype = "dashed", color = "blue") + 
    geom_point(
        aes(
            x = -log2(bonferroni), y = group, size = count, color = log2(count), 
            alpha = count
        )
    ) + 
    geom_label_repel(
        data = df_label, 
        mapping = aes(x = -log2(bonferroni), y = group, label = go, fill = cat), 
        size = 3, color = "white", 
        label.padding = unit(0.1, "lines"), 
        segment.color = "black", 
        min.segment.length = unit(0, "lines"), 
        force = 2, 
        nudge_x = 0.25, nudge_y = 0.25
    ) +
    scale_color_viridis(
        name = "# Genes", option = "C", direction = -1, 
        guide = guide_colorbar(barwidth = 10, barheight = 2)
    ) + 
    scale_fill_discrete(guide = "none") + 
    scale_size_continuous(
        name = "# Genes", 
        range = c(0.1, 20), 
        breaks = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        labels = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        guide = guide_legend()
    ) + 
    scale_alpha_continuous(
        name = "# Genes", 
        range = c(0.4, 0.6), 
        breaks = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        labels = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        guide = guide_legend()
    ) + 
    facet_grid(cat~.) + 
    theme_bw() + 
    theme(
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        strip.background = element_rect(fill = "white"), 
        legend.position = "bottom"
    ) + 
    # annotation_logticks(side = "b", short = unit(0, "cm")) + 
    labs(x = "Significant enrichment level", y = "Experiment group")
ggsave("test.pdf", p, width = 12, height = 8)

p2 <- ggplot(df_ori) + 
    geom_vline(xintercept = -log2(0.05), linetype = "dashed", color = "blue") + 
    geom_point(
        aes(
            x = -log2(bonferroni), y = group, 
            size = count, color = cat, fill = cat
        ), shape = 21, alpha = I(0.4)
    ) + 
    geom_label_repel(
        data = df_label, 
        mapping = aes(x = -log2(bonferroni), y = group, label = go, color = cat), 
        size = 3, 
        label.padding = unit(0.1, "lines"), 
        segment.color = "black", 
        min.segment.length = unit(0, "lines"), 
        force = 2, 
        nudge_x = 0.25, nudge_y = 0.25
    ) +
    # scale_color_viridis(
    #     name = "# Genes", option = "C", direction = -1, 
    #     guide = guide_colorbar(barwidth = 10, barheight = 2)
    # ) + 
    # scale_fill_discrete(guide = "none") + 
    scale_color_discrete(guide = "none") + 
    scale_fill_discrete(guide = "none") + 
    scale_size_continuous(
        name = "# Genes", 
        range = c(0.1, 20), 
        breaks = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        labels = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        guide = guide_legend()
    ) + 
    scale_alpha_continuous(
        name = "# Genes", 
        range = c(0.1, 0.6), 
        breaks = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        labels = c(max(df_ori$count) %/% 5, max(df_ori$count) %/% 5 * 3.5), 
        guide = guide_legend()
    ) + 
    facet_grid(cat~.) + 
    theme_bw() + 
    theme(
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        strip.background = element_rect(fill = "white"), 
        legend.position = "bottom"
    ) + 
    labs(x = "Significant enrichment level", y = "Experiment group")
# p3 <- p2 + scale_color_manual(values = unique(ggplot_build(p2)$data[[2]]$colour))
ggsave("test2.pdf", p2, width = 12, height = 8)
