library(magrittr)
library(openxlsx)

xlsx_heatmap <- function(
    m, file, cp = colorRampPalette(c("#00aedb", "white", "#f37735"))(100), 
    order_row = TRUE, order_col = TRUE, 
    row_names = TRUE, col_names = TRUE, 
    add_legend = TRUE
) {
    require(openxlsx)
    require(scales)
    
    # data
    m_in <- as.matrix(m)
    if (order_row) {
        order_row <- hclust(dist(m_in))$order
    } else {
        order_row <- 1:nrow(m_in)
    }
    if (order_col) {
        order_col <- hclust(dist(t(m_in)))$order
    } else {
        order_col <- 1:ncol(m_in)
    }
    m_out <- m_in[order_row, order_col]
    
    wb <- 
        createWorkbook(creator = "MajorosMask") %T>% 
        addWorksheet(sheetName = "xlsx heatmap", gridLines = TRUE) %T>% 
        writeData(
            sheet = 1, as.data.frame(m_out), 
            rowNames = row_names, colNames = col_names
        )
    # fill
    color_styles <- 
        scales::rescale(m_out, to = c(1, length(cp))) %>% 
        round() %>% 
        extract(cp, .) %>% 
        mapply(FUN = createStyle, fgFill = .)
    mapply(
        addStyle,
        style = color_styles,
        rows = rep(seq(nrow(m_out)) + 1, ncol(m_out)),
        cols = rep(seq(ncol(m_out)) + 1, each = nrow(m_out)),
        MoreArgs = list(
            wb = wb, sheet = 1, gridExpand = FALSE, stack = TRUE
        )
    ) %>% invisible()
    
    if (add_legend) {  # add legend
        index <- 
            cp %>% length %>% seq %>% 
            quantile(probs = seq(1, 0, length.out = 10)) %>% 
            round
        df_dummy <- data.frame(
            "legend" = quantile(
                min(m_out):max(m_out), 
                probs = seq(1, 0, length.out = 10)
            )
        )
        writeData(
            wb, sheet = 1, df_dummy, 
            startRow = 1, 
            startCol = ncol(m_out) + ifelse(row_names, 3, 2)
        )
        legend_style <- mapply(FUN = createStyle, fgFill = cp[index])
        mapply(
            addStyle, 
            style = legend_style, 
            rows = seq(ifelse(row_names, 2, 1), length.out = length(index)), 
            cols = ncol(m_out) + ifelse(row_names, 3, 2), 
            MoreArgs = list(
                wb = wb, sheet = 1, gridExpand = FALSE, stack = TRUE
            )
        ) %>% invisible()
    }
    wb
}

# main ----
set.seed(123)
m <- matrix(rnorm(2000), 50, 40)
m[c(TRUE, FALSE), c(TRUE, FALSE)] %<>% add(5)
m[c(TRUE, FALSE, FALSE, FALSE), ] %<>% subtract(10)
colnames(m) <- paste("sample", 1:40, sep = "")
rownames(m) <- paste("Gene", 1:50, sep = "")
wb <- 
    m %>% 
    xlsx_heatmap %T>% 
    saveWorkbook(file = "@/test.xlsx", overwrite = TRUE)
# TODO parallel to speed up
