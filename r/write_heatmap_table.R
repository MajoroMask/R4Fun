library(openxlsx)
# functions ----
write_heatmap_table <- function(
    wb, sheet = 1, x, color = "default", color_ladder = 10, 
    order_row = TRUE, order_col = TRUE, 
    row_names = TRUE, col_names = TRUE
) {
    # x: input matrix
    # 
    # deal with args ----
    require(openxlsx)
    if (isTRUE(color_ladder)) {
        color_ladder <- 10
    }
    if (!is.numeric(color_ladder)) {
        stop("Wrong color_ladder @ write_heatmap_table")
    }
    if (color == "default") {
        color <- colorRampPalette(
            rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu"))
        )(100)
    }
    
    # deal with data ----
    m_in <- as.matrix(x)
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
    writeData(
        wb, as.data.frame(m_out), sheet = sheet, 
        rowNames = row_names, colNames = col_names
    )
    m_color <- round(scales::rescale(m_out, to = c(1, length(color))))
    m_color <- matrix(
        color[m_color], nrow = nrow(m_color), ncol = ncol(m_color)
    )
    for (i in 1:length(m_color)) {
        addStyle(
            wb, sheet = sheet, stack = TRUE,
            rows = matrix_position(i, nrow(m_color))[1] + 1,
            cols = matrix_position(i, nrow(m_color))[2] + 1,
            style = createStyle(fgFill = m_color[i])
        )
    }
    if (color_ladder) {
        index <- round(
            quantile(
                1:length(color), probs = seq(1, 0, length.out = color_ladder)
            )
        )
        df_dummy <- data.frame(
            "legend" = quantile(
                min(m_out):max(m_out), 
                probs = seq(1, 0, length.out = color_ladder)
            )
        )
        writeData(
            wb, sheet, df_dummy, 
            startRow = 1, 
            startCol = ncol(m_color) + ifelse(row_names, 3, 2)
        )
        for (i in 1:length(index)) {
            addStyle(
                wb, sheet = sheet, stack = TRUE, 
                rows = i + 1, 
                cols = ncol(m_color) + ifelse(row_names, 3, 2), 
                style = createStyle(fgFill = color[index[i]])
            )
        }
    }
}
matrix_position <- function (index, nrow) {
    row <- ifelse(index %% nrow == 0, nrow, index %% nrow)
    col <- ifelse(index %% nrow == 0, index %/% nrow, (index %/% nrow) + 1)
    c(row, col)
}

# example ----
# generate test data
set.seed(123)
test <- matrix(rnorm(200), 20, 10)
test[1:10, seq(1, 10, 2)] <- test[1:10, seq(1, 10, 2)] + 3
test[11:20, seq(2, 10, 2)] <- test[11:20, seq(2, 10, 2)] + 2
test[15:20, seq(2, 10, 2)] <- test[15:20, seq(2, 10, 2)] + 4
colnames(test) <- paste("Test", 1:10, sep = "")
rownames(test) <- paste("Gene", 1:20, sep = "")
# pheatmap::pheatmap(test)  # from pheatmap example

wb <- createWorkbook(creator = 'Su Na')
addWorksheet(wb, sheetName = "excel heatmap", gridLines = T)
# ↓ This function is self-added.
write_heatmap_table(wb, sheet = 1, x = test, row_names = T, color_ladder = 20)
# ↑ This function is self-added.
saveWorkbook(wb, overwrite = T, "test.xlsx")