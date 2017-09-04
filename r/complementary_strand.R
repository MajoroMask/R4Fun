gen_complementary_strand <- function(input, type = "DNA") {
    # 生成输入DNA序列的互补链
    # input为长度为1的字符串，最好别有除了ATGC之外的字母
    # 
    do_the_work <- function(base, type) {
        if (type == "DNA") {
            switch(
                base, 
                "A" = "T", 
                "T" = "A", 
                "G" = "C", 
                "C" = "G", 
                "N"
            )
        } else if (type == "RNA") {
            switch(
                base, 
                "A" = "U", 
                "U" = "A", 
                "G" = "C", 
                "C" = "G", 
                "N"
            )
        }
    }
    if (grepl("[^A^T^C^G^U]", x = input)) {
        message("Unusual base detected\n")
    }
    if (length(input) != 1) {
        stop("input must be at length of 1 @ gen_complementary_strand\n")
    }
    input <- unlist(strsplit(input, split = ""))
    paste(rev(sapply(input, do_the_work, type)), collapse = "")
}