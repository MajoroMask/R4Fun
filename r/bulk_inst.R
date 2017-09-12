bulk_install <- function(dir = "./pkgs/") {
    # 批量安装本地包
    # 
    if (!dir.exists(dir)) {
        stop("Check dir contains the packages @ bulk_install")
    }
    files <- list.files(dir)  # 全部非隐藏文件
    files <- files[grepl("\\.zip$", files)]  # 筛选.zip结尾的文件
    message(length(files), " packages detected, start installing...\n")
    install.packages(  # 本地安装
        file.path(dir, files), repos = NULL, type = "source"
    )
}