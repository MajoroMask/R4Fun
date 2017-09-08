my_install_packages <- function(pkgs, repos = "CRAN") {
    # install packages using domastic CRAN / BioC repos
    # 
    # override CRAN and BioC repos ----
    old_opt <- options()  # backup is always needed
    x <- c(
        "https://mirrors.tuna.tsinghua.edu.cn/CRAN", 
        "http://www.stats.ox.ac.uk/pub/RWin"
    )
    names(x) <- c("CRAN", "CRANextra")
    attr(x, "RStudio") <- T
    options("repos" = x)
    x <- "https://mirrors.tuna.tsinghua.edu.cn/bioconductor"
    names(x) <- "China (Anhui) [https]"
    options("BioC_mirror" = x)
    rm(x)
    
    installed_pkgs <- installed.packages()
    mapply(
        function(pkg, rep) {
            if (!(pkg %in% rownames(installed_pkgs))) {
                if (rep == "CRAN") {
                    install.packages(pkg)
                } else if (rep == "BioC") {
                    if (!("BiocInstaller" %in% installed_pkgs)) {
                        source("https://bioconductor.org/biocLite.R")
                    }
                    BiocInstaller::biocLite(pkg)
                }
            }
        }, 
        pkg = pkgs, rep = repos
    )
    # options(old_opt)
    invisible(NULL)
}

