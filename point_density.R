#!/usr/bin/env R
# from http://r4ds.had.co.nz/

library(tidyverse)
data(mpg)
ggplot(mpg, aes(class, drv)) +  # 离散型x轴和离散型y轴
    geom_count(aes(color = ..prop.., group = 1)) +  # mdzz
    scale_size_area(max_size = 20)

