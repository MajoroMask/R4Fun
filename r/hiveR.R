library(data.table)
library(plyr)
library(igraph)

lemis <- fread("data/LesMiserables.txt")
# 数据集：网络图必用的《悲惨世界》人物关系
df.in <- lemis

gdf <- graph.data.frame(df.in, directed = F)  # make good use of igraph
if (!is_simple(gdf)) gdf <- simplify(gdf)  # avoid looped or mutiple edges
# vcount(gdf)  # data check
# ecount(gdf)

# check for node properties

dgr <- degree(gdf, v = V(gdf), mode = "all")
# degree节点连接度：连接到一个节点的edge数量
btw <- betweenness(gdf, v = V(gdf), directed = FALSE) / 
    (((vcount(gdf) - 1) * (vcount(gdf)-2)) / 2)
# betweenness：对某一节点而言，任意两个其他节点之间的最短连线通过该节点的次数
btw_norm <- (btw - min(btw)) / (max(btw) - min(btw))
# normalize to [0, 1]
# 问题：如果normalize的话，btw没必要除以最短路径总数，为何要除以最短路径总数？
rm(btw)
ds <- similarity(gdf, mode = "all", method = "dice")
# TODO
# http://www.vesnam.com/Rblog/viznets1/
# http://www.vesnam.com/Rblog/viznets2/

