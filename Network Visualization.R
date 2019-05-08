library(tidyverse)
library(igraph)

ig <- ssi_ig

extrafont::loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

#sum up
radial_degree_sum <- apply(radial_degree_matrix, 1, sum)
partial_betweenness_sum <- apply(partial_betweenness_matrix, 1, sum)
#scale
radial_degree_sum <- (radial_degree_sum - min(radial_degree_sum)) / (max(radial_degree_sum) - min(radial_degree_sum))
partial_betweenness_sum <- (partial_betweenness_sum - min(partial_betweenness_sum)) / (max(partial_betweenness_sum) - min(partial_betweenness_sum))

rownames(radial_degree_matrix)
V(ig)$name

alpha <- 0.25
size_sum <- alpha*radial_degree_sum + (1-alpha)*partial_betweenness_sum
V(ig)$size <- 0.25
V(ig)$size[which(V(ig)$type == 'source')] <-  byaxb::lin_scale( size_sum, c(0.15, max(size_sum)))



file_name <- "vh_just.v3.0.190322.xlsx"

vjust <- NULL
hjust <- NULL
fresh_coor <- function() {
    vh_just <- as.data.frame(readxl::read_excel(file_name))

    vjust <- vh_just$vjust
    names(vjust) <- vh_just$nodes
    vjust <- vjust[ig_node_names_old]
    vjust <<- vjust

    hjust <- vh_just$hjust
    names(hjust) <- vh_just$nodes
    hjust <- hjust[ig_node_names_old]
    hjust <<- hjust
}


node_en <- V(ig)$name

node_en_new <- read.csv("node_en.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE)

V(ig)$name <- node_en_new[, 2]
library(GGally)

set.seed(2012)
fresh_coor()
load("coords_manual.rda")

coords_manual[, 1] <- coords_manual[, 1] * 0.5
ggnet2(ig,
       #color = "type",
       color = c("tomato", "steelblue")[ifelse(V(ig)$type != 'source', 1, 2)],
       label = TRUE,
       vjust = vjust,
       hjust = hjust,
       label.size = 4,
       size = V(ig)$size,
       #palette = "Set1",
       arrow.gap = 0.025,
       layout.exp = 0.4,
       arrow.size = 6,
       arrow.type = 'open',
       edge.alpha = 0.25,
       edge.size = 1,
       mode = coords_manual,
       family = "Times New Roman",
       edge.color = c("color", "grey30"),
       color.legend = "Node Type") +
   # geom_point(aes(color = color), size = V(ig)$size * 25, color = "white") +
   # geom_point(aes(color = color), size =  V(ig)$size * 25, alpha = 0.5) +
   # geom_point(aes(color = color), size = V(ig)$size * 20) +
    theme(legend.position = "none")
