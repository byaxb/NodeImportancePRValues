library(tidyverse)
library(igraph)

ig <- ssi_ig

ig_node_names_old <- V(ig)$name

library(extrafont)
loadfonts(device = "win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

#sum up
radial_degree_sum <- apply(radial_degree_matrix, 1, sum)
partial_betweenness_sum <- apply(partial_betweenness_matrix, 1, sum)

node_en_new <- read.csv("node_en.csv",
                        header = TRUE,
                        stringsAsFactors = FALSE)

V(ig)$name <- node_en_new[, 2]

source_node_scores <- data.frame(source = V(ig)$name[1:24],
                                 rd = radial_degree_sum,
                                 pb = partial_betweenness_sum,
                                 stringsAsFactors = FALSE)

#scale
radial_degree_sum_mmscaled <- (radial_degree_sum - min(radial_degree_sum)) / (max(radial_degree_sum) - min(radial_degree_sum))
partial_betweenness_sum_mmscaled <- (partial_betweenness_sum - min(partial_betweenness_sum)) / (max(partial_betweenness_sum) - min(partial_betweenness_sum))


alpha <- 0.25
size_sum <- alpha*radial_degree_sum + (1-alpha)*partial_betweenness_sum

source_node_scores %>%
    mutate(rd_mm = radial_degree_sum_mmscaled,
           pb_mm = partial_betweenness_sum_mmscaled) %>%
    mutate(rd_mm = alpha*rd_mm,
           pb_mm = (1-alpha)*pb_mm) %>%
    mutate(score = rd_mm + pb_mm) -> source_node_scores



library(ggplot2)

source_node_scores %>%
    gather(key = 'metric', value = 'value', rd_mm, pb_mm) %>%
    mutate(metric = ifelse(metric == 'rd_mm', 'Radial Degree', 'Partial Betweenness')) %>%
    arrange(desc(score)) %>%
    ggplot(aes(x = forcats::fct_inorder(source), y = value, fill = metric)) +
    geom_bar(stat='identity', position = "stack") +
    geom_text(aes(y = score + 0.015, label = round(score, 2)), size = 2.75) +
    theme(
        legend.position = c(0.85, 0.85),
        axis.text.x = element_text(
        family = "serif",
        angle = 45,
        hjust = 1,
        vjust = 1),
        text = element_text(family="serif")) +
    labs(x = 'Source of Injury',
         y = 'Importance',
         fill = 'Metrics')
