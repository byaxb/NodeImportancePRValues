library(expm)
library(linkcomm)
library(centiserve)
library(tidyverse)
library(igraph)
library(byaxb)
library(sna)
library(intergraph)

library(extrafont)
loadfonts(device = "win")
windowsFonts(Times=windowsFont("TT Times New Roman"))

load('accidents v3.2.190406.rda')
ig <- ssi_ig

pb <- partial_betweenness_matrix %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    gather(key = injury, value = value, -rowname) %>%
    set_names(c("source", "injury", "value")) %>%
    mutate(type = "pb")
rd <- radial_degree_matrix %>%
    as.data.frame() %>%
    rownames_to_column() %>%
    gather(key = injury, value = value, -rowname) %>%
    set_names(c("source", "injury", "value")) %>%
    mutate(type = "rd")

alpha <- 0.25 #0.75
pr_value <- rbind(pb, rd) %>%
    spread(type, value) %>%
    group_by(source) %>%
    summarise(pb = sum(pb),
              rd = sum(rd)) %>%
    as.data.frame() %>%
    mutate(pb = mm_scale(pb),
       rd = mm_scale(rd)) %>%
    as.data.frame() %>%
    mutate(pr_value = (alpha*rd + (1-alpha)*pb))



v_names <- V(ig)$name
source_idx <- which(V(ig)$type == 'source')
source_pr_values <- pr_value$pr_value
names(source_pr_values) <- pr_value$source
scores <- source_pr_values[v_names[source_idx]]

cal_efficiency <- function(ig, scores, k=1, source_nodes = NULL, sink_nodes = NULL) {
    if(k == 0) {
        ig_remained <- ig
    } else if(k >= 1) {
        vid_to_be_removed <- order(scores, decreasing = TRUE)[1:k]
        ig_remained <- igraph::delete_vertices(ig, igraph::V(ig)[vid_to_be_removed])
    } else {
        return(NULL)
    }
    if(is.null(source_nodes)) {
        source_nodes <- igraph::V(ig_remained)[which(igraph::V(ig_remained)$type == 'source')]
    }
    if(is.null(sink_nodes)) {
        sink_nodes <- igraph::V(ig_remained)[which(igraph::V(ig_remained)$type == 'injury')]
    }
    dists <- igraph::distances(ig_remained,
                       v = which(igraph::V(ig_remained)$name %in% source_nodes$name),
                       to = sink_nodes)
    global_efficiency <- sum(1 / dists) / (length(source_nodes) * length(sink_nodes))
    return(global_efficiency)
}


methods_list <- read.csv(
text = "
pkg,function,name
igraph,degree,Degree
igraph,betweenness,Betweenness
igraph,closeness,Clossness
centiserve,katzcent,Katz
centiserve,leaderrank,LeaderRank
centiserve,clusterrank,ClusterRank
centiserve,diffusion.degree,Diffusion Degree
centiserve,salsa,SALSA
byaxb,node_imp_ExFs,Expected Force
",
sep = ',',
comment.char = '#',
stringsAsFactors = FALSE)

set.seed(2012)
score_list <- list()
for(i in 1:nrow(methods_list)) {
    cat('\nprocessing ', methods_list$name[i])
    myfun <- get(methods_list$function.[i], asNamespace(methods_list$pkg[i]))
    score_list[[methods_list$name[i]]] <- do.call(myfun, list(ig))
}

score_list[['PR values']] <- source_pr_values[v_names[source_idx]]

sort(score_list[['PR values']])
sort(score_list[['Expected Force']])
ExF_value <- score_list[['Expected Force']]
names(ExF_value) <- V(ig)$name
sort(ExF_value)


score_names <- names(score_list)

results <- NULL
selected_metrics <- names(score_list)
#selected_metrics <- c('bottleneck', 'pr_values')

max_k <- 6
for(cur_k in 1:max_k) {
    for(cur_name in selected_metrics) {
        cur_eff <- cal_efficiency(ig,
                                  scores = score_list[[cur_name]],
                                  k = cur_k)
        results <- rbind(results,
                         c(k = cur_k, name = cur_name, efficiency = cur_eff))
    }
}
results %>%
    as.data.frame() %>%
    mutate(efficiency = as.numeric(as.character(efficiency))) %>%
    mutate(k = as.integer(k)) %>%
    #mutate(efficiency = 1 - efficiency / baseline)
    mutate(efficiency = efficiency) -> results


library(ggplot2)
results_emphasis <- results %>%
    filter(name %in% c("PR values"))
legend_y <- results %>%
    filter(k == 6) %>%
    select(name, efficiency)
legend_y$efficiency[which(legend_y$name == 'LeaderRank')] <- legend_y$efficiency[which(legend_y$name == 'LeaderRank')] * 1.01
ggplot(results, aes(x = k, y = efficiency)) +
    geom_point(aes(colour = forcats::fct_inorder(name)), alpha = 0.5, fill = 'white', size = 2) +
    geom_line(aes(colour = forcats::fct_inorder(name)), alpha = 0.5) +
    geom_line(data = results_emphasis,
              mapping = aes(x = k, y = efficiency),
              colour = 'red',
              size = 4, alpha = 0.15) +
    geom_text(data = legend_y,
              aes(x = rep(6.1, nrow(legend_y)), y = legend_y$efficiency, label = legend_y$name),
              #size = 10,
              family="serif",
              hjust = 0) +
    theme(legend.position = c(0.17, 0.25),
          text = element_text(family="serif")) +
    labs(x = 'k',
         y = 'efficiency',
         colour = "Metrics") +
    xlim(1, 7)

results %>%
    mutate(type = ifelse(name == 'PR values', 'Proposed method', 'Existing methods(mean)')) %>%
    group_by(k, type) %>%
    summarise(mean = mean(efficiency)) %>%
    ggplot(aes(x = k, y = mean, fill = type)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    geom_text(aes(y = mean + 0.0035, label = round(mean, 3)),
              family="serif", position = position_dodge(.9)) +
    coord_cartesian(ylim = c(0.35, 0.56)) +
    theme(legend.position = c(0.85, 0.9),
          text = element_text(family="serif")) +
    labs(x = 'k',
         y = 'efficiency',
         fill = "Metrics")
