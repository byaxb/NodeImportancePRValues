cal_pb <- function(source, injury) {
    partial_betweenness <- 0
    for(frm_source in source_names) {
        hi_paths <- lapply(all_shortest_paths(ig,
                                              from = frm_source,
                                              to = injury)$res, names)
        if(length(hi_paths) == 0){
            next
        }
        partial_betweenness <- partial_betweenness + sum(sapply(hi_paths,
                                                                function(x) {
                                                                    idx <- which(x == source)
                                                                    if(length(idx) == 1 && idx != 1) {
                                                                        return(TRUE)
                                                                    }
                                                                    return(FALSE)
                                                                })) / length(hi_paths)
    }
    return(partial_betweenness)
}
partial_betweenness_matrix <- NULL
for(tmp_source in source_names) {
    pb_row <- NULL
    for(tmp_injury in injury_names) {
        pb_row <- c(pb_row, cal_pb(tmp_source, tmp_injury))
    }
    partial_betweenness_matrix <- rbind(partial_betweenness_matrix, pb_row)
}
row.names(partial_betweenness_matrix) <- source_names
colnames(partial_betweenness_matrix) <- injury_names
sort(apply(partial_betweenness_matrix, 1, sum))


cal_rd <- function(source, injury) {
    radial_degree <- 0
    hi_paths <- all_shortest_paths(ig, from = source, to = injury)$res
    for(cur_path in hi_paths) {
        #radial_degree <- radial_degree + exp(-length(cur_path))
        radial_degree <- radial_degree + exp(2-length(cur_path))
        #radial_degree <- radial_degree + 2^(2-length(cur_path))

    }
    return(radial_degree)
}
radial_degree_matrix <- NULL
for(tmp_source in source_names) {
    rd_row <- NULL
    for(tmp_injury in injury_names) {
        rd_row <- c(rd_row, cal_rd(tmp_source, tmp_injury))
    }
    radial_degree_matrix <- rbind(radial_degree_matrix, rd_row)
}
row.names(radial_degree_matrix) <- source_names
colnames(radial_degree_matrix) <- injury_names
