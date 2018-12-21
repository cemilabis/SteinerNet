# Sub-graph of merged steiner trees (SPM or STM)
steinertree8 <- function (optimize, terminals, glist, color) {
        g <- glist[[1]]
        
        queue         <- c()
        results_queue <- c()
        edgeslist     <- c()
        
        prob <- sample(1:length(terminals), 1)
        
        subtree    <- terminals[[prob]]
        nsubtree   <- setdiff(terminals, subtree)
        startpoint <- subtree
        
        paths <- get.all.shortest.paths(g, subtree, nsubtree)
        paths <- paths$res
        
        t  <- sapply(paths, length)
        t2 <- which(t == min(t))
        
        # Put in queue paths with minimal lengths
        for (i in 1:length(t2))
                #queue[length(queue) + 1] <- paths[t2[i]]
                queue[[length(queue) + 1]] <- names(unlist(paths[t2[i]]))
        
        index <- length(t2)
        while (index > 0) {
                edgeslist <- queue[1]
                queue[1]  <- NULL
                index     <- index - 1
                
                if (length(intersect(unlist(terminals), unlist(edgeslist))) == length(terminals)) {
                        #if (length(intersect(unlist(terminals), names(unlist(edgeslist)))) == length(terminals)) {
                        graph_is_new <- TRUE
                        
                        if (length(results_queue) == 0)
                                results_queue[length(results_queue) + 1] <- edgeslist
                        
                        for (count_path in 1:length(results_queue)) {
                                t1 <- unlist(edgeslist[[1]])
                                t2 <- unlist(results_queue[[count_path]])
                                
                                if (length(union(t1, t2)) == length(t1))
                                        if (all(union(t1, t2) %in% t2))
                                                graph_is_new <- FALSE
                        }
                        
                        if (graph_is_new == TRUE)
                                results_queue[length(results_queue) + 1] <- edgeslist
                } else {
                        subtree  <- intersect(unlist(terminals), unlist(edgeslist))
                        #subtree  <- intersect(unlist(terminals), names(unlist(edgeslist)))
                        nsubtree <- setdiff(terminals, subtree)
                        
                        paths    <- get.all.shortest.paths(g, subtree[length(subtree)], nsubtree)
                        paths    <- paths$res
                        
                        t  <- sapply(paths, length)
                        t2 <- which(t == min(t))
                        
                        for (i in 1:length(t2))
                                #queue[[index + i]] <- union(unlist(edgeslist), unlist(paths[t2[i]]))
                                queue[[index + i]] <- union(unlist(edgeslist), names(unlist(paths[t2[i]])))
                        
                        index <- index + length(t2)
                }
        }
        
        paths <- results_queue
        t     <- sapply(paths, length)
        t2    <- which(t == min(t))
        queue <- paths[t2]
        
        steinert_list <- c()
        glst <- c()
        
        for (i in 1:length(t2)) {
                steinert = minimum.spanning.tree(induced_subgraph(g, queue[[i]]))
                
                if (optimize) {
                        a   <- V(steinert)$color
                        b   <- degree(steinert, v = V(steinert), mode = c("all"))
                        a1  <- match(a, "yellow")
                        b1  <- match(b, "1")
                        opt <- sapply(1:length(a1), function (r) a1[r] * b1[r])
                        new_g <- delete.vertices(steinert, grep(1, opt))
                        steinert <- new_g
                }
                
                if (color)
                        V(g)[queue[[i]]]$color <- "green"
                
                steinert_list[[length(steinert_list) + 1]] <- steinert
        }
        
        if (color) {
                #V(g)[as.numeric(terminals)]$color <- "red"
                V(g)[terminals]$color <- "red"
                
                glst[[length(glst) + 1]] <- g
                glst[[length(glst) + 1]] <- steinert_list
        } else
                glst <- steinert_list
        
        return (glst)
}
