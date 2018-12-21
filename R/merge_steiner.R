merge_steiner <- function (treelist) {
        
        merged <- treelist[[1]]
        
        if (length(treelist) > 1) {
                for (i in 2:length(treelist))
                        merged <- union(merged, treelist[[i]])
        } else
                print("Nothing to merge. Only one solution was found")
        
        glist      <- c()
        glist[[1]] <- merged
        
        return(glist)
}
