restore_name_attribute <- function (attr_flag, type, result, color) {
        
        if (color) {
                if (attr_flag) {
                        V(result[[1]])$name <- V(result[[1]])$realname
                        result[[1]] <- delete_vertex_attr(result[[1]], 'realname')
                }
        }
        
        if (type == "EXA" | type == "SPM") {
                if (attr_flag) {
                        numSteiner <- length(result[[length(result)]])
                        
                        for (i in 1:numSteiner) {
                                V(result[[length(result)]][[i]])$name <- V(result[[length(result)]][[i]])$realname
                                result[[length(result)]][[i]] <- delete_vertex_attr(result[[length(result)]][[i]], 'realname')
                        }
                }
        } else {
                if (attr_flag) {
                        V(result[[length(result)]])$name <- V(result[[length(result)]])$realname
                        result[[length(result)]] <- delete_vertex_attr(result[[length(result)]], 'realname')
                }
        }
        
        return(result)
}
