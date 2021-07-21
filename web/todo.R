library("rjson")

todo <- fromJSON(file = "todo.json")$data

writeback <- function() {
    write(toJSON(list("data" = todo)), file = "todo.json")
}

add_task <- function(name) {
    if (!(name %in% todo)) {
        todo[[length(todo) + 1]] <<- list(
            "name" = name,
            "done" = FALSE
        )
        writeback()
        return("OK")
    }
    return("FAIL")
}

remove_task <- function(name) {
    for (i in seq_along(todo)) {
        if (todo[[i]]$name == name) {
            todo <<- todo[-i]
            writeback()
            return("OK")
        }
    }
    return("FAIL")
}

done_task <- function(name) {
    for (i in seq_along(todo)) {
        if (todo[[i]]$name == name) {
            todo[[i]]$done <<- TRUE
            writeback()
            return("OK")
        }
    }
    return("FAIL")
}

undo_task <- function(name) {
    for (i in seq_along(todo)) {
        if (todo[[i]]$name == name) {
            todo[[i]]$done <<- FALSE
            writeback()
            return("OK")
        }
    }
    return("FAIL")
}

get_tasks <- function() {
    transposed_data <- matrix(
        unlist(todo),
        ncol = length(todo[[1]]),
        byrow = TRUE
    )
    colnames(transposed_data) <- names(todo[[1]])

    return(transposed_data)
}