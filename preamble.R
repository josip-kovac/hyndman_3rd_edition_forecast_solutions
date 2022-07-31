require(tidyverse)
require(fpp3)


get_website_code <- function() {
    out <- "https://otexts.com/fpp3/"
    cat(out)
}

cat("\nPreamble is loaded!")

ggplot2::theme_set(tidyquant::theme_tq())


time_series_split <- function(dataset, train_size = 1) {
    
    dataset_size <- nrow(dataset)
    
    train_index <- 1:train_size
    test_index <- (max(train_index) + 1):dataset_size
    
    out <- list(
        train = dataset[train_index, ],
        test = dataset[test_index, ]
    )
    
    return(out)
}
