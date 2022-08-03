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

make_exercise_headers <- function(range_num) {
    
    for (ex in seq_along(range_num)) {
        message <- stringr::str_pad(ex, 2, "left", "0")
        message <- glue::glue("# Exercise {message}")
        cat(message, sep = "\n")
        cat("\n")
    }
    
}


check_forecast <- function(object_fit, object_fc, object_data) {
    
    out <- object_fc %>% 
        autoplot(object_data) +
        facet_wrap(. ~ .model) +
        scale_color_brewer(palette = "Set1") +
        geom_line(data = augment(object_fit), aes(y = .fitted), color = "purple") +
        geom_text(data = accuracy(object_fit),
                  aes(label = glue::glue("TRAIN_RMSE = {scales::comma(RMSE, accuracy = 0.01, scale = 1)}"), x = -Inf, y = Inf),
                  size = 5,
                  hjust = -0.5,
                  vjust = 6,
                  color = "blue") + 
        geom_text(data = accuracy(object_fit),
                  aes(label = glue::glue("TRAIN_MAPE = {scales::percent(MAPE, accuracy = 0.01, scale = 1)}"), x = -Inf, y = Inf),
                  size = 5,
                  hjust = -0.5,
                  vjust = 8,
                  color = "blue")
    
    return(out)
}

check_forecast_with_split_data <- function(object_fit, object_fc, object_data, object_test) {
    
    out <- object_fc %>% 
        autoplot(object_data) +
        facet_wrap(. ~ .model) +
        scale_color_brewer(palette = "Set1") +
        geom_line(data = augment(object_fit), aes(y = .fitted), color = "purple") +
        geom_text(data = accuracy(object_fc, object_test),
                  aes(label = glue::glue("TEST_RMSE = {scales::comma(RMSE, accuracy = 0.01, scale = 1)}"), x = -Inf, y = Inf),
                  size = 5,
                  hjust = -0.5,
                  vjust = 6,
                  color = "blue") + 
        geom_text(data = accuracy(object_fc, object_test),
                  aes(label = glue::glue("TEST_MAPE = {scales::percent(MAPE, accuracy = 0.01, scale = 1)}"), x = -Inf, y = Inf),
                  size = 5,
                  hjust = -0.5,
                  vjust = 8,
                  color = "blue")
    
    return(out)
}
