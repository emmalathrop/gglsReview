add_letters <- ggproto("add_letters", Stat,
                     compute_group = function(data, scales) {
                       data[chull(data$x, data$y), , drop = FALSE]
                     },
                     required_aes = c("x", "y")
)
