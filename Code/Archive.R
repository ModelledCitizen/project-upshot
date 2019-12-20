# original race list
polls <- list.files("~/2018-live-poll-results/data")
polls <- strsplit(polls, "-", fixed = T)
polls <-
    vapply(
        polls,
        FUN = function(x)
            paste0(x[3], "-", strsplit(x[4], ".", fixed = T)[[1]][[1]]),
        FUN.VALUE = "char"
    )


# old plots
plot(
    rep(50, length(data)),
    type = "l",
    main = race,
    xlim = c(0, length(data)),
    ylim = c(25, 75),
    xlab = "",
    ylab = "",
    frame.plot = F,
    xaxt = "n"
)
polygon(
    c(1:length(data), length(data):1),
    c(dem + 100 * errors, rep(50, length(data))),
    col = rgb(0, 0, 1, 1 / 2),
    border = NA
)
# polygon(
#     c(1:length(data), length(data):1),
#     c(dem - 100 * errors, rep(50, length(data))),
#     col = rgb(1, 0, 0, 1 / 2),
#     border = NA
# )
lines(dem, lwd = 2, col = "black")