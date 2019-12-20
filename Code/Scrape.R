library("purrr")
library("RCurl")
library("rjson")
library("ggplot2")



all_races <- getURL("https://int.nyt.com/newsgraphics/2018/live-polls-2018/all-races.json")

all_races <- fromJSON(all_races)

all_races <-
    data.frame(
        page_id = vapply(
            all_races,
            FUN = function(x)
                x[["page_id"]],
            FUN.VALUE = "char"
        ),
        name = vapply(
            all_races,
            FUN = function(x)
                x[["name"]],
            FUN.VALUE = "char"
        ),
        desc = vapply(
            all_races,
            FUN = function(x)
                x[["area_in_english"]],
            FUN.VALUE = "char"
        ),
        status = vapply(
            all_races,
            FUN = function(x)
                x[["status"]],
            FUN.VALUE = "char"
        ),
        n = vapply(
            all_races,
            FUN = function(x)
                as.integer(x[["n"]]),
            FUN.VALUE = 1L
        ),
        targetn = vapply(
            all_races,
            FUN = function(x)
                as.integer(x[["callsToComplete"]]),
            FUN.VALUE = 1L
        ),
        totalCalls = vapply(
            all_races,
            FUN = function(x)
                as.integer(x[["totalCalls"]]),
            FUN.VALUE = 1L
        ),
        startDate = vapply(
            all_races,
            FUN = function(x)
                x[["startDate"]],
            FUN.VALUE = "char"
        ),
        endDate = vapply(
            all_races,
            FUN = function(x)
                x[["endDate"]],
            FUN.VALUE = "char"
        ),
        stringsAsFactors = FALSE
    )



for (racen in 1:nrow(all_races)) {
    
    race <- all_races[racen, "page_id"]
    
    scrape <-
        "https://int.nyt.com/newsgraphics/2018/live-polls-2018/races/%s-timeline.json"
    
    scrape <- sprintf(scrape, race)
    
    scrape <- getURL(scrape)
    
    scrape <- fromJSON(scrape)
 
    if (all_races[racen, "n"] + 1 == length(scrape)) {
        scrape <- scrape[2:length(scrape)]
    }
       
    scrape <-
        data.frame(
            responses = 1:length(scrape),
            margin = vapply(
                scrape,
                FUN = function(x)
                    x[["margin"]],
                FUN.VALUE = 1.0
            ),
            error = vapply(
                scrape,
                FUN = function(x)
                    if (is.null(x[["error"]]))
                        NA
                else
                    x[["error"]],
                FUN.VALUE = 1.0
            )
        )
    
    if (paste0(race, ".csv") %in% list.files("~/2018-live-poll-results/data")) {
        data <-
            read.csv(paste0(
                "~/2018-live-poll-results/data/",
                race,
                ".csv"
            ))

        marg <-
            prop.table(xtabs(final_weight ~ response, data = data))
        marg <- marg[["Rep"]] - marg[["Dem"]]
        marg <- round(marg, 4)

        if (nrow(scrape) != nrow(data)) {
            warning(paste0(race, ": n differs between scrape and data!"),
                    immediate. = T)
            cat(paste("Scrape:", nrow(scrape)), "\n")
            cat(paste("Data:", nrow(data)), "\n")
        }

        if (scrape[nrow(scrape), "margin"] != marg) {
            warning(paste0(race, ": margin differs between scrape and data!"),
                    immediate. = T)
            cat(paste("Scrape:", scrape[nrow(scrape), "margin"]), "\n")
            cat(paste("Data:", marg), "\n")
        }
    }
    
    
    png(
        filename = paste0("Charts/", race, ".png"),
        width = 1000,
        height = 400
    )
    
    plot <- ggplot(scrape, aes(x = responses, y = -margin)) +
        labs(title = all_races[racen, "name"], subtitle = all_races[racen, "desc"]) +
        coord_cartesian(
            ylim = c(-0.25, 0.25),
            xlim = c(0, if (all_races[racen, "status"] != "completed")
                all_races[racen, "targetn"]
                else
                    all_races[racen, "n"]),
            expand = FALSE
        ) +
        scale_y_continuous(
            breaks = c(-0.2, -0.1, 0, 0.1, 0.2),
            labels = c("+20", "+10", "Even", "+10", "+20")
        ) +
        geom_hline(aes(yintercept = 0), size = 0.5) +
        geom_ribbon(aes(
            ymin = ifelse(-margin - error > 0, -margin - error, 0),
            ymax = ifelse(error - margin > 0, error - margin, 0)
        ),
        fill = "dodgerblue1",
        alpha = 0.25) +
        geom_ribbon(aes(
            ymin = ifelse(-margin - error < 0, -margin - error, 0),
            ymax = ifelse(error - margin < 0, error - margin, 0)
        ),
        fill = "firebrick1",
        alpha = 0.25) +
        geom_line(size = 1,
                  color = ifelse(scrape$margin < 0, "dodgerblue3", "firebrick3")) +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid = element_line(linetype = "dashed"))
    
    print(plot)
    
    dev.off()
    
}
