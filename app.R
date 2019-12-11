library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(tidyverse)
library(plotly)
library(RColorBrewer)
library(cowplot)

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

# Import cleaned data
df <- read_csv("data/supermarket_sales_clean.csv")

# Convert columns into factors
df <- mutate_at(df, vars(`Branch`, `City`, `Gender`, `Customer type`, `Product line`, 
                         `Day_of_week`, `Time_of_day`), as.factor)

df$Day_of_week <- df$Day_of_week %>%
    fct_relevel("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

df$Time_of_day <- df$Time_of_day %>%
    fct_relevel("Evening", "Afternoon", "Morning")

# Selection components

store_key <- tibble(label = c("Yangon", "Mandalay", "Naypyitaw"),
value = c("A", "B", "C"))

store_radio_items <- dccRadioItems(
    id = "Store",
    options = map(
        1:nrow(store_key), function(i) {
            list(label = store_key$label[i], value = store_key$value[i])
        }
    ),
    value = "A"
)

app$layout(
    htmlDiv(
        list(
            htmlH1("Supermarket employee scheduling"),
            htmlP("Review historical sales of a supermarket to improve employee scheduling by day of week, time of day and/or department."),
            htmlA("Link to original dataset", href = "https://www.kaggle.com/aungpyaeap/supermarket-sales"),
            htmlP("The dataset contains historical sales of a supermarket chain in Myanmar across three branches over the course of three months."),
            htmlLabel("Select store:"),
            store_radio_items
        ),
        style = list("backgroundColor" = "#feea9d")
    ),
    dccTabs(
        id = "tabs", children = list(
            # First tab
            dccTab(
                label = "Store Performance Summary", children = list(
                    htmlDiv(
                        children = list(
                            htmlDiv(
                                list(
                                    htmlH3("Store Performance Summary"),
                                    
                                    dccMarkdown("
                                    **Purpose:** Identify the day of the week and time of day where the store might be overstaffed/understaffed.

                                    **Guiding questions:**
                                    - Are there periods of time with ***high*** total sales, ***high*** customer traffic, ***high*** average transaction size but ***low*** customer satisfaction?
                                        - Has the store been understaffed when it was busy?
                                    - Are there periods of time with ***low*** total sales, ***low*** customer traffic, ***low*** average transaction size and ***high*** customer satisfaction?
                                        - Has the store been overstaffed when it was slow?
                                    - Are there periods of ***high*** customer traffic but ***low*** average transaction size?
                                        - Would scheduling in extra staff encourage customers to spend more?

                                    **Note:** *Morning* is 9:00-12:59, *Afternoon* is 13:00-16:59 and *Evening* is 17:00-20:59. 
                                    "
                                    )
                                ),
                                style = list("backgroundColor" = "#bdd3e1")
                            )

                            # Arrange heat maps here
                        ),
                        className = "container"
                    )
                )
            ),
            # Second Tab
            dccTab(
                label = "Compare Store Performace by Department", children = list(
                    htmlDiv(
                        children = list(
                            htmlDiv(
                                list(
                                    htmlH3("Compare Store Performance by Department"),

                                    dccMarkdown("
                                    **Purpose:** Compare department-specific performance for a particular day and time to identify when and where to increase/reduce staff.

                                    **Guiding example:** 
                                    "
                                    )
                                ),
                                style = list("backgroundColor" = "#FFD89F")
                            ),
                            htmlLabel("Select first shift to compare:")
                        ),
                        className = "container"
                    )
                )
            )
        )
    )
)

app$run_server()