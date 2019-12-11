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

day_Dropdown1 <- dccDropdown(
  id = "Dayofweek1",
  style = list("width" = "60%"),
  options = map(
    levels(df$Day_of_week), function(x){
    list(label=x, value=x)
  }),
  value = "Monday", #Selects Monday by default
)

time_Dropdown1 <- dccDropdown(
  id = "Timeofday1",
  style = list("width" = "60%"),
  options = map(
    levels(df$Time_of_day), function(x){
    list(label=x, value=x)
  }),
  value = "Morning", #Selects Morning by default
)

day_Dropdown2 <- dccDropdown(
  id = "Dayofweek2",
  style = list("width" = "60%"),
  options = map(
    levels(df$Day_of_week), function(x){
    list(label=x, value=x)
  }),
  value = "Monday", #Selects Sunday by default
)

time_Dropdown2 <- dccDropdown(
  id = "Timeofday2",
  style = list("width" = "60%"),
  options = map(
    levels(df$Time_of_day), function(x){
    list(label=x, value=x)
  }),
  value = "Morning", #Selects Evening by default
)

# bar plots

# total sales
make_barplot_totalSales <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Sales in MMK` = round(sum(Total), 2)) %>%
        ggplot(aes(x = `Product line`, y = `Sales in MMK`, color = `Product line`, fill = `Product line`)) +
            geom_bar(stat = "identity", alpha = 0.8) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
            labs(title = "Total Sales", y = "Sales in MMK") 
    ggplotly(p, tooltip = c("x", "y"), width = 500, height = 300)
}

# customer Traffic
make_barplot_cusTraf <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Transactions` = n()) %>%
        ggplot(aes(x = `Product line`, y = `Transactions`, color = `Product line`, fill = `Product line`)) +
            geom_bar(stat = "identity", alpha = 0.8) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
            labs(title = "Customer Traffic", y = "Transactions") 
    ggplotly(p, tooltip = c("x", "y"), width = 500, height = 300)
}

# average transaction size
make_barplot_avgTranSize <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Sales in MMK` = round(mean(Total), 2)) %>%
        ggplot(aes(x = `Product line`, y = `Sales in MMK`, color = `Product line`, fill = `Product line`)) +
            geom_bar(stat = "identity", alpha = 0.8) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
            labs(title = "Average Transaction Size", y = "Sales in MMK")
    ggplotly(p, tooltip = c("x", "y"), width = 500, height = 300)
}

# average satisfaction 
make_barplot_avgSat <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Average Rating` = round(mean(Rating), 2)) %>%
        ggplot(aes(x = `Product line`, y = `Average Rating`, color = `Product line`, fill = `Product line`)) +
            geom_bar(stat = "identity", alpha = 0.8) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                 axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
            labs(title = "Average Satisfaction", y = "Average Rating") 
    ggplotly(p, tooltip = c("x", "y"), width = 500, height = 300)
}

graph1_1 <- dccGraph(
  id = 'barplot1_1',
  figure = make_barplot_totalSales()
)

graph1_2 <- dccGraph(
  id = 'barplot1_2',
  figure = make_barplot_cusTraf()
)

graph1_3 <- dccGraph(
  id = 'barplot1_3',
  figure = make_barplot_avgTranSize()
)

graph1_4 <- dccGraph(
  id = 'barplot1_4',
  figure = make_barplot_avgSat()
)

graph2_1 <- dccGraph(
  id = 'barplot2_1',
  figure = make_barplot_totalSales()
)

graph2_2 <- dccGraph(
  id = 'barplot2_2',
  figure = make_barplot_cusTraf()
)

graph2_3 <- dccGraph(
  id = 'barplot2_3',
  figure = make_barplot_avgTranSize()
)

graph2_4 <- dccGraph(
  id = 'barplot2_4',
  figure = make_barplot_avgSat()
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
                            htmlDiv(
                                list(
                                    htmlLabel("Select first shift to compare:"),
                                    htmlDiv(
                                        list(
                                            day_Dropdown1,
                                            time_Dropdown1
                                            )
                                        ),
                                    htmlDiv(
                                        list(
                                            graph1_1,
                                            graph1_2,
                                            graph1_3,
                                            graph1_4
                                            ),
                                            style = list('columnCount' = 2)
                                    )
                                )
                            ),
                            htmlDiv(
                                list(
                                    htmlLabel("Select second shift to compare:"),
                                    htmlDiv(
                                        list(
                                            day_Dropdown2,
                                            time_Dropdown2
                                            )
                                        ),
                                    htmlDiv(
                                        list(
                                            graph2_1,
                                            graph2_2,
                                            graph2_3,
                                            graph2_4
                                            ),
                                            style = list('columnCount' = 2)
                                    )
                                )
                            )
                        ),
                        className = "container"
                    )
                )
            )
        )
    )
)

# callback

app$callback(
  output=list(id = 'barplot1_1', property='figure'),
  params=list(input(id = "Dayofweek1", property='value'),
              input(id = 'Timeofday1', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_totalSales(DayofWeek, TimeofDay, branch)
  })

app$callback(
  output=list(id = 'barplot1_2', property='figure'),
  params=list(input(id = "Dayofweek1", property='value'),
              input(id = 'Timeofday1', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_cusTraf(DayofWeek, TimeofDay, branch)
  })

app$callback(
  output=list(id = 'barplot1_3', property='figure'),
  params=list(input(id = "Dayofweek1", property='value'),
              input(id = 'Timeofday1', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_avgTranSize(DayofWeek, TimeofDay, branch)
  })

app$callback(
  output=list(id = 'barplot1_4', property='figure'),
  params=list(input(id = "Dayofweek1", property='value'),
              input(id = 'Timeofday1', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_avgSat(DayofWeek, TimeofDay, branch)
  })  

app$callback(
  output=list(id = 'barplot2_1', property='figure'),
  params=list(input(id = "Dayofweek2", property='value'),
              input(id = 'Timeofday2', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_totalSales(DayofWeek, TimeofDay, branch)
  })

  app$callback(
  output=list(id = 'barplot2_2', property='figure'),
  params=list(input(id = "Dayofweek2", property='value'),
              input(id = 'Timeofday2', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_cusTraf(DayofWeek, TimeofDay, branch)
  })

app$callback(
  output=list(id = 'barplot2_3', property='figure'),
  params=list(input(id = "Dayofweek2", property='value'),
              input(id = 'Timeofday2', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_avgTranSize(DayofWeek, TimeofDay, branch)
  })

app$callback(
  output=list(id = 'barplot2_4', property='figure'),
  params=list(input(id = "Dayofweek2", property='value'),
              input(id = 'Timeofday2', property='value'),
              input(id = 'Store', property='value')),
  function(DayofWeek, TimeofDay, branch) {
    make_barplot_avgSat(DayofWeek, TimeofDay, branch)
  })  

app$run_server()