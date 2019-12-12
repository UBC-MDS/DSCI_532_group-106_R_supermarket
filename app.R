library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(purrr)
library(tibble)
library(forcats)
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

# heat maps
make_heat_map <- function(branch, title, col_title, func) {
    hm <- df %>%
        filter(Branch == branch) %>%
        group_by(Day_of_week, Time_of_day) %>%
        summarize(col_title = {{func}}) %>%
        ggplot(aes(x = Day_of_week, y = Time_of_day, fill = col_title)) +
            geom_tile() +
            scale_fill_distiller(palette = "Greens", direction = 1) +
            ggtitle(title) +
            labs(x = "", y = "", fill = "") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(hm, tooltip = c("x", "y"), width = 500, height = 300) %>%
      config(displayModeBar = FALSE)
}

total_sales_hm <- dccGraph(
  id = 'total_sales_hm',
  figure = make_heat_map("A", "Total Sales (in MMK)", total_sales, sum(Total))
)

cust_traffic_hm <- dccGraph(
  id = 'cust_traffic_hm',
  figure = make_heat_map("A", "Customer Traffic (transactions)", traffic, n())
)

avg_trxn_size_hm <- dccGraph(
  id = 'avg_trxn_size_hm',
  figure = make_heat_map("A", "Average Transaction Size (in MMK)", trxn_size, sum(Total) / n())
)

avg_cust_sat_hm <- dccGraph(
  id = 'avg_cust_sat_hm',
  figure = make_heat_map("A", "Average Customer Satisfaction (out of 10)", cust_sat, mean(Rating))
)

# bar plots
# total sales
make_barplot_totalSales <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Sales in MMK` = round(sum(Total), 2)) %>%
        ggplot(aes(x = `Product line`, y = `Sales in MMK`)) +
            geom_bar(stat = "identity", alpha = 0.8, fill = "#6b5b95") +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
            labs(title = "Total Sales", y = "Sales in MMK") 
    ggplotly(p, tooltip = c("x", "y"), width = 500, height = 300) %>%
      config(displayModeBar = FALSE)
}

# customer Traffic
make_barplot_cusTraf <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Transactions` = n()) %>%
        ggplot(aes(x = `Product line`, y = `Transactions`)) +
            geom_bar(stat = "identity", alpha = 0.8, fill = "#feb236") +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.ticks.x=element_blank(), axis.title.x=element_blank())+
            labs(title = "Customer Traffic", y = "Transactions") 
    ggplotly(p, tooltip = c("x", "y"), width = 500, height = 300)
}

# average transaction size
make_barplot_avgTranSize <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Sales in MMK` = round(mean(Total), 2)) %>%
        ggplot(aes(x = `Product line`, y = `Sales in MMK`)) +
            geom_bar(stat = "identity", alpha = 0.8, fill = "#d64161") +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
            labs(title = "Average Transaction Size", y = "Sales in MMK")
    ggplotly(p, tooltip = c("x", "y"), width = 500, height = 300)
}

# average satisfaction 
make_barplot_avgSat <- function(DayofWeek = "Monday", TimeofDay = "Morning", branch = "A"){
    p <- df %>%
        filter(Branch == branch & Day_of_week == DayofWeek & Time_of_day == TimeofDay) %>%
        group_by(`Product line`) %>%
        summarise(`Average Rating` = round(mean(Rating), 2)) %>%
        ggplot(aes(x = `Product line`, y = `Average Rating`)) +
            geom_bar(stat = "identity", alpha = 0.8, fill = "#ff7b25") +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  axis.text.x = element_text(angle = 45, hjust = 1),
                  axis.ticks.x=element_blank(), axis.title.x=element_blank()) +
            labs(title = "Average Customer Satisfaction", y = "Rating") 
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
        style = list("backgroundColor" = "#DCDCDC")
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

                                    **Note:** Morning is 9:00-12:59, Afternoon is 13:00-16:59 and Evening is 17:00-20:59. 
                                    "
                                    )
                                ),
                                style = list("backgroundColor" = "#DCDCDC")
                            ), 
                            htmlDiv(
                              list(
                                total_sales_hm,
                                cust_traffic_hm,
                                avg_trxn_size_hm,
                                avg_cust_sat_hm
                              ),
                              style = list('columnCount' = 2)
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
                                    I'm considering scheduling more people on Sunday evenings. I'm considering adding more staff to the Sports & Travel department where there is the highest
                                    traffic and lowest satisfaction. Before deciding, I can compare the store performance on Sunday evenings to that of Saturday afternoons. The Sports & Travel department
                                    seems to also have high customer traffic but much lower average transaction sizes on Saturday afternoons when compared to Sunday evenings. Therefore, I should schedule in
                                    more staff in the Sports & Travel department on Saturday afternoons in an attempt to boost sales.
                                    "
                                    )
                                ),
                                style = list("backgroundColor" = "#DCDCDC")
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
  output=list(id = 'total_sales_hm', property = 'figure'),
  params=list(input(id = 'Store', property='value')),
  function(branch) {
    make_heat_map(branch, "Total Sales (in MMK)", total_sales, sum(Total))
  })

app$callback(
  output=list(id = 'cust_traffic_hm', property = 'figure'),
  params=list(input(id = 'Store', property='value')),
  function(branch) {
    make_heat_map(branch, "Customer Traffic (transactions)", traffic, n())
  })

  app$callback(
  output=list(id = 'avg_trxn_size_hm', property = 'figure'),
  params=list(input(id = 'Store', property='value')),
  function(branch) {
    make_heat_map(branch, "Average Transaction Size (in MMK)", trxn_size, sum(Total) / n())
  })
  
  app$callback(
  output=list(id = 'avg_cust_sat_hm', property = 'figure'),
  params=list(input(id = 'Store', property='value')),
  function(branch) {
    make_heat_map(branch, "Average Customer Satisfaction (out of 10)", cust_sat, mean(Rating))
  })

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

app$run_server(host = "0.0.0.0", port = Sys.getenv('PORT', 8050))