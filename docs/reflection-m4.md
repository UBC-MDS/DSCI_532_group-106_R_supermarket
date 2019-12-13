# Reflection for Milestone 4

Authors: Reiko Okamoto, Monique Wong and Haoyu Su

Date: December 14, 2019

## What our dashboard does well
Our Supermarket Staffing dashboard was designed to solve a real issue that supermarket managers face and our implementation of the dashboard sufficiently meets the need. 

Specifically, our dashboard:

* **is purpose-driven**: We are helping a manager answer how to adjust the staffing levels in their store by department, by day of week and time of day based on past trends.
* **is appropriately sequenced for decision making**: Overall store performance by the day of week and time of day is presented first before managers decide which time frames to focus on in their department-specific analysis. 
* **is appropriately interactive**: We limited the options that a user could choose when exploring the data to what we believe is most useful in the context of the decision. For example, we picked the store, day of week, time of day and department as options since store associates are staffed in shifts by department in each store.
* **is simple to understand**: We purposely used simple to interpret charts such as heat maps and bar charts so that the focus is on analysis and decision-making instead of learning how to read the visualization. We also picked simple color schemes that don't distract from interpretation. Colors were only used if they provide meaning (e.g., on the heatmaps). Axis labels have been chosen to improve interpretability.
  
## Limitations and future improvements

While our dashboard is minimally viable, it has its limitations. The limitations and potential future improvements we have identified are:

* Summary data of all weeks are displayed instead of allowing users to select the week they want to see.
  * A supermarket manager may want to see store performance data for a specific week or subset of weeks instead of all historical data to make their decision. This may be a useful functionality if our manager was staffing for Black Friday weekend and wanted to see store performance details of previous holiday weekends.
  * Adding this functionality would involve adding a slider at the top of the dashboard that enables users to select the weeks for which they want to see the visualizations on the dashboard.
* Days of the week and times of day are selected separately and combinations of choices are not possible.
  * A supermarket manager may be interested in staffing not only 4 hour shifts but 8 hour shifts which would require analyzing the data by "Morning" and "Afternoon" or "Afternoon" and "Evening".
  * A supermarket manager may also be interested in seeing performance separated by weekend and weekdays which would require selecting multiple days of the week.
  * Implementing this functionality would require changing the drop down selector to a checkbox selector so that users can select multiple days of the week and/or times of the day in the analysis.
* We also have known issues as we implemented our app in DashR:
  1. Loading lag with top left chart on both tabs: When the app loads, the top left chart is sized smaller but adjusts itself in a few seconds. We do not have a known fix for this problem. 
  2. Compressed heat map legends: The bottom two charts on the heat maps have legend labels that are not very informative for some selections since there isn't a large enough range in the values. Improving this would require adjusting the legends so that a sufficiently large range of values are reflected. For customer satisfaction, for instance, this may mean setting a common scale of 6 to 10 no matter the selection. 
  3. Tooltip formatting on heat map: The labels reflect the column names which is not easily interpreted by the user. We need to update the tooltip so that it shows the value of the selection. 
  4. Cannot hide plotly modebar: This is a known issue and a request has been made to reopen this issue. While the code for hiding this toolbar works in Jupyter Labs, it does not work when deployed on Dash. We do not have a known fix for this. 
  5. Maintaining our code: We have not finished maintaining our code (e.g., adding docstrings) and fully adhering to the tidyverse (e.g., snake case instead of camel case). If we were to work further on our app, this would be an obvious next step. 
