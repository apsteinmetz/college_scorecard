# shiny app to analyze post-college earning data

## Goal

Create a shiny app to visualize earning data found here:

## Main Dashboard Display

1. A dotplot showing one dot per school, per major.
2. A dotplot showing one dot per school.
3. A dotplot showing one dot per major.

The y-axis is geographical latitude of each school.
The x-axis is the post-college student earning as a ratio of the baseline for that school/major data row.

The default color of the dots is light grey.

## Filtering options

Filters are displayed as dropdown lists in a left sidebar.  "All" is the first option for each filter.  Include a "Clear Filters" button.  They are not mutually exclusive but if a combination is not feasible indicate "Not Available" above the filter selectors.

Filters:

1. School
2. Major
3. Athletic conference

Retain all the dots after filtering but highlight the filter-selected dots in blue.

