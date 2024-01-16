# Creating attractive Forest Plots for survival hazard estimates with confidence intervals
# Stephen McDaniel
# 2024-JAN
#
# License: MIT License
# Attribution, adapted from Gantt chart examples from Plotly.


library(plotly)

df_survival <- data.frame(factor.name = c("Massage", "Chiropractic", "Extreme Sports", "Yoga"), 
   HR = c(0.3, 0.8, 4, 1.2), 
   Lower_CI = c(0.15, 0.6, 2.2, 0.9), 
   Upper_CI = c(0.5, 0.95, 12, 1.5)
)

df_calc <- df_survival %>%
    mutate(bar_length = Upper_CI - Lower_CI,
           bar_color = ifelse(Lower_CI > 1, "red", ifelse(Upper_CI < 1, "green", "gray"))
    ) %>%
    arrange(-HR)

# Initialize empty plot
fig <- plot_ly(type = "scatter", mode = "lines")

# core of chart (hazard conf intervals)
# form of Lower_CI (lower) and bar_length (upper - lower)
for(i in 1:(nrow(df_calc))){
 fig <- add_trace(fig,
                 x = c(df_calc$Lower_CI[i], df_calc$Lower_CI[i] + df_calc$bar_length[i]),  # x0, x1
                 y = c(i, i),  # y0, y1
                 line = list(color = df_calc$bar_color[i], width = 20),
                 showlegend = F
  )
}

fig

# background, text labels etc
fig2 <- layout(fig,

            # Axis options:
            # 1. Remove gridlines
            # 2. Customize y-axis tick labels and show task names instead of numbers

            xaxis = list(showgrid = T, tickfont = list(color = "white"), grid = list(color = "white")),

            yaxis = list(showgrid = T, tickfont = list(color = "white"),
                         tickmode = "array", tickvals = 1:nrow(df_calc), ticktext = unique(df_calc$factor.name),
                         domain = c(0, 0.9)),

            plot_bgcolor = "#1A1A1A",  # Chart area color
            paper_bgcolor = "black") # Axis area color


# markers for mid-point of hazard
for(i in 1:(nrow(df_calc))){
 fig2 <- fig2 %>%
    add_markers(x = df_calc$Lower_CI[i] + (df_calc$bar_length[i] / 2) - pmin(1, df_calc$bar_length[i]/40)/2, 
             y = i, 
             marker = list(color = 'white', symbol = 'diamond-tall', size = 10),
             showlegend = F
    ) %>%
    layout(xaxis = list(rangemode = "tozero"))
}

# title info
title <- list(
    xref = "paper",
    yref = "paper",
    x = 0.1,
    y = 1,
    xanchor = "left",
    text = paste0("Survival: "),
    font = list(color = 'white', size = 20, family = "Times New Roman"),
    ax = 0,
    ay = 0,
    align = "left",
    showarrow = FALSE
)

fig2 <- fig2 %>% 
    # add title info
    layout(annotations = title) %>%
    # add reference line at 1
    layout(shapes = list(type = "line", 
                        fillcolor = "gray",
                        line = list(color = "gray"),
                        opacity = 0.3,
                        x0 = 1, x1 = 1, xref = 'x', 
                        y0 = 0.5, y1 = nrow(df_calc), yref = 'y'))

fig2
