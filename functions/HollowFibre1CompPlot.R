HollowFibre1CompPlot  <-
  function(simulated_data,
           lastTimePointHours,
           dosingIntervalHoursBolus = NA,
           dosingIntervalHoursInf = NA,
           adm.type,
           drugName = "DrugA")
  {
    library(dplyr)
    library(ggplot2)
    library(plotly)
    if (adm.type == "Bolus")
    {
      plot <- plot_ly() %>%
        add_trace(
          data = simulated_data,
          type = 'scatter',
          mode = 'lines',
          x = ~ time / 60,
          y = ~ C_DRUG_CENTRAL,
          name = drugName,
          hovertemplate = paste(
            '<b>Concentration</b>: %{y:.2f} mg/L',
            '<br><b>Time</b>: %{x:.2f} h'
          ),
          showlegend = FALSE,
          color = I("red")
        ) %>%
        layout(
          xaxis = list(title = "Time (h)"),
          yaxis = list(title = "Concentration (mg/L)"),
          title = list(text = "Concentration after bolus")
        )
    }
    if (adm.type == "Infusion")
    {
      #Red is drug A
      plot <- plot_ly() %>%
        add_trace(
          data = simulated_data,
          type = 'scatter',
          mode = 'lines',
          x = ~ time / 60,
          y = ~ C_DRUG_CENTRAL,
          hovertemplate = paste(
            '<b>Concentration</b>: %{y:.2f} mg/L',
            '<br><b>Time</b>: %{x:.2f} h<br>'
          ),
          showlegend = FALSE,
          color = I("red")
        ) %>%
        layout(
          xaxis = list(title = "Time (h)"),
          yaxis = list(title = "Concentration (mg/L)"),
          title = list(text = "Concentration after infusion")
        )
    }
    if (adm.type == "Loading dose + Infusion")
    {
      #Red is drug A
      plot <- plot_ly() %>%
        add_trace(
          data = simulated_data,
          type = 'scatter',
          mode = 'lines',
          x = ~ time / 60,
          y = ~ C_DRUG_CENTRAL,
          hovertemplate = paste(
            '<b>Concentration</b>: %{y:.2f} mg/L',
            '<br><b>Time</b>: %{x:.2f} h<br>'
          ),
          showlegend = FALSE,
          color = I("red")
        ) %>%
        layout(
          xaxis = list(title = "Time (h)"),
          yaxis = list(title = "Concentration (mg/L)"),
          title = list(text = "Concentration after infusion")
        )
    }

    return(plot)
  }