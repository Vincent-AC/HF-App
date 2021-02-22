HollowFibre2CompPlot  <- function(simulated_data,
                                  lastTimePointHours = 24,
                                  drugNameA = "DrugA",
                                  drugNameB = "DrugB",
                                  adm.type = "Bolus")
{
        library(mrgsolve)
        library(dplyr)
        library(plotly)

        if (adm.type == "Bolus")
        {
                plot <- plot_ly() %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_A_CENTRAL,
                                name = drugNameA,
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                color = I("red")
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_B_CENTRAL,
                                name = drugNameB,
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                color = I("blue")
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
                                y = ~ C_DRUG_A_CENTRAL,
                                name = drugNameA,
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                color = I("red")
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_B_CENTRAL,
                                name = drugNameB,
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                color = I("blue")
                        ) %>%
                layout(
                        xaxis = list(title = "Time (h)"),
                        yaxis = list(title = "Concentration (mg/L)"),
                        title = list(text = "Concentration after infusion")
                )
        }



        return(plot)
}