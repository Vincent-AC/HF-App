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
                                name = paste(drugNameA,"Central"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(255, 0, 0)', dash = 'dash')
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_A_CARTRIDGE,
                                name = paste(drugNameA,"Cartridge"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(255, 0, 0)')
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_B_CENTRAL,
                                name = paste(drugNameB,"Central"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(0, 0, 255)', dash = 'dash')
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_B_CARTRIDGE,
                                name = paste(drugNameB,"Cartridge"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(0, 0, 255)')
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
                                name = paste(drugNameA,"Central"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(255, 0, 0)', dash = 'dash')
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_A_CARTRIDGE,
                                name = paste(drugNameA,"Cartridge"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(255, 0, 0)')
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_B_CENTRAL,
                                name = paste(drugNameB,"Central"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(0, 0, 255)', dash = 'dash')
                        ) %>%
                        add_trace(
                                data = simulated_data,
                                type = 'scatter',
                                mode = 'lines',
                                x = ~ time / 60,
                                y = ~ C_DRUG_B_CARTRIDGE,
                                name = paste(drugNameB,"Cartridge"),
                                hovertemplate = paste(
                                        '<b>Concentration</b>: %{y:.2f} mg/L',
                                        '<br><b>Time</b>: %{x:.2f} h'
                                ),
                                showlegend = FALSE,
                                line = list(color = 'rgb(0, 0, 255)')
                        ) %>%
                layout(
                        xaxis = list(title = "Time (h)"),
                        yaxis = list(title = "Concentration (mg/L)"),
                        title = list(text = "Concentration after infusion")
                )
        }



        return(plot)
}