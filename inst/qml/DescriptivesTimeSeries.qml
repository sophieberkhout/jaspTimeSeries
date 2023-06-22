import QtQuick 2.11
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

Form
{
  info: qsTr("Descriptives allows the user to obtain descriptive statistics and descriptive plots for univariate time-series.")
    VariablesForm
    {
        AvailableVariablesList { name: "variables" }
        AssignedVariablesList  
        {
            name: "dependent"
            label: qsTr("Variable")
            allowedColumns: ["ordinal", "scale"]
            singleVariable: true
            info: qsTr("A variable that is measured repeatedly over time.")
        }
        AssignedVariablesList  
        {
            name: "time"
            label: qsTr("Time")
            allowedColumns: ["ordinal", "nominalText"]
            singleVariable: true
            info: qsTr("Optional. Can either be an ordinal variable indicating the order of the observations, or a text variable indicating the date/time stamp of the observations. Combined date and time values should be in the standard format 'YYYY-MM-DD HH:MM:SS', where seconds (':SS') can also be omitted. Date-only values should be in the format 'YYYY-MM-DD'. If a time variable is not supplied, the row order of the data is used.")
        }
    }

    Group
    {
      CheckBox
      {
          name: "descriptivesTableTransposed"
          label: qsTr("Transpose descriptives table")
      }
    }

    Section
    {
        title: qsTr("Plots")
        columns: 2
        CheckBox
        {
            name: "timeSeriesPlot"
            id:    tsPlot
            label: qsTr("Time series plot")
            checked: true
            info: qsTr("Plots the dependent variable (y-axis) over time (x-axis).")
            RadioButtonGroup
            {
                name:	"timeSeriesPlotType"
                radioButtonsOnSameRow: true
                RadioButton { value: "points";	label: qsTr("Points") }
                RadioButton { value: "line";	label: qsTr("Line") }
                RadioButton { value: "both";	label: qsTr("Both");	checked: true }
            }
            RadioButtonGroup
            {
                name:	"timeSeriesPlotDistribution"
                title: qsTr("Distribution")
                RadioButton { value: "density";	label: qsTr("Density") }
                RadioButton { value: "histogram";	label: qsTr("Histogram") }
                RadioButton { value: "none";	label: qsTr("None");	checked: true }
            }
        }
        CheckBox
        {
            name: "lagPlot"
            id: sspPlot
            label: qsTr("Lag plot")
            info: qsTr("Plots the dependent variable (y-axis) against a lagged version of itself (x-axis). The lag stands for the amount of observations in between the dependent variable and the lagged version of itself. The regression line is the autoregression at the specified lag.")
            Group
            {
                IntegerField
                {
                    name: "lagPlotLag"
                    label: qsTr("Lag")
                    defaultValue: 1
                    min: 1
                }
                CheckBox
                {
                    name: "lagPlotRegressionLine"
                    id: sspSmooth
                    label: qsTr("Add regression line")
                    checked: true
                    RadioButtonGroup
                    {
                        name:	"lagPlotRegressionType"
                        radioButtonsOnSameRow: true
                        RadioButton { value: "smooth";	label: qsTr("Smooth");	checked: true }
                        RadioButton { value: "linear";	label: qsTr("Linear")	}
                    }

                    CheckBox
                    {
                        name: "lagPlotRegressionCi"
                        label: qsTr("Confidence interval")
                        checked: true
                        childrenOnSameRow: true
                        CIField { name: "lagPlotRegressionCiLevel" }
                    }
                }
            }
        }
        CheckBox
        {
            name: "acf"
            id: acf
            label: qsTr("Autocorrelation function")
            IntegerField { name: "acfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
            CheckBox { name: "acfZeroLag"; label: qsTr("Zero lag") }
            info: qsTr("Plots the autocorrelation for a specified number of lags. The confidence interval may be given assuming either a white noise process, or assuming for a lag q a moving average process of order q - 1")
            CheckBox
            {
                name: "acfCi"
                id: acfCi
                label: qsTr("Confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "acfCiLevel" }
                
            }
            RadioButtonGroup
            {
              name: "acfCiType"
              enabled: acfCi.checked
              Layout.leftMargin: 25 * preferencesModel.uiScale
              RadioButton
              {
                value: "whiteNoise"
                label: qsTr("Based on white noise")
                checked: true
              }
              RadioButton
              {
                value: "movingAverage"
                label: qsTr("Based on moving average")
              }
            }
        }
        CheckBox
        {
            name: "pacf"
            id: pacf
            label: qsTr("Partial autocorrelation function")
            info: qsTr("Plots the partial autocorrelation for a specified number of lags.")
            IntegerField { name: "pacfMaxLag"; label: qsTr("Maximum lag"); min: 1; defaultValue: 10 }
            CheckBox
            {
                name: "pacfCi"
                label: qsTr("Confidence interval")
                checked: true
                childrenOnSameRow: true
                CIField { name: "pacfCiLevel" }
            }
        }
    }
}
