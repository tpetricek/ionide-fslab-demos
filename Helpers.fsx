namespace XPlot.GoogleCharts
open XPlot.GoogleCharts

module Chart =
  let DarkTheme (ch:XPlot.GoogleCharts.GoogleChart) =
    let gridlines = "#394556"
    let background = "#313C4A"
    let textcolor = "#99A6B8"
    let opts =
      ch.GetType().GetField(
         "options", System.Reflection.BindingFlags.NonPublic |||
          System.Reflection.BindingFlags.Instance ).GetValue(ch) :?> XPlot.GoogleCharts.Configuration.Options
    opts.backgroundColor <- BackgroundColor(fill="transparent")
    opts.hAxis <- Axis(baselineColor = gridlines, gridlines = Gridlines(color=gridlines))
    opts.vAxis <- Axis(baselineColor = gridlines, gridlines = Gridlines(color=gridlines))
    opts.hAxis.textStyle <- TextStyle(color=textcolor)
    opts.vAxis.textStyle <- TextStyle(color=textcolor)
    opts.chartArea <- ChartArea(backgroundColor = BackgroundColor(fill=background))
    opts.legend.textStyle <- TextStyle(color=textcolor)
    opts.titleTextStyle <- TextStyle(color=textcolor)
    opts.colors <- "#aec7e8,#ffbb78,#98df8a,#ff9896,#c5b0d5,#c49c94,#f7b6d2,#c7c7c7,#dbdb8d,#9edae5".Split(',')
    ch
