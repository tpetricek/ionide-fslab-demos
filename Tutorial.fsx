#nowarn "211"
#load "FsLab.fsx"
#load "Helpers.fsx"

open Deedle
open FSharp.Data
open XPlot.GoogleCharts
open XPlot.GoogleCharts.Deedle

// Connect to the WorldBank and access indicators EU and CZ
let wb = WorldBankData.GetDataContext()
let cz = wb.Countries.``Czech Republic``.Indicators
let eu = wb.Countries.``European Union``.Indicators

// Use Deedle to get time-series with school enrollment data
let czschool = series cz.``Gross enrolment ratio, tertiary, both sexes (%)``
let euschool = series eu.``Gross enrolment ratio, tertiary, both sexes (%)``

// Eval the following to get HTML output:
czschool
euschool

// Get 5 years with the largest difference between EU and CZ
abs (czschool - euschool)
|> Series.sort
|> Series.rev
|> Series.take 5

// Compare CZ and EU side-by-side in a dataframe
frame [ "CZ" => czschool; "EU" => euschool ]

// Plot a line chart comparing the two data sets
// (Opens a web browser window with the chart)
[ czschool.[1975 .. 2010]; euschool.[1975 .. 2010] ]
|> Chart.Line
|> Chart.WithOptions (Options(legend=Legend(position="bottom")))
|> Chart.WithLabels ["CZ"; "EU"]
|> Chart.WithTitle("School enrollment in EU and CZ")
|> Chart.DarkTheme















(*

//ch.WithHeight(100)
//ch.WithWidth(100)
//let opts = ch.GetType().GetField("options", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)

type Html = { Html : string }
fsi.AddHtmlPrinter(fun (h:Html) -> h.Html)
let html s = { Html = s }

let url = FsLab.Server.server.Value.AddPage """<html><body>
  <button onclick="try { window.parent.postMessage('Hi','*'); document.getElementById('out').innerHTML = 'sent'; } catch(e) { document.getElementById('out').innerHTML = e.toString(); }">Click</button>
  <div id='out'>yo</div>
  </body></html>"""
html (sprintf "<iframe id='if3' src='%s' />" url)

//

{ Html = """
<table>
  <thead>
    <tr id="head"><th>#</th><th>Header 1</th><th>Header 2</th><th>Header 3</th></tr>
  </thead>
  <tbody id="body">
    <tr><th>1</th><td>Hello</td><td>Some</td><td>More</td></tr>
  </tbody>
</table>
""" }



(*
fsi.AddHtmlPrinter(fun (ch:XPlot.GoogleCharts.GoogleChart) ->
  let sz = XPlot.GoogleCharts.Chart.WithSize (600, 300) ch
  """<script type="text/javascript" src="https://www.google.com/jsapi"></script>""" +
  ch.InlineHtml)

fsi.Add

fsi.AddPrinter(fun (n:int) -> sprintf "Number: %d" n)

42
*)
*)
