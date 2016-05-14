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











/// The type can be used to configure formatting of frames, series and
/// matrices in FsLab journals. Use it if you want to override the default
/// numbers of columns and rows that are printed.
type FormatConfig =
  { /// How many columns of a frame should be rendered at the start
    StartColumnCount : int
    /// How many columns of a frame should be rendered at the end
    EndColumnCount : int
    /// How many rows of a frame should be rendered at the start
    StartRowCount : int
    /// How many rows of a frame should be rendered at the end
    EndRowCount : int

    /// How many items from a series should be rendered at the start
    StartItemCount : int
    /// How many items from a series should be rendered at the end
    EndItemCount : int

    // How many columns from a matrix should be rendered at the start
    MatrixStartColumnCount : int
    // How many columns from a matrix should be rendered at the end
    MatrixEndColumnCount : int
    // How many rows from a matrix should be rendered at the start
    MatrixStartRowCount : int
    // How many rows from a matrix should be rendered at the end
    MatrixEndRowCount : int

    // How many items from a vector should be rendered at the start
    VectorStartItemCount : int
    // How many items from a vector should be rendered at the end
    VectorEndItemCount : int }
    static member Create () =
      { FormatConfig.StartColumnCount = 3
        EndColumnCount = 3
        StartRowCount = 8
        EndRowCount = 4

        StartItemCount = 5
        EndItemCount = 3

        MatrixStartColumnCount = 7
        MatrixEndColumnCount = 2
        MatrixStartRowCount = 10
        MatrixEndRowCount = 4

        VectorStartItemCount = 7
        VectorEndItemCount = 2 }

    /// Transform the context using the specified function
    member x.With(f:FormatConfig -> FormatConfig) = f x

    // Tuples with the counts, for easy use later on
    member internal x.fcols = x.StartColumnCount, x.EndColumnCount
    member internal x.frows = x.StartRowCount, x.EndRowCount
    member internal x.sitms = x.StartItemCount, x.EndItemCount
    member internal x.mcols = x.MatrixStartColumnCount, x.MatrixEndColumnCount
    member internal x.mrows = x.MatrixStartRowCount, x.MatrixEndRowCount
    member internal x.vitms = x.VectorStartItemCount, x.VectorEndItemCount


open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra


let (|Float|_|) (v:obj) = if v :? float then Some(v :?> float) else None
let (|Float32|_|) (v:obj) = if v :? float32 then Some(v :?> float32) else None

let inline (|PositiveInfinity|_|) (v: ^T) =
  if (^T : (static member IsPositiveInfinity: 'T -> bool) (v)) then Some PositiveInfinity else None
let inline (|NegativeInfinity|_|) (v: ^T) =
  if (^T : (static member IsNegativeInfinity: 'T -> bool) (v)) then Some NegativeInfinity else None
let inline (|NaN|_|) (v: ^T) =
  if (^T : (static member IsNaN: 'T -> bool) (v)) then Some NaN else None

/// Use 'f' to transform all values, then call 'g' with Some for
/// values to show and None for "..." in the middle
let mapSteps (startCount, endCount) f g input =
  input
  |> Seq.map f |> Deedle.Internal.Seq.startAndEnd startCount endCount
  |> Seq.map (function Choice1Of3 v | Choice3Of3 v -> g (Some v) | _ -> g None)
  |> List.ofSeq

let inline formatMathValue (floatFormat:string) = function
  | PositiveInfinity -> "\\infty"
  | NegativeInfinity -> "-\\infty"
  | NaN -> "\\times"
  | Float v -> v.ToString(floatFormat)
  | Float32 v -> v.ToString(floatFormat)
  | v -> v.ToString()

let formatMatrix config (formatValue: 'T -> string) (matrix: Matrix<'T>) =
  let mappedColumnCount = min (config.MatrixStartColumnCount + config.MatrixEndColumnCount + 1) matrix.ColumnCount
  String.concat System.Environment.NewLine
    [ "\\begin{bmatrix}"
      matrix.EnumerateRows()
        |> mapSteps config.mrows id (function
          | Some row -> row |> mapSteps config.mcols id (function Some v -> formatValue v | _ -> "\\cdots") |> String.concat " & "
          | None -> Array.zeroCreate matrix.ColumnCount |> mapSteps config.mcols id (function Some v -> "\\vdots" | _ -> "\\ddots") |> String.concat " & ")
        |> String.concat ("\\\\ " + System.Environment.NewLine)
      "\\end{bmatrix}" ]

let formatVector (config:FormatConfig) (formatValue: 'T -> string) (vector: Vector<'T>) =
  String.concat System.Environment.NewLine
    [ "\\begin{bmatrix}"
      vector.Enumerate()
        |> mapSteps config.vitms id (function | Some v -> formatValue v | _ -> "\\cdots")
        |> String.concat " & "
      "\\end{bmatrix}" ]

let inline formatMathValue (floatFormat:string) = function
  | PositiveInfinity -> "\\infty"
  | NegativeInfinity -> "-\\infty"
  | NaN -> "\\times"
  | Float v -> v.ToString(floatFormat)
  | Float32 v -> v.ToString(floatFormat)
  | v -> v.ToString()


type Html = { Html : string }
fsi.AddHtmlPrinter(fun (h:Html) -> h.Html)
let html s = { Html = s }

open Suave
open Suave.Filters
open Suave.Operators

let host math =
  let app =
    choose [
      Filters.pathScan "/%d/math" (fun _ ->
        Successful.OK("""
  <!DOCTYPE html>
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <meta http-equiv="X-UA-Compatible" content="IE=edge" />
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <script type="text/x-mathjax-config">
    MathJax.Hub.Config({
      tex2jax: {inlineMath: [["$","$"],["\\(","\\)"]]}
    });
  </script>
  <script type="text/javascript" src="MathJax.js?config=TeX-AMS_HTML-full"></script>
  </head>
  <body style="color:#e0e0e0">
  $$[MATH]$$
  </body>
  </html>                  """.Replace("[MATH]", math)))
      Filters.pathScan "/%d/%s" (fun (n, _) ctx ->
          let url = ctx.request.url.ToString().Replace("/" + string n + "/", "/")
          let ctx =
            { ctx with
                request = { ctx.request with url = System.Uri(url) }
                runtime = { ctx.runtime with homeDirectory = __SOURCE_DIRECTORY__ + "\\mathjax" } }
          Files.browseHome ctx) ]

  let url = FsLab.Server.server.Value.AddPart(app)
  System.String.Format("""<iframe src="{0}/math"
    style="border:none;width:100%;height:500px;" />""", url)


fsi.AddHtmlPrinter(fun (m:Matrix<float>) ->
  m
  |> formatMatrix (FormatConfig.Create()) (formatMathValue "G4")
  |> host)


matrix [ for j in 0.0 .. 10.0 ->[for i in 0.0 .. 10.0 -> sin i*j] ]


//|> html


(*

//ch.WithHeight(100)
//ch.WithWidth(100)
//let opts = ch.GetType().GetField("options", System.Reflection.BindingFlags.NonPublic ||| System.Reflection.BindingFlags.Instance)


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
