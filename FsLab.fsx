#nowarn "211"
#I "packages/Deedle/lib/net40"
#I "packages/Deedle.1.2.4/lib/net40"
#I "packages/Deedle.RPlugin/lib/net40"
#I "packages/Deedle.RPlugin.1.2.4/lib/net40"
#I "packages/FSharp.Charting/lib/net40"
#I "packages/FSharp.Charting.0.90.13/lib/net40"
#I "packages/FSharp.Data/lib/net40"
#I "packages/FSharp.Data.2.2.5/lib/net40"
#I "packages/MathNet.Numerics/lib/net40"
#I "packages/MathNet.Numerics.3.11.0/lib/net40"
#I "packages/MathNet.Numerics.FSharp/lib/net40"
#I "packages/MathNet.Numerics.FSharp.3.11.0/lib/net40"
#I "packages/DynamicInterop/lib/net40"
#I "packages/DynamicInterop.0.7.4/lib/net40"
#I "packages/R.NET.Community/lib/net40"
#I "packages/R.NET.Community.1.6.5/lib/net40"
#I "packages/R.NET.Community.FSharp/lib/net40"
#I "packages/R.NET.Community.FSharp.1.6.5/lib/net40"
#I "packages/RProvider/lib/net40"
#I "packages/RProvider.1.1.20/lib/net40"
#I "packages/XPlot.Plotly/lib/net45"
#I "packages/XPlot.Plotly.1.3.1/lib/net45"
#I "packages/XPlot.GoogleCharts/lib/net45"
#I "packages/XPlot.GoogleCharts.1.3.1/lib/net45"
#I "packages/XPlot.GoogleCharts.Deedle/lib/net45"
#I "packages/XPlot.GoogleCharts.Deedle.1.3.1/lib/net45"
#I "packages/Google.DataTable.Net.Wrapper/lib"
#I "packages/Google.DataTable.Net.Wrapper.3.1.2.0/lib"
#I "packages/Newtonsoft.Json/lib/net40"
#I "packages/Newtonsoft.Json.8.0.3/lib/net40"
#r "Deedle.dll"
#r "Deedle.RProvider.Plugin.dll"
#r "System.Windows.Forms.DataVisualization.dll"
#r "FSharp.Charting.dll"
#r "FSharp.Data.dll"
#r "MathNet.Numerics.dll"
#r "MathNet.Numerics.FSharp.dll"
#r "DynamicInterop.dll"
#r "RDotNet.dll"
#r "RDotNet.NativeLibrary.dll"
#r "RDotNet.FSharp.dll"
#r "RProvider.Runtime.dll"
#r "RProvider.dll"
#r "XPlot.Plotly.dll"
#r "XPlot.GoogleCharts.dll"
#r "XPlot.GoogleCharts.Deedle.dll"
#r "Google.DataTable.Net.Wrapper.dll"
#r "Newtonsoft.Json.dll"

#r "packages/Suave/lib/net40/Suave.dll"
namespace FsLab

// --------------------------------------------------------------------------------------

open Suave

[<AutoOpen>]
module Server =
  type SimpleServer() =
    let pages = ResizeArray<_>()

    // Server that serves pages from the given array
    let handlePage n =
      if n < pages.Count then pages.[n]
      else RequestErrors.NOT_FOUND "Page not found"
    let app =
      choose [ Filters.pathScan "/%d" handlePage
               Filters.pathScan "/%d/%s" (fst >> handlePage) ]


    /// Start server on the first available port in the range 8000..10000
    /// and return the port number once the server is started (asynchronously)
    let startServer () =
      Async.FromContinuations(fun (cont, _, _) ->
        let startedEvent = Event<_>()
        startedEvent.Publish.Add(cont)
        async {
          // Try random ports until we find one that works
          let rnd = System.Random()
          while true do
            let port = 8000 + rnd.Next(2000)
            let local = Suave.Http.HttpBinding.mkSimple HTTP "127.0.0.1" port
            let logger = Suave.Logging.Loggers.saneDefaultsFor Logging.LogLevel.Error
            let config = { defaultConfig with bindings = [local]; logger = logger }
            let started, start = startWebServerAsync config app
            // If it starts OK, we get TCP binding & report success via event
            async { let! running = started
                    startedEvent.Trigger(running) } |> Async.Start
            // Try starting the server and handle SocketException
            try do! start
            with :? System.Net.Sockets.SocketException -> () }
        |> Async.Start )

    // Start the server and wait for the port as task, while other things happen
    let port =
      async { let! pts = startServer()
              let first = pts |> Seq.choose id |> Seq.head
              return first.binding.port }
      |> Async.StartAsTask

    /// Returns the port where the server is running
    member x.Port = port.Result

    /// Add web part to the server. Returns the URL prefix where it's hosted
    member x.AddPart(part) =
      pages.Add(part)
      sprintf "http://localhost:%d/%d" port.Result (pages.Count - 1)

    /// Add page to the server. Returns the URL where it's hosted
    member x.AddPage(page) =
      pages.Add(Successful.OK(page))
      sprintf "http://localhost:%d/%d" port.Result (pages.Count - 1)

  let server = Lazy.Create(fun () -> SimpleServer())

// --------------------------------------------------------------------------------------

module DeedleHtmlFormatters =
  open Suave
  open Deedle
  open Deedle.Internal
  open FSharp.Data

  type GridJson = JsonProvider<"""{
      "metadata":{"columns":["Foo","Bar"], "rows":100},
      "row":{"key":"Foo","columns":["Foo","Bar"]}
    }""">

  let (|Float|_|) (v:obj) = if v :? float then Some(v :?> float) else None
  let (|Float32|_|) (v:obj) = if v :? float32 then Some(v :?> float32) else None

  /// Format value as a single-literal paragraph
  let formatValue (floatFormat:string) def = function
    | OptionalValue.Present(Float v) -> v.ToString(floatFormat)
    | OptionalValue.Present(Float32 v) -> v.ToString(floatFormat)
    | OptionalValue.Present(v) -> v.ToString()
    | _ -> def

  let floatFormat = "G4"

  let nextGridId =
    let counter = ref 0
    let pid = System.Diagnostics.Process.GetCurrentProcess().Id
    fun () -> incr counter; sprintf "fslab-grid-%d-%d" pid counter.Value

  let registerGrid colKeys rowCount getRow =
    let metadata = GridJson.Metadata(colKeys, rowCount).ToString()
    let app =
      choose [
        Filters.pathScan "/%d/grid" (fun _ ->
            System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/frame.html")
            |> Successful.OK )
        Filters.pathScan "/%d/metadata" (fun _ ->
            Successful.OK (metadata) )
        Filters.pathScan "/%d/rows/%d" (fun (_, row) -> request (fun r ->
            let count = int (Utils.Choice.orDefault "100" (r.queryParam("count")))
            let count = min rowCount (row + count) - row
            let rows =
              Array.init count (fun i ->
                let row = row + i
                let key, cols = getRow row
                GridJson.Row(string key, cols).JsonValue)
            JsonValue.Array(rows).ToString()
            |> Successful.OK ))
        Filters.pathScan "/%d/%s" (fun (n, _) ctx ->
            let url = ctx.request.url.ToString().Replace("/" + string n + "/", "/")
            let ctx =
              { ctx with
                  request = { ctx.request with url = System.Uri(url) }
                  runtime = { ctx.runtime with homeDirectory = __SOURCE_DIRECTORY__ } }
            Files.browseHome ctx)
      ]
    let url = server.Value.AddPart(app)
    System.String.Format("""<iframe src="{0}/grid?{1}"
      style="border:none;width:100%;height:100px;" id='{1}' />""", url, nextGridId())

  type ISeriesOperation<'R> =
    abstract Invoke<'K, 'V when 'K : equality> : Series<'K, 'V> -> 'R

  let (|Series|_|) (value:obj) =
    value.GetType()
    |> Seq.unfold (fun t -> if t = null then None else Some(t, t.BaseType))
    |> Seq.tryFind (fun t -> t.Name = "Series`2")
    |> Option.map (fun t -> t.GetGenericArguments())

  let invokeSeriesOperation tys obj (op:ISeriesOperation<'R>) =
    typeof<ISeriesOperation<'R>>.GetMethod("Invoke")
      .MakeGenericMethod(tys).Invoke(op, [| obj |]) :?> 'R

  let registerFormattable (obj:IFsiFormattable) =
    match obj with
    | Series tys ->
      { new ISeriesOperation<string> with
          member x.Invoke(s) =
            let colKeys = [| "Value" |]
            let rowCount = s.KeyCount
            let getRow index =
              box (s.GetKeyAt(index)),
              [| formatValue floatFormat "N/A" (s.TryGetAt(index)) |]
            registerGrid colKeys rowCount getRow }
      |> invokeSeriesOperation tys obj
    | :? IFrame as f ->
      { new IFrameOperation<string> with
          member x.Invoke(df) =
            let colKeys = df.ColumnKeys |> Seq.map (box >> string) |> Array.ofSeq
            let rowCount = df.RowCount
            let getRow index =
              box (df.GetRowKeyAt(int64 index)),
              df.GetRowAt(index).Vector.DataSequence
                |> Seq.map (formatValue floatFormat "N/A")
                |> Array.ofSeq
            registerGrid colKeys rowCount getRow }
      |> f.Apply
    | _ -> "(Error: Deedle object implements IFsiFormattable, but it's not a frame or series)"

// --------------------------------------------------------------------------------------

#if NO_FSI_ADDPRINTER
#else
module FsiAutoShow =
  open FSharp.Charting
  open RProvider

  fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) ->
    ch.ShowChart() |> ignore; "(Chart)")
  fsi.AddPrinter(fun (synexpr:RDotNet.SymbolicExpression) ->
    synexpr.Print())

#if HAS_FSI_ADDHTMLPRINTER
  fsi.AddHtmlPrinter(fun (obj:Deedle.Internal.IFsiFormattable) ->
    DeedleHtmlFormatters.registerFormattable obj)

  let createIFrame height body =
    let url = server.Value.AddPage(body)
    System.String.Format("""<iframe src="{0}"
      style="border:none;width:100%;height:{1}px;" />""", url, height)

  fsi.AddPrinter(fun (chart:XPlot.GoogleCharts.GoogleChart) ->
    "(Google Chart)")
  fsi.AddHtmlPrinter(fun (chart:XPlot.GoogleCharts.GoogleChart) ->
    let ch = chart |> XPlot.GoogleCharts.Chart.WithSize (800, 600)
    ch.Html |> createIFrame 600)


#else
  fsi.AddPrinter(fun (printer:Deedle.Internal.IFsiFormattable) ->
    "\n" + (printer.Format()))

  let displayHtml html =
    let url = server.Value.AddPage(html)
    System.Diagnostics.Process.Start(url) |> ignore

  fsi.AddPrinter(fun (chart:XPlot.GoogleCharts.GoogleChart) ->
    let ch = chart |> XPlot.GoogleCharts.Chart.WithSize (800, 600)
    ch.Html |> displayHtml
    "(Google Chart)")

  fsi.AddPrinter(fun (chart:XPlot.Plotly.PlotlyChart) ->
    """<!DOCTYPE html>
    <html>
    <head>
        <title>Plotly Chart</title>
        <script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
    </head>
    <body>""" + chart.GetInlineHtml() + "</body></html>" |> displayHtml
    "(Plotly Chart)" )
#endif
#endif

namespace FSharp.Charting
open FSharp.Charting
open Deedle

[<AutoOpen>]
module FsLabExtensions =
  type FSharp.Charting.Chart with
    static member Line(data:Series<'K, 'V>, ?Name, ?Title, ?Labels, ?Color, ?XTitle, ?YTitle) =
      Chart.Line(Series.observations data, ?Name=Name, ?Title=Title, ?Labels=Labels, ?Color=Color, ?XTitle=XTitle, ?YTitle=YTitle)
    static member Column(data:Series<'K, 'V>, ?Name, ?Title, ?Labels, ?Color, ?XTitle, ?YTitle) =
      Chart.Column(Series.observations data, ?Name=Name, ?Title=Title, ?Labels=Labels, ?Color=Color, ?XTitle=XTitle, ?YTitle=YTitle)
    static member Pie(data:Series<'K, 'V>, ?Name, ?Title, ?Labels, ?Color, ?XTitle, ?YTitle) =
      Chart.Pie(Series.observations data, ?Name=Name, ?Title=Title, ?Labels=Labels, ?Color=Color, ?XTitle=XTitle, ?YTitle=YTitle)
    static member Area(data:Series<'K, 'V>, ?Name, ?Title, ?Labels, ?Color, ?XTitle, ?YTitle) =
      Chart.Area(Series.observations data, ?Name=Name, ?Title=Title, ?Labels=Labels, ?Color=Color, ?XTitle=XTitle, ?YTitle=YTitle)
    static member Bar(data:Series<'K, 'V>, ?Name, ?Title, ?Labels, ?Color, ?XTitle, ?YTitle) =
      Chart.Bar(Series.observations data, ?Name=Name, ?Title=Title, ?Labels=Labels, ?Color=Color, ?XTitle=XTitle, ?YTitle=YTitle)

open Deedle

namespace MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra
open Deedle

module Matrix =
  let inline toFrame matrix = matrix |> Matrix.toArray2 |> Frame.ofArray2D
module DenseMatrix =
  let inline ofFrame frame = frame |> Frame.toArray2D |> DenseMatrix.ofArray2
module SparseMatrix =
  let inline ofFrame frame = frame |> Frame.toArray2D |> SparseMatrix.ofArray2
module Vector =
  let inline toSeries vector = vector |> Vector.toSeq |> Series.ofValues
module DenseVector =
  let inline ofSeries series = series |> Series.values |> Seq.map (float) |> DenseVector.ofSeq
module SparseVector =
  let inline ofSeries series = series |> Series.values |> Seq.map (float) |> SparseVector.ofSeq


namespace Deedle
open Deedle
open MathNet.Numerics.LinearAlgebra

module Frame =
  let inline ofMatrix matrix = matrix |> Matrix.toArray2 |> Frame.ofArray2D
  let inline toMatrix frame = frame |> Frame.toArray2D |> DenseMatrix.ofArray2

  let ofCsvRows (data:FSharp.Data.Runtime.CsvFile<'T>) =
    match data.Headers with
    | None -> Frame.ofRecords data.Rows
    | Some names -> Frame.ofRecords data.Rows |> Frame.indexColsWith names

module Series =
  let inline ofVector vector = vector |> Vector.toSeq |> Series.ofValues
  let inline toVector series = series |> Series.values |> Seq.map (float) |> DenseVector.ofSeq
