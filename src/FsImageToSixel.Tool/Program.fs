(*
MIT License

Copyright (c) 2024 Mårten Rånge

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open System
open System.Collections.Generic
open System.IO
open System.Text

open System.CommandLine

// I use the excellent SixLabors ImageSharp for image processing
//  Note this library has a split license, basically for open source it's free,
//  in a commercial setting it costs money. 
//  There are more nuances to this, check the license file.
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Processing.Processors.Quantization

open FSharp.Core.Printf

exception AbortException of (int)

let writeToConsole (cc : ConsoleColor) (prelude : string) (msg : string) =
  let occ = Console.ForegroundColor
  Console.ForegroundColor <- cc
  try
    Console.Write prelude
    Console.Write " - "
    Console.WriteLine msg
  finally
    Console.ForegroundColor <- occ

let good    msg = writeToConsole ConsoleColor.Green    "GOOD" msg
let hili    msg = writeToConsole ConsoleColor.Cyan     "HILI" msg
let info    msg = writeToConsole ConsoleColor.Gray     "INFO" msg
let warn    msg = writeToConsole ConsoleColor.Yellow   "WARN" msg
let fail    msg = writeToConsole ConsoleColor.Red      "FAIL" msg

let goodf  fmt  = kprintf good fmt
let hilif  fmt  = kprintf hili fmt
let infof  fmt  = kprintf info fmt
let warnf  fmt  = kprintf warn fmt
let failf  fmt  = kprintf fail fmt

let abort exitCode msg = 
  fail msg
  raise (AbortException exitCode)

let abortf exitCode fmt  = kprintf (abort exitCode) fmt

let inline isNotNull o = not (isNull o)

type InputImagePath   = 
  | InputImagePath of string*string

  member x.Pretty : string =
    let (InputImagePath (_, fullPath)) = x
    fullPath

type OutputImagePath  = 
  | OutputImagePath of string*string
  | OutputImageToStdOut
  
  member x.Pretty : string =
    match x with
    | OutputImagePath (_, fullPath) -> fullPath
    | OutputImageToStdOut           -> "STDOUT"

let rootCommandHandler 
  (input          : string|null )
  (output         : string|null )
  (maxNoOfColors  : int         )
  (overwriteOutput: bool        )
  (escape         : bool        )
  : unit =
  try
    // The input parameter is required and not expected to be null
    assert (isNotNull input)

    let input = InputImagePath (input, Path.GetFullPath input)
    let output = 
      if isNull output then
        OutputImageToStdOut
      else
        OutputImagePath (output, Path.GetFullPath output)

    hili "FsSixelImage.Tool - Converts image to sixel image"
    infof "  Input image path         : %s" input.Pretty
    infof "  Output sixel image path  : %s" output.Pretty
    infof "  Max no of colors used    : %d" maxNoOfColors
    infof "  Overwrite sixel image    : %A" overwriteOutput
    infof "  Escape sixel image output: %A" escape

    let (InputImagePath (_, fullInputPath)) = input

    if not (File.Exists fullInputPath) then
      abort 90 "Input file doesn't exists"

    if not overwriteOutput then
      match output with
      | OutputImagePath (_, fullPath) -> 
        if File.Exists fullPath then
          abort 91 "Output file already exists, specify overwrite option to overwrite it"
      | OutputImageToStdOut           -> ()

    if maxNoOfColors < 2 then
      abort 92 "Max no of colors must be between 2 and 256"

    if maxNoOfColors > 256 then
      abort 93 "Max no of colors must be between 2 and 256"

    hilif "Loading image: %s" fullInputPath
    use image = Image.Load<Rgba32> fullInputPath

    infof "Image size is: %dx%d" image.Width image.Height

    do
      hilif "Quantizing image to %d colors" maxNoOfColors
      let mutator (ctx : IImageProcessingContext) =
        let options   = QuantizerOptions (MaxColors = maxNoOfColors)
        let quantizer = WuQuantizer options
        ignore <| ctx.Quantize quantizer
      image.Mutate mutator

    let palette = Dictionary ()

    do
      hili "Computing palette"
      let pa = 
        PixelAccessorAction<Rgba32> (
          fun a -> 
            for y = 0 to a.Height - 1 do
              let row = a.GetRowSpan y
              for x = 0 to a.Width - 1 do
                let pix = row.[x]
                if pix.A > 127uy then
                  palette.TryAdd (pix.Rgb, palette.Count) |> ignore
        )
      image.ProcessPixelRows pa
      infof "Found %d palette entries" palette.Count
      if palette.Count > maxNoOfColors then
        abort 94 "The palette contains more than the desired max no of colors"

    do
      hili "Generating Sixel image"

      let palette = 
        palette
        |> Array.ofSeq
        |> Array.map (fun kv -> kv.Value, kv.Key)
        |> Array.sortBy fst

      let sb = StringBuilder ()

      let inline str (s : string) = sb.Append s |> ignore
      let inline ch  (c : char  ) = sb.Append c |> ignore
      let inline strf fmt         = kprintf str fmt 

      let toTop, sixelPrelude, sixelEpilogue =
        if escape then
          @"\x1B[H", @"\x1BPq", @"\x1B\\"
        else
          "\x1B[H", "\x1BPq", "\x1B\\"

      // Move cursor to top
      // str toTop
      // Start the sixel bitmap
      str sixelPrelude

      for i, rgb in palette do
        let inline f (v : byte) = int (round (float v*100./255.))
        strf "#%d;2;%d;%d;%d" i (f rgb.R) (f rgb.G) (f rgb.B)

      let pa = 
        PixelAccessorAction<Rgba32> (
          fun a -> 
            let empty   : int array = Array.zeroCreate a.Width
            let sixels  : int array = Array.zeroCreate a.Width
            for y6 = 0 to a.Height/6-1 do
              let y = y6*6
              for i, rgb in palette do
                Array.Copy (empty, sixels, sixels.Length)
                str ("#" + string i)
                let rem = min (a.Height - y - 1) 5
                for i = 0 to rem do
                  let y = y+i
                  let row = a.GetRowSpan y
                  for x = 0 to a.Width-1 do
                    let pix = row.[x]
                    if pix.A > 127uy && pix.Rgb = rgb then
                      sixels.[x] <- sixels.[x] ||| (1 <<< i)

                for x = 0 to a.Width-1 do
                  let tbw = char (63+(sixels.[x]&&&0x3F))
                  ch tbw
                  if escape && tbw = '\\' then
                    ch tbw
                  
                ch '$'
              ch '-'
          )
      image.ProcessPixelRows pa

      // End the sixel bitmap
      str sixelEpilogue

      let sixelData = sb.ToString ()

      match output with
      | OutputImagePath (_, fullPath) ->
        hilif "Writing sixel image: %s" fullPath
        File.WriteAllText (fullPath, sixelData, Encoding.ASCII)
      | OutputImageToStdOut ->
        Console.Write sixelData
    
    good "We are done"

  with
  | :? AbortException as e ->
    // Already been logged
    // e.Data0
    ()

[<EntryPoint>]
let main 
  (args : string array)
  : int =
  let inputOption = 
    Option<string>(
        aliases         = [|"-i"; "--input"|]
      , description     = "Input image path"
      , IsRequired      = true
      )

  let outputOption = 
    Option<string>(
        aliases         = [|"-o"; "--output"|]
      , description     = "Output sixel image page"
      )

  let escapeOption = 
    Option<bool>(
        aliases         = [|"-e"; "--escape"|]
      , description     = "Escape special characters"
      , getDefaultValue = fun () -> false
      )

  let overwriteOutputOption = 
    Option<bool>(
        aliases         = [|"-oo"; "--overwrite-output"|]
      , description     = "If the output file exists should we overwrite it"
      , getDefaultValue = fun () -> false
      )

  let maxNoOfColorsOption = 
    Option<int>(
        aliases         = [|"-max"; "--max-no-of-colors"|]
      , description     = "Max number of colors used (1-256)"
      , getDefaultValue = fun () -> 127
      )

  let rootCommand = RootCommand "FsSixelImage.Tool - Converts image to sixel image"
  ([|
    inputOption
    outputOption
    maxNoOfColorsOption
    overwriteOutputOption
    escapeOption
    |] : Option array) 
    |> Array.iter rootCommand.AddOption

  rootCommand.SetHandler (
      rootCommandHandler
    , inputOption
    , outputOption
    , maxNoOfColorsOption
    , overwriteOutputOption
    , escapeOption
    )

  rootCommand.Invoke args
