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
open System.Globalization
open System.IO
open System.Text
open System.Threading

open FSharp.Core.Printf

(*
# Dependencies
1. **SixLabors.ImageSharp** – A powerful image processing library by SixLabors.
   ImageSharp is split-licensed under the Apache License 2.0 and a commercial
   license. For this open-source project, it qualifies for usage under the
   Apache 2.0 license, as it meets the criteria for open-source software use.
2. **System.CommandLine** – A command-line parser library from Microsoft,
   licensed under the MIT License.
*)
open System.CommandLine

// I use the excellent SixLabors ImageSharp for image processing
//  Note this library has a split license, basically for open source it's free,
//  in a commercial setting it costs money.
//  There are more nuances to this, check the license file.
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Processing.Processors.Quantization

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

// Found out the right way in System.CommandLine to send exit code from
//  a command handler
let mutable exitCode = 80

module Loops =
  let inline RLEToken escape (sb : StringBuilder) rep current =
    let tbw = char (63+(current&&&0x3F))
    if rep > 3 then
      sb.Append '!' |> ignore
      sb.Append rep |> ignore
      sb.Append tbw |> ignore
      if escape && tbw = '\\' then
        sb.Append tbw |> ignore
    else
      for i = 0 to rep-1 do
        sb.Append tbw |> ignore
        if escape && tbw = '\\' then
          sb.Append tbw |> ignore

  let rec RLE escape (sixels : int array) (sb : StringBuilder) rep current i =
    if i < sixels.Length then
      let next = sixels.[i]
      if next = current then
        RLE escape sixels sb (rep + 1) current (i + 1)
      else
        RLEToken escape sb rep current
        RLE escape sixels sb 1 next (i + 1)
    else
      RLEToken escape sb rep current


let fillPixels (pixels : Rgba32 array) (frame : Rgba32 ImageFrame) width height : unit =
  let pa =
    PixelAccessorAction<Rgba32> (
      fun a ->
        if a.Width <> width then
          abortf 194 "Frame width %d don't match image width %d" a.Width width
        if a.Height <> height then
          abortf 195 "Frame height %d don't match image height %d" a.Height height
        for y = 0 to height - 1 do
          let yoff = width*y
          let row = a.GetRowSpan y
          for x = 0 to width - 1 do
            pixels.[yoff+x] <- row.[x]

    )
  frame.ProcessPixelRows pa

let rootCommandHandler
  (input          : string|null )
  (output         : string|null )
  (maxNoOfColors  : int         )
  (scaleImage     : int         )
  (imageRatio     : int         )
  (overwriteOutput: bool        )
  (escape         : bool        )
  (whatIf         : bool        )
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

    hili  "fsimg2sixel - Converts image to sixel image"
    infof "  Input image path         : %s"   input.Pretty
    infof "  Output sixel image path  : %s"   output.Pretty
    infof "  Max no of colors used    : %d"   maxNoOfColors
    infof "  Scale image by (%%)       : %d"  scaleImage
    infof "  Desired image ratio (%%)  : %d"  imageRatio
    infof "  Max no of colors used    : %d"   maxNoOfColors
    infof "  Overwrite sixel image    : %A"   overwriteOutput
    infof "  Escape sixel image output: %A"   escape
    infof "  Skip writing sixel image : %A"   whatIf

    let (InputImagePath (_, fullInputPath)) = input

    if maxNoOfColors < 2 then
      abort 90 "Max no of colors must be between 2 and 256"

    if maxNoOfColors > 256 then
      abort 91 "Max no of colors must be between 2 and 256"

    if scaleImage < 1 then
      abort 92 "Scale image must be between 1 and 1000 (%)"

    if scaleImage > 1000 then
      abort 93 "Scale image must be between 1 and 1000 (%)"

    if imageRatio < 1 then
      abort 94 "Scale image must be between 1 and 1000 (%)"

    if imageRatio > 1000 then
      abort 95 "Scale image must be between 1 and 1000 (%)"

    if not (File.Exists fullInputPath) then
      abort 96 "Input file doesn't exists"

    if not overwriteOutput then
      match output with
      | OutputImagePath (_, fullPath) ->
        if File.Exists fullPath then
          abort 97 "Output file already exists, use -oo to overwrite it"
      | OutputImageToStdOut           ->
        // Detect if we are running a sixel capable device

        // Remove any potential input on the STDIN
        while Console.KeyAvailable do
          Console.ReadKey() |> ignore

        // Ask the terminal what capabilities it has
        Console.Write "\x1B[c"

        // Wait for awhile to let the terminal respond
        Thread.Sleep 100
        let sb = StringBuilder ()

        // Read all input available, non-blocking
        while Console.KeyAvailable do
          let key = Console.ReadKey ()
          sb.Append key.KeyChar |> ignore

        let response  = sb.ToString ()
        // Quick-n-dirty way to detect sixel capability
        let split     = response.Split ';'

        // 4 is the sixel capability
        if split |> Array.contains "4" |> not then
          abort 98 "Your terminal doesn't seem to support sixel output, you can still write the image to a file using -o"

    hilif "Loading image: %s" fullInputPath
    use image = Image.Load<Rgba32> fullInputPath

    infof "Image size is: %dx%d" image.Width image.Height

    let desiredWidth  = int (round (float image.Width )*(float scaleImage/100.)*(float imageRatio/100.))
    let desiredHeight = int (round (float image.Height)*(float scaleImage/100.))

    infof "Desired image size is: %dx%d" desiredWidth desiredHeight

    do
      hilif "Resizing image from %dx%d to: %dx%d" image.Width image.Height desiredWidth desiredHeight
      let mutator (ctx : IImageProcessingContext) =
        let options = ResizeOptions (
            Mode    = ResizeMode.Stretch
          , Sampler = KnownResamplers.Hermite
          , Size    = Size(int desiredWidth, int desiredHeight)
          )
        ignore <| ctx.Resize options
      image.Mutate mutator

    do
      hilif "Quantizing image to %d colors" maxNoOfColors
      let mutator (ctx : IImageProcessingContext) =
        let options   = QuantizerOptions (MaxColors = maxNoOfColors)
        let quantizer = WuQuantizer options
        ignore <| ctx.Quantize quantizer
      image.Mutate mutator

    let frameCount = image.Frames.Count

    infof "Found %d frames" frameCount

    if whatIf then
      warn "Skipping writing of Sixel image"
    else
      match output with
      | OutputImagePath (_, fullPath) ->
        hilif "Deleting existing sixel image: %s" fullPath
        File.Delete fullPath
      | OutputImageToStdOut ->
        ()

      let palette = Dictionary ()
      let sb      = StringBuilder ()

      let inline str (s : string) = sb.Append s |> ignore
      let inline ch  (c : char  ) = sb.Append c |> ignore
      let inline num (i : int   ) = sb.Append i |> ignore
      let strf fmt                = kprintf str fmt

      let width   = image.Width
      let height  = image.Height

      let empty   : int array     = Array.zeroCreate width
      let sixels  : int array     = Array.zeroCreate width
      let pixels  : Rgba32 array  = Array.zeroCreate (width*height)

      for frameNo = 0 to frameCount-1 do
        hilif "Processing frame #%d" (frameNo + 1)

        let frame = image.Frames.[frameNo]

        sb.Clear () |> ignore

        palette.Clear ()

        fillPixels pixels frame width height

        do
          info "Computing palette"
          for i = 0 to pixels.Length - 1 do
            let pix = pixels.[i]
            if pix.A > 127uy then
              palette.TryAdd (pix.Rgb, palette.Count) |> ignore

          infof "Found %d palette entries" palette.Count
          if palette.Count > maxNoOfColors then
            abort 99 "The palette contains more than the desired max no of colors"

        let palette =
          palette
          |> Array.ofSeq
          |> Array.map (fun kv -> kv.Value, kv.Key)
          |> Array.sortBy fst

        // Sixels explained here: https://en.wikipedia.org/wiki/Sixel
        hili "Generating Sixel image"

        let toTop, sixelPrelude, sixelEpilogue =
          if escape then
            @"\x1B[H", @"\x1BP7;1;q", @"\x1B\\"
          else
            "\x1B[H", "\x1BP7;1;q", "\x1B\\"

        // Move cursor to top if it's a multi frame image
        if frameCount > 1 then
          str toTop
        // Start the sixel bitmap
        str sixelPrelude

        for i, rgb in palette do
          let inline f (v : byte) = int (round (float v*100./255.))
          strf "#%d;2;%d;%d;%d" i (f rgb.R) (f rgb.G) (f rgb.B)

        let h6 = height/6
        for y6 = 0 to h6-1 do
          let y = y6*6
          for i, rgb in palette do
            let mutable ones = 0
            Array.Copy (empty, sixels, sixels.Length)
            let rem = min (height - y - 1) 5
            for i = 0 to rem do
              let bit   = 1 <<< i
              let y     = y+i
              let yoff  = width*y
              for x = 0 to width-1 do
                let pix = pixels.[yoff+x]
                if pix.A > 127uy && rgb.R = pix.R && rgb.G = pix.G && rgb.B = pix.B then
                  ones <- ones + 1
                  sixels.[x] <- sixels.[x] ||| bit

            if ones > 0 then
              ch '#'
              num i
              Loops.RLE escape sixels sb 1 sixels.[0] 1

              // Overwrite current line with next color
              ch '$'


          // We are done with this line, goto next
          ch '-'

        // End the sixel bitmap
        str sixelEpilogue

        let sixelData = sb.ToString ()

        match output with
        | OutputImagePath (_, fullPath) ->
          hilif "Writing sixel image: %s" fullPath
          File.AppendAllText(fullPath, sixelData)
        | OutputImageToStdOut ->
          Console.Write sixelData

    good "We are done"

    exitCode <- 0

  with
  | :? AbortException as e ->
    exitCode <- e.Data0

[<EntryPoint>]
let main
  (args : string array)
  : int =
  let invariant = CultureInfo.InvariantCulture
  CultureInfo.CurrentCulture                <- invariant
  CultureInfo.CurrentUICulture              <- invariant
  CultureInfo.DefaultThreadCurrentCulture   <- invariant
  CultureInfo.DefaultThreadCurrentUICulture <- invariant

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

  let maxNoOfColorsOption =
    Option<int>(
        aliases         = [|"-max"; "--max-no-of-colors"|]
      , description     = "Max number of colors used (1-256)"
      , getDefaultValue = fun () -> 256
      )

  let scaleImageOption =
    Option<int>(
        aliases         = [|"-s"; "--scale-image"|]
      , description     = "Scale image in %"
      , getDefaultValue = fun () -> 50
      )

  let imageRatioOption =
    Option<int>(
        aliases         = [|"-r"; "--image-ratio"|]
      , description     = "The ratio applied to the image in %. Default is 200 which scales width to 2x of height. Compensates for fonts being not square."
      , getDefaultValue = fun () -> 100
      )

  let overwriteOutputOption =
    Option<bool>(
        aliases         = [|"-oo"; "--overwrite-output"|]
      , description     = "If the output file exists should we overwrite it"
      , getDefaultValue = fun () -> false
      )

  let escapeOption =
    Option<bool>(
        aliases         = [|"-e"; "--escape"|]
      , description     = "Escape special characters"
      , getDefaultValue = fun () -> false
      )

  let whatIfOption =
    Option<bool>(
        aliases         = [|"-wi"; "--what-if"|]
      , description     = "Reads the image and compute size but don't generate the sixel image"
      , getDefaultValue = fun () -> false
      )

  let rootCommand = RootCommand "fsimg2sixel - Converts image to sixel image"

  ([|
      inputOption
      outputOption
      maxNoOfColorsOption
      scaleImageOption
      imageRatioOption
      overwriteOutputOption
      escapeOption
      whatIfOption
    |] : Option array)
    |> Array.iter rootCommand.AddOption

  rootCommand.SetHandler (
      rootCommandHandler
    , inputOption
    , outputOption
    , maxNoOfColorsOption
    , scaleImageOption
    , imageRatioOption
    , overwriteOutputOption
    , escapeOption
    , whatIfOption
    )

  rootCommand.Invoke args |> ignore

  exitCode
