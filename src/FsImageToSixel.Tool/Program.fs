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
open System.CommandLine.Invocation

// I use the excellent SixLabors ImageSharp for image processing
//  Note this library has a split license, basically for open source it's free,
//  in a commercial setting it costs money.
//  There are more nuances to this, check the license file.
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open SixLabors.ImageSharp.Processing
open SixLabors.ImageSharp.Processing.Processors.Quantization

exception AbortException of (int)

let writeToConsole silent (cc : ConsoleColor) (prelude : string) (msg : string) =
  if not silent then
    let occ = Console.ForegroundColor
    Console.ForegroundColor <- cc
    try
      Console.Write prelude
      Console.Write " - "
      Console.WriteLine msg
    finally
      Console.ForegroundColor <- occ

let good silent msg   = writeToConsole silent ConsoleColor.Green    "GOOD" msg
let hili silent msg   = writeToConsole silent ConsoleColor.Cyan     "HILI" msg
let info silent msg   = writeToConsole silent ConsoleColor.Gray     "INFO" msg
let warn silent msg   = writeToConsole silent ConsoleColor.Yellow   "WARN" msg
let fail silent msg   = writeToConsole silent ConsoleColor.Red      "FAIL" msg

let goodf silent fmt  = kprintf (good silent) fmt
let hilif silent fmt  = kprintf (hili silent) fmt
let infof silent fmt  = kprintf (info silent) fmt
let warnf silent fmt  = kprintf (warn silent) fmt
let failf silent fmt  = kprintf (fail silent) fmt

let abort exitCode msg =
  fail false msg
  raise (AbortException exitCode)

let abortf exitCode fmt  = kprintf (abort exitCode) fmt

let inline isNotNull o = not (isNull o)

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
    , description     = "The ratio applied to the image in %."
    , getDefaultValue = fun () -> 100
    )

let skipFramesOption =
  Option<int>(
      aliases         = [|"-sf"; "--skip-frames"|]
    , description     = "Skip # frames in a multi frame image"
    , getDefaultValue = fun () -> 0
    )

let takeFramesOption =
  Option<int>(
      aliases         = [|"-tf"; "--take-frames"|]
    , description     = "Takes at least # frames in a multi frame image"
    , getDefaultValue = fun () -> 100
    )

let transparencyCutoffOption =
  Option<byte>(
      aliases         = [|"-tc"; "--transparency-cutoff"|]
    , description     = "The transparency cutoff level (0-255)"
    , getDefaultValue = fun () -> 127uy
    )

let ditherScaleOption =
  Option<float32>(
      aliases         = [|"-ds"; "--dither-scale"|]
    , description     = "The dither scale, controls the amount of dithering when reducing number of colors (0.0-1.0)"
    , getDefaultValue = fun () -> QuantizerConstants.MaxDitherScale
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

let enableIframeOption =
  Option<bool>(
      aliases         = [|"-ei"; "--enable-iframe"|]
    , description     = "Enable 'iframe' like compression (EXPERIMENTAL)"
    , getDefaultValue = fun () -> false
    )

let whatIfOption =
  Option<bool>(
      aliases         = [|"-wi"; "--what-if"|]
    , description     = "Reads the image and compute size but don't generate the sixel image"
    , getDefaultValue = fun () -> false
    )

let silentOption      =
  Option<bool>(
      aliases         = [|"-z"; "--silent"|]
    , description     = "Suppresses non-error messages"
    , getDefaultValue = fun () -> false
    )


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
      if (current&&&0x3F) <> 0 then
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

let printFile name =
  let baseDir  = AppDomain.CurrentDomain.BaseDirectory
  let fileName = Path.GetFullPath (Path.Combine (baseDir, name))
  let text     = File.ReadAllText fileName
  Console.WriteLine text

let noticeCommandHandler
  (ctx            : InvocationContext )
  : unit =

  printFile "NOTICE"

  ctx.ExitCode <- 0

let readmeCommandHandler
  (ctx            : InvocationContext )
  : unit =

  printFile "README.md"

  ctx.ExitCode <- 0

let licenseCommandHandler
  (ctx            : InvocationContext )
  : unit =

  printFile "LICENSE"

  ctx.ExitCode <- 0

let rootCommandHandler
  (ctx            : InvocationContext )
  : unit =
  let inline getValue option = ctx.ParseResult.GetValueForOption option

  let input           = getValue inputOption
  let output          = getValue outputOption
  let maxNoOfColors   = getValue maxNoOfColorsOption
  let scaleImage      = getValue scaleImageOption
  let imageRatio      = getValue imageRatioOption
  let skipFrames      = getValue skipFramesOption
  let takeFrames      = getValue takeFramesOption
  let cutoff          = getValue transparencyCutoffOption
  let ditherScale     = getValue ditherScaleOption
  let overwriteOutput = getValue overwriteOutputOption
  let escape          = getValue escapeOption
  let enableIframe    = getValue enableIframeOption
  let whatIf          = getValue whatIfOption
  let silent          = getValue silentOption

  let exitCode =
    try
      // The input parameter is required and not expected to be null
      assert (isNotNull input)

      let input = InputImagePath (input, Path.GetFullPath input)
      let output =
        if isNull output then
          OutputImageToStdOut
        else
          OutputImagePath (output, Path.GetFullPath output)

      hili  silent "fsimg2sixel - Converts image to sixel image"
      infof silent "  Input image path         : %s"   input.Pretty
      infof silent "  Output sixel image path  : %s"   output.Pretty
      infof silent "  Max no of colors used    : %d"   maxNoOfColors
      infof silent "  Scale image by (%%)       : %d"  scaleImage
      infof silent "  Desired image ratio (%%)  : %d"  imageRatio
      infof silent "  Max no of colors used    : %d"   maxNoOfColors
      infof silent "  Skip # frames            : %d"   skipFrames
      infof silent "  Take at least # frames   : %d"   takeFrames
      infof silent "  Transparency cutoff      : %d"   cutoff
      infof silent "  Dither scale             : %f"   ditherScale
      infof silent "  Overwrite sixel image    : %A"   overwriteOutput
      infof silent "  Escape sixel image output: %A"   escape
      infof silent "  'iframe' compression     : %A"   enableIframe
      infof silent "  Skip writing sixel image : %A"   whatIf

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
        abort 94 "Image ratio must be between 1 and 1000 (%)"

      if imageRatio > 1000 then
        abort 95 "Image ratio must be between 1 and 1000 (%)"

      if not (File.Exists fullInputPath) then
        abort 96 "Input file doesn't exists"

      if skipFrames < 0 then
        abort 97 "Skip frames must be at least 0"

      if takeFrames < 0 then
        abort 98 "Take frames must be at least 0"

      if not overwriteOutput then
        match output with
        | OutputImagePath (_, fullPath) ->
          if File.Exists fullPath then
            abort 197 "Output file already exists, use -oo to overwrite it"
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
            abort 198 "Your terminal doesn't seem to support sixel output, you can still write the image to a file using -o"

      hilif silent "Loading image: %s" fullInputPath
      use image = Image.Load<Rgba32> fullInputPath

      infof silent "Image size is: %dx%d" image.Width image.Height

      let desiredWidth  = int (round (float image.Width )*(float scaleImage/100.)*(float imageRatio/100.))
      let desiredHeight = int (round (float image.Height)*(float scaleImage/100.))

      infof silent "Desired image size is: %dx%d" desiredWidth desiredHeight

      if desiredWidth <> image.Width && desiredHeight <> image.Height then
        hilif silent "Resizing image from %dx%d to: %dx%d" image.Width image.Height desiredWidth desiredHeight
        let mutator (ctx : IImageProcessingContext) =
          let options = ResizeOptions (
              Mode    = ResizeMode.Stretch
            , Sampler = KnownResamplers.Hermite
            , Size    = Size(int desiredWidth, int desiredHeight)
            )
          ignore <| ctx.Resize options
        image.Mutate mutator
      else
        hilif silent "Skipping resize of image"

      do
        hilif silent "Quantizing image to %d colors" maxNoOfColors
        let mutator (ctx : IImageProcessingContext) =
          let options   = QuantizerOptions (
              MaxColors   = maxNoOfColors
            , DitherScale = ditherScale
            )
          let quantizer = WuQuantizer options
          ignore <| ctx.Quantize quantizer
        image.Mutate mutator

      let frameCount          = image.Frames.Count
      let stop                = min frameCount (skipFrames+takeFrames)
      let effectiveFrameCount = stop - skipFrames

      infof silent "Found %d frames"   frameCount
      infof silent "Writing %d frames" effectiveFrameCount

      if whatIf then
        warn silent "Skipping writing of Sixel image"
      else
        match output with
        | OutputImagePath (_, fullPath) ->
          // We append to the text file, so we need to clear any existing ones
          File.WriteAllText (fullPath, "", Encoding.ASCII)
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

        let empty         : int array     = Array.zeroCreate width
        let sixels        : int array     = Array.zeroCreate width
        let mutable back  : Rgba32 array  = Array.zeroCreate (if enableIframe then width*height else 1)
        let mutable front : Rgba32 array  = Array.zeroCreate (width*height)

        for frameNo = skipFrames to stop-1 do
          hilif silent "Processing frame #%d" (frameNo + 1)

          let frame = image.Frames.[frameNo]

          sb.Clear () |> ignore

          palette.Clear ()

          fillPixels front frame width height

          do
            info silent "Computing palette"
            for i = 0 to front.Length - 1 do
              let pix = front.[i]
              if pix.A > cutoff then
                palette.TryAdd (pix.Rgb, palette.Count) |> ignore

            infof silent "Found %d palette entries" palette.Count
            if palette.Count > maxNoOfColors then
              abort 199 "The palette contains more than the desired max no of colors"

          let palette =
            palette
            |> Array.ofSeq
            |> Array.map (fun kv -> kv.Value, kv.Key)
            |> Array.sortBy fst

          // Sixels explained here: https://en.wikipedia.org/wiki/Sixel
          hili silent "Generating Sixel image"

          let toTop, sixelPrelude, sixelEpilogue =
            if escape then
              @"\x1B[H", @"\x1BP7;1;q", @"\x1B\\"
            else
              "\x1B[H", "\x1BP7;1;q", "\x1B\\"

          // Move cursor to top if it's a multi frame image
          if effectiveFrameCount > 1 then
            str toTop
          // Start the sixel bitmap
          str sixelPrelude

          for i, rgb in palette do
            let inline f (v : byte) = int (round (float v*100./255.))
            strf "#%d;2;%d;%d;%d" i (f rgb.R) (f rgb.G) (f rgb.B)

          let h6 = height/6
          for y6 = 0 to h6-1 do
            let y = y6*6
            let rem = min (height - y - 1) 5
            for i, rgb in palette do
              let mutable ones = 0
              Array.Copy (empty, sixels, sixels.Length)
              for i = 0 to rem do
                let bit   = 1 <<< i
                let y     = y+i
                let yoff  = width*y
                for x = 0 to width-1 do
                  let fpix = front.[yoff+x]
                  if rgb.R = fpix.R && rgb.G = fpix.G && rgb.B = fpix.B then
                    if fpix.A > cutoff then
                      if enableIframe then
                        let bpix = back.[yoff+x]
                        if bpix.A > cutoff && (bpix.R = fpix.R && bpix.G = fpix.G && bpix.B = fpix.B) then
                          // Same as previous frame. Don't generate sixel
                          ()
                        else
                          ones <- ones + 1
                          sixels.[x] <- sixels.[x] ||| bit
                      else
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
            hilif silent "Writing sixel image: %s" fullPath
            File.AppendAllText(fullPath, sixelData)
          | OutputImageToStdOut ->
            Console.Write sixelData

          if enableIframe then
            for i = 0 to front.Length - 1 do
              let fpix = front.[i]
              if fpix.A <= cutoff then
                // The front pixel is transparent. Copy back into front
                front.[i] <- back.[i]

            // Swap front and back
            let tmp = front
            front <- back
            back  <- tmp
      good silent "We are done"

      0
    with
    | :? AbortException as e ->
      e.Data0

  ctx.ExitCode <- exitCode

  ()

[<EntryPoint>]
let main
  (args : string array)
  : int =
  let invariant = CultureInfo.InvariantCulture
  CultureInfo.CurrentCulture                <- invariant
  CultureInfo.CurrentUICulture              <- invariant
  CultureInfo.DefaultThreadCurrentCulture   <- invariant
  CultureInfo.DefaultThreadCurrentUICulture <- invariant

  let rootCommand = RootCommand "fsimg2sixel - Converts image to sixel image"

  ([|
      inputOption
      outputOption
      maxNoOfColorsOption
      scaleImageOption
      imageRatioOption
      skipFramesOption
      takeFramesOption
      transparencyCutoffOption
      ditherScaleOption
      overwriteOutputOption
      escapeOption
      enableIframeOption
      whatIfOption
      silentOption
    |] : Option array)
    |> Array.iter rootCommand.AddOption

  rootCommand.SetHandler rootCommandHandler

  let readmeCommand = Command ("readme", "Displays fsimg2sixel's README file")
  readmeCommand.SetHandler readmeCommandHandler
  rootCommand.AddCommand readmeCommand

  let licenseCommand = Command ("license", "Displays fsimg2sixel's LICENSE file")
  licenseCommand.SetHandler licenseCommandHandler
  rootCommand.AddCommand licenseCommand

  let noticeCommand = Command ("notice", "Displays fsimg2sixel's NOTICE file")
  noticeCommand.SetHandler noticeCommandHandler
  rootCommand.AddCommand noticeCommand


  rootCommand.Invoke args
