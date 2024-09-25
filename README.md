# FsImageToSixel.Tool

A .NET tool for converting standard images into **Sixel** format, which produces multicolored, pixelated images displayable in terminals that support it (e.g., Windows Terminal 1.22+).

For more information on Sixel, check out the [Wikipedia article](https://en.wikipedia.org/wiki/Sixel).

## Dependencies

1. **SixLabors.ImageSharp** – A powerful image processing library by SixLabors. ImageSharp is split-licensed under the Apache License 2.0 and a commercial license. For this open-source project, it qualifies for usage under the Apache 2.0 license, as it meets the criteria for open-source software use.
2. **System.CommandLine** – A command-line parser library from Microsoft, licensed under the MIT License.

## Build Instructions

```bash
cd src/FsImageToSixel.Tool
dotnet build -c Release
```

## How to run

```bash
cd src/FsImageToSixel.Tool
# -i: Specifies the input image file
# -s: Specifies the scale percentage
# -h: Use -h or --help to print the help
dotnet run -c Release -- -i ../../assets/dotnet-bot_branded.png -s 100
```

## Generate reference assets

```bash
cd src/FsImageToSixel.Tool
dotnet run -c Release -- -i ../../assets/dotnet-bot_branded.png -s 100 -o ../../assets/dotnet-bot_branded.txt -oo

dotnet run -c Release -- -i ../../assets/icon.png -s 100 -o ../../assets/icon.txt -oo

dotnet run -c Release -- -i ../../assets/cube1s.gif -s 100 -o ../../assets/cube1s.txt -oo
```


