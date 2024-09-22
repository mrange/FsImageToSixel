# FsImageToSixel.Tool


A dotnet tool to convert images to Sixel images which are multicolored pixel like images that can be shown in a terminal that supports it (such as Windows Terminal 1.22+).

## How to build

```bash
cd src/FsImageToSixel.Tool
dotnet build -c Release
```

## How to run

```bash
cd src/FsImageToSixel.Tool
dotnet run -c Release -- -i ../../assets/dotnet-bot_branded.png -s 100

```
