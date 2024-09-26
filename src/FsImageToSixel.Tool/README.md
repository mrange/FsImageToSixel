# FsImageToSixel.Tool

A .NET tool for converting standard images into **Sixel** format, which produces multicolored, pixelated images displayable in terminals that support it (e.g., Windows Terminal 1.22+).

For more information on Sixel, check out the [Wikipedia article](https://en.wikipedia.org/wiki/Sixel).

## Dependencies

1. **SixLabors.ImageSharp** – A powerful image processing library by SixLabors. ImageSharp is split-licensed under the Apache License 2.0 and a commercial license. For this open-source project, it qualifies for usage under the Apache 2.0 license, as it meets the criteria for open-source software use.
2. **System.CommandLine** – A command-line parser library from Microsoft, licensed under the MIT License.

## Examples

```bash
# Converts a PNG to a sixel image and display it in the terminal
fsimg2sixel -i theimage.png
```

```bash
# Converts a PNG to a sixel image and writes it to a text file
fsimg2sixel -i theimage.png -o sixel.txt
```

```bash
# Prints the help
fsimg2sixel --help
```