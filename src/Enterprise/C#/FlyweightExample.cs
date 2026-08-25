using System;
using System.Collections.Generic;

sealed record TextStyle(string Font, int Size, string Color);

sealed class StyleFactory
{
    private readonly Dictionary<string, TextStyle> _styles = new();

    public TextStyle Get(string font, int size, string color)
    {
        var key = $"{font}|{size}|{color}";
        if (!_styles.TryGetValue(key, out var style))
        {
            style = new TextStyle(font, size, color);
            _styles[key] = style;
        }

        return style;
    }

    public int Count => _styles.Count;
}

sealed record Glyph(char Character, int Position, TextStyle Style);

static class Program
{
    public static void Main()
    {
        var factory = new StyleFactory();
        var red1 = factory.Get("Inter", 12, "red");
        var red2 = factory.Get("Inter", 12, "red");
        var blue = factory.Get("Inter", 12, "blue");
        var glyphs = new[]
        {
            new Glyph('A', 1, red1),
            new Glyph('B', 2, red2),
            new Glyph('C', 3, blue)
        };

        Console.WriteLine(
            $"styles={factory.Count};shared={ReferenceEquals(glyphs[0].Style, glyphs[1].Style).ToString().ToLowerInvariant()};text={string.Concat(Array.ConvertAll(glyphs, glyph => glyph.Character))}");
    }
}
