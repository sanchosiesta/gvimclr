using System;
using System.IO;
using clojure.lang;
using Avalonia;
using Avalonia.Controls;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Input;
using Avalonia.Input.Platform;
using Avalonia.Themes.Fluent;
using Avalonia.Threading;

namespace GVimClr;

public static class Ui
{
    public static void OnClick(Button button, IFn fn)
    {
        button.Click += (s, e) => fn.invoke();
    }

    public static void OnTextChanged(TextBox textBox, IFn fn)
    {
        textBox.TextChanged += (s, e) => fn.invoke(textBox.Text ?? "");
    }

    public static void OnKeyDown(InputElement elem, IFn fn)
    {
        elem.AddHandler(InputElement.KeyDownEvent, (s, e) =>
        {
            var key = e.Key.ToString();
            var shift = (e.KeyModifiers & KeyModifiers.Shift) != 0;
            var ctrl = (e.KeyModifiers & KeyModifiers.Control) != 0;
            var alt = (e.KeyModifiers & KeyModifiers.Alt) != 0;
            var res = fn.invoke(key, shift, ctrl, alt);
            if (res is bool b && b)
            {
                e.Handled = true;
            }
        }, Avalonia.Interactivity.RoutingStrategies.Tunnel);
    }

    public static void FocusElement(InputElement elem)
    {
        elem.Focus();
    }

    public static bool IsFocused(InputElement elem)
    {
        return elem.IsFocused;
    }

    public static int GetCaretIndex(TextBox tb)
    {
        return tb.CaretIndex;
    }

    public static void SetCaretIndex(TextBox tb, int index)
    {
        tb.CaretIndex = Math.Max(0, Math.Min(index, (tb.Text ?? "").Length));
    }

    public static void SelectRange(TextBox tb, int start, int end)
    {
        tb.SelectionStart = start;
        tb.SelectionEnd = end;
    }

    public static string? GetClipboardText(IClipboard cb)
    {
        try
        {
            return ClipboardExtensions.TryGetTextAsync(cb).GetAwaiter().GetResult();
        }
        catch
        {
            return null;
        }
    }

    public static void SetClipboardText(IClipboard cb, string text)
    {
        try
        {
            ClipboardExtensions.SetTextAsync(cb, text).GetAwaiter().GetResult();
        }
        catch
        {
        }
    }
}

public static class Program
{
    [STAThread]
    public static int Main(string[] args)
    {
        // Touch RT.TagKey to ensure proper ClojureCLR bootstrap order
        _ = RT.TagKey;

        var asm = typeof(Program).Assembly;
        using var stream = asm.GetManifestResourceStream("gvim.core.clj");
        if (stream == null)
            throw new InvalidOperationException("Missing embedded resource: gvim.core.clj");
        using var reader = new StreamReader(stream);
        Compiler.load(reader, "gvim.core.clj");

        return AppBuilder.Configure<App>()
            .UsePlatformDetect()
            .WithInterFont()
            .StartWithClassicDesktopLifetime(args);
    }
}

public class App : Application
{
    public override void Initialize()
    {
        Styles.Add(new FluentTheme());
        base.Initialize();
    }

    public override void OnFrameworkInitializationCompleted()
    {
        base.OnFrameworkInitializationCompleted();
        if (ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktop)
        {
            var buildWindow = (IFn)RT.var("gvim.core", "build-window");
            desktop.MainWindow = (Window)buildWindow.invoke();
        }
    }
}
