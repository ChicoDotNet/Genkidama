Imports System.Windows.Forms

Namespace WinForms
    Friend Module Program
        <STAThread>
        Public Sub Main()
            Application.SetHighDpiMode(HighDpiMode.SystemAware)
            Application.EnableVisualStyles()
            Application.SetCompatibleTextRenderingDefault(False)
            Application.Run(New MainForm())
        End Sub
    End Module
End Namespace
