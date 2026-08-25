Imports System.IO

Namespace Core
    ''' <summary>Creates non-destructive filesystem backups of persisted quote documents.</summary>
    Public NotInheritable Class QuoteBackupService
        Private Sub New()
        End Sub

        ''' <summary>Copies every top-level *.quote.json file in deterministic order to a different directory without modifying the source.</summary>
        Public Shared Function CreateBackup(sourceDirectory As String, destinationDirectory As String) As Integer
            If String.IsNullOrWhiteSpace(sourceDirectory) Then Throw New ArgumentException("El directorio de origen es obligatorio.", NameOf(sourceDirectory))
            If String.IsNullOrWhiteSpace(destinationDirectory) Then Throw New ArgumentException("El directorio de destino es obligatorio.", NameOf(destinationDirectory))

            Dim sourcePath = Path.GetFullPath(sourceDirectory)
            Dim destinationPath = Path.GetFullPath(destinationDirectory)
            If Not Directory.Exists(sourcePath) Then Throw New DirectoryNotFoundException($"No existe el directorio de origen: {sourcePath}")
            Dim pathComparison = If(OperatingSystem.IsWindows(), StringComparison.OrdinalIgnoreCase, StringComparison.Ordinal)
            If String.Equals(sourcePath.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar), destinationPath.TrimEnd(Path.DirectorySeparatorChar, Path.AltDirectorySeparatorChar), pathComparison) Then
                Throw New ArgumentException("El respaldo debe escribirse en un directorio distinto al origen.", NameOf(destinationDirectory))
            End If

            Directory.CreateDirectory(destinationPath)
            Dim files = Directory.GetFiles(sourcePath, "*.quote.json", SearchOption.TopDirectoryOnly).ToList()
            files.Sort(StringComparer.OrdinalIgnoreCase)
            Dim targets = files.Select(Function(filePath) Path.Combine(destinationPath, Path.GetFileName(filePath))).ToList()

            For Each targetPath In targets
                If File.Exists(targetPath) Then Throw New IOException($"El respaldo ya contiene un archivo llamado {Path.GetFileName(targetPath)}.")
            Next

            For index = 0 To files.Count - 1
                File.Copy(files(index), targets(index), False)
            Next

            Return files.Count
        End Function
    End Class
End Namespace
