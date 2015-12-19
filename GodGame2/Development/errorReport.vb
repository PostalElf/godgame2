Public Class errorReport
    Friend Property text As String
    Friend Property colour As ConsoleColor

    Public Sub New(aText As String, Optional aColour As ConsoleColor = ConsoleColor.Gray)
        text = aText
        colour = aColour
    End Sub
    Friend Sub consoleReport(indent As Integer)
        Console.ForegroundColor = colour
        Console.WriteLine(vbSpace(indent) & text)
    End Sub
End Class
