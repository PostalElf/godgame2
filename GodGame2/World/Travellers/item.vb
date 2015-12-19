Public Class item
    Friend id As Integer
    Friend Property hero As hero
    Friend Property name As String
    Friend Property equipmentSlot As String
    Friend Property modifiers As New List(Of modifier)

    Public Overrides Function ToString() As String
        Return name & " (" & equipmentSlot & ")"
    End Function
    Friend Sub consoleReport(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)

        Console.WriteLine(ind & prefix & equipmentSlot & ": " & name)
        If modifiers.Count > 0 Then
            For n = 0 To modifiers.Count - 1
                Dim modifier As modifier = modifiers(n)
                Console.WriteLine(indd & "└ " & modifier.qualityDescription)
                'Console.Write(modifier.name & " " & withSign(modifier.value(hero)))
                'If n <> modifiers.Count - 1 Then Console.Write(", ")
            Next
        End If
    End Sub
End Class
