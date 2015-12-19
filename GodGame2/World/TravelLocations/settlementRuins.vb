Public Class settlementRuins
    Inherits travelLocation
    Public Sub New(Optional aName As String = "Precursor Ruins")
        name = aName
    End Sub
    Public Overrides Function ToString() As String
        Return "[Ruins] " & name
    End Function
End Class
