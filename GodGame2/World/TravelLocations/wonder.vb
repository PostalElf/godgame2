Public Class wonder
    Inherits travelLocation

    Public Overrides Function ToString() As String
        Return "[Wonder] " & name
    End Function

    Friend Property wildernessType As wildernessType
    Friend Property modifiers As New List(Of modifier)
End Class
