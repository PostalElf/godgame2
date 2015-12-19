Public Class reward
    Friend Property name As String
    Friend Property equipmentSlot As String
    Friend Property modifiers As New Dictionary(Of Integer, List(Of modifier))          'tier, modifier

    Friend Property minTier As Integer
    Friend ReadOnly Property powerCost(tier As Integer) As Integer
        Get
            Select Case tier
                Case 1 : Return 3
                Case 2 : Return 5
                Case 3 : Return 7
                Case Else
                    Debug.Print("Invalid reward tier.")
                    Return 0
            End Select
        End Get
    End Property

    Public Sub New()
        For n = 1 To 3
            modifiers.Add(n, New List(Of modifier))
        Next
    End Sub
    Friend Sub briefConsoleReport(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent) & prefix
        Console.WriteLine(ind & name)
    End Sub

    Friend Function convertToItem(tier As Integer) As item
        If tier < minTier Then Return Nothing

        Dim item As New item
        With item
            .name = name
            .equipmentSlot = equipmentSlot
            .modifiers = New List(Of modifier)(modifiers(tier))
        End With
        Return item
    End Function
End Class
