Public Class philosophy
    Friend id As Integer
    Friend parent As zeitgeist = Nothing
    Friend name As String
    Friend shape As philosophyShape
    Friend progress As Integer
    Friend cost As Integer
    Friend modifiers As New List(Of modifier)

    Public Sub New()
    End Sub
    Public Sub New(aParent As zeitgeist, aName As String, aShape As philosophyShape, aCost As Integer, Optional aModifiers As List(Of modifier) = Nothing)
        parent = aParent
        name = aName
        shape = aShape
        cost = aCost
        If aModifiers Is Nothing = False Then modifiers = aModifiers
    End Sub
    Public Overrides Function ToString() As String
        Return name
    End Function
    Friend Sub consoleReport(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent)

        Console.WriteLine(ind & prefix & "(" & shape.ToString & ") " & name)
    End Sub

    Friend Shared Function philosophyFileget(aId As Integer) As philosophy
        Dim pathname As String = "data/philosophy.csv"
        If authenticator.checkHash(pathname) = False Then
            Debug.Print("Authentication failed.")
            Return Nothing
        End If


        Dim total As List(Of String()) = csvFileget(pathname)
        For Each line In total
            If CInt(line(0)) = aId Then
                Dim philosophy As New philosophy
                Dim n As New rollingCounter(0)
                With philosophy
                    .id = CInt(line(n.Tick))
                    .name = line(n.Tick)
                    .shape = constants.getPhilosophyShapeFromString(line(n.Tick))

                    While n.Tick < line.Count AndAlso line(n.Last) <> ""
                        Dim modifier As New modifier(Nothing, .name, line(n.Last))
                        .modifiers.Add(modifier)
                    End While
                End With
                Return philosophy
            End If
        Next
        Return Nothing
    End Function
End Class

Public Enum philosophyShape
    Religious
    Economic
    Political
    Sociological
End Enum