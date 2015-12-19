Public Class zeitgeist
    Friend id As Integer
    Friend name As String
    Friend description As String
    Friend modifiers As New List(Of modifier)
    Friend maxPhilosophy As Dictionary(Of philosophyShape, Integer) = initMaxPhilosophy
    Friend ReadOnly Property maxPhilosophyString() As String
        Get
            Dim totalList As New List(Of String)
            For Each philo In maxPhilosophy
                If philo.Value > 0 Then totalList.Add(philo.Key.ToString & " " & philo.Value)
            Next

            If totalList.Count = 0 Then
                Return "Nothing"
            Else
                Dim total As String = ""
                For n = 0 To totalList.Count - 1
                    total &= totalList(n)
                    If n <> totalList.Count - 1 Then total &= ", "
                Next
                Return total
            End If
        End Get
    End Property
    Private Shared Function initMaxPhilosophy() As Dictionary(Of philosophyShape, Integer)
        Dim total As New Dictionary(Of philosophyShape, Integer)
        For Each shape In constants.philosophyShapeArray
            total.Add(shape, 0)
        Next
        Return total
    End Function

    Public Sub New()
    End Sub
    Public Overrides Function ToString() As String
        Return name & " (" & maxPhilosophyString & ")"
    End Function
    Friend Shared Function zeitgeistFileget(aId As Integer) As zeitgeist
        Dim pathname As String = "data/zeitgeist.csv"
        If authenticator.checkHash(pathname) = False Then
            Debug.Print("Authentication failed.")
            Return Nothing
        End If


        Dim total As List(Of String()) = csvFileget(pathname)
        For Each line In total
            If CInt(line(0)) = aId Then
                Dim zeitgeist As New zeitgeist
                Dim n As New rollingCounter(0)
                With zeitgeist
                    .id = CInt(line(n.Tick))
                    .name = line(n.Tick)
                    .description = line(n.Tick)

                    Dim split As String() = line(n.Tick).Split(" ")
                    For i = 0 To split.Count - 1 Step 2
                        Dim shapeStr As String = split(i)
                        Dim shape As philosophyShape = constants.getPhilosophyShapeFromString(shapeStr)
                        Dim value As Integer = split(i + 1)
                        .maxPhilosophy(shape) += value
                    Next

                    While n.Tick < line.Count AndAlso line(n.Last) <> ""
                        Dim modifier As New modifier(Nothing, .name, line(n.Last))
                        .modifiers.Add(modifier)
                    End While
                End With
                Return zeitgeist
            End If
        Next
        Return Nothing
    End Function
End Class
