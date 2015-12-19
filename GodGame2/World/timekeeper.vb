Public Class timekeeper
    Private Property pTurnCounter As Integer = 0
    Friend ReadOnly Property turnCounter As Integer
        Get
            Return pTurnCounter
        End Get
    End Property

    Private Const weeksInMonth As Integer = 6
    Private Const monthsInYear As Integer = 9
    Private Property weekInt As Integer = 0
    Private Property monthInt As Integer = 1
    Private Property yearInt As Integer = 1
    Private ReadOnly Property month As String
        Get
            Select Case monthInt
                Case 1 : Return "Ascel"
                Case 2 : Return "Roinl"
                Case 3 : Return "Duryn"
                Case 4 : Return "Cuspe"
                Case 5 : Return "Feons"
                Case 6 : Return "Weind"
                Case 7 : Return "Irtea"
                Case 8 : Return "Yarow"
                Case 9 : Return "Umuen"
                Case Else : Return Nothing
            End Select
        End Get
    End Property

    Friend Sub tick()
        pTurnCounter += 1

        weekInt += 1
        If weekInt > weeksInMonth Then
            weekInt = 1
            monthInt += 1
            If monthInt > monthsInYear Then
                monthInt = 1
                yearInt += 1
            End If
        End If
    End Sub
    Friend Sub consoleReport(indent As Integer)
        Console.WriteLine(vbSpace(indent) & "Turn " & turnCounter & " - Week " & weekInt & " of " & month & ", Year " & yearInt)
    End Sub
    Friend Sub briefConsoleReport(indent As Integer)
        Console.WriteLine(vbSpace(indent) & "Turn " & turnCounter)
    End Sub
End Class
