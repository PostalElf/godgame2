Public Class building
    Friend id As Integer
    Friend name As String
    Friend requiredBuildingID As Integer
    Friend progress As Integer
    Friend cost As Integer
    Friend modifiers As New List(Of modifier)
    Friend ReadOnly Property requiredWildernessTypes As List(Of wildernessType)
        Get
            Dim total As New List(Of wildernessType)
            For Each modifier In modifiers
                If modifier.isConditionalOnWildernessType = True Then total.Add(modifier.wildernessType)
            Next
            Return total
        End Get
    End Property
    Friend ReadOnly Property requiredGoods As List(Of good)
        Get
            Dim total As New List(Of good)
            For Each modifier In modifiers
                If modifier.isConditionalOnGood = True Then total.Add(modifier.good)
            Next
            Return total
        End Get
    End Property

    Public Overrides Function ToString() As String
        Return name & " (" & progress & "/" & cost & ")"
    End Function

    Friend Shared Function buildingFileget(aID As Integer) As building
        Dim pathname As String = constants.pathnameBuilding
        If authenticator.checkHash(pathname) = False Then
            Debug.Print("Authentication failed.")
            Return Nothing
        End If

        Dim total As List(Of String()) = csvFileget(pathname)
        For Each line In total
            If CInt(line(0)) = aID Then
                Dim n As New rollingCounter(0)
                Dim building As New building

                building.id = CInt(line(n.Tick))
                building.name = line(n.Tick)
                building.cost = CInt(line(n.Tick))
                building.requiredBuildingID = CInt(line(n.Tick))

                While n.Tick < line.Count AndAlso line(n.Last) <> ""
                    Dim modifier As New modifier(Nothing, building.name, line(n.Last))
                    building.modifiers.Add(modifier)
                End While

                Return building
            End If
        Next
        Return Nothing
    End Function
End Class