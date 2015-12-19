Public MustInherit Class travelLocation
    Friend Property name As String
    Friend Property shard As shard
    Friend Property heroes As New List(Of hero)
    Private Property pThreat As threat = Nothing
    Friend ReadOnly Property threat As threat
        Get
            Return pThreat
        End Get
    End Property
    Friend Sub setThreat(threat As threat)
        If threat Is Nothing = False Then
            threat.location = Me
            report.Add(name & " on " & shard.name & " Shard is now threatened by the " & threat.name & "!", reportQueueType.ThreatNew, Nothing)
        ElseIf threat Is Nothing AndAlso pThreat Is Nothing = False Then
            report.Add("The threat in " & name & " on " & shard.name & " Shard has been eliminated.", reportQueueType.ThreatDestroyed, Nothing)
        End If

        pThreat = threat
    End Sub

    Friend Function getDistanceTo(destination As travelLocation) As Integer
        'if on same shard, 1 turn
        If destination.shard.Equals(shard) Then Return 1

        'otherwise return the straight line distance
        Dim shardDistance As Integer = pythogoras(shard.coords, destination.shard.coords)
        Return constrain(shardDistance, 1, 500)
    End Function

    Friend ReadOnly Property isAttackable(aEmpire As empire) As Boolean
        Get
            If threat Is Nothing = False Then
                Return True
            ElseIf TypeOf Me Is settlement Then
                Dim settlement As settlement = CType(Me, settlement)
                If settlement.empire.Equals(aEmpire) = False Then
                    Return True
                End If
            End If

            Return False
        End Get
    End Property
End Class