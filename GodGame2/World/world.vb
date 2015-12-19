Public Class world
    Friend Property timekeeper As New timekeeper
    Friend Property shards As New List(Of shard)
    Friend Property empires As New List(Of empire)
    Friend Property armies As New List(Of army)
    Private Property pModifiers As New List(Of modifier)
    Friend ReadOnly Property modifiers As List(Of modifier)
        Get
            Return pModifiers
        End Get
    End Property
    Friend Property openDomains As New List(Of divineDomain)

    Friend Shared Property NatureRedInToothAndClaw As New empire
    Friend Shared Property playerEmpire As empire = Nothing

    Public Sub New()
        For Each domain In constants.divineDomainArray
            openDomains.Add(domain)
        Next

        With NatureRedInToothAndClaw
            .name = "Nature"
            .isAI = True
        End With
    End Sub
    Friend Sub tick()
        For Each empire In empires
            report.Clear(empire)
        Next
        report.Clear(NatureRedInToothAndClaw)

        timekeeper.tick()

        For n = armies.Count - 1 To 0 Step -1
            Dim army As army = armies(n)
            army.tick()
        Next

        For Each empire In empires
            empire.tick()
        Next

        For Each shard In shards
            shard.tick()
        Next

        For n = pModifiers.Count - 1 To 0 Step -1
            Dim modifier As modifier = pModifiers(n)
            modifier.tick()
            If modifier.duration = 0 Then
                pModifiers.Remove(modifier)
                report.Add("'" & modifier.name & "' has expired.", reportQueueType.WorldModifierExpire, Nothing)
                For Each empire In empires
                    empire.mustRefresh = True
                Next
            End If
        Next

    End Sub
    Friend Sub consoleReport(indent As Integer)
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)

        If empires.Count > 0 Then
            Console.WriteLine(ind & "Empires:")
            For Each empire In empires
                Console.WriteLine(indd & "└ " & empire.name)
            Next
            Console.WriteLine()
            Console.WriteLine()
        End If

        Console.WriteLine(ind & "Shards:")
        For Each shard In shards
            shard.consoleReport(indent + 1)
            Console.WriteLine()
        Next
        Console.WriteLine()

        If armies.Count > 0 Then
            Console.WriteLine(ind & "Armies:")
            For Each army In armies
                army.consoleReport(indent + 1, "└ ")
            Next
        End If
    End Sub
    Friend ReadOnly Property habitableShards() As List(Of shard)
        Get
            Dim total As New List(Of shard)
            For Each shard In shards
                If shard.habitable = True Then total.Add(shard)
            Next
            Return total
        End Get
    End Property
    Friend ReadOnly Property attackableShards(aEmpire As empire) As List(Of shard)
        Get
            Dim total As New List(Of shard)
            For Each shard In shards
                Dim attackableLocations As List(Of travelLocation) = shard.getAttackableLocations(aEmpire)
                If attackableLocations.Count > 0 Then total.Add(shard)
            Next
            Return total
        End Get
    End Property

    Friend Sub addWorldModifier(modifier As modifier)
        If pModifiers.Contains(modifier) Then
            Debug.Print("World already contains modifier.")
            Exit Sub
        End If

        modifier.parent = pModifiers
        pModifiers.Add(modifier)
        report.Add("The world gains the '" & modifier.name & "' modifier.", reportQueueType.WorldModifierNew, Nothing)
    End Sub
End Class
