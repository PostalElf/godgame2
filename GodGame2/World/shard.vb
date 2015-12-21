Public Class shard
    Public Sub New(aWorld As world)
        world = aWorld
    End Sub
    Public Overrides Function ToString() As String
        Return name & " Shard"
    End Function
    Friend Property name As String
    Friend Property world As world
    Friend Sub tick()
        'threats
        For Each threat In threats
            threat.tick()
        Next


        'modifiers
        For n = shardModifiers.Count - 1 To 0 Step -1
            Dim modifier As modifier = shardModifiers(n)
            modifier.tick()
            If modifier.duration = 0 Then
                removeShardModifier(modifier)
            End If
        Next
    End Sub
    Friend Sub consoleReport(indent As Integer)
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)
        Dim inddd As String = vbSpace(indent + 2)

        Console.WriteLine(ind & name & " Shard")
        For Each travelLocation In pTravelLocations
            If TypeOf travelLocation Is settlement Then
                Dim settlement As settlement = CType(travelLocation, settlement)
                Console.Write(indd & "└ " & settlement.name & ", " & settlement.empire.name)
            Else
                Console.Write(indd & "└ " & travelLocation.ToString)
            End If
            If travelLocation.threat Is Nothing = False Then Console.Write(" (" & travelLocation.threat.name & ")")
            Console.WriteLine()
        Next
    End Sub


    'travelLocations
    Friend Property coords As xy
    Private Property pTravelLocations As New List(Of travelLocation)
    Friend ReadOnly Property travelLocations As List(Of travelLocation)
        Get
            Return pTravelLocations
        End Get
    End Property
    Friend Sub addTravelLocation(travelLocation As travelLocation, Optional index As Integer = -1)
        If pTravelLocations.Contains(travelLocation) Then
            Debug.Print("Shard already contains location.")
            Exit Sub
        End If

        If index = -1 Then pTravelLocations.Add(travelLocation) Else pTravelLocations.Insert(index, travelLocation)
        travelLocation.shard = Me

        If TypeOf travelLocation Is wonder Then
            Dim wonder As wonder = CType(travelLocation, wonder)
            For Each modifier In wonder.modifiers
                addShardModifier(modifier, True)
            Next
        End If
    End Sub
    Friend Sub removeTravelLocation(travelLocation As travelLocation)
        If pTravelLocations.Contains(travelLocation) = False Then
            Debug.Print("Does not contain travelLocation.")
            Exit Sub
        End If

        pTravelLocations.Remove(travelLocation)
        travelLocation.shard = Nothing

        If TypeOf travelLocation Is wonder Then
            Dim wonder As wonder = CType(travelLocation, wonder)
            For Each modifier In wonder.modifiers
                removeShardModifier(modifier, True)
            Next
        End If
    End Sub
    Friend Function settleSettlement(ByRef empire As empire, Optional index As Integer = -1) As settlement
        Dim possibleSite As travelLocation = Nothing
        If index = -1 Then
            'search for settlementSite because index is not specified
            For Each travelLocation In pTravelLocations
                If TypeOf (travelLocation) Is settlementSite Then possibleSite = travelLocation
            Next
        Else
            If pTravelLocations.Count - 1 >= index Then possibleSite = pTravelLocations(index)
        End If

        If possibleSite Is Nothing OrElse TypeOf possibleSite Is settlementSite = False Then
            Debug.Print("TravelLocation at index " & index & " is not a settlement site.")
            Return Nothing
        End If

        Dim settlementSite As settlementSite = CType(possibleSite, settlementSite)
        settleSettlement = settlementSite.settle(empire)
        pTravelLocations.Remove(settlementSite)
    End Function
    Friend Function getSettlementSite() As settlementSite
        For Each travelLocation In travelLocations
            If TypeOf travelLocation Is settlementSite Then
                Dim settlementSite As settlementSite = CType(travelLocation, settlementSite)
                Return settlementSite
            End If
        Next
        Return Nothing
    End Function
    Friend Function getSettlements() As List(Of settlement)
        Dim total As New List(Of settlement)
        For Each travelLocation In pTravelLocations
            If TypeOf (travelLocation) Is settlement Then total.Add(travelLocation)
        Next
        Return total
    End Function
    Friend Function getAttackableLocations(aEmpire As empire) As List(Of travelLocation)
        Dim total As New List(Of travelLocation)
        For Each location In pTravelLocations
            If location.isAttackable(aEmpire) = True Then total.Add(location)
        Next
        Return total
    End Function
    Friend ReadOnly Property habitable As Boolean
        Get
            Dim total As Boolean = False
            For Each travelLocation In pTravelLocations
                If TypeOf (travelLocation) Is settlementSite Then total = True
            Next
            Return total
        End Get
    End Property


    'modifiers
    Private Property pMustRefresh As Boolean
    Friend Property mustRefresh As Boolean
        Get
            Return pMustRefresh
        End Get
        Set(value As Boolean)
            If pMustRefresh = False AndAlso value = True Then
                For Each s In getSettlements()
                    s.mustRefresh = True
                Next
            End If

            pMustRefresh = value
        End Set
    End Property
    Private Property pShardModifiers As New List(Of modifier)
    Friend ReadOnly Property shardModifiers As List(Of modifier)
        Get
            Return pShardModifiers
        End Get
    End Property
    Friend Sub addShardModifier(modifier As modifier, Optional doNotReport As Boolean = False)
        If pShardModifiers.Contains(modifier) Then
            Debug.Print(name & " already contains modifier.")
            Exit Sub
        End If

        pShardModifiers.Add(modifier)
        modifier.parent = pShardModifiers
        If doNotReport = False Then report.Add(name & " Shard gains the '" & modifier.name & "' modifier.", reportQueueType.ShardModifierNew, Nothing)

        mustRefresh = True
    End Sub
    Friend Sub removeShardModifier(modifier As modifier, Optional doNotReport As Boolean = False)
        If pShardModifiers.Contains(modifier) = False Then
            Debug.Print("Shard does not contain " & modifier.name & " modifier.")
            Exit Sub
        End If

        pShardModifiers.Remove(modifier)
        If doNotReport = False Then report.Add("'" & modifier.name & "' for the " & name & " Shard has expired.", reportQueueType.ShardModifierExpire, Nothing)

        mustRefresh = True
    End Sub


    'wild resources
    Friend ReadOnly Property wildernessTypes As List(Of wildernessType)
        Get
            Dim total As New List(Of wildernessType)
            For Each travelLocation In pTravelLocations
                If TypeOf (travelLocation) Is wilderness Then
                    Dim wilderness As wilderness = CType(travelLocation, wilderness)
                    total.Add(wilderness.type)
                ElseIf TypeOf travelLocation Is wonder Then
                    Dim wonder As wonder = CType(travelLocation, wonder)
                    total.Add(wonder.wildernessType)
                End If
            Next
            Return total
        End Get
    End Property


    'threats
    Friend ReadOnly Property threats As List(Of threat)
        Get
            Dim total As New List(Of threat)
            For Each location In pTravelLocations
                If location.threat Is Nothing = False Then total.Add(location.threat)
            Next
            Return total
        End Get
    End Property
End Class