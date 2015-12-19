Public Class worldBuilder
    Private Const maxX As Integer = 100
    Private Const maxY As Integer = 100
    Private Shared sharedXList As List(Of Integer) = initList(maxX)
    Private Shared sharedYList As List(Of Integer) = initList(maxY)
    Private Shared Function initList(max As Integer) As List(Of Integer)
        Dim total As New List(Of Integer)
        For n = 1 To max
            total.Add(n)
        Next
        Return total
    End Function

    Private Function getRandom(ByRef total As List(Of String)) As String
        Dim roll As Integer = rng.Next(total.Count)
        getRandom = total(roll)
        total.RemoveAt(roll)
    End Function

    Friend Function buildWorld(Optional size As Integer = 5) As world
        Dim world As New world
        With world
            For n = 1 To size
                Dim shard As shard = buildShard(world)
            Next
        End With

        Return world
    End Function
    Private Function buildShard(ByRef world As world) As shard
        Dim shard As New shard(world)
        shard.name = getRandomShardName()

        'create coords
        Dim rollX As Integer = rng.Next(sharedXList.Count)
        Dim x As Integer = sharedXList(rollX)
        sharedXList.Remove(x)
        Dim rollY As Integer = rng.Next(sharedYList.Count)
        Dim y As Integer = sharedYList(rollY)
        sharedYList.Remove(y)
        shard.coords = New xy(x, y)


        'build confirmed settlement site
        shard.addTravelLocation(buildSettlementSite)


        'build wilderness and other travel locations
        buildRandomWilderness(shard)
        For n = 1 To 3
            Dim roll As Integer = rng.Next(1, 11)
            Select Case roll
                Case 1 To 3 : shard.addTravelLocation(buildRuins)
                Case 4 To 6 : shard.addTravelLocation(buildWasteland)
                Case 7 To 9 : shard.addTravelLocation(buildPlains)
                Case Else           '10% chance of adding nothing, giving a smaller shard
            End Select
        Next


        'add to world and return built shard
        world.shards.Add(shard)
        Return shard
    End Function
    Private Property shardNames As New List(Of String)
    Private Function getRandomShardName()
        If shardNames.Count = 0 Then shardNames = fileget("data/shardNames.txt")
        Return getRandom(shardNames)
    End Function
    Private Property wildernessList As New List(Of wildernessType)
    Private Sub buildWildernessList()
        wildernessList.Clear()
        For Each wild In constants.wildernessTypeArray
            If wild <> wildernessType.Plains Then wildernessList.Add(wild)
        Next
    End Sub
    Private Function buildSettlementSite() As settlementSite
        Dim settlementSite As New settlementSite
        With settlementSite
            .name = settlementSite.getRandomSettlementName()
        End With
        Return settlementSite
    End Function
    Private Sub buildRandomWilderness(ByRef shard As shard)
        If wildernessList.Count = 0 Then buildWildernessList()

        Dim wilderness As New wilderness
        With wilderness
            Dim roll As Integer = rng.Next(wildernessList.Count)
            .type = wildernessList.Count
            wildernessList.RemoveAt(roll)

            .name = wilderness.getRandomWildernessName(.type)
        End With
        shard.addTravelLocation(wilderness)
    End Sub
    Private Function buildRuins() As settlementRuins
        Dim ruins As New settlementRuins
        With ruins
            .name = "Precursor Ruins"
        End With
        Return ruins
    End Function
    Private Function buildPlains() As wilderness
        Dim plains As New wilderness(wildernessType.Plains)
        plains.name = wilderness.getRandomWildernessName(wildernessType.Plains)
        Return plains
    End Function
    Private Function buildWasteland() As wasteland
        Dim wasteland As New wasteland
        With wasteland
            .name = getRandomWastelandName()
        End With
        Return wasteland
    End Function
    Private Property wastePreNames As New List(Of String)
    Private Property wasteMidNames As New List(Of String)
    Private Property wasteSufNames As New List(Of String)
    Private Function getRandomWastelandName() As String
        If wastePreNames.Count = 0 Then wastePreNames = filegetBracket("data/preWildernessNames.txt", "Wasteland")
        If wasteMidNames.Count = 0 Then wasteMidNames = filegetBracket("data/midWildernessNames.txt", "Wasteland")
        If wasteSufNames.Count = 0 Then wasteSufNames = filegetBracket("data/sufWildernessNames.txt", "Wasteland")

        Dim prefix As String = getRandom(wastePreNames)
        Dim mid As String = getRandom(wasteMidNames)
        Dim suffix As String = getRandom(wasteSufNames)

        Return prefix & mid & suffix
    End Function

    Friend Function buildEmpire(ByRef world As world, name As String, isAI As Boolean) As empire
        Dim empire As New empire
        With empire
            .name = name
            .world = world
            .isAI = isAI
            .secondaryInitialisation()
        End With

        buildGod(world, empire, "Moloc", isAI)

        'settle on random shard
        Dim shardRoll As Integer = rng.Next(world.habitableShards.Count)
        Dim shard As shard = world.habitableShards(shardRoll)
        shard.settleSettlement(empire, 0)

        world.empires.Add(empire)
        If isAI = False Then world.playerEmpire = empire
        Return empire
    End Function

    Friend Sub buildGod(ByRef world As world, ByRef empire As empire, name As String, isAI As Boolean)
        Dim randomDomain As divineDomain = getRandomDomain(world)
        Dim god As New god(empire, randomDomain)
        With god
            .name = name
            .isAI = isAI
        End With

        empire.god = god
    End Sub
    Private Property openJurisdictions As New List(Of String)({"Cosmic", "Chthonic", "Conceptual", "Craft"})
    Private Function getRandomDomain(world As world) As divineDomain
        'roll for juris
        Dim jurisRoll As Integer = rng.Next(openJurisdictions.Count)
        Dim juris As String = openJurisdictions(jurisRoll)
        openJurisdictions.RemoveAt(jurisRoll)

        Select Case juris
            Case "Cosmic" : Return 1
            Case "Chthonic" : Return 11
            Case "Conceptual" : Return 21
            Case "Craft" : Return 31
            Case Else : Return Nothing
        End Select
    End Function
End Class
