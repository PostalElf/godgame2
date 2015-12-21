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
                Dim wonderShard As Boolean = False
                If n Mod 4 = 0 Then wonderShard = True
                Dim shard As shard = buildShard(world, wonderShard)
            Next
        End With

        Return world
    End Function
    Private Function buildShard(ByRef world As world, isWonderShard As Boolean) As shard
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


        'branch for wonderShard
        If isWonderShard = True Then
            'determine wilderness type
            Dim roll As Integer = rng.Next(wildernessList.Count)
            Dim wildernessType As wildernessType = wildernessList(roll)
            wildernessList.RemoveAt(roll)

            'determine wonder from wildernessType
            If wonderList.Count = 0 Then buildWonderList()
            Dim potentialWonders As List(Of wonder) = wonderList(wildernessType)
            Dim wonder As wonder
            If potentialWonders.Count > 0 Then
                roll = rng.Next(potentialWonders.Count)
                wonder = potentialWonders(roll)
                potentialWonders.RemoveAt(roll)
            Else
                'default wonder when wonderlist runs out of options
                wonder = New wonder
                With wonder
                    .name = "The Fields of Oblivion"
                    .wildernessType = wildernessType.Plains
                    .modifiers.Add(New modifier(.modifiers, "The Fields of Oblivion", "SettlementPopulationIncome Farmers -20"))
                End With
            End If

            'add
            shard.addTravelLocation(wonder, 0)
        Else
            'build confirmed settlement site
            shard.addTravelLocation(buildSettlementSite)

            'build wilderness
            buildRandomWilderness(shard)
        End If


        'build other travel locations
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
            .type = wildernessList(roll)
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
    Private Property wonderList As New Dictionary(Of wildernessType, List(Of wonder))
    Private Sub buildWonderList()
        wonderList.Clear()

        For Each wild In constants.wildernessTypeArray
            wonderList.Add(wild, New List(Of wonder))
            For n = 1 To 4
                Dim wonder As New wonder
                With wonder
                    Select Case wild
                        Case wildernessType.Forest
                            Select Case n
                                Case 1
                                    .name = "The Eternal Garden"
                                    .modifiers.Add(New modifier(.modifiers, "The Eternal Garden", "SettlementIncome Food +30"))
                                    .modifiers.Add(New modifier(.modifiers, "The Eternal Garden", "SettlementIncome Faith +30"))
                                Case 2
                                    .name = "The Sword Tree"
                                    .modifiers.Add(New modifier(.modifiers, "The Sword Tree", "GoodUnlock Metal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Sword Tree", "WarriorEfficiency +10"))
                                Case 3
                                    .name = "The Lifeoak"
                                    .modifiers.Add(New modifier(.modifiers, "The Lifeoak", "GoodUnlock Reagents"))
                                    .modifiers.Add(New modifier(.modifiers, "The Lifeoak", "SettlementPublicOrder +5/All"))
                                Case 4
                                    .name = "The Baluster Chandelier"
                                    .modifiers.Add(New modifier(.modifiers, "The Baluster Chandelier", "GoodUnlock Crystal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Baluster Chandelier", "SettlementPopulationIncome Traders +10"))
                            End Select

                        Case wildernessType.Lake
                            Select Case n
                                Case 1
                                    .name = "The Reflecting Pond"
                                    .modifiers.Add(New modifier(.modifiers, "The Reflecting Pond", "SettlementPopulationIncome Priests +10"))
                                    .modifiers.Add(New modifier(.modifiers, "The Reflecting Pond", "SettlementIncome Faith +10/Savants"))
                                Case 2
                                    .name = "The Bloody Lake"
                                    .modifiers.Add(New modifier(.modifiers, "The Bloody Lake", "GoodUnlock Metal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Bloody Lake", "SettlementPublicOrder +20"))
                                Case 3
                                    .name = "The Fountain of Youth"
                                    .modifiers.Add(New modifier(.modifiers, "The Fountain of Youth", "GoodUnlock Reagents"))
                                    .modifiers.Add(New modifier(.modifiers, "The Fountain of Youth", "SettlementIncome Faith +30"))
                                Case 4
                                    .name = "The Floating Forest"
                                    .modifiers.Add(New modifier(.modifiers, "The Floating Forest", "GoodUnlock Wood"))
                                    .modifiers.Add(New modifier(.modifiers, "The Floating Forest", "SettlementIncome Science +10/Farmers"))
                            End Select

                        Case wildernessType.Mountain
                            Select Case n
                                Case 1
                                    .name = "The Obsidian Ridge"
                                    .modifiers.Add(New modifier(.modifiers, "The Obsidian Ridge", "SettlementPopulationIncome Traders +10"))
                                    .modifiers.Add(New modifier(.modifiers, "The Obsidian Ridge", "SettlementPopulationIncome Savants +10"))
                                Case 2
                                    .name = "The Fire Mountain"
                                    .modifiers.Add(New modifier(.modifiers, "The Fire Mountain", "GoodUnlock Reagents"))
                                    .modifiers.Add(New modifier(.modifiers, "The Fire Mountain", "SettlementPopulationIncome Savants +10"))
                                Case 3
                                    .name = "The Giant's Mirror"
                                    .modifiers.Add(New modifier(.modifiers, "The Giant's Mirror", "GoodUnlock Crystal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Giant's Mirror", "WarmageEfficiency +10"))
                                Case 4
                                    .name = "The High Woods"
                                    .modifiers.Add(New modifier(.modifiers, "The High Woods", "GoodUnlock Wood"))
                                    .modifiers.Add(New modifier(.modifiers, "The High Woods", "SettlementPopulationIncome Traders +10"))
                            End Select

                        Case wildernessType.Plains
                            Select Case n
                                Case 1
                                    .name = "The Emerald Sea"
                                    .modifiers.Add(New modifier(.modifiers, "The Emerald Sea", "SettlementRecruit Farmers +2"))
                                    .modifiers.Add(New modifier(.modifiers, "The Emerald Sea", "SettlementPopulationIncome Farmers +15"))
                                Case 2
                                    .name = "The Field of Dreams"
                                    .modifiers.Add(New modifier(.modifiers, "The Field of Dreams", "GoodUnlock Wood"))
                                    .modifiers.Add(New modifier(.modifiers, "The Field of Dreams", "GoodUnlock Crystal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Field of Dreams", "GoodUnlock Reagents"))
                                    .modifiers.Add(New modifier(.modifiers, "The Field of Dreams", "GoodUnlock Metal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Field of Dreams", "SettlementPublicOrder -60"))
                                Case 3
                                    .name = "The Tree of Words"
                                    .modifiers.Add(New modifier(.modifiers, "The Tree of Words", "SettlementRecruit Priests +2"))
                                    .modifiers.Add(New modifier(.modifiers, "The Tree of Words", "SettlementPopulationIncome Priests +15"))
                                Case 4
                                    .name = "The Wordstone"
                                    .modifiers.Add(New modifier(.modifiers, "The Wordstone", "WarmageEfficiency +1"))
                                    .modifiers.Add(New modifier(.modifiers, "The Wordstone", "SettlementRecruit Warmages +1"))
                            End Select

                        Case wildernessType.Swamp
                            Select Case n
                                Case 1
                                    .name = "The Cauldron"
                                    .modifiers.Add(New modifier(.modifiers, "The Cauldron", "SettlementIncome Wealth +10/Farmers"))
                                    .modifiers.Add(New modifier(.modifiers, "The Cauldron", "SettlementIncome Science +10/Farmers"))
                                Case 2
                                    .name = "The Drowned Forest"
                                    .modifiers.Add(New modifier(.modifiers, "The Drowned Forest", "GoodUnlock Wood"))
                                    .modifiers.Add(New modifier(.modifiers, "The Drowned Forest", "SettlementIncome Faith +10/Farmers"))
                                Case 3
                                    .name = "The Iron Bog"
                                    .modifiers.Add(New modifier(.modifiers, "The Iron Bog", "GoodUnlock Metal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Iron Bog", "SettlementPopulationIncome Savants +10"))
                                Case 4
                                    .name = "The Crystal Fields"
                                    .modifiers.Add(New modifier(.modifiers, "The Crystal Fields", "GoodUnlock Crystal"))
                                    .modifiers.Add(New modifier(.modifiers, "The Crystal Fields", "SettlementPopulationIncome Savants +10"))
                            End Select
                    End Select
                End With
                If wonder.name <> "" Then wonderList(wild).Add(wonder)
            Next
        Next
    End Sub

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
