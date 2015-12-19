Public Class threat
    Friend Property name As String
    Friend Property evolution As String()
    Friend Property id As Integer
    Friend Property tier As Integer
    Friend ReadOnly Property powerCost As Integer
        Get
            Select Case tier
                Case 1 : Return 3
                Case 2 : Return 5
                Case 3 : Return 7
                Case Else
                    Debug.Print("Invalid threat tier.")
                    Return 0
            End Select
        End Get
    End Property
    Friend Property location As travelLocation
    Friend ReadOnly Property shard As shard
        Get
            Return location.shard
        End Get
    End Property
    Friend Property activeEffects As New List(Of modifier)
    Friend Property treasure As New List(Of item)

    Public Sub New()
        For n = 1 To 3
            armyTemplate.Add(n, Nothing)
        Next
    End Sub
    Friend Sub briefConsoleReport(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent) & prefix
        Console.WriteLine(name)
    End Sub
    Friend Shared Function threatFileget(aID As Integer) As threat
        Dim pathname As String = constants.pathnameThreat
        If authenticator.checkHash(pathname) = False Then
            Debug.Print("Authentication failed.")
            Return Nothing
        End If

        Dim total As List(Of String()) = csvFileget(pathname)
        For Each line In total
            If CInt(line(0)) = aID Then
                Dim n As New rollingCounter(0)
                Dim threat As New threat
                With threat
                    .id = CInt(line(n.Tick))
                    .name = line(n.Tick)

                    Dim evolutionStr As String() = line(n.Tick).Split(";")
                    .evolution(0) = .name
                    .evolution(1) = evolutionStr(0)
                    .evolution(2) = evolutionStr(1)

                    For p = 0 To 2
                        .armyTemplate(p) = army.getArmyFromString(line(n.Tick), p)
                    Next
                    .armyTimerOriginal = CInt(line(n.Tick))
                    .armyTimer = .armyTimerOriginal

                    For i = 1 To 5
                        Dim challengeID As Integer = (aID * 5) + i
                        Dim challenge As challenge = challenge.challengeFileget(CInt(line(n.Tick)))
                        If challenge Is Nothing = False Then threat.challenges.Add(challenge)
                    Next

                    While n.Tick < line.Count AndAlso line(n.Last) <> ""
                        Dim modifier As New modifier(.activeEffects, .name, line(n.Last))
                        .activeEffects.Add(modifier)
                    End While
                End With
                Return threat
            End If
        Next
        Return Nothing
    End Function

    Friend Property armyTemplate As New Dictionary(Of Integer, army)
    Friend Property armyTimer As Integer
    Friend Property armyTimerOriginal As Integer
    Friend Sub buildArmy(aDestination As travelLocation)
        Dim settlement As settlement = CType(aDestination, settlement)
        report.Add("The " & name & " raises an army against " & aDestination.name & "!", reportQueueType.ArmyRaised, settlement.empire)

        Dim newArmy As New army(armyTemplate(tier))
        shard.world.armies.Add(newArmy)
        With newArmy
            .threat = Me
            .empire = world.NatureRedInToothAndClaw

            .teleportTo(location)
            .home = location
            .moveTo(aDestination)
        End With

        'set armyTimer to some arbitrary high number so that it will stop spawning new armies whilst the current army is on the march
        armyTimer = 1000
    End Sub
    Friend Sub disbandArmy(ByRef army As army)
        report.Add("The " & name & " disbands " & army.name & ".", reportQueueType.ArmyDisbanded, army.empire)

        'build list of empires who have settlement on shard, then warn them about threat
        Dim empireList As New List(Of empire)
        For Each settlement In shard.getSettlements
            If empireList.Contains(settlement.empire) = False Then empireList.Add(settlement.empire)
        Next
        For Each empire In empireList
            report.Add("The " & name & " will raise a new army in " & armyTimerOriginal & " turn(s).", reportQueueType.ThreatArmyWarning, empire)
        Next

        'actual disband
        shard.world.armies.Remove(army)
        army = Nothing

        'reset armyTimer
        armyTimer = armyTimerOriginal
    End Sub


    Friend Property challenges As New List(Of challenge)
    Friend Sub challenge(hero As hero)
        Dim heroShard As shard = hero.location.shard
        If heroShard.Equals(shard) = False Then
            Debug.Print("Hero not at shard, cannot challenge.")
            Exit Sub
        End If


        'refresh hero
        If hero.mustRefresh = True Then hero.forceRefresh()


        'build actual challenges based on tier
        Dim actualChallenges As New Queue(Of challenge)
        Select Case tier
            Case 1
                actualChallenges.Enqueue(challenges(0))
                actualChallenges.Enqueue(challenges(1))
                actualChallenges.Enqueue(challenges(3))

            Case 2
                For n = 0 To 3
                    actualChallenges.Enqueue(challenges(n))
                Next

            Case 3
                For n = 0 To 4
                    actualChallenges.Enqueue(challenges(n))
                Next

            Case Else
                Debug.Print("Tier not recognised for challenge.")
                Exit Sub
        End Select


        'display text
        Console.WriteLine(hero.nameAndTitle & " arrives at " & location.name & " and challenges the " & name & "!")
        Console.WriteLine()


        'resolve challenges
        While actualChallenges.Count > 0
            Dim challenge As challenge = actualChallenges.Dequeue
            Console.WriteLine()
            challenge.consoleReport(0)
            Console.WriteLine()

            Dim choice As Integer = menu.getNumInput(0, 1, 3, "> ")
            choice -= 1
            Console.WriteLine()

            Select Case challenge.challenge(hero, choice, 0)
                Case challengeConsequence.Success           'do nothing
                Case challengeConsequence.Death
                    Console.WriteLine()
                    Console.WriteLine(hero.nameAndTitle & " was slain!")
                    hero.kill()
                    Console.ReadKey()
                    Exit Sub

                Case challengeConsequence.ReturnHome
                    Console.WriteLine()
                    hero.questReturnHome()
                    report.DisplayImmediateReports(0, hero.empire)
                    Exit Sub
            End Select
        End While


        'success!
        'remove activeEffect modifiers
        For Each modifier In activeEffects
            shard.removeShardModifier(modifier)
        Next


        'remove threat
        location.setThreat(Nothing)


        'claim treasure
        For Each reward In treasure
            hero.addEquipment(reward)
        Next
    End Sub


    Friend Sub tick()
        If armyTemplate(tier) Is Nothing = False AndAlso armyTimerOriginal > 0 Then
            If armyTimer <= 0 Then
                Dim destinations As List(Of settlement) = shard.getSettlements
                Dim destination As settlement = destinations(rng.Next(destinations.Count))
                buildArmy(destination)
            Else
                armyTimer -= 1
            End If
        End If
    End Sub
End Class
