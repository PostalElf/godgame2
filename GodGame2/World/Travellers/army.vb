Public Class army
    Implements traveller
    Friend Property name As String Implements traveller.name
    Public Sub New()
    End Sub
    Public Sub New(armyTemplate As army)
        With armyTemplate
            name = .name
            warriors = .warriors
            warriorPower = .warriorPower
            warmages = .warmages
            warmagePower = .warmagePower

            empire = .empire
            home = .home

            travelSpeed = .travelSpeed
        End With
    End Sub
    Public Overrides Function ToString() As String
        Return name
    End Function
    Friend Sub consoleReport(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent) & prefix
        Dim indd As String = vbSpace(indent + 1)
        Dim inddd As String = vbSpace(indent + 2)

        Console.WriteLine(ind & name & ", " & empire.name)

        Console.WriteLine(indd & "└ " & locationReport)

        Console.WriteLine(indd & "└ Army Strength: " & attackPower)
        If hero Is Nothing = False Then Console.WriteLine(inddd & "└ " & hero.nameAndTitle & ": +" & heroPower)
        If warriors > 0 Then Console.WriteLine(inddd & "└ Warriors: " & warriors & " x " & warriorPower)
        If warmages > 0 Then Console.WriteLine(inddd & "└ Warmages: " & warmages & " x " & warmagePower)

        If wealth > 0 Then Console.WriteLine(indd & "└ Bounty: " & wealth)
    End Sub
    Friend Shared Function getArmyFromString(rawStr As String, tier As Integer) As army
        'army name-1/2/3x10-1/2/3x10

        Dim split As String() = rawStr.Split("-")
        Dim n As New rollingCounter(0)
        Dim army As New army
        With army
            .name = split(n.Tick)
            Dim warriorXpower As String() = split(n.Tick).Split("x")
            Dim warriors As String() = warriorXpower(0).Split("/")
            .warriors = CInt(warriors(tier - 1))
            .warriorPower = CInt(warriorXpower(1))

            Dim warmageXpower As String() = split(n.Tick).Split("x")
            Dim warmages As String() = warmageXpower(0).Split("/")
            .warmages = CInt(warmages(tier - 1))
            .warmagePower = CInt(warmageXpower(1))
        End With
        Return army
    End Function

    Friend ReadOnly Property isAI As Boolean
        Get
            Return empire.isAI
        End Get
    End Property
    Friend Property threat As threat = Nothing

    Friend Property warriors As Integer
    Friend Property warriorPower As Integer
    Friend Property warmages As Integer
    Friend Property warmagePower As Integer
    Friend Property hero As hero
    Private ReadOnly Property heroPower As Integer
        Get
            If hero Is Nothing Then Return 0 Else Return hero.highestHeroSkill.Value
        End Get
    End Property
    Private ReadOnly Property attackPower As Integer
        Get
            Return (warriors * warriorPower) + (warmages * warmagePower) + heroPower
        End Get
    End Property

    Friend Property empire As empire = Nothing Implements traveller.empire
    Friend Property home As travelLocation = Nothing
    Friend Property wealth As Integer

    Private Property travelLocation As travelLocation = Nothing Implements traveller.travelLocation
    Private Property travelDestination As travelLocation Implements traveller.travelDestination
    Private Property travelCost As Integer Implements traveller.travelCost
    Private Property travelProgress As Integer Implements traveller.travelProgress
    Private Property travelSpeed As Integer = 10 Implements traveller.travelSpeed
    Friend ReadOnly Property locationReport As String
        Get
            If travelLocation Is Nothing = True Then
                Return "Travelling To: " & travelDestination.name & " on " & travelDestination.shard.name & " Shard (" & travelProgress & "/" & travelCost & ", +" & travelSpeed & ")"
            Else
                Return "Location: " & travelLocation.name & " on " & travelLocation.shard.name & " Shard"
            End If
        End Get
    End Property
    Friend Sub moveTo(aDestination As travelLocation) Implements traveller.moveTo
        If travelLocation Is Nothing Then
            Debug.Print("Army is still in transit.")
            Exit Sub
        End If

        travelCost = travelLocation.getDistanceTo(aDestination)
        travelProgress = 0
        travelLocation = Nothing
        travelDestination = aDestination

        report.AddAndImmediateReport(name & " leaves for " & aDestination.name & " on " & aDestination.shard.name & " Shard.", reportQueueType.ArmyLeaves, empire)
    End Sub
    Friend Sub teleportTo(aDestination As travelLocation) Implements traveller.teleportTo
        travelLocation = aDestination
        travelDestination = Nothing
        travelProgress = 0
        travelCost = 0
    End Sub
    Private Function getTravelCost(origin As travelLocation, destination As travelLocation) As Integer Implements traveller.getTravelCost
        Return origin.getDistanceTo(destination)
    End Function
    Friend Sub tick()
        'movement
        If travelLocation Is Nothing Then
            travelProgress += travelSpeed
            If travelProgress >= travelCost Then
                teleportTo(travelDestination)

                If travelLocation.Equals(home) Then
                    'back home; check to see who should disband army
                    If TypeOf home Is settlement Then
                        'settlement disbands
                        Dim s As settlement = CType(home, settlement)
                        s.disbandArmy(Me)
                    ElseIf threat Is Nothing = False Then
                        'threat disbands
                        threat.disbandArmy(Me)
                    Else
                        'throw exception, unrecognised location
                        Debug.Print("Unrecognised travel location for army.")
                        Exit Sub
                    End If
                Else
                    'arrived at enemy destination, attack or go home
                    If travelLocation.isAttackable(empire) = True Then attack() Else moveTo(home)
                End If
            End If
        End If
    End Sub

    Private Sub attack()
        If travelLocation Is Nothing Then Exit Sub

        If TypeOf travelLocation Is settlement Then
            Dim settlement As settlement = CType(travelLocation, settlement)
            Dim defenceRoll As Integer = rollDice("3d6") * 10
            Dim defence As Integer = defenceRoll + settlement.totalDefence
            Dim attackRoll As Integer = rollDice("3d6") * 10
            Dim attack As Integer = attackRoll + attackPower

            'report for defender
            report.AddAndImmediateReport(settlement.name & " was attacked by " & name & "!", reportQueueType.ArmyAttack, settlement.empire)
            report.ImmediateReport(settlement.name & " rolls " & defenceRoll & " + " & settlement.totalDefence & " = " & defence, reportQueueType.ArmyAttack, settlement.empire)
            report.ImmediateReport(name & " rolls " & attackRoll & " + " & attackPower & " = " & attack, reportQueueType.ArmyAttack, settlement.empire)

            'report for attacker
            report.AddAndImmediateReport(name & " attacks " & settlement.name & "!", reportQueueType.ArmyAttack, empire)
            report.ImmediateReport(name & " rolls " & attackRoll & " + " & attackPower & " = " & attack, reportQueueType.ArmyAttack, empire)
            report.ImmediateReport(settlement.name & " rolls " & defenceRoll & " + " & settlement.totalDefence & " = " & defence, reportQueueType.ArmyAttack, empire)


            Dim attackSuccessful As Boolean
            If attack > defence Then
                'attack successful
                Dim damage As Integer = CInt((attack - defence) / 30)
                damage = constrain(damage, 1, 10)
                settlement.addDamage(damage)

                wealth = constrain(damage * 50, 0, 500)

                'check for destruction
                If settlement.damagedTicks >= settlement.damageThreshold Then
                    'destroyed
                    report.AddAndImmediateReport(name & " razes " & settlement.name & " to the ground!", reportQueueType.SettlementDestroyed, empire)
                    settlement.destroy()


                Else
                    'not destroyed; report damage
                    report.AddAndImmediateReport(settlement.name & " was pillaged and will take " & damage & " turn(s) to recover.", reportQueueType.SettlementDamaged, settlement.empire)
                    report.AddAndImmediateReport(name & " successfully pillaged " & wealth & " Wealth from " & settlement.name & ".", reportQueueType.SettlementDamaged, empire)

                    '50% chance to lose a random citizen
                    If percentRoll(50) = True Then
                        Dim availablePopsegs As New List(Of popseg)
                        For Each kvp In settlement.population
                            If kvp.Key <> popseg.Warmages AndAlso kvp.Key <> popseg.Warriors Then
                                For n = 1 To kvp.Value
                                    availablePopsegs.Add(kvp.Key)
                                Next
                            End If
                        Next
                        Dim roll As Integer = rng.Next(availablePopsegs.Count)
                        Dim rPopseg As popseg = availablePopsegs(roll)

                        settlement.removePopulation(rPopseg)
                        report.AddAndImmediateReport(settlement.name & " loses a " & stripS(rPopseg.ToString) & " to the attack.", reportQueueType.SettlementCitizenLost, settlement.empire)
                    End If
                End If


                'report attack successful
                attackSuccessful = True
            Else
                'attack failed
                report.AddAndImmediateReport(settlement.name & " successfully repelled the attack!", reportQueueType.ArmyAttackFailed, settlement.empire)
                report.AddAndImmediateReport(name & " was repelled by the settlement.", reportQueueType.ArmyAttackFailed, empire)
                attackSuccessful = False
            End If


            'determine casualties if non-ai army
            If isAI = False Then
                Dim casualtyPercentage As Integer
                If attackSuccessful = True Then casualtyPercentage = 10 Else casualtyPercentage = 30
                If percentRoll(casualtyPercentage) = True Then
                    If warmages > 0 AndAlso warriors > 0 Then
                        If coinFlip() = True Then warriorCasualty() Else warmageCasualty()
                    ElseIf warmages > 0 Then
                        warmageCasualty()
                    ElseIf warriors > 0 Then
                        warriorCasualty()
                    End If
                End If
            End If


            'send home
            moveTo(home)


            'display reports for player
            report.DisplayImmediateReports(0, world.playerEmpire)
        End If
    End Sub
    Private Sub warmageCasualty()
        warmages -= 1
        report.AddAndImmediateReport(name & " loses a Warmage in the attack.", reportQueueType.ArmyAttackCasualty, empire)
    End Sub
    Private Sub warriorCasualty()
        warriors -= 1
        report.AddAndImmediateReport(name & " loses a Warrior in the attack.", reportQueueType.ArmyAttackCasualty, empire)
    End Sub
End Class
