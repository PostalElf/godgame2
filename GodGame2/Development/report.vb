Public Class report
    Friend Property text As String
    Friend Property reportQueueType As reportQueueType
    Friend Property textColour As ConsoleColor
    Friend Sub consoleReport(indent As Integer)
        If reportQueueType = reportQueueType.Blank Then
            Console.WriteLine()
        Else
            Dim ind As String = vbSpace(indent)
            Console.ForegroundColor = textColour
            Console.WriteLine(ind & text)
            Console.ResetColor()
        End If
    End Sub


    Private Shared Property replists As New Dictionary(Of empire, Queue(Of report))
    Private Shared Property immediateReplists As New Dictionary(Of empire, Queue(Of report))
    Private Shared Property tempReplist As New Queue(Of report)
    Friend Shared Sub AddEmpire(aEmpire As empire)
        If replists.ContainsKey(aEmpire) = False Then replists.Add(aEmpire, New Queue(Of report))
        If immediateReplists.ContainsKey(aEmpire) = False Then immediateReplists.Add(aEmpire, New Queue(Of report))
    End Sub


    Friend Shared Sub Add(aText As String, aReportQueueType As reportQueueType, aEmpire As empire, Optional aTextColour As ConsoleColor = Nothing)
        Dim report As New report
        report.text = aText
        report.reportQueueType = aReportQueueType
        report.textColour = getTextColour(aTextColour, aReportQueueType)

        If aEmpire Is Nothing Then
            For Each kvp In replists
                replists(kvp.Key).Enqueue(report)
            Next
        Else
            replists(aEmpire).Enqueue(report)
        End If
        tempReplist.Enqueue(report)
    End Sub
    Friend Shared Sub AddAndImmediateReport(aText As String, aReportQueueType As reportQueueType, aEmpire As empire, Optional aTextColour As ConsoleColor = Nothing)
        Dim report As New report
        report.text = aText
        report.reportQueueType = aReportQueueType
        report.textColour = getTextColour(aTextColour, aReportQueueType)

        If aEmpire Is Nothing Then
            For Each kvp In replists
                replists(kvp.Key).Enqueue(report)
            Next
            For Each kvp In immediateReplists
                immediateReplists(kvp.Key).Enqueue(report)
            Next
        Else
            replists(aEmpire).Enqueue(report)
            immediateReplists(aEmpire).Enqueue(report)
        End If
        tempReplist.Enqueue(report)
    End Sub
    Friend Shared Sub ImmediateReport(aText As String, aReportQueueType As reportQueueType, aEmpire As empire, Optional aTextColour As ConsoleColor = Nothing)
        Dim report As New report
        report.text = aText
        report.reportQueueType = aReportQueueType
        report.textColour = getTextColour(aTextColour, aReportQueueType)

        If aEmpire Is Nothing Then
            For Each kvp In immediateReplists
                immediateReplists(kvp.Key).Enqueue(report)
            Next
        Else
            immediateReplists(aEmpire).Enqueue(report)
        End If
    End Sub
    Friend Shared Sub DisplayImmediateReports(indent As Integer, aEmpire As empire)
        If aEmpire Is Nothing = True Then
            For Each kvp In immediateReplists
                While immediateReplists(kvp.Key).Count > 0
                    immediateReplists(kvp.Key).Dequeue.consoleReport(indent)
                End While
            Next

        Else

            If immediateReplists.ContainsKey(aEmpire) = False Then Exit Sub
            While immediateReplists(aEmpire).Count > 0
                immediateReplists(aEmpire).Dequeue.consoleReport(indent)
            End While
        End If

        Console.ReadKey()
    End Sub
    Friend Shared Sub DisplayTempReports(indent As Integer)
        While tempReplist.Count > 0
            tempReplist.Dequeue.consoleReport(indent)
        End While
    End Sub
    Friend Shared Sub fullConsoleReport(indent As Integer, aEmpire As empire)
        If aEmpire Is Nothing = True Then
            For Each kvp In replists
                Dim reports As New Queue(Of report)(replists(kvp.Key))
                While reports.Count > 0
                    reports.Dequeue.consoleReport(indent)
                End While
            Next

        Else

            Dim reports As New Queue(Of report)(replists(aEmpire))
            While reports.Count > 0
                reports.Dequeue.consoleReport(indent)
            End While
        End If
    End Sub

    Private Shared Function getTextColour(aTextColour As ConsoleColor, aReportQueueType As reportQueueType) As ConsoleColor
        If aTextColour = Nothing Then
            'default to colour based on reportQueueType
            Select Case aReportQueueType
                Case reportQueueType.Priority : Return ConsoleColor.Red
                Case reportQueueType.HeroLeavesLocation : Return ConsoleColor.DarkCyan
                Case reportQueueType.HeroArrivesLocation : Return ConsoleColor.DarkCyan
                Case reportQueueType.HeroModifierExpire : Return ConsoleColor.DarkGray
                Case reportQueueType.HeroDeath : Return ConsoleColor.Red
                Case reportQueueType.HeroQuestSuccess : Return ConsoleColor.DarkCyan
                Case reportQueueType.HeroQuestFailReturnHome : Return ConsoleColor.DarkCyan

                Case reportQueueType.ShardModifierNew : Return ConsoleColor.DarkGray
                Case reportQueueType.ShardModifierExpire : Return ConsoleColor.DarkGray
                Case reportQueueType.WorldModifierNew : Return ConsoleColor.DarkGray
                Case reportQueueType.WorldModifierExpire : Return ConsoleColor.DarkGray

                Case reportQueueType.SettlementNew : Return ConsoleColor.Yellow
                Case reportQueueType.SettlementDestroyed : Return ConsoleColor.Red
                Case reportQueueType.SettlementWealthTick : Return ConsoleColor.Gray
                Case reportQueueType.SettlementStarving : Return ConsoleColor.Red
                Case reportQueueType.SettlementGrowth : Return ConsoleColor.Green
                Case reportQueueType.SettlementBuildingProjectBegin : Return ConsoleColor.DarkGreen
                Case reportQueueType.SettlementBuildingProjectHalted : Return ConsoleColor.DarkGreen
                Case reportQueueType.SettlementBuildingProjectCompleted : Return ConsoleColor.DarkGreen
                Case reportQueueType.SettlementLowPublicOrder : Return ConsoleColor.DarkRed
                Case reportQueueType.SettlementCitizenLost : Return ConsoleColor.Red
                Case reportQueueType.SettlementRebel : Return ConsoleColor.Red
                Case reportQueueType.SettlementGoodNew : Return ConsoleColor.Yellow
                Case reportQueueType.SettlementModifierNew : Return ConsoleColor.DarkGray
                Case reportQueueType.SettlementModifierExpire : Return ConsoleColor.DarkGray
                Case reportQueueType.SettlementTradeNew : Return ConsoleColor.DarkGreen
                Case reportQueueType.SettlementEnshrineArtefact : Return ConsoleColor.DarkGreen
                Case reportQueueType.SettlementTerraformCompleted : Return ConsoleColor.DarkGreen
                Case reportQueueType.ArmyAttack : Return ConsoleColor.DarkYellow
                Case reportQueueType.SettlementDamaged : Return ConsoleColor.DarkYellow

                Case reportQueueType.ArmyRaised : Return ConsoleColor.DarkYellow
                Case reportQueueType.ArmyDisbanded : Return ConsoleColor.DarkYellow
                Case GodGame.reportQueueType.ArmyTribute : Return ConsoleColor.DarkYellow
                Case reportQueueType.ArmyAttackFailed : Return ConsoleColor.DarkYellow
                Case reportQueueType.ArmyAttackCasualty : Return ConsoleColor.Red
                Case reportQueueType.ArmyLeaves : Return ConsoleColor.DarkYellow
                Case reportQueueType.ArmyArrives : Return ConsoleColor.DarkYellow
                Case reportQueueType.ThreatNew : Return ConsoleColor.DarkCyan
                Case reportQueueType.ThreatDestroyed : Return ConsoleColor.DarkCyan
                Case reportQueueType.ThreatArmyWarning : Return ConsoleColor.DarkCyan

                Case reportQueueType.EmpireResearchBegin : Return ConsoleColor.Green
                Case reportQueueType.EmpireResearchStop : Return ConsoleColor.Green
                Case reportQueueType.EmpireResearchComplete : Return ConsoleColor.Green
                Case reportQueueType.EmpireModifierNew : Return ConsoleColor.DarkGray
                Case reportQueueType.EmpireModifierExpire : Return ConsoleColor.DarkGray
                Case reportQueueType.EmpireZeitgeistNew : Return ConsoleColor.Green
                Case reportQueueType.EmpireBuildingUnlock : Return ConsoleColor.DarkGreen
                Case reportQueueType.EmpireGoodUnlock : Return ConsoleColor.Yellow
                Case reportQueueType.EmpireZeitgeistUnlock : Return ConsoleColor.DarkGreen
                Case reportQueueType.EmpirePhilosophyUnlock : Return ConsoleColor.DarkGreen
                Case reportQueueType.GodDiceRoll : Return ConsoleColor.White
                Case Else : Return ConsoleColor.Gray
            End Select
        Else
            Return aTextColour
        End If
    End Function
    Friend Shared Sub Clear(aEmpire As empire)
        replists(aEmpire).Clear()
        immediateReplists(aEmpire).Clear()
        tempReplist.Clear()
    End Sub
End Class


Public Enum reportQueueType
    Blank

    HeroLeavesLocation
    HeroArrivesLocation
    HeroModifierExpire
    HeroDeath
    HeroQuestSuccess
    HeroQuestFailReturnHome

    ShardModifierNew
    ShardModifierExpire
    WorldModifierNew
    WorldModifierExpire

    SettlementNew
    SettlementDestroyed
    SettlementIncome
    SettlementWealthTick
    SettlementStarving
    SettlementGrowth
    SettlementCitizenLost
    SettlementLowPublicOrder
    SettlementRebel
    SettlementGoodNew
    SettlementModifierNew
    SettlementModifierExpire
    SettlementBuildingProjectHalted
    SettlementBuildingProjectBegin
    SettlementBuildingProjectCompleted
    SettlementTradeNew
    SettlementEnshrineArtefact
    SettlementTerraformCompleted
    ArmyAttack
    SettlementDamaged

    ArmyAttackFailed
    ArmyAttackCasualty
    ArmyRaised
    ArmyDisbanded
    ArmyTribute
    ArmyLeaves
    ArmyArrives
    ThreatNew
    ThreatDestroyed
    ThreatArmyWarning

    EmpireResearchBegin
    EmpireResearchStop
    EmpireResearchComplete
    EmpireIncomeThisTurn
    EmpireIncome
    EmpireModifierNew
    EmpireModifierExpire
    EmpireZeitgeistNew
    EmpireBuildingUnlock
    EmpireGoodUnlock
    EmpireZeitgeistUnlock
    EmpirePhilosophyUnlock

    GodDiceRoll
    GodDomainNew

    Priority
End Enum
