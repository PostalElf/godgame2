Public Class empire
    Public Sub New()
        'initialise resources & resourceIncomeThisTick
        For Each resource In constants.resourceArray
            resources.Add(resource, 0)
            resourceIncomeThisTick.Add(resource, 0)
        Next

        'initialise tech
        initTechLists(resource.Science)
        addAvailableTechTier(resource.Science, 0)

        initTechLists(resource.Culture)
        addAvailableTechTier(resource.Culture, 0)

        'add self to report dictionary
        report.AddEmpire(Me)
    End Sub
    Friend Sub secondaryInitialisation()
        'call from worldBuilder once empire is built

        'adds initial tier 0 tech
        techInProgress(resource.Science) = techAvailable(resource.Science)(0)
        acquireTech(resource.Science)
        techInProgress(resource.Culture) = techAvailable(resource.Culture)(0)
        acquireTech(resource.Culture)
        Dim zgt As zeitgeist = zeitgeistAvailable(0)
        adoptZeitgeist(zgt)
    End Sub
    Public Overrides Function ToString() As String
        Return "The " & name
    End Function
    Friend Property name As String
    Friend Property world As world
    Friend Property god As god
    Friend Sub tick()
        'reset thisTick counters 
        For Each resourceType In constants.resourceArray
            resourceIncomeThisTick(resourceType) = 0
        Next


        'tick settlement: income + settlement modifiers
        For n = pSettlements.Count - 1 To 0 Step -1
            Dim settlement As settlement = pSettlements(n)

            'refresh and tick
            If mustRefresh = True Then settlement.mustRefresh = True
            settlement.tick()

            'check for destruction (after tick because tick may deal damage)
            If settlement.damagedTicks >= settlement.damageThreshold Then settlement.destroy()
        Next
        For Each modifier In empireModifiers
            applyModifierEffect(modifier)
        Next
        reportIncome()
        mustRefresh = False


        'tick empire modifiers
        If tickModifierList(pEmpireModifiers, reportQueueType.EmpireModifierExpire) = True Then
            mustRefresh = True
        End If
        If tickModifierList(pEmpireHeroModifiers, reportQueueType.EmpireModifierExpire) = True Then
            For Each hero In pHeroes
                hero.mustRefresh = True
            Next
        End If



        'apply mustRefresh to settlements if necessary from modifier expiry
        If mustRefresh = True Then
            For Each settlement In pSettlements
                settlement.mustRefresh = True
            Next
            mustRefresh = False
        End If


        'tick tech
        For resource As resource = resource.Science To resource.Culture
            If techInProgress(resource) Is Nothing Then researchNextTech(resource)
            If techInProgress(resource) Is Nothing = False Then
                Dim income As Integer = resourceIncomeThisTick(resource)
                If income <= minTechIncome Then income = minTechIncome
                techInProgress(resource).tick(income)
                If techInProgress(resource).completed = True Then acquireTech(resource)
            End If
        Next


        'tick heroes
        For Each hero In pHeroes
            hero.tick()
        Next


        'spacer
        report.Add("", reportQueueType.Blank, Me)
    End Sub
    Private Function tickModifierList(ByRef modifierList As List(Of modifier), reportQueueType As reportQueueType) As Boolean
        If modifierList.Count = 0 Then Return False

        Dim total As Boolean = False
        For n = modifierList.Count - 1 To 0 Step -1
            Dim modifier As modifier = modifierList(n)
            modifier.tick()
            If modifier.duration = 0 Then
                modifierList.Remove(modifier)
                report.Add("'" & modifier.name & "' for the " & name & " has expired.", reportQueueType, Me)
                total = True
            End If
        Next
        Return total
    End Function
    Private Sub applyModifierEffect(modifier As modifier)
        Select Case modifier.quality
            Case modifierQuality.EmpireIncome : addResource(modifier.resource, modifier.value(Me))
        End Select
    End Sub
    Friend Sub consoleReport(indent As Integer)
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)
        Dim inddd As String = vbSpace(indent + 2)
        Dim indddd As String = vbSpace(indent + 3)

        Console.WriteLine(ind & name)

        'report god
        Console.WriteLine(indd & "└ Worships: " & god.name)
        Console.WriteLine(inddd & fakeTab("└ Faith: ", 11) & resources(resource.Faith).ToString("N0"))
        Console.WriteLine(inddd & fakeTab("└ Fate: ", 11) & god.power(divinePower.Fate))
        Console.WriteLine(inddd & fakeTab("└ Destiny: ", 11) & god.power(divinePower.Destiny))

        'report zeitgeist
        If zeitgeist Is Nothing Then
            Console.WriteLine(indd & "└ Zeitgeist: Nothing")
        Else
            Console.WriteLine(indd & "└ Zeitgeist: " & zeitgeist.name)
            Console.WriteLine(inddd & "└ Ideology: " & zeitgeist.maxPhilosophyString)
            If zeitgeist.modifiers.Count > 0 Then
                Console.WriteLine(inddd & "└ Modifiers:")
                For Each modifier In zeitgeist.modifiers
                    Console.WriteLine(indddd & "└ " & modifier.ToString)
                Next
            End If
            If zeitgeistPhilosophies.Count > 0 Then
                Console.WriteLine(inddd & "└ Philosophies:")
                For Each philosophy In zeitgeistPhilosophies
                    philosophy.consoleReport(indent + 3, "└ ")
                Next
            End If
        End If

        'report tech
        For resource As resource = resource.Science To resource.Culture
            Dim descriptor As String
            If resource = resource.Science Then descriptor = "└ Science Research: " Else descriptor = "└ Culture Research: "

            If techInProgress(resource) Is Nothing Then
                Console.WriteLine(indd & descriptor & "Nothing")
            Else
                Console.WriteLine(indd & descriptor & techInProgress(resource).ToString)
            End If
        Next

        'report modifiers
        Dim printedModifier As Boolean = False
        Console.WriteLine(indd & "└ Modifiers:")
        For Each modifier In empireModifiers
            If modifier.mustHide = False Then
                modifier.consoleReport(indent + 2, "└ ")
                printedModifier = True
            End If
        Next
        For Each modifier In empireHeroModifiers
            If modifier.mustHide = False Then
                modifier.consoleReport(indent + 2, "└ ")
                printedModifier = True
            End If
        Next
        If printedModifier = False Then
            Console.WriteLine(vbSpace(indent + 2) & "└ Nothing")
        End If
        Console.WriteLine()

        ''report settlements
        'Console.WriteLine()
        'For Each settlement In pSettlements
        '    If mustRefresh = True Then settlement.mustRefresh = True
        '    settlement.consoleReport(indent + 1)
        '    Console.WriteLine()
        'Next

        ''report heroes
        'Console.WriteLine()
        'If heroes.Count > 0 Then
        '    Console.WriteLine(indd & "Heroes:")
        '    For Each hero In heroes
        '        hero.consoleReport(indent + 2)
        '        Console.WriteLine()
        '    Next
        'End If
    End Sub
    Friend Sub briefConsoleReport(indent As Integer)
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)
        Dim inddd As String = vbSpace(indent + 2)

        Console.WriteLine(ind & name)

        Console.WriteLine(indd & "└ Worships: " & god.name)
        Console.WriteLine(inddd & fakeTab("└ Faith: ", 11) & resources(resource.Faith).ToString("N0"))
        Console.WriteLine(inddd & fakeTab("└ Fate: ", 11) & god.power(divinePower.Fate))
        Console.WriteLine(inddd & fakeTab("└ Destiny: ", 11) & god.power(divinePower.Destiny))

        Console.WriteLine(indd & "└ Zeitgeist: " & zeitgeist.name)

        For resource As resource = resource.Science To resource.Culture
            Dim descriptor As String
            If resource = resource.Science Then descriptor = "└ Science Research: " Else descriptor = "└ Culture Research: "

            If techInProgress(resource) Is Nothing Then
                Console.WriteLine(indd & descriptor & "Nothing")
            Else
                Console.WriteLine(indd & descriptor & techInProgress(resource).ToString)
            End If
        Next

        Console.WriteLine(indd & "└ Settlements:")
        For Each settlement In settlements
            settlement.briefConsoleReport(indent + 2, "└ ")
        Next
    End Sub



    'AI
    Friend Property isAI As Boolean = False



    'empire-wide modifiers
    Private pEmpireModifiers As New List(Of modifier)
    Friend ReadOnly Property empireModifiers As List(Of modifier)
        Get
            Return pEmpireModifiers
        End Get
    End Property
    Private Sub addEmpireModifier(modifier As modifier)
        If pEmpireModifiers.Contains(modifier) Then
            Debug.Print(name & " already contains modifier.")
            Exit Sub
        End If

        Select Case modifier.quality
            Case modifierQuality.GoodUnlock
                addAvailableGood(modifier.good)
                modifier.mustHide = True
            Case modifierQuality.BuildingUnlock
                addAvailableBuilding(modifier.building)
                modifier.mustHide = True
            Case modifierQuality.ZeitgeistUnlock
                addAvailableZeitgeist(modifier.zeitgeist)
                modifier.mustHide = True
            Case modifierQuality.PhilosophyUnlock
                addAvailablePhilosophy(modifier.philosophy)
                modifier.mustHide = True
            Case Else
                pEmpireModifiers.Add(modifier)
                modifier.parent = pEmpireModifiers

                report.Add("The " & name & " gains the '" & modifier.name & "' modifier.", reportQueueType.EmpireModifierNew, Me)
                mustRefresh = True
        End Select

    End Sub
    Private pEmpireHeroModifiers As New List(Of modifier)
    Friend ReadOnly Property empireHeroModifiers As List(Of modifier)
        Get
            Return pEmpireHeroModifiers
        End Get
    End Property
    Private Sub addEmpireHeroModifier(modifier As modifier)
        If pEmpireHeroModifiers.Contains(modifier) Then
            Debug.Print(name & " already contains modifier.")
            Exit Sub
        End If

        pEmpireHeroModifiers.Add(modifier)
        modifier.parent = pEmpireHeroModifiers
        For Each hero In pHeroes
            hero.mustRefresh = True
        Next
    End Sub
    Friend Sub addSortedModifier(modifier As modifier)
        Select Case modifier.quality
            Case 1 To 100 : addEmpireModifier(modifier)
            Case 101 To 200 : addEmpireHeroModifier(modifier)
            Case 201 To 300 : addEmpireModifier(modifier)
            Case 301 To 400 : addEmpireModifier(modifier)
        End Select
    End Sub
    Friend Sub removeSortedModifier(modifier As modifier)
        If pEmpireModifiers.Contains(modifier) Then pEmpireModifiers.Remove(modifier)
        If pEmpireHeroModifiers.Contains(modifier) Then pEmpireHeroModifiers.Remove(modifier)
    End Sub



    'resources & income
    Private pResources As New Dictionary(Of resource, Integer)
    Friend ReadOnly Property resources As Dictionary(Of resource, Integer)
        Get
            Return pResources
        End Get
    End Property
    Private resourceIncomeThisTick As New Dictionary(Of resource, Integer)
    Private pMustRefresh As Boolean = True
    Friend Property mustRefresh As Boolean
        Get
            Return pMustRefresh
        End Get
        Set(value As Boolean)
            If pMustRefresh = False AndAlso value = True Then
                For Each settlement In settlements
                    settlement.mustRefresh = True
                Next
                For Each hero In heroes
                    hero.mustRefresh = True
                Next
            End If
            pMustRefresh = value
        End Set
    End Property
    Friend Sub addResources(aResources As Dictionary(Of resource, Integer))
        For Each kvp As KeyValuePair(Of resource, Integer) In aResources
            addResource(kvp.Key, kvp.Value)
        Next
    End Sub
    Friend Sub addResource(resource As resource, value As Integer)
        If pResources.ContainsKey(resource) = False Then
            Debug.Print("Does not contain resource key.")
            Exit Sub
        End If

        pResources(resource) += value
        resourceIncomeThisTick(resource) += value
    End Sub
    Private Sub reportIncome()
        Dim fod As Integer = resourceIncomeThisTick(resource.Food)
        Dim fth As Integer = resourceIncomeThisTick(resource.Faith)
        Dim sci As Integer = resourceIncomeThisTick(resource.Science)
        Dim cul As Integer = resourceIncomeThisTick(resource.Culture)

        report.Add("Settlements in the " & name & " produced a total of " & fth & " faith, " & sci & " science & " & cul & " culture this turn.", reportQueueType.EmpireIncomeThisTurn, Me)
    End Sub



    'settlements
    Private Property pSettlements As New List(Of settlement)
    Friend ReadOnly Property settlements As List(Of settlement)
        Get
            Return pSettlements
        End Get
    End Property
    Private Property pAvailableBuildings As New List(Of building)
    Friend ReadOnly Property availableBuildings As List(Of building)
        Get
            Return pAvailableBuildings
        End Get
    End Property
    Friend Sub addSettlement(ByRef settlement As settlement)
        pSettlements.Add(settlement)
        settlement.empire = Me
    End Sub
    Friend Sub removeSettlement(ByRef settlement As settlement)
        If pSettlements.Contains(settlement) = False Then
            Debug.Print("Does not contain settlement.")
            Exit Sub
        End If

        pSettlements.Remove(settlement)
    End Sub
    Friend Sub addAvailableBuilding(building As building)
        If availableBuildings.Contains(building) Then
            Debug.Print("Already in empire.availablebuildings.")
            Exit Sub
        End If

        pAvailableBuildings.Add(building)
        For Each settlement In pSettlements
            settlement.addAvailableBuilding(building)
        Next
        report.Add("The " & name & " uncovers the blueprints for " & building.name & ".", reportQueueType.EmpireBuildingUnlock, Me)
    End Sub



    'goods
    Private Property pGoodsAvailable As New List(Of good)
    Friend ReadOnly Property goodsAvailable As List(Of good)
        Get
            Return pGoodsAvailable
        End Get
    End Property
    Friend Sub addAvailableGood(good As good)
        If pGoodsAvailable.Contains(good) Then
            Debug.Print("Already in goodsAvailable.")
            Exit Sub
        End If

        pGoodsAvailable.Add(good)
        report.Add("Settlements in the " & name & " may now begin trading and utilising " & good.ToString & ".", reportQueueType.EmpireGoodUnlock, Me)
    End Sub



    'tech
    Private Const minTechIncome As Integer = 1
    Private Property techAcquired As New Dictionary(Of resource, List(Of tech))
    Private Property techAvailable As New Dictionary(Of resource, List(Of tech))
    Friend ReadOnly Property availableTechs As Dictionary(Of resource, List(Of tech))
        Get
            Return techAvailable
        End Get
    End Property
    Private Property techInProgress As New Dictionary(Of resource, tech)
    Private Sub initTechLists(resource As resource)
        techAcquired.Add(resource, New List(Of tech))
        techAvailable.Add(resource, New List(Of tech))
        techInProgress.Add(resource, Nothing)
    End Sub
    Private Function researchNextTech(resource As resource) As tech
        If techAvailable(resource).Count = 0 Then Return Nothing

        Dim roll As Integer = rng.Next(techAvailable(resource).Count)
        Dim newTech As tech = techAvailable(resource)(roll)
        changeTech(resource, newTech)

        Return newTech
    End Function
    Friend Sub changeTech(resource As resource, newTech As tech)
        If techInProgress(resource) Is Nothing = False Then
            report.Add("The " & name & " stops research on '" & techInProgress(resource).name & "'.", reportQueueType.EmpireResearchStop, Me)
        End If

        techInProgress(resource) = newTech

        Dim descriptor As String
        If resource = resource.Culture Then descriptor = "cultural" Else descriptor = "scientific"
        report.Add("The " & name & " begins " & descriptor & " research on '" & newTech.name & "'.", reportQueueType.EmpireResearchBegin, Me)
    End Sub
    Private Sub acquireTech(resource As resource)
        If techInProgress.ContainsKey(resource) = False Then
            Debug.Print("Invalid resource for acquireTech.")
            Exit Sub
        End If


        'set variables based on resource
        Dim tAcquired As List(Of tech) = techAcquired(resource)
        Dim tAvailable As List(Of tech) = techAvailable(resource)
        Dim tInProgress As tech = techInProgress(resource)


        'remove from available and add to acquired
        tAvailable.Remove(tInProgress)
        tAcquired.Add(tInProgress)
        report.Add("The " & name & " completes research on '" & tInProgress.name & "'.", reportQueueType.EmpireResearchComplete, Me)


        'apply tech modifiers
        For Each modifier In tInProgress.modifiers
            addSortedModifier(modifier)
        Next


        'if keytech, add new tier of tech to available
        If tInProgress.isKeyTechTo <> 0 Then
            addAvailableTechTier(resource, tInProgress.isKeyTechTo)
        End If


        'set techInProgress to nothing
        techInProgress(resource) = Nothing
    End Sub
    Private Sub addAvailableTechTier(resource As resource, tier As Integer)
        Dim total As List(Of tech) = tech.techFileGet(resource, tier)
        techAvailable(resource).AddRange(total)
    End Sub



    'zeitgeist
    Private Property zeitgeist As zeitgeist = Nothing
    Private Property zeitgeistAvailable As New List(Of zeitgeist)
    Private Property zeitgeistMaxPhilosophy As New Dictionary(Of philosophyShape, Integer)
    Private Property zeitgeistPhilosophies As New List(Of philosophy)
    Private Property zeitgeistPhilosophiesAvailable As New List(Of philosophy)
    Friend ReadOnly Property availableZeitgeists As List(Of zeitgeist)
        Get
            Return zeitgeistAvailable
        End Get
    End Property
    Friend ReadOnly Property availablePhilosophies As List(Of philosophy)
        Get
            Return zeitgeistPhilosophiesAvailable
        End Get
    End Property
    Friend ReadOnly Property philosophies As List(Of philosophy)
        Get
            Return zeitgeistPhilosophies
        End Get
    End Property
    Private Sub addAvailableZeitgeist(aZeitgeist As zeitgeist)
        If zeitgeistAvailable.Contains(aZeitgeist) Then
            Debug.Print("Already contains zeitgeist.")
            Exit Sub
        End If

        zeitgeistAvailable.Add(aZeitgeist)
        report.Add("The " & name & " has been exposed to the ideas of '" & aZeitgeist.name & "'.", reportQueueType.EmpireZeitgeistUnlock, Me)
    End Sub
    Friend Sub addPhilosophy(philosophy As philosophy)
        checkAddPhilosophy(philosophy)

        zeitgeistPhilosophiesAvailable.Remove(philosophy)
        zeitgeistPhilosophies.Add(philosophy)
        zeitgeistMaxPhilosophy(philosophy.shape) -= 1
        For Each modifier In philosophy.modifiers
            addSortedModifier(modifier)
        Next

        mustRefresh = True
    End Sub
    Friend Sub removePhilosophy(philosophy As philosophy)
        checkRemovePhilosophy(philosophy)

        zeitgeistPhilosophies.Remove(philosophy)
        zeitgeistPhilosophiesAvailable.Add(philosophy)
        zeitgeistMaxPhilosophy(philosophy.shape) += 1
        For Each modifier In philosophy.modifiers
            removeSortedModifier(modifier)
        Next

        mustRefresh = True
    End Sub
    Private Sub addAvailablePhilosophy(philosophy As philosophy)
        If zeitgeistPhilosophiesAvailable.Contains(philosophy) Then
            Debug.Print("Already contains philosophy.")
            Exit Sub
        End If

        zeitgeistPhilosophiesAvailable.Add(philosophy)
        report.Add("The citizens of the " & name & " have been exposed to the philosophies of '" & philosophy.name & "'.", reportQueueType.EmpirePhilosophyUnlock, Me)
    End Sub
    Friend Sub adoptZeitgeist(aZeitgeist As zeitgeist)
        If zeitgeistAvailable.Contains(aZeitgeist) = False Then
            Debug.Print("Zeitgeist not in available list.")
            Exit Sub
        End If


        'remove previous zeitgeist
        If zeitgeist Is Nothing = False Then
            zeitgeistAvailable.Add(zeitgeist)
            zeitgeist = Nothing
        End If
        If zeitgeistPhilosophies.Count > 0 Then
            For n = zeitgeistPhilosophies.Count - 1 To 0 Step -1
                Dim philosophy As philosophy = zeitgeistPhilosophies(n)
                removePhilosophy(philosophy)
            Next
        End If


        'add new zeitgeist
        zeitgeistAvailable.Remove(aZeitgeist)
        zeitgeist = aZeitgeist
        zeitgeistMaxPhilosophy = New Dictionary(Of philosophyShape, Integer)(aZeitgeist.maxPhilosophy)


        'apply zeitgeist modifiers
        report.Add("The " & name & " embraces the ideologies of '" & zeitgeist.name & "'.", reportQueueType.EmpireZeitgeistNew, Me)
        For Each modifier In zeitgeist.modifiers
            modifier.mustHide = True
            addSortedModifier(modifier)
        Next

        mustRefresh = True
    End Sub
    Friend Function checkAddPhilosophy(philosophy As philosophy) As errorReport
        If zeitgeistPhilosophies.Contains(philosophy) Then
            Debug.Print("Already contains philosophy.")
            Return New errorReport("Philosophy already added.")
        ElseIf zeitgeistPhilosophiesAvailable.Contains(philosophy) = False Then
            Debug.Print("Not in available philosophies.")
            Return New errorReport("Invalid philosophy.")
        ElseIf zeitgeistMaxPhilosophy(philosophy.shape) - 1 < 0 Then
            Debug.Print("Already at max philosophies for " & philosophy.shape.ToString & ".")
            Return New errorReport("Already at max philosophies for " & philosophy.shape.ToString & ".")
        End If

        Return Nothing
    End Function
    Friend Function checkRemovePhilosophy(philosophy As philosophy) As errorReport
        If zeitgeistPhilosophies.Contains(philosophy) = False Then
            Debug.Print("Does not contain philosophy.")
            Return New errorReport("Invalid philosophy.")
        End If

        Return Nothing
    End Function



    'wars
    Private Property pHeroes As New List(Of hero)
    Friend ReadOnly Property heroes As List(Of hero)
        Get
            Return pHeroes
        End Get
    End Property
    Friend Sub addHero(hero As hero, settlement As settlement)
        If heroes.Contains(hero) Then
            Debug.Print("Empire already has hero.")
            Exit Sub
        End If

        pHeroes.Add(hero)
        hero.empire = Me
        hero.teleportTo(settlement)
        hero.forceRefresh()
    End Sub
    Friend Sub removeHero(hero As hero)
        heroes.Remove(hero)
        hero.empire = Nothing
    End Sub
End Class
