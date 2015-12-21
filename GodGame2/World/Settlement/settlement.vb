Public Class settlement
    Inherits travelLocation

    'generics
    Public Sub New(aEmpire As empire, aShard As shard)
        aEmpire.addSettlement(Me)
        aShard.addTravelLocation(Me, 0)

        'initialise population dictionary & set farmers to 1
        For Each popseg In constants.popsegArray
            pPopulation.Add(popseg, 0)
        Next
        pPopulation(popseg.Farmers) = 1

        refresh()
    End Sub
    Public Overrides Function ToString() As String
        Return name & " on " & shard.name
    End Function
    Friend Property empire As empire
    Friend Sub tick()
        'refresh
        If mustRefresh = True Then refresh()


        'make AI decisions before resolving rest of tick
        If isAI = True Then aiTick()


        'create copy of income and reduce it by public order
        Dim netIncome As New Dictionary(Of resource, Integer)(income)
        reduceIncomeByPublicOrder(netIncome)


        'if damaged, produce nothing and reduce damage by 1; otherwise, add income
        If pDamagedTicks > 0 Then
            pDamagedTicks -= 1
            report.Add(name & " was damaged and will not produce any income for " & damagedTicks & " more turn(s).", reportQueueType.SettlementIncome, empire)
        Else
            empire.addResources(netIncome)
            incomeReport(netIncome)
            addWealth(income(resource.Wealth))
        End If


        'growth
        Dim food As Integer = income(resource.Food)
        growth += food
        If growth < 0 Then
            'starving; set new growth at 75% of new growth limit
            growth = CInt(((populationSize - 1) * 10) * 0.75)
            starvePopulation()
        End If
        If mustNotGrow = True Then
            'do not grow
            If growth > growthTarget Then growth = growthTarget
        Else
            While growth >= growthTarget
                'grow
                growth -= growthTarget

                Dim newGrowPopulation As List(Of String) = getGrowPopulation()
                Dim newPopseg As popseg
                If newGrowPopulation.Count = 1 Then
                    newPopseg = constants.getPopSegFromString(newGrowPopulation(0))
                ElseIf isAI = True Then
                    'is ai, get ai choice
                    Dim aiChoice As String = aiGrowPopulation(newGrowPopulation)
                    newPopseg = constants.getPopSegFromString(aiChoice)
                Else
                    'not ai, get player choice
                    Dim menuChoice As String = menu.getListChoice(newGrowPopulation, 0, name & " has grown!  Choose a new citizen to recruit:")
                    newPopseg = constants.getPopSegFromString(menuChoice)
                End If

                addPopulation(newPopseg)
                report.Add(name & " grows and gains a new " & stripS(newPopseg.ToString) & ".", reportQueueType.SettlementGrowth, empire)
            End While
        End If


        'modifier
        If settlementModifiers.Count > 0 Then
            Dim mustRefreshAgain As Boolean = False
            For n = settlementModifiers.Count - 1 To 0 Step -1
                Dim modifier As modifier = settlementModifiers(n)
                modifier.tick()
                If modifier.duration = 0 Then
                    settlementModifiers.Remove(modifier)
                    report.Add("'" & modifier.name & "' in " & name & " has expired.", reportQueueType.SettlementModifierExpire, empire)
                    mustRefreshAgain = True
                End If
            Next
            If mustRefreshAgain = True Then refresh()
        End If


        'building
        If buildingInProgress Is Nothing Then startNextBuilding()
        If buildingInProgress Is Nothing = False Then
            With buildingInProgress
                .progress += buildingEfficiency
                If .progress >= .cost Then buildingComplete()
            End With
        End If


        'spacer
        report.Add("", reportQueueType.Blank, empire)
    End Sub
    Friend Sub consoleReport(indent As Integer)
        If mustRefresh = True Then refresh()

        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)
        Dim inddd As String = vbSpace(indent + 2)

        Console.WriteLine(ind & name)
        Console.WriteLine(indd & "└ Shard: " & shard.name)


        Console.WriteLine(indd & "└ Public Order: " & publicOrderNet)
        If publicOrderNet < 0 Then
            Select Case publicOrderNet
                Case -49 To -1 : Console.WriteLine(inddd & "└ Income will be halved until public order rises above -1.")
                Case -99 To -50 : Console.WriteLine(inddd & "└ Income will be quartered until public order rises above -50.")
                Case Is < -99 : Console.WriteLine(inddd & "└ City is in revolt and will damage itself until public order rises above -100.")
            End Select
        End If

        Console.WriteLine(indd & "└ Population: " & populationSize & " (" & growth & "/" & growthTarget & ")")
        For Each popsegg In constants.popsegArray
            Dim resource As resource = constants.getResourceFromPopseg(popsegg)
            Console.Write(inddd & "└ " & popsegg.ToString & ": " & population(popsegg))
            If resource <> Nothing Then
                Console.Write(" (" & withSign(popIncome(popsegg)) & " " & resource.ToString & ") - " & popsegGrowthPercentage(popsegg) & "%")
            Else
                If popsegg = popseg.Warriors Then
                    Console.Write(" (" & pWarriorEfficiency & " Power) - " & popsegGrowthPercentage(popsegg) & "%")
                ElseIf popsegg = popseg.Warmages Then
                    Console.Write(" (" & pWarmageEfficiency & " Power) - " & popsegGrowthPercentage(popsegg) & "%")
                End If
            End If
            Console.Write(vbCrLf)
        Next


        Console.WriteLine(indd & "└ Income:")
        For Each resource In constants.resourceArray
            Console.Write(inddd & "└ " & fakeTab(resource.ToString, 7) & ": " & popIncome(resource) * population(constants.getPopsegFromResource(resource)))
            Console.Write(" + " & baseIncome(resource))

            If resource.ToString = "Food" Then
                Dim popFoodCost As Integer = populationSize * 10
                Console.Write(" - " & popFoodCost)
            ElseIf resource.ToString = "Faith" Then
                Console.Write(" - " & income(resource.Science))
            End If

            Console.Write(" = " & income(resource))
            Console.Write(vbCrLf)
        Next


        Console.Write(indd & "└ Wealth      : ")
        Console.Write(wealth & "/" & wealthTickover)
        Console.Write(vbCrLf)


        Console.WriteLine(indd & "└ Trade Routes: " & tradeRoutes.Count & "/" & maxTradeRoutes)
        If tradeRoutes.Count > 0 Then
            For Each settlement In tradeRoutes
                Console.WriteLine(inddd & "└ " & settlement.name & ": " & settlement.getGoodsReport)
            Next
        End If


        Console.WriteLine(indd & "└ Resources   : " & getGoodsReport())


        Console.Write(indd & "└ Defence     : " & baseDefence)
        Console.Write(" + " & warriorTotalEfficiency & " + " & warmageTotalEfficiency & " = ")
        Console.Write(totalDefence)
        Console.Write(vbCrLf)
        If damagedTicks > 0 Then
            Console.WriteLine(inddd & "└ Damage: " & damagedTicks & " turn(s) remaining.")
        End If


        If buildingInProgress Is Nothing Then
            Console.WriteLine(indd & "└ Now Building: Nothing")
        Else
            With buildingInProgress
                Console.WriteLine(indd & "└ Now Building: " & buildingInProgress.name & " (" & .progress & "/" & .cost & ")")
            End With
        End If

        If buildingModifiers.Count > 0 Then
            Console.WriteLine(indd & "└ Buildings:")
            For Each modifier In buildingModifiers
                If modifier.meetsConditional(Me) = True Then modifier.consoleReport(indent + 2, "└ ") Else modifier.consoleReport(indent + 2, "x ")
            Next
        End If


        Console.WriteLine(indd & "└ Modifiers:")
        Dim hasPrintedModifier As Boolean = False
        For Each modifier In empireModifiers
            Select Case modifier.quality
                'escape clause for non-settlement modifiers 
                Case modifierQuality.EmpireIncome

                Case Else
                    If modifier.mustHide = False AndAlso modifier.meetsConditional(Me) = True Then
                        modifier.consoleReport(indent + 2, "- ")
                        hasPrintedModifier = True
                    End If
            End Select
        Next
        For Each modifier In settlementModifiers
            If modifier.mustHide = False AndAlso modifier.meetsConditional(Me) = True Then
                modifier.consoleReport(indent + 2, "└ ")
                hasPrintedModifier = True
            End If
        Next
        For Each modifier In shardModifiers
            If modifier.mustHide = False AndAlso modifier.meetsConditional(Me) = True Then
                modifier.consoleReport(indent + 2, "╙ ")
                hasPrintedModifier = True
            End If
        Next
        For Each modifier In inactiveModifiers
            If modifier.mustHide = False Then
                modifier.consoleReport(indent + 2, "x ")
                hasPrintedModifier = True
            End If
        Next
        If hasPrintedModifier = False Then Console.WriteLine(vbSpace(indent + 2) & "Nothing")


        If heroes.Count > 0 Then
            Console.WriteLine(indd & "└ Heroes:")
            For Each hero In heroes
                Console.WriteLine(inddd & "└ " & hero.nameAndTitle)
            Next
        End If
    End Sub
    Friend Sub briefConsoleReport(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent) & prefix
        Console.WriteLine(ind & name & " on " & shard.name & " Shard (pop. " & populationSize & ")")
    End Sub



    'AI
    Private ReadOnly Property isAI As Boolean
        Get
            Return empire.isAI
        End Get
    End Property
    Private Function aiGrowPopulation(newGrowPopulation As List(Of String)) As String
        If newGrowPopulation.Contains("Farmers") Then Return "Farmers" Else Return newGrowPopulation(rng.Next(newGrowPopulation.Count))
    End Function
    Private Sub aiTick()
        If publicOrderNet < 10 Then mustNotGrow = True
    End Sub



    'refresh
    Friend mustRefresh As Boolean = False
    Private Const minRawIncome As Integer = 0
    Friend Sub forceRefresh()
        refresh()
    End Sub
    Private Sub refresh()
        'reset all dependent variables
        pPopsegGrowth = New List(Of popseg)(constants.popsegArray)
        buildingEfficiency = 10
        publicOrder = 30
        inactiveModifiers = New List(Of modifier)
        pGoods.Clear()
        pTradedGoods.Clear()
        pWarriorEfficiency = 10
        pWarmageEfficiency = 10


        'initiate popIncome (farmers start at 20, all else at 10) and baseIncome
        popIncome.Clear()
        baseIncome.Clear()
        For Each resource In constants.resourceArray
            popIncome.Add(resource, 10)
            baseIncome.Add(resource, 0)
        Next
        popIncome(resource.Food) += 10


        'combine modifiers
        allModifiers.Clear()
        allModifiers.AddRange(empireModifiers)
        allModifiers.AddRange(worldModifiers)
        allModifiers.AddRange(shardModifiers)
        allModifiers.AddRange(settlementModifiers)
        allModifiers.AddRange(buildingModifiers)


        'get trade goods and goods
        'good unlocks are stored in modifiers, so iterate through modifiers one time to unlock all goods
        If tradeRoutes.Count > 0 Then
            For Each settlement In tradeRoutes
                pTradedGoods.AddRange(settlement.goods)
            Next
        End If
        For Each modifier In allModifiers
            Select Case modifier.quality
                Case modifierQuality.GoodUnlock : If modifier.meetsConditional(Me) = True Then addGood(modifier.good)
            End Select
        Next


        'iterate through modifiers and apply those that meetConditional
        'those that don't are added to inactiveModifiers
        For Each modifier In allModifiers
            If modifier.meetsConditional(Me) = True Then
                applyModifierEffect(modifier)
            Else
                inactiveModifiers.Add(modifier)
            End If
        Next


        'initiate new gross income dictionary
        grossIncome.Clear()
        For Each resource In constants.resourceArray
            'add minimum raw income
            If grossIncome.ContainsKey(resource) = False Then grossIncome.Add(resource, minRawIncome)

            'add base income
            grossIncome(resource) += baseIncome(resource)

            'add pop income
            Dim popseg As popseg = constants.getPopsegFromResource(resource)
            If popseg <> Nothing Then
                grossIncome(resource) += population(popseg) * popIncome(resource)
                If grossIncome(resource) < 0 Then grossIncome(resource) = 0
            End If
        Next


        'make copy of rawIncome into pIncome
        pIncome = New Dictionary(Of resource, Integer)(grossIncome)


        'reduce faith by science and food by population
        pIncome(resource.Faith) -= pIncome(resource.Science)
        If pIncome(resource.Faith) < 0 Then pIncome(resource.Faith) = 0
        pIncome(resource.Food) -= populationSize * 10


        'toggle mustBuildIncome
        mustRefresh = False
        shard.mustRefresh = False
    End Sub
    Private Sub applyModifierEffect(modifier As modifier)
        'caller should check if modifier meets conditional; this method just applies the effect

        Select Case modifier.quality
            Case modifierQuality.SettlementPopulationIncome : popIncome(modifier.popseg) += modifier.value(Me)
            Case modifierQuality.SettlementIncome : baseIncome(modifier.resource) += modifier.value(Me)
            Case modifierQuality.SettlementRecruit
                For n = 1 To modifier.value(Me)
                    pPopsegGrowth.Add(modifier.popseg)
                Next
            Case modifierQuality.SettlementGuaranteedRecruit : popsegGuarantee = modifier
            Case modifierQuality.SettlementBuildEfficiency : buildingEfficiency += modifier.value(Me)
            Case modifierQuality.SettlementDefence : baseDefence += modifier.value(Me)
            Case modifierQuality.SettlementPublicOrder : publicOrder += modifier.value(Me)
            Case modifierQuality.WarriorEfficiency : pWarriorEfficiency += modifier.value(Me)
            Case modifierQuality.WarmageEfficiency : pWarmageEfficiency += modifier.value(Me)
        End Select
    End Sub


    'modifiers
    Private Property allModifiers As New List(Of modifier)
    Private ReadOnly Property empireModifiers As List(Of modifier)
        Get
            If empire Is Nothing Then Return Nothing
            Return empire.empireModifiers
        End Get
    End Property
    Private ReadOnly Property worldModifiers As List(Of modifier)
        Get
            Return shard.world.modifiers
        End Get
    End Property
    Private ReadOnly Property shardModifiers As List(Of modifier)
        Get
            Return shard.shardModifiers
        End Get
    End Property
    Private Property settlementModifiers As New List(Of modifier)
    Private Property buildingModifiers As New List(Of modifier)         'do not tick because building modifiers are permanent
    Private Property inactiveModifiers As New List(Of modifier)         'purely for reporting purposes, do not tick
    Friend Sub addSettlementModifier(modifier As modifier, Optional doNotReport As Boolean = False)
        If settlementModifiers.Contains(modifier) Then
            Debug.Print(name & " already contains modifier '" & modifier.name & "'.")
            Exit Sub
        End If

        settlementModifiers.Add(modifier)
        modifier.parent = settlementModifiers

        If doNotReport = False Then
            Select Case modifier.quality
                Case modifierQuality.GoodUnlock : report.Add(name & " now has access to " & modifier.good.ToString & ".", reportQueueType.SettlementGoodNew, empire)
                Case Else : report.Add(name & " gains the '" & modifier.name & "' modifier.", reportQueueType.SettlementModifierNew, empire)
            End Select
        End If

        mustRefresh = True
    End Sub
    Private Sub addBuildingModifier(modifier As modifier, Optional doNotReport As Boolean = False)
        If buildingModifiers.Contains(modifier) Then
            Debug.Print(name & " already contains building modifier '" & modifier.name & "'.")
            Exit Sub
        End If

        buildingModifiers.Add(modifier)
        modifier.parent = buildingModifiers
        If doNotReport = False Then report.Add(name & " gains the '" & modifier.name & "' modifier.", reportQueueType.SettlementModifierNew, empire)
        mustRefresh = True
    End Sub



    'population & income
    Private Property publicOrder As Integer
    Private ReadOnly Property publicOrderNet As Integer
        Get
            Return publicOrder - (populationSize * 10)
        End Get
    End Property
    Private Sub reduceIncomeByPublicOrder(ByRef netIncome As Dictionary(Of resource, Integer))
        Select Case publicOrderNet
            Case Is > 0
                'do nothing

            Case -49 To -1
                'halve income
                For Each r In constants.resourceArray
                    If r <> resource.Wealth Then netIncome(r) = Math.Floor(pIncome(r) / 2)
                Next
                report.Add(name & "'s income was halved due to low public order.", reportQueueType.SettlementLowPublicOrder, empire)

            Case -99 To -50
                'quartered income
                For Each r In constants.resourceArray
                    If r <> resource.Wealth Then netIncome(r) = Math.Floor(pIncome(r) / 4)
                Next
                report.Add(name & "'s income was quartered due to low public order.", reportQueueType.SettlementLowPublicOrder, empire)

            Case Is < -99
                'no income
                For Each r In constants.resourceArray
                    If r <> resource.Wealth Then netIncome(r) = 0
                Next
                report.Add(name & " is producing nothing due to low public order.", reportQueueType.SettlementLowPublicOrder, empire)

                'city damaged
                Dim damage As Integer = rng.Next(1, 3)
                addDamage(damage)
                report.Add(name & " rebels due to low public order, causing " & damage & " turn(s) of damage to the city.", reportQueueType.SettlementRebel, empire)
        End Select
    End Sub

    Private Property pPopulation As New Dictionary(Of popseg, Integer)
    Friend ReadOnly Property population As Dictionary(Of popseg, Integer)
        Get
            Return pPopulation
        End Get
    End Property
    Friend ReadOnly Property populationSize As Integer
        Get
            Dim total As Integer = 0
            For Each kvp As KeyValuePair(Of popseg, Integer) In pPopulation
                total += kvp.Value
            Next
            Return total + ghostPopulation
        End Get
    End Property
    Friend Sub addPopulation(popseg As popseg, Optional value As Integer = 1)
        If population.ContainsKey(popseg) = False Then
            Debug.Print(name & " does not contain " & popseg.ToString)
            Exit Sub
        ElseIf pPopulation(popseg) + value < 0 Then
            Debug.Print("Population cannot be reduced below 0.")
            Exit Sub
        End If


        pPopulation(popseg) += value
        mustRefresh = True
    End Sub
    Friend Sub removePopulation(popseg As popseg, Optional value As Integer = 1)
        addPopulation(popseg, value * -1)
    End Sub

    Private Property growth As Integer = 0
    Friend Property mustNotGrow As Boolean = False
    Private Property pPopsegGrowth As New List(Of popseg)
    Friend ReadOnly Property popsegGrowth As List(Of popseg)
        Get
            Return pPopsegGrowth
        End Get
    End Property
    Friend ReadOnly Property popsegGrowthPercentage(popseg As popseg) As String
        Get
            Dim count As Integer = 0
            For Each pop In popsegGrowth
                If pop = popseg Then count += 1
            Next
            Dim rawCount As Integer = (count / popsegGrowth.Count) * 100
            Return rawCount.ToString("N0")
        End Get
    End Property
    Private Property popsegGuarantee As modifier
    Private ReadOnly Property growthTarget As Integer
        Get
            Dim total As Integer = populationSize * 20
            Return constrain(total, 40, 1000)
        End Get
    End Property
    Private Function getGrowPopulation() As List(Of String)
        If mustRefresh = True Then refresh()

        Dim newPopsegGrowth As New List(Of popseg)(pPopsegGrowth)
        Dim total As New List(Of String)
        If popsegGuarantee Is Nothing Then
            'no guarantee, roll and return 3 choices
            For n = 1 To 3
                Dim roll As Integer = rng.Next(newPopsegGrowth.Count)
                Dim newPopseg As popseg = newPopsegGrowth(roll)
                newPopsegGrowth.RemoveAt(roll)
                total.Add(newPopseg.ToString)
            Next
            Return total
        Else
            'guarantee, return a single choice
            'if there were multiple guarantees, popsegGuarantee always = latest one to be added
            total.Add(popsegGuarantee.popseg.ToString)
            popsegGuarantee.parent.Remove(popsegGuarantee)
            popsegGuarantee = Nothing
            mustRefresh = True
        End If
        Return total
    End Function
    Private Sub starvePopulation()
        'populate list with popseg >= 1
        Dim availablePopsegs As New List(Of popseg)
        For Each kvp In population
            If kvp.Value > 0 Then availablePopsegs.Add(kvp.Key)
        Next

        'bias away from farmers if possible
        If availablePopsegs.Count > 1 Then
            If availablePopsegs.Contains(popseg.Farmers) Then availablePopsegs.Remove(popseg.Farmers)
        End If

        'roll and remove popseg; if no popseg to remove, damage settlement instead
        If availablePopsegs.Count > 0 Then
            Dim roll As Integer = rng.Next(availablePopsegs.Count)
            Dim deadPopseg As popseg = availablePopsegs(roll)
            removePopulation(deadPopseg)
            report.Add(name & " loses a " & stripS(deadPopseg.ToString) & " to starvation.", reportQueueType.SettlementCitizenLost, empire)
        Else
            addDamage(2)
            report.Add(name & " riots due to starvation, causing 2 ticks worth of damage.", reportQueueType.SettlementDamaged, empire)
        End If
    End Sub

    Private Property popIncome As New Dictionary(Of resource, Integer)
    Private Property baseIncome As New Dictionary(Of resource, Integer)
    Private Property grossIncome As New Dictionary(Of resource, Integer)
    Private Property pIncome As Dictionary(Of resource, Integer)
    Friend ReadOnly Property income As Dictionary(Of resource, Integer)
        Get
            Return pIncome
        End Get
    End Property
    Private Sub incomeReport(netIncome As Dictionary(Of resource, Integer))
        Dim n As New rollingCounter(1)
        report.Add(name & " (pop. " & populationSize & ") produced " & netIncome(n.Tick) & " food, " & netIncome(n.Tick) & " faith, " & _
                   netIncome(n.Tick) & " science, " & netIncome(n.Tick) & " culture & " & netIncome(n.Tick) & " wealth.", reportQueueType.SettlementIncome, empire)
        If income(resource.Food) < 0 Then report.Add(name & " is starving!", reportQueueType.SettlementStarving, empire)
    End Sub



    'buildings
    Private Property buildingInProgress As building
    Private Property buildingEfficiency As Integer
    Private Property pBuildingsAvailable As New List(Of building)
    Friend ReadOnly Property availableBuildings As List(Of building)
        Get
            Return pBuildingsAvailable
        End Get
    End Property
    Private Property buildingsCompleted As New List(Of building)
    Private Sub startNextBuilding()
        If pBuildingsAvailable.Count = 0 Then Exit Sub

        'clone buildingsAvailable and trim
        Dim tBuildingsAvailable As New List(Of building)
        For Each building In pBuildingsAvailable
            Dim OKtoAdd As Boolean = True

            With building
                'don't add special project buildings
                If .name.Contains("Terraform") Then OKtoAdd = False

                'don't add buildings that key off non-existant shard resources
                If .requiredGoods.Count > 0 Then
                    For Each good In .requiredGoods
                        If allGoods.Contains(good) = False Then
                            OKtoAdd = False
                            Exit For
                        End If
                    Next
                ElseIf .requiredWildernessTypes.Count > 0 Then
                    For Each wild In .requiredWildernessTypes
                        If shard.wildernessTypes.Contains(wild) = False Then
                            OKtoAdd = False
                            Exit For
                        End If
                    Next
                End If

                'don't add buildings whose requirements are not built
                If .requiredBuildingID <> 0 AndAlso getBuilding(.requiredBuildingID, buildingsCompleted) Is Nothing = True Then
                    OKtoAdd = False
                End If
            End With

            If OKtoAdd = True Then tBuildingsAvailable.Add(building)
        Next

        If tBuildingsAvailable.Count = 0 Then Exit Sub

        Dim roll As Integer = rng.Next(tBuildingsAvailable.Count)
        buildingInProgress = tBuildingsAvailable(roll)
    End Sub
    Friend Sub setNextBuilding(aBuilding As building)
        If buildingInProgress Is Nothing = False Then
            report.Add(name & " halts work on " & aOrAn(buildingInProgress.name) & ".", reportQueueType.SettlementBuildingProjectHalted, empire)
        End If

        buildingInProgress = aBuilding

        Dim reportStr As String
        If buildingInProgress.name.Contains("Terraform") AndAlso terraformTarget Is Nothing = False Then
            Dim split As String() = buildingInProgress.name.Split(" ")
            reportStr = name & " begins Terraforming " & terraformTarget.name & " into " & split(1) & "."
        Else
            reportStr = name & " begins work on " & aOrAn(buildingInProgress.name) & "."
        End If
        report.AddAndImmediateReport(reportStr, reportQueueType.SettlementBuildingProjectBegin, empire)
    End Sub
    Private Sub buildingComplete()
        If pBuildingsAvailable.Contains(buildingInProgress) = False Then
            Debug.Print("Building not in buildingsAvailable list.")
            Exit Sub
        End If

        'set progress to cost for neatness
        buildingInProgress.progress = buildingInProgress.cost


        If buildingInProgress.name.Contains("Terraform") Then
            'terraform; get wilderness type
            Dim targetWildernessTypeStr As String = buildingInProgress.name.Split(" ")(1)
            Dim targetWildernessType As wildernessType = constants.getWildernessTypeFromString(targetWildernessTypeStr)

            'error check
            If targetWildernessType = Nothing Then
                Debug.Print("Target wilderness type invalid.")
                Exit Sub
            ElseIf terraformTarget Is Nothing Then
                Debug.Print("Terraform target not set.")
                Exit Sub
            End If

            'perform terraform
            terraform(terraformTarget, targetWildernessType)
            terraformTarget = Nothing
        Else
            'remove from available and add to completed
            pBuildingsAvailable.Remove(buildingInProgress)
            buildingsCompleted.Add(buildingInProgress)

            'add building modifiers
            For Each modifier In buildingInProgress.modifiers
                addBuildingModifier(modifier, True)
            Next

            'report
            report.Add(name & " completes construction on " & buildingInProgress.name & ".", reportQueueType.SettlementBuildingProjectCompleted, empire)
            buildingInProgress = Nothing
        End If
    End Sub
    Friend Sub addAvailableBuilding(building As building)
        If pBuildingsAvailable.Contains(building) Then
            Debug.Print("Already in available buildings.")
            Exit Sub
        End If

        pBuildingsAvailable.Add(building)
    End Sub
    Private Property terraformTarget As travelLocation
    Friend Sub setTerraformTarget(travelLocation As travelLocation)
        terraformTarget = travelLocation
    End Sub
    Private Sub terraform(travelLocation As travelLocation, targetWildernessType As wildernessType)
        If checkTerraform(travelLocation) = False Then Exit Sub

        Dim newLocation As New wilderness(targetWildernessType)
        newLocation.name = wilderness.getRandomWildernessName(newLocation.type)
        Dim newLocationIndex As Integer = shard.travelLocations.IndexOf(travelLocation)

        report.Add(name & " successfully terraforms " & travelLocation.name & " into " & newLocation.name & ".", reportQueueType.SettlementTerraformCompleted, empire)

        shard.addTravelLocation(newLocation, newLocationIndex)
        shard.removeTravelLocation(travelLocation)
    End Sub
    Friend Function checkTerraform(travelLocation As travelLocation)
        If TypeOf travelLocation Is wasteland = False AndAlso _
            TypeOf travelLocation Is settlementRuins = False AndAlso _
            TypeOf travelLocation Is wilderness = False Then
            Debug.Print("Invalid terraforming target.")
            Return False
        End If

        Return True
    End Function
    Private Function getBuilding(id As Integer, ByRef buildingList As List(Of building))
        For Each building In buildingList
            If building.id = id Then Return building
        Next
        Return Nothing
    End Function



    'trading and wealth
    Private Property tradeRoutes As New List(Of settlement)
    Private ReadOnly Property maxTradeRoutes As Integer
        Get
            Return population(popseg.Traders)
        End Get
    End Property
    Friend Sub addTradeRoute(destination As settlement)
        If checkAddTradeRoute(destination) Is Nothing Then Exit Sub

        tradeRoutes.Add(destination)
        mustRefresh = True
        report.Add(name & " begins trading with " & destination.name & ".", reportQueueType.SettlementTradeNew, empire)
    End Sub
    Friend Function checkAddTradeRoute(destination As settlement) As errorReport
        If tradeRoutes.Contains(destination) Then
            Debug.Print("Already trading with destination.")
            Return New errorReport("Already trading with " & destination.name & ".")
        ElseIf tradeRoutes.Count + 1 > maxTradeRoutes Then
            Debug.Print("Insufficient trade routes.")
            Return New errorReport(name & " has insufficient trade routes.")
        End If

        Return Nothing
    End Function

    Private Property pGoods As New List(Of good)
    Private Property pTradedGoods As New List(Of good)
    Friend ReadOnly Property goods As List(Of good)
        Get
            Return pGoods
        End Get
    End Property
    Friend ReadOnly Property allGoods As List(Of good)
        Get
            Dim total As New List(Of good)
            total.AddRange(pGoods)
            total.AddRange(pTradedGoods)
            Return total
        End Get
    End Property
    Private Sub addGood(good As good)
        If empire.goodsAvailable.Contains(good) = False Then
            Debug.Print("Good not in empire's availableGoods list.")
            Exit Sub
        ElseIf pGoods.Contains(good) Then
            Debug.Print("Good already in goods list.")
            Exit Sub
        End If

        pGoods.Add(good)
    End Sub
    Friend Function getGoodsReport() As String
        If goods.Count = 0 Then Return "Nothing"

        Dim total As String = ""
        For n = goods.Count - 1 To 0 Step -1
            Dim good As good = goods(n)
            total &= good.ToString
            If n <> goods.Count - 1 Then total &= ", "
        Next
        Return total
    End Function

    Private Property wealth As Integer = 0
    Private Const wealthTickover As Integer = 500
    Friend Sub addWealth(value As Integer)
        wealth += value

        If wealth > wealthTickover Then
            'build a popseg list (based on popsegGrowth) that trims out traders, warmages and warriors
            Dim trimmedPopsegGrowth As New List(Of popseg)
            For Each popseg In popsegGrowth
                Select Case popseg
                    Case popseg.Traders
                    Case popseg.Warmages
                    Case popseg.Warriors
                    Case Else : trimmedPopsegGrowth.Add(popseg)
                End Select
            Next

            While wealth > wealthTickover
                wealth -= wealthTickover

                Dim roll As Integer = rng.Next(trimmedPopsegGrowth.Count)
                Dim sPopseg As popseg = trimmedPopsegGrowth(roll)
                Dim resource As resource = constants.getResourceFromPopseg(sPopseg)

                empire.addResource(resource, 100)
                report.Add(name & "'s traders produce an additional 100 " & resource.ToString & " this turn.", reportQueueType.SettlementWealthTick, empire)
            End While
        End If
    End Sub



    'war
    Friend Const damageThreshold As Integer = 10
    Private Property pDamagedTicks As Integer
    Friend ReadOnly Property damagedTicks As Integer
        Get
            Return pDamagedTicks
        End Get
    End Property
    Friend Sub addDamage(value As Integer)
        pDamagedTicks += value
    End Sub
    Friend Sub destroy()
        'clear all lists and variables
        allModifiers.Clear()
        settlementModifiers.Clear()
        buildingModifiers.Clear()
        inactiveModifiers.Clear()

        population.Clear()
        pPopsegGrowth.Clear()
        popsegGuarantee = Nothing

        popIncome.Clear()
        baseIncome.Clear()
        grossIncome.Clear()
        pIncome.Clear()

        buildingInProgress = Nothing
        pBuildingsAvailable.Clear()
        buildingsCompleted.Clear()

        tradeRoutes.Clear()
        pGoods.Clear()
        pTradedGoods.Clear()


        'report
        report.AddAndImmediateReport(name & " was razed to the ground!", reportQueueType.SettlementDestroyed, empire)


        'remove empire and shard
        empire.removeSettlement(Me)
        empire = Nothing
        shard.removeTravelLocation(Me)
        shard.addTravelLocation(New settlementRuins(name & " Ruins"), 0)
        shard = Nothing
    End Sub
    Private Property baseDefence As Integer
    Private Property pWarriorEfficiency As Integer
    Friend ReadOnly Property warriorEfficiency As Integer
        Get
            Return pWarriorEfficiency
        End Get
    End Property
    Private ReadOnly Property warriorTotalEfficiency As Integer
        Get
            Return population(popseg.Warriors) * pWarriorEfficiency
        End Get
    End Property
    Private Property pWarmageEfficiency As Integer
    Friend ReadOnly Property warmageEfficiency As Integer
        Get
            Return pWarmageEfficiency
        End Get
    End Property
    Private ReadOnly Property warmageTotalEfficiency As Integer
        Get
            Return population(popseg.Warmages) * pWarmageEfficiency
        End Get
    End Property
    Private Property ghostPopulation As Integer                 'used to maintain food income when army is off galivanting; do not reset in refresh
    Friend ReadOnly Property totalDefence As Integer
        Get
            Dim heroTotal As Integer = 0
            For Each hero In heroes
                heroTotal += hero.highestHeroSkill.Value
            Next

            Return baseDefence + warriorTotalEfficiency + warmageTotalEfficiency + heroTotal
        End Get
    End Property
    Friend Function buildArmy(aName As String, destination As travelLocation, aWarriors As Integer, aWarmages As Integer, Optional aHero As hero = Nothing) As army
        If checkBuildArmy(destination, aWarriors, aWarmages, empire, aHero) Is Nothing = False Then Return Nothing

        Dim army As New army
        With army
            If aName = Nothing Then .name = name & "'s Irregulars" Else .name = aName
            .home = Me
            .teleportTo(Me)
            .empire = empire

            .warriors = aWarriors
            .warriorPower = pWarriorEfficiency
            addPopulation(popseg.Warriors, -aWarriors)

            .warmages = aWarmages
            .warmagePower = pWarmageEfficiency
            addPopulation(popseg.Warmages, -aWarmages)

            If aHero Is Nothing = False Then
                .hero = aHero
                heroes.Remove(aHero)

                .hero.army = army
                .hero.teleportTo(Nothing)
            End If
        End With
        shard.world.armies.Add(army)

        ghostPopulation = aWarriors + aWarmages

        report.AddAndImmediateReport(name & " raises an army under the banner of '" & army.name & "'.", reportQueueType.ArmyRaised, empire)
        army.moveTo(destination)
        buildArmy = army

        mustRefresh = True
    End Function
    Friend Sub disbandArmy(ByRef army As army)
        If army.wealth > 0 Then report.Add(name & " receives " & army.wealth & " Wealth in tribute from " & army.name & ".", reportQueueType.ArmyTribute, army.empire)
        report.Add(name & " disbands " & army.name & ".", reportQueueType.ArmyDisbanded, army.empire)

        With army
            addPopulation(popseg.Warriors, .warriors)
            addPopulation(popseg.Warmages, .warmages)

            If .hero Is Nothing = False Then
                .hero.army = Nothing
                .hero.teleportTo(Me)
            End If

            addWealth(army.wealth)
        End With
        shard.world.armies.Remove(army)

        ghostPopulation = 0

        army = Nothing
        mustRefresh = True
    End Sub
    Friend Function checkBuildArmy(aDestination As travelLocation, aWarriors As Integer, aWarmages As Integer, aEmpire As empire, Optional aHero As hero = Nothing) As errorReport
        If empire.Equals(aEmpire) = False Then
            Debug.Print("Empire argument invalid for checkBuildArmy")
            Return New errorReport("Unexpected error.")
        ElseIf aWarriors = 0 AndAlso aWarmages = 0 Then
            Debug.Print("Army cannot have 0 Warriors and Warmages.")
            Return New errorReport("An army must have at least 1 Warrior or Warmage.")
        ElseIf population(popseg.Warriors) < aWarriors Then
            Debug.Print("Insufficient warriors.")
            Return New errorReport(name & " only has " & population(popseg.Warriors) & " Warriors.")
        ElseIf population(popseg.Warmages) < aWarmages Then
            Debug.Print("Insufficient warmages.")
            Return New errorReport(name & " only has " & population(popseg.Warmages) & " Warmages.")
        ElseIf aHero Is Nothing = False Then
            If heroes.Contains(aHero) = False Then
                Debug.Print("Invalid hero selection.")
                Return New errorReport(aHero.name & " is not in " & name & ".")
            End If
        ElseIf aDestination.isAttackable(empire) = False Then
            Debug.Print("Destination cannot be attacked.")
            Return New errorReport(empire.name & " cannot attack " & aDestination.name & ".")
        End If

        Return Nothing
    End Function
    Friend Sub enshrineArtefact(hero As hero)
        If hero.equipment.ContainsKey("Artefact") = False Then
            Debug.Print("Hero has no artefact to enshrine.")
            Exit Sub
        End If

        'remove from hero
        Dim item As item = hero.equipment("Artefact")
        hero.removeEquipment(item)


        'report
        report.AddAndImmediateReport(item.name & " is now permanently enshrined in " & name & ".", reportQueueType.SettlementEnshrineArtefact, empire)


        'apply modifiers
        For Each modifier In item.modifiers
            addSettlementModifier(modifier)
        Next


        'zero out item
        item.hero = Nothing
        item.modifiers.Clear()
        item = Nothing
    End Sub
End Class
