Module Module1
    Dim world As world
    Dim player As empire
    Dim enemy As empire


    Sub Main()
        Console.SetWindowSize(120, 80)
        generateHashes()
        Dim turncounter As Integer = 0

        Dim worldBuilder As New worldBuilder
        world = worldBuilder.buildWorld
        player = worldBuilder.buildEmpire(world, "Republic of Is", False)
        enemy = worldBuilder.buildEmpire(world, "Devious Deviants", True)
        worldBuilder = Nothing

        immediateTests()

        menuviewReports()
        menuMain()
    End Sub

    Private Sub immediateTests()
        Dim settlement As settlement = player.settlements(0)

        'insert blank report
        report.Add("", reportQueueType.Blank, player)


        player.addResource(resource.Faith, 200000000)
        player.god.addPower(divinePower.Fate, 100)
        player.god.addPower(divinePower.Destiny, 100)
    End Sub
    Private Sub generateHashes()
        authenticator.generateHash("data/buildings.csv")
        authenticator.generateHash("data/science.csv")
        authenticator.generateHash("data/culture.csv")
        authenticator.generateHash("data/zeitgeist.csv")
        authenticator.generateHash("data/philosophy.csv")
        'authenticator.generateHash("data/threats.csv")
        'authenticator.generateHash("data/challenges.csv")
    End Sub


    Private Sub menuMain()
        Dim menuChoices As New Dictionary(Of Char, String)
        menuChoices.Add("v"c, "View Reports")
        menuChoices.Add("w"c, "Examine World")
        menuChoices.Add("s"c, "Examine Settlements")
        menuChoices.Add("h"c, "Examine Heroes")
        menuChoices.Add("d"c, "Examine Divinity")
        menuChoices.Add("r"c, "Roll Dice")
        menuChoices.Add("m"c, "Create Miracle")
        menuChoices.Add("|"c, "Cheat")
        menuChoices.Add("z"c, "End Turn")

        While True
            Console.Clear()
            world.timekeeper.consoleReport(0)
            Console.WriteLine()

            player.consoleReport(0)
            Console.WriteLine()
            Console.WriteLine()

            Select Case menu.getListChoice(menuChoices, 0)
                Case "v"c : menuViewReports()
                Case "w"c : menuViewWorld()
                Case "s"c : menuViewSettlement()
                Case "h"c : menuViewHeroes()
                Case "d"c : menuViewGod()
                Case "r"c : menuRollDice()
                Case "m"c : menuMiracle()

                Case "|"c : menuCheat()

                Case "z"c : menuEndTurn()
                Case vbCrLf : menuEndTurn()
                Case Else : Exit While
            End Select
        End While
    End Sub
    Private Sub menuViewReports()
        Console.Clear()
        world.timekeeper.consoleReport(0)
        Console.WriteLine()
        report.fullConsoleReport(0, player)
        Console.ReadKey()
    End Sub
    Private Sub menuViewWorld()
        Console.Clear()
        world.consoleReport(0)
        Console.ReadKey()
    End Sub
    Private Sub menuViewSettlement()
        Console.WriteLine()
        Console.WriteLine()

        Dim settlement As settlement = Nothing
        If player.settlements.Count = 0 Then
            Exit Sub
        ElseIf player.settlements.Count = 1 Then
            settlement = player.settlements(0)
        Else
            settlement = menu.getListChoice(player.settlements, 0, "Select a settlement:")
            Console.WriteLine()
        End If

        While True
            Console.Clear()
            settlement.consoleReport(0)

            Dim menuChoices As New Dictionary(Of Char, String)
            Dim mustNotGrow As String
            If settlement.mustNotGrow = True Then mustNotGrow = "Off" Else mustNotGrow = "On"
            menuChoices.Add("g"c, "Toggle Birth Control " & mustNotGrow)
            menuChoices.Add("c"c, "Divine Conception")
            menuChoices.Add("t"c, "Establish Trade Route")
            menuChoices.Add("h"c, "Summon Hero")
            menuChoices.Add("a"c, "Raise Army")
            menuChoices.Add("e"c, "Enshrine Artefact")
            menuChoices.Add("b"c, "Alter Building Project")

            Console.WriteLine()
            Console.WriteLine()
            Dim input As Char = menu.getListChoice(menuChoices, 0)
            Select Case input
                Case "g"c
                    settlement.mustNotGrow = Not (settlement.mustNotGrow)
                    Console.WriteLine()
                    Console.WriteLine()
                    If settlement.mustNotGrow = True Then Console.WriteLine(settlement.name & " will now stop growing.") Else Console.WriteLine(settlement.name & " will now continue to grow.")
                    Console.ReadKey()
                Case "c"c : menuDivineConception(settlement)
                Case "t"c : menuEstablishTradeRoute(settlement)
                Case "h"c : menuCreateHero(settlement)
                Case "a"c : menuCreateArmy(settlement)
                Case "e"c : menuEnshrineArtefact(settlement)
                Case "b"c : menuAlterBuilding(settlement)
                Case Else
                    Exit Sub
            End Select
        End While
    End Sub
    Private Sub menuEnshrineArtefact(aSettlement As settlement)
        Console.WriteLine()
        Console.WriteLine()

        Dim chosenHero As hero = Nothing
        Dim heroList As New List(Of hero)
        For Each hero In aSettlement.heroes
            If hero.equipment.ContainsKey("Artefact") = True Then heroList.Add(hero)
        Next

        If heroList.Count = 0 Then
            Console.WriteLine("The heroes in " & aSettlement.name & " have no Artefacts to enshrine.")
            Console.ReadKey()
            Exit Sub
        ElseIf heroList.Count = 1 Then
            chosenHero = heroList(0)
        Else
            chosenHero = menu.getListChoice(heroList, "Select a Hero:")
        End If

        If chosenHero Is Nothing Then Exit Sub
        aSettlement.enshrineArtefact(chosenHero)
        report.DisplayImmediateReports(0, player)
    End Sub
    Private Sub menuEnshrineArtefact(aHero As hero)
        Console.WriteLine()
        Console.WriteLine()

        If aHero Is Nothing Then
            Exit Sub
        ElseIf aHero.equipment.ContainsKey("Artefact") = False Then
            Console.WriteLine(aHero.nameAndTitle & " does not have any Artefacts.")
            Console.ReadKey()
            Exit Sub
        ElseIf TypeOf aHero.location Is settlement = False Then
            Console.WriteLine(aHero.nameAndTitle & " is not currently in a settlement.")
            Console.ReadKey()
            Exit Sub
        End If

        Dim artefact As item = aHero.equipment("Artefact")
        Dim settlement As settlement = CType(aHero.location, settlement)

        artefact.consoleReport(1)
        Console.WriteLine()
        If menu.confirmChoice(0, "Enshrine " & artefact.name & " in " & settlement.name & "? ") = False Then Exit Sub
        Console.WriteLine()
        Console.WriteLine()
        settlement.enshrineArtefact(aHero)
        report.DisplayImmediateReports(0, player)
    End Sub
    Private Sub menuViewHeroes()
        Console.Clear()

        Dim hero As hero = Nothing
        If player.heroes.Count = 0 Then
            Exit Sub
        ElseIf player.heroes.Count = 1 Then
            hero = player.heroes(0)
        Else
            hero = menu.getListChoice(player.heroes, 0, "Select a hero:")
            Console.WriteLine()
        End If

        hero.consoleReport(0)

        Console.WriteLine()
        Console.WriteLine()
        Dim menuChoices As New Dictionary(Of Char, String)
        If TypeOf hero.location Is settlement Then menuChoices.Add("a"c, "Summon Army")
        menuChoices.Add("e"c, "Enshrine Artefact")
        menuChoices.Add("q"c, "Add Geas")

        Console.WriteLine()
        Select Case menu.getListChoice(menuChoices, 0)
            Case "a"c : menuCreateArmy(Nothing, hero)
            Case "e"c : menuEnshrineArtefact(hero)
            Case "q"c : menuCreateGeas(hero)
            Case Else : Exit Sub
        End Select
    End Sub
    Private Sub menuRollDice()
        Console.WriteLine()

        Dim maxRoll As Integer = Math.Floor(player.resources(resource.Faith) / god.diceFaithCost)
        If maxRoll <= 0 Then
            Console.WriteLine("Insufficient Faith!")
            Console.WriteLine("Each roll requires " & god.diceFaithCost & " Faith.")
            Console.ReadKey()
            Console.WriteLine()
            Exit Sub
        End If

        Console.WriteLine()
        player.god.consoleReportDice(0)
        Console.WriteLine()
        Console.WriteLine("Each roll of the dice requires " & god.diceFaithCost & " Faith.")
        Console.WriteLine("You can roll up to " & maxRoll.ToString("N0") & " times.")
        Dim input As Integer = menu.getNumInput(0, 0, maxRoll, "How many times do you want to roll? ")
        player.god.rollDice(input)
    End Sub
    Private Sub menuEndTurn()
        world.tick()

        menuViewReports()
    End Sub

    Private Sub menuViewGod()
        Console.Clear()

        Dim god As god = player.god
        god.consoleReport(0)

        Dim menuChoices As New Dictionary(Of Char, String)
        menuChoices.Add("d"c, "Expand Divine Domain")
        menuChoices.Add("h"c, "Examine Hand")
        menuChoices.Add("t"c, "Purchase Threat Cards")
        menuChoices.Add("r"c, "Purchase Reward Cards")

        Console.WriteLine()
        Select Case menu.getListChoice(menuChoices, 0)
            Case "d"c : menuExpandDomain()
            Case "h"c : menuCardHand()
            Case "t"c : menuPurchaseCard("Threat")
            Case "r"c : menuPurchaseCard("Reward")
            Case Else : Exit Sub
        End Select
    End Sub
    Private Sub menuExpandDomain()
        Dim god As god = player.god

        Console.WriteLine()


        'check if faith is sufficient
        If god.faith < god.domainFaithCost Then
            Console.WriteLine("Insufficient Faith!")
            Console.WriteLine("You need at least " & god.domainFaithCost & " to purchase a new domain.")
            Console.ReadKey()
            Console.WriteLine()
            Exit Sub
        End If


        'build open domain lists
        Dim openDomains As New List(Of divineDomain)
        Dim openNonspecDomains As New List(Of divineDomain)
        For Each domain In world.openDomains
            If god.getDomainJurisdiction(domain) = player.god.jurisdiction Then
                openDomains.Add(domain)
            Else
                openNonspecDomains.Add(domain)
            End If
        Next

        Console.WriteLine()
        Console.WriteLine("Jurisdictive Domains (" & god.domainFaithCost & " Faith)")
        For Each domain In openDomains
            Console.WriteLine(vbSpace(1) & "└ " & domain.ToString)
        Next
        Console.WriteLine()
        Console.WriteLine("Extrajudical Domains (" & god.domainExtrajurisFaithCost & " Faith)")
        For Each domain In openNonspecDomains
            Console.WriteLine(vbSpace(1) & "└ " & domain.ToString)
        Next

        Console.WriteLine()
        Console.Write("Purchase Jurisdictive (D)omain or (E)xtrajudical Domain? ")
        Dim jurisInput As ConsoleKeyInfo = Console.ReadKey
        Console.WriteLine()
        Console.WriteLine()

        Dim domainList As New List(Of String)
        If jurisInput.Key = ConsoleKey.D Then
            Console.WriteLine("Jurisdictive Domains:")
            For Each domain In openDomains
                domainList.Add(domain.ToString)
            Next
        ElseIf jurisInput.Key = ConsoleKey.E Then
            Console.WriteLine("Extrajudical Domains:")
            For Each domain In openNonspecDomains
                domainList.Add(domain.ToString)
            Next
        Else
            Exit Sub
        End If

        Dim chosenDomainStr As String = menu.getListChoice(domainList, 1)
        Dim chosenDomain As divineDomain = constants.getDivineDomainFromString(chosenDomainStr)
        player.god.buyDomain(chosenDomain)
    End Sub
    Private Sub menuCardHand()
        Dim god As god = player.god

        Console.WriteLine()
        Console.WriteLine()

        If god.handRewards.Count = 0 AndAlso god.handThreats.Count = 0 Then
            Console.WriteLine("You have no cards in hand.")
            Console.ReadKey()
            Exit Sub
        End If

        For Each threat In god.handThreats
            threat.briefConsoleReport(1)
        Next
        For Each reward In god.handRewards
            reward.briefConsoleReport(1)
        Next

        Console.WriteLine()
        Console.ReadKey()
    End Sub
    Private Sub menuPurchaseCard(cardType As String)
        Dim god As god = player.god
        Const cost As Integer = 1000

        Console.WriteLine()
        Console.WriteLine()
        Console.WriteLine("It will cost you " & cost & " Faith to draw a card.")

        If god.faith < cost Then
            Console.WriteLine("You do not have enough Faith.")
            Console.ReadKey()
            Exit Sub
        Else
            If menu.confirmChoice(0) = False Then Exit Sub
        End If

        Console.WriteLine()
        Console.WriteLine()
        If cardType = "Threat" Then
            Dim threat As threat = god.drawThreatCard
            Console.WriteLine("Success!  You draw and add the '" & threat.name & "' card to your hand.")
            Console.ReadKey()

            god.faith -= cost
            god.addHandCard(threat)
        ElseIf cardType = "Reward" Then
            Dim reward As reward = god.drawRewardCard
            Console.WriteLine("Success!  You draw and add the '" & reward.name & "' card to your hand.")
            Console.ReadKey()

            god.faith -= cost
            god.addHandCard(reward)
        Else
            Debug.Print("Unrecognised cardType.")
            Exit Sub
        End If
    End Sub

    Private Sub menuMiracle(Optional aSettlement As settlement = Nothing)
        Dim god As god = player.god

        Dim menuChoices As New Dictionary(Of Char, String)
        menuChoices.Add("h"c, "Summon a Hero")
        menuChoices.Add("a"c, "Summon Army")
        menuChoices.Add("q"c, "Add Geas")
        menuChoices.Add("g"c, "Divine Conception")
        menuChoices.Add("t"c, "Establish Trade Route")
        menuChoices.Add("b"c, "Alter Building Project")
        menuChoices.Add("s"c, "Alter Science Research")
        menuChoices.Add("c"c, "Alter Culture Research")
        menuChoices.Add("z"c, "Change Zeitgeist")
        menuChoices.Add("p"c, "Change Philosophy")

        While True
            Console.Clear()
            Console.WriteLine("You have " & god.power(divinePower.Fate) & " Fate and " & god.power(divinePower.Destiny) & " Destiny.")
            Console.WriteLine()

            Select Case menu.getListChoice(menuChoices, 0)
                Case "h"c : menuCreateHero(aSettlement)
                Case "a"c : menuCreateArmy(aSettlement)
                Case "g"c : menuDivineConception()
                Case "t"c : menuEstablishTradeRoute(aSettlement)
                Case "b"c : menuAlterBuilding(aSettlement)
                Case "s"c : menuAlterTech(resource.Science)
                Case "c"c : menuAlterTech(resource.Culture)
                Case "z"c : menuAlterZeitgeist()
                Case "p"c : menuAlterPhilosophy()
                Case Else : Exit While
            End Select
        End While
    End Sub
    Private Sub menuCreateHero(Optional aSettlement As settlement = Nothing)
        Console.WriteLine()
        Console.WriteLine()

        Dim heroCost As Integer = constrain(player.heroes.Count, 1, 10)
        If menuSpendFateAndDestiny(heroCost, "raise a hero.") = False Then Exit Sub

        Console.WriteLine()
        Console.WriteLine()

        Dim settlement As settlement = Nothing
        If aSettlement Is Nothing Then
            settlement = menu.getListChoice(player.settlements, 1, "Choose a settlement:")
            If settlement Is Nothing Then
                player.god.addPower(divinePower.Fate, heroCost)             'refund cost
                player.god.addPower(divinePower.Destiny, heroCost)          'refund cost
                Exit Sub
            End If
        Else
            settlement = aSettlement
        End If

        Dim herolist As New List(Of hero)
        For n = 1 To 3
            herolist.Add(hero.buildRandomHero(settlement))
        Next
        Console.WriteLine()

        Dim chosenHero As hero = Nothing
        While True
            chosenHero = menu.getListChoice(herolist, 1, "Three Heroes respond to your call...")
            If chosenHero Is Nothing Then Console.WriteLine("Invalid selection!") Else Exit While
        End While

        Console.WriteLine()
        Console.WriteLine("You spend " & heroCost & " Fate and Destiny to raise " & chosenHero.name & " as your chosen champion!")
        player.addHero(chosenHero, settlement)
        Console.ReadKey()
    End Sub
    Private Sub menuCreateArmy(Optional aSettlement As settlement = Nothing, Optional aHero As hero = Nothing, Optional aDestination As travelLocation = Nothing)
        Console.WriteLine()
        Console.WriteLine()

        Const cost As Integer = 2
        Const powerCost As divinePower = divinePower.Fate
        If menuSpendPower(cost, powerCost, "raise an army.") = False Then Exit Sub

        Dim destination As travelLocation = Nothing
        If aDestination Is Nothing Then
            Dim destinationShard As shard = Nothing

            Dim shardChoices As List(Of shard) = world.attackableShards(player)
            If shardChoices.Count = 0 Then
                Console.WriteLine()
                Console.WriteLine()
                Console.WriteLine("No valid target for an army to attack!")
                Console.ReadKey()
                player.god.addPower(powerCost, cost)
                Exit Sub
            Else
                Console.WriteLine()
                Console.WriteLine()
                destinationShard = menu.getListChoice(shardChoices, 1, "Select a shard to attack:")
            End If

            Dim attackChoices As List(Of travelLocation) = destinationShard.getAttackableLocations(player)
            If attackChoices.Count = 0 Then
                Console.WriteLine()
                Console.WriteLine()
                Console.WriteLine("No valid target for an army to attack!")
                Console.ReadKey()
                player.god.addPower(powerCost, cost)
                Exit Sub
            Else
                Console.WriteLine()
                Console.WriteLine()
                destination = menu.getListChoice(attackChoices, 1, "Select a location to attack:")
            End If
        Else
            destination = aDestination
        End If

        Dim settlement As settlement = Nothing
        Dim warriors As Integer = 0
        Dim warmages As Integer = 0
        Dim hero As hero = Nothing

        If aHero Is Nothing = False Then
            If TypeOf aHero.location Is settlement = False Then
                player.god.addPower(powerCost, cost)
                Exit Sub
            End If
            settlement = CType(aHero.location, settlement)
            hero = aHero
        Else
            If aSettlement Is Nothing Then
                Console.WriteLine()
                Console.WriteLine()
                settlement = menu.getListChoice(player.settlements, 1, "Choose a home settlement to raise a banner in:")
                If settlement Is Nothing Then
                    player.god.addPower(powerCost, cost)
                    Exit Sub
                End If
            Else
                settlement = aSettlement
            End If
        End If

        If settlement.population(popseg.Warriors) > 0 Then
            Console.WriteLine()
            Console.WriteLine()
            Console.WriteLine(settlement.name & " has " & settlement.population(popseg.Warriors) & " Warriors available.")
            Console.WriteLine("Each Warrior will contribute " & settlement.warriorEfficiency & " Power.")
            warriors = menu.getNumInput(0, 0, settlement.population(popseg.Warriors), "Draft how many Warriors? ")
        End If

        If settlement.population(popseg.Warmages) > 0 Then
            Console.WriteLine()
            Console.WriteLine(settlement.name & " has " & settlement.population(popseg.Warmages) & " Warmages available.")
            Console.WriteLine("Each Warmage will contribute " & settlement.warmageEfficiency & " Power.")
            warmages = menu.getNumInput(0, 0, settlement.population(popseg.Warmages), "Draft how many Warmages? ")
        End If

        If aHero Is Nothing AndAlso settlement.heroes.Count > 0 Then
            Console.WriteLine()
            If menu.confirmChoice(0, "Put a Hero in charge? ") = True Then
                If settlement.heroes.Count = 1 Then
                    hero = settlement.heroes(0)
                Else
                    Console.WriteLine()
                    Console.WriteLine()
                    hero = menu.getListChoice(player.heroes, 1, "Select a hero:")
                    If hero Is Nothing Then
                        player.god.addPower(powerCost, cost)
                        Exit Sub
                    End If
                End If
            End If
        End If

        Dim checkError As errorReport = settlement.checkBuildArmy(destination, warriors, warmages, player, hero)
        If checkError Is Nothing = False Then
            checkError.consoleReport(0)
            Console.ReadKey()
            player.god.addPower(powerCost, cost)
            Exit Sub
        End If

        Dim totalPower As Integer = (warriors * settlement.warriorEfficiency) + (warmages * settlement.warmageEfficiency)
        Dim heroPower As New KeyValuePair(Of heroSkill, Integer)(Nothing, 0)
        If hero Is Nothing = False Then heroPower = hero.highestHeroSkill
        totalPower += heroPower.Value

        Console.WriteLine()
        Console.WriteLine()
        Console.WriteLine("Warriors: " & warriors & " x" & settlement.warriorEfficiency)
        Console.WriteLine("Warmages: " & warmages & " x" & settlement.warmageEfficiency)
        If hero Is Nothing = False Then Console.WriteLine("Led by: " & hero.nameAndTitle & " (" & heroPower.Key.ToString & " " & heroPower.Value & ")")
        Console.WriteLine("Attacking: " & destination.ToString)
        Console.WriteLine("Total Power: " & totalPower)
        Console.WriteLine()
        If menu.confirmChoice(1) = False Then
            player.god.addPower(powerCost, cost)
            Exit Sub
        End If

        Console.WriteLine()
        Console.WriteLine()
        Console.Write("Name your army...  ")
        Dim armyName As String = Console.ReadLine.ToString

        settlement.buildArmy(armyName, destination, warriors, warmages, hero)
        report.DisplayImmediateReports(0, player)
    End Sub
    Private Sub menuCreateGeas(Optional aHero As hero = Nothing)
        Dim shardList As New List(Of shard)
        For Each shard In world.shards
            If shard.threats.Count > 0 Then shardList.Add(shard)
        Next
        If shardList.Count = 0 Then Exit Sub

        Console.WriteLine()
        Console.WriteLine()
        Const cost As Integer = 1
        Dim chosenPower As divinePower = menuSpendFateOrDestiny(cost, "send a Hero on a quest.")
        If chosenPower = Nothing Then Exit Sub

        Dim chosenHero As hero = Nothing
        If aHero Is Nothing Then
            chosenHero = menu.getListChoice(player.heroes, 1, "Select a hero:")
            If chosenHero Is Nothing Then
                player.god.addPower(chosenPower, cost)
                Exit Sub
            End If
        Else
            chosenHero = aHero
        End If

        Console.WriteLine()
        Console.WriteLine()
        Dim chosenShard As shard = menu.getListChoice(shardList, 1, "Select a shard:")
        If chosenShard Is Nothing Then
            player.god.addPower(chosenPower, cost)
            Exit Sub
        End If

        Dim chosenLocation As travelLocation = Nothing
        Dim locationList As New List(Of travelLocation)
        For Each location In chosenShard.travelLocations
            If location.threat Is Nothing = False Then locationList.Add(location)
        Next
        If locationList.Count = 0 Then
            Console.WriteLine()
            Console.WriteLine()
            Console.WriteLine(chosenShard.name & " has no threats.")
            Console.ReadKey()
            player.god.addPower(chosenPower, cost)
            Exit Sub
        ElseIf locationList.Count = 1 Then
            chosenLocation = locationList(0)
        Else
            Console.WriteLine()
            Console.WriteLine()
            chosenLocation = menu.getListChoice(locationList, 1, "Select a location:")
        End If
        If chosenLocation Is Nothing Then
            player.god.addPower(chosenPower, cost)
            Exit Sub
        End If

        Console.WriteLine()
        Console.WriteLine()
        Dim chosenThreat As threat = chosenLocation.threat
        If menu.confirmChoice(0, "Send " & aHero.nameAndTitle & " to deal with the " & chosenThreat.name & " on " & chosenShard.name & " Shard? ") = False Then
            player.god.addPower(chosenPower, cost)
            Exit Sub
        End If

        Console.WriteLine()
        Console.WriteLine()
        chosenHero.questGeas(chosenThreat)
        report.DisplayImmediateReports(0, player)
    End Sub
    Private Sub menuEstablishTradeRoute(Optional aOrigin As settlement = Nothing, Optional aDestination As settlement = Nothing)
        Const cost As Integer = 1
        Const powerCost As divinePower = divinePower.Destiny
        Dim god As god = player.god

        Console.WriteLine()
        Console.WriteLine()
        Console.WriteLine("Trade routes allow you to share Resources between settlements.")

        If player.settlements.Count < 2 Then
            Console.WriteLine("You need at least 2 settlements to establish a trade route.")
            Console.ReadKey()
            Exit Sub
        End If

        If menuSpendPower(cost, powerCost, "establish a trade route.") = False Then Exit Sub

        Dim origin As settlement = Nothing
        If aOrigin Is Nothing Then
            Console.WriteLine()
            Console.WriteLine()
            origin = menu.getListChoice(player.settlements, 1, "Select an origin:")
            If origin Is Nothing Then
                god.addPower(powerCost, cost)
                Exit Sub
            End If
        Else
            origin = aOrigin
        End If

        Dim destination As settlement = Nothing
        If aDestination Is Nothing Then
            Console.WriteLine()
            Console.WriteLine()

            Dim tSettlements As New List(Of settlement)(player.settlements)
            tSettlements.Remove(origin)
            destination = menu.getListChoice(tSettlements, 1, "Select a destination:")
            If destination Is Nothing Then
                god.addPower(powerCost, cost)
                Exit Sub
            End If
        Else
            destination = aDestination
        End If

        Console.WriteLine()
        Console.WriteLine()

        If destination.Equals(origin) Then
            Console.WriteLine("Error!")
            Console.WriteLine("The origin and destination must be different cities!")
            Console.ReadKey()
            god.addPower(powerCost, cost)
            Exit Sub
        ElseIf origin.checkAddTradeRoute(destination) Is Nothing = False Then
            origin.checkAddTradeRoute(destination).consoleReport(0)
            Console.ReadKey()
            god.addPower(powerCost, cost)
            Exit Sub
        End If

        Console.WriteLine("You are about to establish a trade route from " & origin.name & " to " & destination.name & ".")
        If menu.confirmChoice(0) = False Then
            god.addPower(powerCost, cost)
            Exit Sub
        End If

        origin.addTradeRoute(destination)
        destination.forceRefresh()              'forced refresh to update goods in destination
    End Sub
    Private Sub menuAlterBuilding(Optional aSettlement As settlement = Nothing)
        Console.WriteLine()
        Console.WriteLine()

        Const alterCost As Integer = 1
        Dim chosenPower As divinePower = menuSpendFateOrDestiny(alterCost, "change the building project.")
        If chosenPower = Nothing Then Exit Sub

        Console.WriteLine()
        Console.WriteLine()

        Dim settlement As settlement = Nothing
        If aSettlement Is Nothing Then
            settlement = menu.getListChoice(player.settlements, 1, "Choose a settlement:")
            If settlement Is Nothing Then
                player.god.addPower(chosenPower, alterCost)
                Exit Sub
            End If
        Else
            settlement = aSettlement
        End If

        Dim chosenBuilding As building = menu.getListChoice(settlement.availableBuildings, 1, "Select a new building project:")
        If chosenBuilding Is Nothing Then
            player.god.addPower(chosenPower, alterCost)
            Exit Sub
        End If

        'check for special cases in building name that require further input 
        Dim nameSplit As String() = chosenBuilding.name.Split(" ")
        If nameSplit(0) = "Terraform" Then
            Console.WriteLine()
            Console.WriteLine()

            Dim targetWildernessType As wildernessType = constants.getWildernessTypeFromString(nameSplit(1))
            If targetWildernessType = Nothing Then
                Console.WriteLine("Invalid wilderness type to terraform into.")
                player.god.addPower(chosenPower, alterCost)
                Exit Sub
            End If

            Dim tLocations As New List(Of travelLocation)
            For Each location In settlement.shard.travelLocations
                If TypeOf location Is wasteland OrElse TypeOf location Is wilderness OrElse TypeOf location Is settlementRuins Then
                    tLocations.Add(location)
                End If
            Next
            Dim chosenLocation As travelLocation = menu.getListChoice(tLocations, 1, "Select a location to terraform into " & nameSplit(1) & ":")
            If chosenLocation Is Nothing Then
                player.god.addPower(chosenPower, alterCost)
                Exit Sub
            End If

            settlement.setTerraformTarget(chosenLocation)
        End If

        Console.WriteLine()
        settlement.setNextBuilding(chosenBuilding)
        report.DisplayImmediateReports(0, player)
    End Sub
    Private Sub menuAlterTech(resource As resource)
        Console.WriteLine()
        Console.WriteLine()

        Dim god As god = player.god
        Dim descriptor As String
        If resource = resource.Culture Then descriptor = "cultural" Else descriptor = "scientific"
        Dim chosenPower As divinePower = menuSpendFateOrDestiny(1, "alter " & descriptor & " research.")
        If chosenPower = Nothing Then Exit Sub

        While True
            Console.WriteLine()
            Dim newTech As tech = menu.getListChoice(player.availableTechs(resource), 1, "Select new research:")
            Console.WriteLine()

            If newTech Is Nothing = False Then
                player.changeTech(resource, newTech)
                Console.WriteLine("You spend 1 " & chosenPower.ToString & " to begin " & descriptor & " research on '" & newTech.name & "'.")
                Console.ReadKey()
                Exit While
            Else
                Console.WriteLine("Invalid selection.")
            End If
        End While
    End Sub
    Private Sub menuAlterZeitgeist()
        Const cost As Integer = 4

        Console.WriteLine()
        Console.WriteLine()

        If player.availableZeitgeists.Count = 0 Then
            Console.WriteLine("There are no zeitgeists to adopt.")
            Console.ReadKey()
            Exit Sub
        End If

        Dim chosenPower As divinePower = menuSpendFateOrDestiny(cost, "adopt a new zeigeist.")
        If chosenPower = Nothing Then Exit Sub

        Console.WriteLine()
        Dim chosenZeitgeist As zeitgeist = menu.getListChoice(player.availableZeitgeists, 1, "Select zeitgeist to adopt:")
        If chosenZeitgeist Is Nothing Then
            player.god.addPower(chosenPower, cost)     'refund paid cost
            Exit Sub
        End If

        Console.WriteLine()
        Console.WriteLine("WARNING: This will remove the following philosophies:")
        Dim printedPhilo As Boolean = False
        For Each philo In player.philosophies
            Console.WriteLine(vbSpace(1) & "└ " & philo.name)
            For Each modifier In philo.modifiers
                modifier.consoleReport(2, "└ ")
            Next
            printedPhilo = True
        Next
        If printedPhilo = False Then Console.WriteLine(vbSpace(1) & "└ Nothing")
        Console.WriteLine()

        If menu.confirmChoice(0) = True Then
            Console.WriteLine()
            Console.WriteLine()
            player.adoptZeitgeist(chosenZeitgeist)
            Console.WriteLine("You spend " & cost & " " & chosenPower.ToString & " to adopt the '" & chosenZeitgeist.name & "' zeitgeist.")
            Console.ReadKey()
        Else
            player.god.addPower(chosenPower, cost)     'refund paid cost
            Exit Sub
        End If
    End Sub
    Private Sub menuAlterPhilosophy()
        Console.WriteLine()
        Console.WriteLine()

        Console.Write("(A)dd or (R)emove Philosophy? ")
        Dim input As ConsoleKeyInfo = Console.ReadKey

        If input.Key = ConsoleKey.A Then
            menuAddPhilosophy()
        ElseIf input.Key = ConsoleKey.R Then
            menuRemovePhilosophy()
        Else
            Exit Sub
        End If
    End Sub
    Private Sub menuAddPhilosophy()
        Console.WriteLine()
        Console.WriteLine()

        If player.philosophies.Count = 0 Then
            Console.WriteLine("There are no philosophies to add.")
            Console.ReadKey()
            Exit Sub
        End If

        If menuSpendFateOrDestiny(1, "add a philosophy.") = Nothing Then Exit Sub

        Console.WriteLine()
        Dim chosenPhilosophy As philosophy = menu.getListChoice(player.availablePhilosophies, 1, "Select philosophy to add:")
        If player.checkAddPhilosophy(chosenPhilosophy) Is Nothing = False Then
            player.checkAddPhilosophy(chosenPhilosophy).consoleReport(0)
            Exit Sub
        End If

        player.addPhilosophy(chosenPhilosophy)
    End Sub
    Private Sub menuRemovePhilosophy()
        Console.WriteLine()
        Console.WriteLine()

        If player.availablePhilosophies.Count = 0 Then
            Console.WriteLine("There are no philosophies to remove.")
            Console.ReadKey()
            Exit Sub
        End If

        Dim chosenPhilosophy As philosophy = menu.getListChoice(player.philosophies, 1, "Select philosophy to remove:")
        If player.checkRemovePhilosophy(chosenPhilosophy) Is Nothing = False Then
            player.checkRemovePhilosophy(chosenPhilosophy).consoleReport(0)
            Exit Sub
        End If

        player.removePhilosophy(chosenPhilosophy)
    End Sub
    Private Sub menuDivineConception(Optional aSettlement As settlement = Nothing)
        Const cost As Integer = 2
        Const powerCost As divinePower = divinePower.Destiny
        Dim god As god = player.god

        Console.WriteLine()
        Console.WriteLine()

        If menuSpendPower(cost, powerCost, "concieve a child.") = False Then Exit Sub

        Console.WriteLine()
        Console.WriteLine()
        Dim settlement As settlement
        If aSettlement Is Nothing Then
            settlement = menu.getListChoice(player.settlements, 1, "Select a settlement:")
            If settlement Is Nothing Then
                god.addPower(powerCost, cost)
                Exit Sub
            End If
        Else
            settlement = aSettlement
        End If

        Console.WriteLine()
        Dim popsegs As New List(Of String)
        For Each popseg In constants.popsegArray
            popsegs.Add(popseg.ToString)
        Next
        Dim popStr As String = menu.getListChoice(popsegs, 1, "Select a Citizen:")
        Dim pop As popseg = constants.getPopSegFromString(popStr)
        If pop = Nothing Then
            god.addPower(powerCost, cost)
            Exit Sub
        End If

        Console.WriteLine()
        Console.WriteLine("You spend " & cost & " " & powerCost.ToString & " to ensure the next birth in " & settlement.name & " is a " & stripS(popStr) & ".")
        settlement.addSettlementModifier(New modifier(Nothing, "Divine Conception", "SettlementGuaranteedRecruit " & popStr))
        Console.ReadKey()
    End Sub

    Private Sub menuCheat()
        Console.Clear()
        Console.WriteLine("--------")
        Console.WriteLine("|CHEATS|")
        Console.WriteLine("--------")

        Dim menuChoices As New Dictionary(Of Char, String)
        menuChoices.Add("s"c, "Spawn New Settlement")
        menuChoices.Add("r"c, "Add Resource")
        menuChoices.Add("m"c, "Add Modifier")
        menuChoices.Add("1"c, "Setup Modifier Tests")
        menuChoices.Add("2"c, "Setup Eluvar Trade Test")
        menuChoices.Add("3"c, "Setup Army Test")
        menuChoices.Add("4"c, "Setup Hero and Artefact Test")
        menuChoices.Add("5"c, "Setup Test Threat")
        menuChoices.Add("6"c, "Setup Terraform Test")

        Select Case menu.getListChoice(menuChoices, 1)
            Case "s"c
                Console.WriteLine()
                Console.WriteLine()
                Dim targetShard As shard = menu.getListChoice(world.habitableShards, 1)
                targetShard.settleSettlement(player)
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "r"c
                Console.WriteLine()
                Console.WriteLine()
                Dim targetSettlement As settlement = menu.getListChoice(player.settlements, 1)
                Dim goodList As New List(Of String)
                For Each item In constants.goodArray
                    goodList.Add(item.ToString)
                Next
                Dim targetGood As good = constants.getGoodFromString(menu.getListChoice(goodList, 1))
                If player.goodsAvailable.Contains(targetGood) = False Then player.addAvailableGood(targetGood)
                targetSettlement.addSettlementModifier(New modifier(Nothing, targetGood.ToString & " Harvesting", "GoodUnlock Eluvar"))
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "m"c
                Console.WriteLine()
                Console.WriteLine()
                Console.Write("Input modifier raw string: ")
                Dim input As String = Console.ReadLine
                If input = "" Then Exit Sub
                Dim modifier As New modifier(Nothing, "Cheat Modifier", input)
                Dim scope As String = menu.getListChoice({"Empire", "Settlement", "Hero"}, 1, "Select scope")
                Select Case scope
                    Case "Empire"
                        player.addSortedModifier(modifier)
                    Case "Settlement"
                        Dim settlement As settlement = menu.getListChoice(player.settlements, 1, "Select settlement")
                        settlement.addSettlementModifier(modifier)
                    Case "Hero"
                        Dim hero As hero = menu.getListChoice(player.heroes, 1, "Select hero")
                        hero.addHeroModifier(modifier)
                End Select
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "1"c
                Console.WriteLine()
                Console.WriteLine()
                Dim settlement As settlement = player.settlements(0)
                Dim shard As shard = settlement.shard
                world.addWorldModifier(New modifier(Nothing, "Plague", "SettlementIncome Food -10", 5))                                     'world scope
                player.addSortedModifier(New modifier(Nothing, "Scientific Convention", "EmpireIncome Science +10/Savants"))                'empire scope, dependent modifier
                shard.addShardModifier(New modifier(Nothing, "Travelling Architects", "SettlementBuildEfficiency +10 with Wood", 10))       'shard scope, conditional modifier
                settlement.addSettlementModifier(New modifier(Nothing, "Divine Conception", "SettlementGuaranteedRecruit Traders", 5))      'settlement scope
                settlement.empire.addAvailableGood(good.Wood)
                settlement.addSettlementModifier(New modifier(Nothing, "Logging", "GoodUnlock Wood"))
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "2"c
                Console.WriteLine()
                Console.WriteLine()
                Dim targetShard As shard = world.habitableShards(0)
                Dim targetWilderness As wildernessType = targetShard.wildernessTypes(0)
                Dim newSettlement As settlement = targetShard.settleSettlement(player)
                player.addAvailableGood(good.Eluvar)
                newSettlement.addSettlementModifier(New modifier(Nothing, targetWilderness.ToString & " Harvesting", "GoodUnlock Eluvar with " & targetWilderness.ToString))
                player.settlements(0).addPopulation(popseg.Traders)
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "3"c
                Console.WriteLine()
                Console.WriteLine()
                Dim settlement As settlement = player.settlements(0)
                Dim hero As hero = hero.buildRandomHero(settlement)
                player.addHero(hero, settlement)
                settlement.addPopulation(popseg.Warriors, 10)
                settlement.addSettlementModifier(New modifier(Nothing, "War Camps", "SettlementIncome Food +200"))
                settlement.addSettlementModifier(New modifier(Nothing, "Martial Law", "SettlementPublicOrder +200"))
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "4"c
                Console.WriteLine()
                Console.WriteLine()
                Dim settlement As settlement = player.settlements(0)
                Dim hero As hero
                If player.heroes.Count > 0 Then
                    hero = player.heroes(0)
                Else
                    hero = hero.buildRandomHero(settlement)
                    player.addHero(hero, settlement)
                End If
                Dim blessing As New modifier(Nothing, "Grognark's Vision", "HeroSkill Charisma +10", 5)
                hero.addHeroModifier(blessing)
                Dim sword As New item
                With sword
                    .name = "Grognark's Sword"
                    .equipmentSlot = "Weapon"
                    .modifiers.Add(New modifier(.modifiers, .name, "HeroSkill Combat +10"))
                End With
                hero.addEquipment(sword)
                Dim replacementWeapon As New item
                With replacementWeapon
                    .name = "Grognark's Greatsword"
                    .equipmentSlot = "Weapon"
                    .modifiers.Add(New modifier(.modifiers, .name, "HeroAllSkills +10"))
                End With
                hero.addEquipment(replacementWeapon)
                Dim artefact As New item
                With artefact
                    .name = "Grognark's Skull"
                    .equipmentSlot = "Artefact"
                    .modifiers.Add(New modifier(.modifiers, .name, "SettlementIncome Faith +10"))
                End With
                hero.addEquipment(artefact)
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "5"c
                Console.WriteLine()
                Console.WriteLine()
                Dim threatLocation As travelLocation = player.settlements(0).shard.travelLocations(1)
                Dim threat As New threat
                With threat
                    .name = "Evil Necromancer"
                    .tier = 1
                    .armyTimerOriginal = 3
                    .armyTimer = 3
                    .armyTemplate(1) = New army
                    With .armyTemplate(1)
                        .name = "Scary Skeletons"
                        .warriors = 3
                        .warriorPower = 10
                    End With
                    For n = 1 To 5
                        Dim name As String = "Challenge Title " & n
                        Dim text As String = "Challenge Text " & n
                        Dim choiceText As String() = {"Test your luck!", "Test your strength!", "Test your charm!"}
                        Dim skill As heroSkill() = {heroSkill.Thievery, heroSkill.Combat, heroSkill.Charisma}
                        Dim difficulty As Integer() = {100, 120, 150}
                        Dim challenge As New challenge(name, text, choiceText, skill, difficulty, challengeConsequence.ReturnHome)
                        .challenges.Add(challenge)
                    Next

                    .activeEffects.Add(New modifier(.activeEffects, .name, "SettlementIncome Faith -10"))
                End With
                threatLocation.setThreat(threat)
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case "6"c
                Console.WriteLine()
                Console.WriteLine()
                Dim terraformProject As New building
                With terraformProject
                    .cost = 100
                    .name = "Terraform Forest"
                End With
                player.addAvailableBuilding(terraformProject)
                report.DisplayTempReports(0)
                Console.WriteLine()
                Console.WriteLine("Done.")
                Console.ReadKey()


            Case Else
                Exit Sub
        End Select
    End Sub

    Private Function menuSpendPower(cost As Integer, divinePower As divinePower, purpose As String) As Boolean
        Dim god As god = player.god
        Console.WriteLine("It will cost you " & cost & " " & divinePower.ToString & " to " & purpose)
        If god.power(divinePower) < cost Then
            Console.WriteLine("You do not have enough power.")
            Console.ReadKey()
            Return False
        End If

        If menu.confirmChoice(0, "Do you want to proceed? ") = True Then
            god.addPower(divinePower, -cost)
            Return True
        Else
            Return False
        End If
    End Function
    Private Function menuSpendFateOrDestiny(cost As Integer, purpose As String) As divinePower
        Dim god As god = player.god
        Console.WriteLine("It will cost you " & cost & " Fate or Destiny to " & purpose)
        If god.power(divinePower.Fate) < cost AndAlso god.power(divinePower.Destiny) < cost Then
            Console.WriteLine("You do not have enough power.")
            Console.ReadKey()
            Return Nothing
        End If

        Dim chosenPower As divinePower = menu.getFateDestinyChoice
        If chosenPower = Nothing Then Return Nothing
        If god.power(chosenPower) < cost Then
            Console.WriteLine("You do not have enough " & chosenPower.ToString & ".")
            Console.ReadKey()
            Return Nothing
        End If

        god.addPower(chosenPower, -cost)
        Return chosenPower
    End Function
    Private Function menuSpendFateAndDestiny(cost As Integer, purpose As String) As Boolean
        Dim god As god = player.god
        Console.WriteLine("It will cost you " & cost & " Fate and Destiny to " & purpose)
        If god.power(divinePower.Fate) < cost OrElse god.power(divinePower.Destiny) < cost Then
            Console.WriteLine("You do not have enough power.")
            Console.ReadKey()
            Return False
        End If

        If menu.confirmChoice(0, "Do you want to proceed? ") = True Then
            god.addPower(divinePower.Fate, -cost)
            god.addPower(divinePower.Destiny, -cost)
            Return True
        Else
            Return False
        End If
    End Function
End Module
