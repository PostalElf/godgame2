Public Class god
    Friend Property world As world
    Friend Property empire As empire
    Friend Property name As String
    Friend Property jurisdiction As String
    Friend Property faith As Integer
        Get
            Return empire.resources(resource.Faith)
        End Get
        Set(value As Integer)
            empire.resources(resource.Faith) = value
        End Set
    End Property
    Public Sub New(aEmpire As empire, aDomain As divineDomain)
        For n As divinePower = divinePower.Fate To divinePower.Destiny
            pPower.Add(n, 0)
            powerDice.Add(n, 4)
        Next

        empire = aEmpire
        world = empire.world

        addDomain(aDomain)
        jurisdiction = getDomainJurisdiction(aDomain)
    End Sub
    Public Overrides Function ToString() As String
        Return name
    End Function
    Friend Sub consoleReport(indent As Integer)
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)
        Dim inddd As String = vbSpace(indent + 2)

        Console.WriteLine(ind & name)
        Console.WriteLine(indd & "└ Faith: " & faith.ToString("N0"))
        consoleReportDice(2, "└ ")

        Console.WriteLine(indd & "└ Juris: " & jurisdiction)
        Console.WriteLine(indd & "└ Domains:")
        For Each domain In domains
            Console.WriteLine(inddd & "└ " & domain.ToString)
        Next
    End Sub

    Friend Property isAI As Boolean = False

    Private Property pPower As New Dictionary(Of divinePower, Integer)
    Friend ReadOnly Property power As Dictionary(Of divinePower, Integer)
        Get
            Return pPower
        End Get
    End Property
    Private Property powerDice As New Dictionary(Of divinePower, Integer)
    Friend Const diceFaithCost As Integer = 1000
    Friend Sub addPower(aPower As divinePower, value As Integer)
        If pPower(aPower) + value < 0 Then
            Debug.Print("Insufficient " & aPower)
            Exit Sub
        End If

        pPower(aPower) += value
    End Sub
    Friend Sub rollDice(number As Integer)
        For n = 1 To number
            If empire.resources(resource.Faith) < diceFaithCost Then
                Debug.Print("Insufficient faith to roll dice")
                Exit Sub
            End If

            empire.addResource(resource.Faith, -diceFaithCost)

            Dim fateIncome As Integer = rng.Next(1, powerDice(divinePower.Fate) + 1)
            Dim destinyIncome As Integer = rng.Next(1, powerDice(divinePower.Destiny) + 1)
            addPower(divinePower.Fate, fateIncome)
            addPower(divinePower.Destiny, destinyIncome)

            report.ImmediateReport(name & " rolled the dice and received " & fateIncome & " Fate and " & destinyIncome & " Destiny.", reportQueueType.GodDiceRoll, empire)
        Next

        report.DisplayImmediateReports(0, empire)
    End Sub
    Friend Sub consoleReportDice(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent) & prefix
        Dim len As Integer = ("Destiny: ").Count

        Console.WriteLine(ind & fakeTab("Fate: ", len) & pPower(divinePower.Fate) & " (1d" & powerDice(divinePower.Fate) & ")")
        Console.WriteLine(ind & fakeTab("Destiny: ", len) & pPower(divinePower.Destiny) & " (1d" & powerDice(divinePower.Destiny) & ")")
    End Sub

    Private Property pDomains As New List(Of divineDomain)
    Friend ReadOnly Property domains As List(Of divineDomain)
        Get
            Return pDomains
        End Get
    End Property
    Friend ReadOnly Property domainFaithCost As Integer
        Get
            Return 5000 + (pDomains.Count * 1000)
        End Get
    End Property
    Friend ReadOnly Property domainExtrajurisFaithCost As Integer
        Get
            Return CInt(domainFaithCost * 1.2)
        End Get
    End Property
    Friend Sub buyDomain(domain As divineDomain)
        Dim cost As Integer
        If getDomainJurisdiction(domain) = jurisdiction Then cost = domainFaithCost Else cost = domainExtrajurisFaithCost
        If faith < cost Then
            Debug.Print("Insufficient Faith.")
            Exit Sub
        End If

        report.ImmediateReport(name & " spends " & cost & " Faith and acquires the domain of " & domain.ToString & ".", reportQueueType.GodDomainNew, empire)
        report.DisplayImmediateReports(0, empire)
        empire.addResource(resource.Faith, -cost)
        addDomain(domain)
    End Sub
    Private Sub addDomain(domain As divineDomain)
        If pDomains.Contains(domain) Then
            Debug.Print("Already contains domain.")
            Exit Sub
        End If

        pDomains.Add(domain)
        world.openDomains.Remove(domain)
    End Sub
    Private Sub removeDomain(domain As divineDomain)
        If pDomains.Contains(domain) = False Then
            Debug.Print("Does not contain domain.")
            Exit Sub
        End If

        pDomains.Remove(domain)
    End Sub
    Friend Shared Function getDomainJurisdiction(domain As divineDomain) As String
        Select Case domain
            Case 1 To 10 : Return "Cosmic"
            Case 11 To 20 : Return "Chthonic"
            Case 21 To 30 : Return "Conceptual"
            Case 31 To 40 : Return "Craft"
            Case Else : Return Nothing
        End Select
    End Function

    Private Const maxHand As Integer = 10
    Private Property pHandRewards As New List(Of reward)
    Friend ReadOnly Property handRewards As List(Of reward)
        Get
            Return pHandRewards
        End Get
    End Property
    Private Property pHandThreats As New List(Of threat)
    Friend ReadOnly Property handThreats As List(Of threat)
        Get
            Return pHandThreats
        End Get
    End Property
    Friend Function addHandCard(reward As reward) As errorReport
        If pHandRewards.Count + pHandThreats.Count + 1 > maxHand Then
            Debug.Print("Insufficient space in hand.")
            Return New errorReport("Too many cards already in hand; discard or play some before attempting to add more.")
        End If

        pHandRewards.Add(reward)
        Return Nothing
    End Function
    Friend Function addHandCard(threat As threat) As errorReport
        If pHandRewards.Count + pHandThreats.Count + 1 > maxHand Then
            Debug.Print("Insufficient space in hand.")
            Return New errorReport("Too many cards already in hand; discard or play some before attempting to add more.")
        End If

        pHandThreats.Add(threat)
        Return Nothing
    End Function
    Private Shared Property rewardCards As New List(Of reward)
    Private Shared Function buildRewardCards() As List(Of reward)
        Dim total As New List(Of reward)
        Dim equipmentSlots As New List(Of String) From {"Weapon", "Helm", "Armour"}

        '7x3 standard gear
        For n As heroSkill = 1 To 7
            For Each eq In equipmentSlots
                Dim reward As New reward
                With reward
                    .name = eq & " of " & n.ToString
                    .minTier = 1
                    .equipmentSlot = eq
                    For i = 1 To 3
                        .modifiers(i).Add(New modifier(.modifiers(i), .name, "HeroSkill " & n.ToString & " +" & i * 10))
                    Next
                End With
                total.Add(reward)
            Next
        Next


        '16 special
        For n = 1 To 16
            Dim reward As New reward
            With reward
                Select Case n
                    Case 1
                        .name = "Amulet of Speed"
                        .minTier = 1
                        .equipmentSlot = "Amulet"
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroTravelSpeed +5"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroTravelSpeed +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroTravelSpeed +15"))

                    Case 2
                        .name = "Ring of Haste"
                        .minTier = 1
                        .equipmentSlot = "Boots"
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroTravelSpeed +5"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroTravelSpeed +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroTravelSpeed +15"))

                    Case 3
                        .name = "Skullcrusher"
                        .minTier = 2
                        .equipmentSlot = "Weapon"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Constitution +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Combat +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Constitution +40"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Combat +20"))

                    Case 4
                        .name = "Shepherd's Crook"
                        .minTier = 2
                        .equipmentSlot = "Weapon"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Willpower +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Lore +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Willpower +40"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Lore +20"))

                    Case 5
                        .name = "Forbidden Tome"
                        .minTier = 2
                        .equipmentSlot = "Weapon"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Lore +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Magic +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Lore +40"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Magic +20"))

                    Case 6
                        .name = "Golden Lyre"
                        .minTier = 2
                        .equipmentSlot = "Weapon"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Charisma +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Thievery +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Charisma +40"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Thievery +20"))

                    Case 7
                        .name = "Bloodletter Knife"
                        .minTier = 2
                        .equipmentSlot = "Weapon"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Thievery +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Charisma +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Thievery +40"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Charisma +20"))

                    Case 8
                        .name = "Singer's Sword"
                        .minTier = 2
                        .equipmentSlot = "Weapon"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Combat +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Constitution +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Combat +40"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Constitution +20"))

                    Case 9
                        .name = "Precursor Wand"
                        .minTier = 2
                        .equipmentSlot = "Weapon"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Magic +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Lore +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Magic +40"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Lore +20"))

                    Case 10
                        .name = "Ring of Power"
                        .minTier = 2
                        .equipmentSlot = "Ring"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroAllSkills +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroAllSkills +15"))

                    Case 11
                        .name = "Talisman of Skill"
                        .minTier = 2
                        .equipmentSlot = "Amulet"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroAllSkills +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroAllSkills +15"))

                    Case 12
                        .name = "Gorgoth's Skull"
                        .minTier = 2
                        .equipmentSlot = "Helm"
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroAllSkills +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroAllSkills +15"))

                    Case 13
                        .name = "Swordsworn Mail"
                        .minTier = 1
                        .equipmentSlot = "Armour"
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Constitution +5"))
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Combat +5"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Constitution +10"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Combat +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Constitution +15"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Combat +15"))

                    Case 14
                        .name = "Mithril Circlet"
                        .minTier = 1
                        .equipmentSlot = "Helm"
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Willpower +5"))
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Lore +5"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Willpower +10"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Lore +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Willpower +15"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Lore +15"))

                    Case 15
                        .name = "Sage's Ring"
                        .minTier = 1
                        .equipmentSlot = "Ring"
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Magic +5"))
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Lore +5"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Magic +10"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Lore +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Magic +15"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Lore +15"))

                    Case 16
                        .name = "Cloak of Living Shadow"
                        .minTier = 1
                        .equipmentSlot = "Armour"
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Thievery +5"))
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "HeroSkill Charisma +5"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Thievery +10"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "HeroSkill Charisma +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Thievery +15"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "HeroSkill Charisma +15"))
                End Select
            End With
        Next

        'artefacts
        For n = 1 To 10
            Dim reward As New reward
            With reward
                .equipmentSlot = "Artefact"
                Select Case n
                    Case 1
                        .name = "Cornucopia of Plenty"
                        .minTier = 2
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementPopulationIncome Farmers +10"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementIncome Food +10/Priests"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementPopulationIncome Farmers +20"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementIncome Food +20/Priests"))

                    Case 2
                        .name = "Bones of the Anonymous Saint"
                        .minTier = 1
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "SettlementIncome Faith +20"))
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "SettlementIncome Faith +5/Traders"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementIncome Faith +50"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementIncome Faith +10/Traders"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementIncome Faith +100"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementIncome Faith +10/Traders"))

                    Case 3
                        .name = "Holy Scripture"
                        .minTier = 1
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "SettlementPopulationIncome Priests +5"))
                        .modifiers(1).Add(New modifier(.modifiers(1), .name, "SettlementIncome Faith +5/Artists"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementPopulationIncome Priests +10"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementIncome Faith +10/Artists"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementPopulationIncome Priests +15"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementIncome Faith +15/Artists"))

                    Case 4
                        .name = "Bonehowler Totem"
                        .minTier = 1
                        .modifiers(1).Add(New modifier(.modifiers(2), .name, "SettlementDefence +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementDefence +30"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementDefence +40"))

                    Case 5
                        .name = "Coldiron Crown"
                        .minTier = 2
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementPublicOrder +20"))
                        .modifiers(2).Add(New modifier(.modifiers(2), .name, "SettlementDefence +10"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementPublicOrder +30"))
                        .modifiers(3).Add(New modifier(.modifiers(3), .name, "SettlementDefence +30"))

                End Select
            End With
            total.Add(reward)
        Next

        Return total
    End Function
    Friend Function drawRewardCard() As reward
        If rewardCards.Count = 0 Then rewardCards = buildRewardCards()
        Dim roll As Integer = rng.Next(rewardCards.Count)
        drawRewardCard = rewardCards(roll)
        rewardCards.RemoveAt(roll)
    End Function
    Friend Function drawThreatCard() As threat
        Dim roll As Integer = rng.Next(1, 11)
        Dim threat As threat = threat.threatFileget(roll)
        Return threat
    End Function
End Class

Public Enum divinePower
    Fate = 1
    Destiny
End Enum

Public Enum divineDomain
    Sky = 1
    Moon
    Stars
    Sun
    Storm
    Ether

    Earth = 11
    Wealth
    Death
    Harvest
    Water
    Darkness

    War = 21
    Adventure
    Love
    Fire
    Time
    Chaos

    Sorcery = 31
    Smithing
    Arts
    Prophecy
    Medicine
    Trickery
End Enum
