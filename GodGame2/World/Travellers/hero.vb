Public Class hero
    Implements traveller
    Friend Property name As String Implements traveller.name
    Friend Property profession As String
    Friend ReadOnly Property nameAndTitle As String
        Get
            Return name & " the " & profession
        End Get
    End Property
    Friend Property isMale As Boolean
    Friend Property age As Integer
    Friend ReadOnly Property pronoun As String
        Get
            If isMale = True Then Return "He" Else Return "She"
        End Get
    End Property
    Friend ReadOnly Property hisOrHer As String
        Get
            If isMale = True Then Return "His" Else Return "Her"
        End Get
    End Property
    Friend Property empire As empire Implements traveller.empire
    Friend Property army As army = Nothing
    Friend Sub tick()
        If mustRefresh = True Then refresh()

        'movement
        If army Is Nothing AndAlso travelLocation Is Nothing Then
            travelProgress += travelSpeed
            If travelProgress >= travelCost Then
                teleportTo(travelDestination)
                report.Add(name & " arrives at his/her destination of " & travelLocation.name & " on " & travelLocation.shard.name & ".", reportQueueType.HeroArrivesLocation, empire)
            End If
        End If


        'challenge threat
        If travelLocation Is Nothing = False AndAlso travelLocation.threat Is Nothing = False Then
            travelLocation.threat.challenge(Me)
        End If


        'modifiers
        If heroModifiers.Count > 0 Then
            For n = heroModifiers.Count - 1 To 0
                Dim modifier As modifier = heroModifiers(n)
                modifier.tick()
                If modifier.duration = 0 Then
                    heroModifiers.Remove(modifier)
                    mustRefresh = True
                    report.Add("'" & modifier.name & "' for " & name & " has expired.", reportQueueType.HeroModifierExpire, empire)
                End If
            Next
        End If
    End Sub
    Public Overrides Function ToString() As String
        Return name & " the " & profession
    End Function
    Friend Sub consoleReport(indent As Integer)
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)
        Dim inddd As String = vbSpace(indent + 2)

        If mustRefresh = True Then refresh()

        Console.WriteLine(ind & nameAndTitle)
        Dim gender As String
        If isMale = True Then gender = "Male" Else gender = "Female"
        Console.WriteLine(indd & "└ Age: " & age)
        Console.WriteLine(indd & "└ Gender: " & gender)


        'report location
        If army Is Nothing Then
            If travelLocation Is Nothing Then
                Console.WriteLine(indd & "└ Travelling To: " & travelDestination.name & ", " & travelDestination.shard.name & " Shard (" & travelProgress & "/" & travelCost & ", +" & travelSpeed & ")")
            Else
                Console.WriteLine(indd & "└ Location: " & travelLocation.name & ", " & travelLocation.shard.name & " Shard")
            End If
        Else
            Console.WriteLine(indd & "└ Warband: " & army.name)
            Console.WriteLine(inddd & "└ " & army.locationReport)
        End If

        'report skills
        Dim netHeroSkills As New Dictionary(Of heroSkill, Integer)
        For Each heroSkillValue In heroSkills
            If heroSkillValue.Value <> 0 Then netHeroSkills.Add(heroSkillValue.Key, heroSkillValue.Value)
        Next
        If netHeroSkills.Count > 0 Then
            Console.WriteLine(indd & "└ Skills:")
            For Each kvp In netHeroSkills
                Dim heroSkill As heroSkill = kvp.Key
                Dim value As Integer = kvp.Value
                Console.WriteLine(inddd & "└ " & parseHeroSkill(heroSkill))
            Next
        End If

        'report equipment
        If pEquipment.Count > 0 Then
            Console.WriteLine(indd & "└ Equipment:")
            For Each item In pEquipment
                item.Value.consoleReport(indent + 2, "└ ")
            Next
        End If

        'report modifiers
        If heroModifiers.Count > 0 Then
            Console.WriteLine(indd & "└ Modifiers:")
            For Each modifier In heroModifiers
                modifier.consoleReport(indent + 2, "└ ")
            Next
        End If
    End Sub
    Private Function parseHeroSkill(heroSkill As heroSkill) As String
        Const fakeTabLength As Integer = 14
        Dim total As String = fakeTab(heroSkill.ToString & ": ", fakeTabLength)

        Dim rawSkill As Integer = heroRawSkills(heroSkill)
        Dim modifierSkill As Integer = heroModifierSkills(heroSkill)
        Dim equipmentSkill As Integer = heroEquipmentSkills(heroSkill)

        total &= rawSkill
        If modifierSkill <> 0 Then total &= " " & sign(modifierSkill) & " " & modifierSkill & " (Mods)"
        If equipmentSkill <> 0 Then total &= " " & sign(equipmentSkill) & " " & equipmentSkill & " (Gear)"

        Return total
    End Function


    'create new hero and destroy
    Public Sub New()
        initHeroSkills()
        For Each heroSkill In constants.heroSkillArray
            heroRawSkills.Add(heroSkill, 0)
        Next
    End Sub
    Friend Shared Function buildRandomHero(settlement As settlement)
        Dim hero As New hero
        With hero
            .travelLocation = settlement
            .empire = settlement.empire

            Dim popsegs As New List(Of popseg)(settlement.popsegGrowth)
            Dim roll As Integer = rng.Next(popsegs.Count)
            Select Case popsegs(roll).ToString
                Case "Farmers"
                    .profession = "Barbarian"
                    .addHeroRawSkill(heroSkill.Constitution, 20)
                    .addHeroRawSkill(heroSkill.Combat, 10)

                Case "Priests"
                    .profession = "Prophet"
                    .addHeroRawSkill(heroSkill.Willpower, 20)
                    .addHeroRawSkill(heroSkill.Lore, 10)

                Case "Savants"
                    .profession = "Wizard"
                    .addHeroRawSkill(heroSkill.Lore, 20)
                    .addHeroRawSkill(heroSkill.Magic, 10)

                Case "Artists"
                    .profession = "Bard"
                    .addHeroRawSkill(heroSkill.Charisma, 20)
                    .addHeroRawSkill(heroSkill.Thievery, 10)

                Case "Traders"
                    .profession = "Rogue"
                    .addHeroRawSkill(heroSkill.Thievery, 20)
                    .addHeroRawSkill(heroSkill.Charisma, 10)

                Case "Warmages"
                    .profession = "Sorcerer"
                    .addHeroRawSkill(heroSkill.Magic, 20)
                    .addHeroRawSkill(heroSkill.Willpower, 10)

                Case "Warriors"
                    .profession = "Fighter"
                    .addHeroRawSkill(heroSkill.Combat, 20)
                    .addHeroRawSkill(heroSkill.Constitution, 10)

                Case Else
                    Debug.Print("Invalid popseg")
                    Return Nothing
            End Select

            .setRandomHeroNameAndSex(.profession)
            .age = 20 + rng.Next(5)
        End With
        Return hero
    End Function
    Private Sub setRandomHeroNameAndSex(profession As String)
        isMale = coinFlip()

        Dim prePathname As String = "data/preNames.txt"
        Dim sufPathname As String = "data/sufNames.txt"
        'If isMale = True Then
        '    prePathname &= "preMaleNames.txt"
        '    sufPathname &= "sufMaleNames.txt"
        'Else
        '    prePathname &= "preFemNames.txt"
        '    sufPathname &= "sufFemNames.txt"
        'End If

        Dim preNames As List(Of String) = fileget(prePathname)
        Dim preName As String = preNames(rng.Next(preNames.Count))
        Dim sufNames As List(Of String) = fileget(sufPathname)
        Dim sufName As String = sufNames(rng.Next(sufNames.Count))

        name = preName & sufName
    End Sub
    Friend Sub kill()
        army = Nothing
        pHeroSkills.Clear()
        heroRawSkills.Clear()
        heroModifierSkills.Clear()
        heroModifiers.Clear()
        heroEquipmentSkills.Clear()
        pEquipment.Clear()

        travelLocation = Nothing
        travelDestination = Nothing
        questHome = Nothing

        report.Add(nameAndTitle & " was slain!", reportQueueType.HeroDeath, empire)

        empire.removeHero(Me)
        empire = Nothing
    End Sub


    'refresh
    Friend Property mustRefresh As Boolean = False
    Private Sub refresh()
        'reset all heroskill dictionaries (except rawSkills) to 0, and travelSpeed to 10
        initHeroSkills()
        travelSpeed = 10


        'consolidate modifiers into totalHeroModifiers list
        Dim totalHeroModifiers As New List(Of modifier)
        totalHeroModifiers.AddRange(heroModifiers)
        totalHeroModifiers.AddRange(empireHeroModifiers)


        'calculate HeroModifierSkills
        For Each modifier In totalHeroModifiers
            applyModifierEffect(modifier, heroModifierSkills)
        Next


        'calculate heroEquipmentSkills
        For Each kvp In pEquipment
            Dim item As item = kvp.Value
            For Each modifier In item.modifiers
                applyModifierEffect(modifier, heroEquipmentSkills)
            Next
        Next


        'add to heroSkills
        For Each heroSkill In constants.heroSkillArray
            pHeroSkills(heroSkill) += heroRawSkills(heroSkill)
            pHeroSkills(heroSkill) += heroModifierSkills(heroSkill)
            pHeroSkills(heroSkill) += heroEquipmentSkills(heroSkill)
        Next


        'set minimums
        If travelSpeed < minTravelSpeed Then travelSpeed = minTravelSpeed


        'toggle off mustBuildHeroSkills
        mustRefresh = False
    End Sub
    Friend Sub forceRefresh()
        refresh()
    End Sub
    Private Sub applyModifierEffect(modifier As modifier, modifierList As Dictionary(Of heroSkill, Integer))
        Select Case modifier.quality
            Case modifierQuality.HeroAllSkills
                For Each heroSkill In constants.heroSkillArray
                    modifierList(heroSkill) += modifier.value(Me)
                Next

            Case modifierQuality.HeroSkill
                modifierList(modifier.heroSkill) += modifier.value(Me)

            Case modifierQuality.HeroTravelSpeed
                travelSpeed += modifier.value(Me)
        End Select
    End Sub


    'hero skills, modifiers and equipment
    Private Property pHeroSkills As New Dictionary(Of heroSkill, Integer)
    Private Sub initHeroSkills()
        'raw skills are not touched because initHeroSkills is also called during refresh
        For Each heroSkill In constants.heroSkillArray
            If pHeroSkills.ContainsKey(heroSkill) Then pHeroSkills(heroSkill) = 0 Else pHeroSkills.Add(heroSkill, 0)
            If heroModifierSkills.ContainsKey(heroSkill) Then heroModifierSkills(heroSkill) = 0 Else heroModifierSkills.Add(heroSkill, 0)
            If heroEquipmentSkills.ContainsKey(heroSkill) Then heroEquipmentSkills(heroSkill) = 0 Else heroEquipmentSkills.Add(heroSkill, 0)
        Next
    End Sub
    Friend ReadOnly Property getHeroRawSkill(heroSkill As heroSkill) As Integer
        Get
            Return heroRawSkills(heroSkill)
        End Get
    End Property
    Friend ReadOnly Property heroSkills As Dictionary(Of heroSkill, Integer)
        Get
            Return pHeroSkills
        End Get
    End Property
    Friend ReadOnly Property highestHeroSkill As KeyValuePair(Of heroSkill, Integer)
        Get
            Dim highestKey As heroSkill
            Dim highestValue As Integer = -1
            For Each kvp In heroSkills
                If kvp.Value > highestValue Then
                    highestKey = kvp.Key
                    highestValue = kvp.Value
                End If
            Next
            Return New KeyValuePair(Of heroSkill, Integer)(highestKey, highestValue)
        End Get
    End Property

    Private Property heroRawSkills As New Dictionary(Of heroSkill, Integer)
    Friend Sub addHeroRawSkill(heroSkill As heroSkill, value As Integer)
        heroRawSkills(heroSkill) += value
        mustRefresh = True
    End Sub
    Friend Sub removeHeroRawSkill(heroSkill As heroSkill, value As Integer)
        addHeroRawSkill(heroSkill, value * -1)
    End Sub

    Private Property heroModifierSkills As New Dictionary(Of heroSkill, Integer)
    Private Property heroModifiers As New List(Of modifier)
    Private ReadOnly Property empireHeroModifiers As List(Of modifier)
        Get
            Return empire.empireHeroModifiers
        End Get
    End Property
    Friend Sub addHeroModifier(modifier As modifier)
        If modifier.quality < 101 OrElse modifier.quality > 199 Then
            Debug.Print("Invalid modifier for a hero.")
            Exit Sub
        ElseIf modifier.quality = modifierQuality.HeroSkill AndAlso modifier.heroSkill = Nothing Then
            Debug.Print("HeroSkill unspecified for modifier.")
            Exit Sub
        End If

        heroModifiers.Add(modifier)
        modifier.parent = heroModifiers
        mustRefresh = True
    End Sub

    Private Property heroEquipmentSkills As New Dictionary(Of heroSkill, Integer)
    Private Property pEquipment As New Dictionary(Of String, item)          'dictionary key = equipment slot
    Friend ReadOnly Property equipment As Dictionary(Of String, item)
        Get
            Return pEquipment
        End Get
    End Property
    Friend Sub addEquipment(item As item)
        Dim equipmentSlot As String = item.equipmentSlot
        If pEquipment.ContainsKey(equipmentSlot) Then
            'has existing item in slot
            Dim newItem As item
            If empire.isAI = True Then newItem = aiEquipItem(item) Else newItem = menu.heroEquip(Me, item)

            newItem.hero = Me
            pEquipment(equipmentSlot) = newItem
        Else
            'empty slot; just add
            item.hero = Me
            pEquipment.Add(item.equipmentSlot, item)
        End If
        mustRefresh = True
    End Sub
    Friend Function removeEquipment(item As item) As item
        If pEquipment.ContainsValue(item) = False Then
            Debug.Print(name & " does not have " & item.name & " equipped.")
            Return Nothing
        End If

        removeEquipment = pEquipment(item.equipmentSlot)
        pEquipment.Remove(item.equipmentSlot)
        mustRefresh = True
    End Function



    'movement
    Private Property travelLocation As travelLocation = Nothing Implements traveller.travelLocation
    Friend ReadOnly Property location As travelLocation
        Get
            Return travelLocation
        End Get
    End Property
    Private Property travelDestination As travelLocation = Nothing Implements traveller.travelDestination
    Private Property travelSpeed As Integer Implements traveller.travelSpeed
    Private Property travelProgress As Integer = 0 Implements traveller.travelProgress
    Private Property travelCost As Integer = 0 Implements traveller.travelCost
    Private Const minTravelSpeed As Integer = 1
    Friend Sub moveTo(aDestination As travelLocation) Implements traveller.moveTo
        If travelLocation Is Nothing = True Then
            Debug.Print(name & " is already in transit.")
            Exit Sub
        ElseIf army Is Nothing = False Then
            Debug.Print(name & " is in an army and cannot move independently.")
            Exit Sub
        End If

        report.AddAndImmediateReport(name & " leaves " & travelLocation.name & " on " & travelLocation.shard.ToString & " for " & aDestination.name & " on " & aDestination.shard.name & ".", reportQueueType.HeroLeavesLocation, empire)

        travelCost = getTravelCost(travelLocation, aDestination)
        travelProgress = 0
        travelLocation.heroes.Remove(Me)
        travelLocation = Nothing
        travelDestination = aDestination
    End Sub
    Friend Sub teleportTo(aDestination As travelLocation) Implements traveller.teleportTo
        If travelLocation Is Nothing = False Then
            If travelLocation.heroes.Contains(Me) Then travelLocation.heroes.Remove(Me)
        End If

        travelLocation = aDestination
        If travelLocation Is Nothing = False Then travelLocation.heroes.Add(Me)
        travelDestination = Nothing
        travelProgress = 0
        travelCost = 0
    End Sub
    Private Function getTravelCost(aOrigin As travelLocation, aDestination As travelLocation) As Integer Implements traveller.getTravelCost
        Return aOrigin.getDistanceTo(aDestination)
    End Function

    Private Property questHome As travelLocation = Nothing
    Friend Sub questGeas(threat As threat)
        questHome = travelLocation
        moveTo(threat.location)
    End Sub
    Friend Sub questReturnHome()
        report.AddAndImmediateReport(name & " fails to complete " & hisOrHer.ToLower & " quest and returns home in ignoble defeat...", reportQueueType.HeroQuestFailReturnHome, empire)
        moveTo(questHome)
    End Sub




    'AI
    Private Function aiEquipItem(newItem As item) As item
        Dim oldItem As item = equipment(newItem.equipmentSlot)
        Dim oldItemValue As Integer = aiGetItemValue(oldItem)
        Dim newItemValue As Integer = aiGetItemValue(newItem)

        'favours new items if values are identical
        If oldItemValue > newItemValue Then Return oldItem Else Return newItem
    End Function
    Private Function aiGetItemValue(item As item) As Integer
        Dim total As Integer = 0
        For Each modifier In item.modifiers
            Select Case modifier.quality
                Case modifierQuality.HeroSkill : total += modifier.value(Me)
                Case modifierQuality.HeroAllSkills : total += 8 * modifier.value(Me)
                Case modifierQuality.HeroTravelSpeed : total += CInt(modifier.value(Me) / 10)
            End Select
        Next
        Return total
    End Function
End Class