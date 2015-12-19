Public Class modifier
    Friend Property name As String
    Friend Property parent As List(Of modifier)
    Friend Property quality As modifierQuality
    Friend Property mustHide As Boolean = False
    Friend Property resource As resource = Nothing
    Friend Property popseg As popseg = Nothing
    Friend Property heroSkill As heroSkill = Nothing
    Friend Property good As good = Nothing
    Friend Property building As building = Nothing
    Friend Property zeitgeist As zeitgeist = Nothing
    Friend Property philosophy As philosophy = Nothing
    Friend Property philosophyShape As philosophyShape = Nothing
    Friend Property wildernessType As wildernessType = Nothing
    Friend Property item As item = Nothing

    Private pValue As Integer
    Friend ReadOnly Property value(aHero As hero) As Integer
        Get
            If isDependent = True Then Return dependentValue(aHero) Else Return pValue
        End Get
    End Property
    Friend ReadOnly Property value(aSettlement As settlement) As Integer
        Get
            If isDependent = True Then Return dependentValue(aSettlement) Else Return pValue
        End Get
    End Property
    Friend ReadOnly Property value(aEmpire As empire) As Integer
        Get
            If isDependent = True Then Return dependentValue(aEmpire) Else Return pValue
        End Get
    End Property
    Friend ReadOnly Property qualityDescription As String
        Get
            Dim valueStr As String
            If isDependent = False Then
                valueStr = withSign(pValue)
            Else
                If dependentPopseg <> Nothing Then
                    valueStr = withSign(pValue) & " per " & stripS(dependentPopseg.ToString)
                ElseIf dependentRawHeroSkill <> Nothing Then
                    valueStr = withSign(pValue) & " per " & dependentRawHeroSkill.ToString
                Else
                    valueStr = withSign(pValue)
                End If
            End If

            Dim durationStr As String
            If duration = -1 Then durationStr = " permanently" Else durationStr = " for " & duration & " turn(s)"
            Dim durationStr2 As String
            If duration = -1 Then durationStr2 = " " Else durationStr2 = " within " & duration & " turn(s) "

            Dim endStr As String
            If isConditionalOnGood = True Then
                endStr = " with " & good.ToString & "."
            ElseIf isConditionalOnWildernessType = True Then
                endStr = " with " & wildernessType.ToString & "."
            Else
                endStr = "."
            End If

            Select Case quality
                Case modifierQuality.SettlementPopulationIncome : Return stripS(popseg.ToString) & " production " & valueStr & durationStr & endStr
                Case modifierQuality.SettlementIncome : Return "Base " & resource.ToString & " production " & valueStr & durationStr & endStr
                Case modifierQuality.SettlementRecruit : Return "Chances of recruiting new " & popseg.ToString & " " & valueStr & durationStr & endStr
                Case modifierQuality.SettlementGuaranteedRecruit : Return "Next citizen recruited" & durationStr2 & "guaranteed to be a " & stripS(popseg.ToString) & "."
                Case modifierQuality.SettlementBuildEfficiency : Return "Construction speed " & valueStr & durationStr & endStr
                Case modifierQuality.SettlementDefence : Return "Settlement defence " & valueStr & durationStr & endStr
                Case modifierQuality.SettlementPublicOrder : Return "Settlement public order " & valueStr & durationStr & endStr
                Case modifierQuality.WarriorEfficiency : Return "Warriors " & valueStr & durationStr & endStr
                Case modifierQuality.WarmageEfficiency : Return "Warmages " & valueStr & durationStr & endStr

                Case modifierQuality.HeroAllSkills : Return "All hero skills " & valueStr & durationStr & endStr
                Case modifierQuality.HeroSkill : Return heroSkill.ToString & " " & valueStr & durationStr & endStr
                Case modifierQuality.HeroTravelSpeed : Return "Travel speed " & valueStr & durationStr & endStr

                Case modifierQuality.GoodUnlock : Return "May now trade and utilise " & good.ToString & "."
                Case modifierQuality.BuildingUnlock : Return "May now build " & building.name & "."
                Case modifierQuality.ZeitgeistUnlock : Return "May now embrace the '" & zeitgeist.name & "' zeitgeist."
                Case modifierQuality.PhilosophyUnlock : Return "May now embrace the '" & philosophy.name & "' philosophy."

                Case modifierQuality.EmpireIncome : Return "Empire base " & resource.ToString & " production " & valueStr & durationStr & endStr
                Case Else : Return "Unknown modifier."
            End Select
        End Get
    End Property
    Friend duration As Integer = -1                                      ' -1 = infinite
    Friend invalidString As Boolean = False


    Public Sub New()
    End Sub
    Public Sub New(ByRef aParent As List(Of modifier), aName As String, rawString As String, Optional aDuration As Integer = -1)
        name = aName
        parent = aParent
        duration = aDuration

        Dim split As String() = rawString.Split(" ")
        Select Case split(0)
            Case "SettlementPopulationIncome"
                quality = modifierQuality.SettlementPopulationIncome
                popseg = constants.getPopSegFromString(split(1))
                resource = constants.getResourceFromPopseg(popseg)
                parseValue(split(2))
                checkForConditional(split, 3)

            Case "SettlementIncome"
                quality = modifierQuality.SettlementIncome
                resource = constants.getResourceFromString(split(1))
                popseg = constants.getPopsegFromResource(resource)
                parseValue(split(2))
                checkForConditional(split, 3)

            Case "SettlementRecruit"
                quality = modifierQuality.SettlementRecruit
                popseg = constants.getPopSegFromString(split(1))
                parseValue(split(2))
                checkForConditional(split, 3)

            Case "SettlementGuaranteedRecruit"
                quality = modifierQuality.SettlementGuaranteedRecruit
                popseg = constants.getPopSegFromString(split(1))
                checkForConditional(split, 2)

            Case "SettlementBuildEfficiency"
                quality = modifierQuality.SettlementBuildEfficiency
                parseValue(split(1))
                checkForConditional(split, 2)

            Case "SettlementDefence"
                quality = modifierQuality.SettlementDefence
                parseValue(split(1))
                checkForConditional(split, 2)

            Case "SettlementPublicOrder"
                quality = modifierQuality.SettlementPublicOrder
                parseValue(split(1))
                checkForConditional(split, 2)

            Case "WarriorEfficiency"
                quality = modifierQuality.WarriorEfficiency
                parseValue(split(1))
                checkForConditional(split, 2)

            Case "WarmageEfficiency"
                quality = modifierQuality.WarmageEfficiency
                parseValue(split(1))
                checkForConditional(split, 2)

            Case "HeroAllSkills"
                quality = modifierQuality.HeroAllSkills
                If name = "" Then name = "All Skills"
                parseValue(split(1))
                checkForConditional(split, 2)

            Case "HeroSkill"
                quality = modifierQuality.HeroSkill
                heroSkill = constants.getHeroSkillFromString(split(1))
                If name = "" Then name = heroSkill.ToString
                parseValue(split(2))
                checkForConditional(split, 3)

            Case "HeroTravelSpeed"
                quality = modifierQuality.HeroTravelSpeed
                If name = "" Then name = "Travel Speed"
                parseValue(split(1))
                checkForConditional(split, 2)

            Case "GoodUnlock"
                quality = modifierQuality.GoodUnlock
                good = constants.getGoodFromString(split(1))
                checkForConditional(split, 2)

            Case "BuildingUnlock"
                quality = modifierQuality.BuildingUnlock
                Dim id As Integer = CInt(split(1))
                building = building.buildingFileget(id)

            Case "ZeitgeistUnlock"
                quality = modifierQuality.ZeitgeistUnlock
                Dim id As Integer = CInt(split(1))
                zeitgeist = zeitgeist.zeitgeistFileget(id)

            Case "PhilosophyUnlock"
                quality = modifierQuality.PhilosophyUnlock
                Dim id As Integer = CInt(split(1))
                philosophy = philosophy.philosophyFileget(id)

            Case "EmpireIncome"
                quality = modifierQuality.EmpireIncome
                resource = constants.getResourceFromString(split(1))
                popseg = constants.getPopsegFromResource(resource)
                parseValue(split(2))
                checkForConditional(split, 3)

            Case Else
                invalidString = True
        End Select
    End Sub

    Private dependentPopseg As popseg = Nothing
    Private dependentRawHeroSkill As heroSkill = Nothing
    Friend isDependent As Boolean = False                               'multiplies value by dependentPopseg or dependentRawHeroSkill
    Private ReadOnly Property dependentValue(aSettlement As settlement) As Integer
        Get
            Return aSettlement.population(dependentPopseg) * pValue
        End Get
    End Property
    Private ReadOnly Property dependentValue(aHero As hero) As Integer
        Get
            Return aHero.getHeroRawSkill(dependentRawHeroSkill) * pValue
        End Get
    End Property
    Private ReadOnly Property dependentValue(aEmpire As empire) As Integer
        Get
            Dim totalPop As Integer = 0
            For Each settlement In aEmpire.settlements
                totalPop += settlement.population(dependentPopseg)
            Next
            Return totalPop * pValue
        End Get
    End Property
    Private Sub parseValue(rawString As String)
        If rawString.Contains("/") = False Then
            pValue = CInt(rawString)
        Else
            Dim split As String() = rawString.Split("/")
            pValue = CInt(split(0))

            dependentPopseg = constants.getPopSegFromString(split(1))
            dependentRawHeroSkill = constants.getHeroSkillFromString(split(1))
            isDependent = True
        End If
    End Sub

    Friend Sub tick()
        If duration <> -1 Then duration -= 1
    End Sub
    Public Overrides Function ToString() As String
        Return name & ": " & qualityDescription
    End Function
    Friend Sub consoleReport(indent As Integer, Optional prefix As String = "")
        Dim ind As String = vbSpace(indent)

        Console.WriteLine(ind & prefix & name & ": " & qualityDescription)
    End Sub

    Friend isConditionalOnGood As Boolean = False                       'conditionals should only be applied to settlements
    Friend isConditionalOnWildernessType As Boolean = False
    Private Sub checkForConditional(split As String(), indexOfWith As Integer)
        If indexOfWith < split.Count Then
            If split(indexOfWith).ToLower = "with" Then
                'has keyword "with", is conditional; determine if it's conditional on good or wildernesstype
                Dim rawStr As String = split(indexOfWith + 1)

                If constants.getGoodFromString(rawStr) <> Nothing Then
                    'conditionalOnGood
                    good = constants.getGoodFromString(rawStr)
                    isConditionalOnGood = True
                ElseIf constants.getWildernessTypeFromString(rawStr) <> Nothing Then
                    'conditionalOnWilderness
                    wildernessType = constants.getWildernessTypeFromString(rawStr)
                    isConditionalOnWildernessType = True
                Else
                    'has keyword but is of unrecognised type
                    Dim fullstr As String = ""
                    For n = 0 To split.Length - 1
                        fullstr &= split(n) & " "
                    Next
                    Debug.Print("Conditional not recognised: '" & rawStr & "' in '" & fullstr & "'")
                    Exit Sub
                End If
            End If
        End If
    End Sub
    Friend Function meetsConditional(settlement As settlement) As Boolean
        Dim total As Boolean = True

        If isConditionalOnGood = True Then
            If settlement.allGoods.Contains(good) = False Then total = False
        End If
        If isConditionalOnWildernessType = True Then
            If settlement.shard.wildernessTypes.Contains(wildernessType) = False Then total = False
        End If

        Return total
    End Function
    Friend Sub setConditionalOnGood(aGood As good)
        isConditionalOnGood = True
        good = aGood
    End Sub
End Class


Public Enum modifierQuality
    SettlementPopulationIncome = 1
    SettlementIncome
    SettlementRecruit
    SettlementGuaranteedRecruit
    SettlementBuildEfficiency
    SettlementDefence
    SettlementPublicOrder
    WarriorEfficiency
    WarmageEfficiency

    HeroAllSkills = 101
    HeroSkill
    HeroTravelSpeed

    GoodUnlock = 201
    BuildingUnlock
    ZeitgeistUnlock
    PhilosophyUnlock

    EmpireIncome = 301
End Enum