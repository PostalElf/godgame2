Public Class challenge
    Private Property id As Integer
    Private Property name As String
    Private Property text As String                             'overall text describing challenge
    Private Property choiceText As String()                     'text player sees describing each choice
    Private Property skill As heroSkill()                       'skill being tested in choice
    Private Property difficulty As Integer()                    'difficulty of skill test
    Private Property consequence As challengeConsequence        'consequences of failure

    Public Sub New()
    End Sub
    Public Sub New(aName As String, aText As String, aChoiceText As String(), aSkill As heroSkill(), aDifficulty As Integer(), aConsequence As challengeConsequence)
        name = aName
        text = aText
        choiceText = aChoiceText
        skill = aSkill
        difficulty = aDifficulty
        consequence = aConsequence
    End Sub
    Friend Shared Function challengeFileget(aID As Integer) As challenge
        Dim pathname As String = constants.pathnameChallenges
        If authenticator.checkHash(pathname) = False Then
            Debug.Print("Authentication failed.")
            Return Nothing
        End If

        Dim total As List(Of String()) = csvFileget(pathname)
        For Each line In total
            If CInt(line(0)) = aID Then
                Dim n As New rollingCounter(0)
                Dim challenge As New challenge
                With challenge
                    .id = CInt(line(n.Tick))
                    .name = line(n.Tick)
                    .text = line(n.Tick)

                    For p = 0 To 2
                        .choiceText(p) = line(n.Tick)

                        Dim skillText As String() = line(n.Tick).Split(" ")
                        .skill(p) = constants.getHeroSkillFromString(skillText(0))
                        .difficulty(p) = CInt(skillText(1))
                    Next
                End With
                Return challenge
            End If
        Next
        Return Nothing
    End Function

    Friend Sub consoleReport(indent As Integer)
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)

        Console.WriteLine(ind & name)
        Console.WriteLine(ind & text)
        Console.WriteLine()
        For n = 0 To 2
            Console.WriteLine(indd & (n + 1) & ") " & choiceText(n))
        Next
    End Sub
    Friend Function challenge(hero As hero, choice As Integer, indent As Integer) As challengeConsequence
        Dim ind As String = vbSpace(indent)
        Dim indd As String = vbSpace(indent + 1)

        Dim chosenSkill As heroSkill = skill(choice)
        Dim chosenDifficulty As Integer = difficulty(choice)
        Dim roll As Integer = rollDice("3d6") * 10
        Dim total As Integer = roll + hero.heroSkills(chosenSkill)

        Console.WriteLine(indd & "{0} has {1} {2}.", hero.name, hero.heroSkills(chosenSkill), chosenSkill.ToString)
        Console.WriteLine(indd & hero.pronoun & " rolls a " & roll & " + " & hero.heroSkills(chosenSkill) & " = " & total)

        If total >= chosenDifficulty Then
            Console.WriteLine(indd & hero.pronoun & " is successful!")
            Return challengeConsequence.Success
        Else
            Return consequence
        End If
    End Function
End Class

Public Enum challengeConsequence
    Success = 1
    Death
    ReturnHome
End Enum