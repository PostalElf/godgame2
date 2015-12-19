Public Class tech
    Friend Property name As String
    Private Property pModifiers As New List(Of modifier)
    Private Property pTier As Integer
    Friend ReadOnly Property tier As Integer
        Get
            Return pTier
        End Get
    End Property
    Private Property cost As Integer
    Private Property progress As Integer
    Private Property description As String
    Friend ReadOnly Property completed As Boolean
        Get
            If progress >= cost Then Return True Else Return False
        End Get
    End Property
    Private Property pIsKeyTechTo As Integer
    Friend ReadOnly Property isKeyTechTo As Integer
        Get
            Return pIsKeyTechTo
        End Get
    End Property

    Public Sub New()
    End Sub
    Public Sub New(aName As String, aTier As Integer, aIsKeyTechTo As Integer, aCost As Integer, aModifiers As List(Of modifier))
        name = aName
        pTier = aTier
        pIsKeyTechTo = aIsKeyTechTo
        cost = aCost
        pModifiers.AddRange(aModifiers)
    End Sub
    Public Overrides Function ToString() As String
        Return name & " (" & progress & "/" & cost & ")"
    End Function
    Friend Sub tick(progressIncome As Integer)
        progress += progressIncome
        If progressIncome >= cost Then progressIncome = cost
    End Sub
    Friend ReadOnly Property modifiers As List(Of modifier)
        Get
            Return pModifiers
        End Get
    End Property

    Friend Shared Function techFileGet(resource As resource, tier As Integer) As List(Of tech)
        Dim pathname As String
        If resource = resource.Science Then pathname = constants.pathnameScience Else pathname = constants.pathnameCulture
        If authenticator.checkHash(pathname) = False Then
            Debug.Print("Authentication failed.")
            Return Nothing
        End If


        Dim total As New List(Of tech)
        Dim rawcsv As List(Of String()) = csvFileget(pathname)
        For Each line In rawcsv
            Dim i As rollingCounter = New rollingCounter(0)
            Dim sName As String = line(i.Tick)
            Dim sTier As Integer = CInt(line(i.Tick))

            If sTier = tier Then
                Dim sIsKeyTechTo As Integer = CInt(line(i.Tick))
                Dim sCost As Integer = CInt(line(i.Tick))
                Dim sDescription As String = line(i.Tick)

                Dim sModifiers As New List(Of modifier)

                While i.Tick < line.Count AndAlso line(i.Last) <> ""
                    Dim modifier As New modifier(sModifiers, sName, line(i.Last))
                    sModifiers.Add(modifier)
                End While

                Dim tech As New tech(sName, sTier, sIsKeyTechTo, sCost, sModifiers)
                tech.description = sDescription
                total.Add(tech)
            End If
            If sTier > tier Then Exit For
        Next
        Return total
    End Function
End Class
