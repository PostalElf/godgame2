Public Class constants
    Public Shared resourceArray As Array = System.Enum.GetValues(GetType(resource))
    Public Shared popsegArray As Array = System.Enum.GetValues(GetType(popseg))
    Public Shared rqtArray As Array = System.Enum.GetValues(GetType(reportQueueType))
    Public Shared heroSkillArray As Array = System.Enum.GetValues(GetType(heroSkill))
    Public Shared modifierQualityArray As Array = System.Enum.GetValues(GetType(modifierQuality))
    Public Shared goodArray As Array = System.Enum.GetValues(GetType(good))
    Public Shared philosophyShapeArray As Array = System.Enum.GetValues(GetType(philosophyShape))
    Public Shared wildernessTypeArray As Array = System.Enum.GetValues(GetType(wildernessType))
    Public Shared divineDomainArray As Array = System.Enum.GetValues(GetType(divineDomain))

    Public Const pathnameBuilding As String = "data/buildings.csv"
    Public Const pathnameScience As String = "data/science.csv"
    Public Const pathnameCulture As String = "data/culture.csv"
    Public Const pathnameZeitgeist As String = "data/zeitgeist.csv"
    Public Const pathnamePhilosophy As String = "data/philosophy.csv"
    Public Const pathnameThreat As String = "data/threats.csv"
    Public Const pathnameChallenges As String = "data/challenges.csv"


    Public Shared Function getPopsegFromResource(resource As resource) As popseg
        Select Case resource
            Case resource.Food : Return popseg.Farmers
            Case resource.Faith : Return popseg.Priests
            Case resource.Science : Return popseg.Savants
            Case resource.Culture : Return popseg.Artists
            Case resource.Wealth : Return popseg.Traders
            Case Else : Return Nothing
        End Select
    End Function
    Public Shared Function getPopSegFromString(str As String) As popseg
        Return getEnumFromString(str, popsegArray)
    End Function
    Public Shared Function getResourceFromPopseg(popseg As popseg) As resource
        Select Case popseg
            Case popseg.Farmers : Return resource.Food
            Case popseg.Priests : Return resource.Faith
            Case popseg.Savants : Return resource.Science
            Case popseg.Artists : Return resource.Culture
            Case popseg.Traders : Return resource.Wealth
            Case Else : Return Nothing
        End Select
    End Function
    Public Shared Function getResourceFromString(str As String) As resource
        Return getEnumFromString(str, resourceArray)
    End Function
    Public Shared Function getHeroSkillFromString(str As String) As heroSkill
        Return getEnumFromString(str, heroSkillArray)
    End Function
    Public Shared Function getGoodFromString(str As String) As good
        Return getEnumFromString(str, goodArray)
    End Function
    Public Shared Function getPhilosophyShapeFromString(str As String) As philosophyShape
        Return getEnumFromString(str, philosophyShapeArray)
    End Function
    Public Shared Function getWildernessTypeFromString(str As String) As wildernessType
        Return getEnumFromString(str, wildernessTypeArray)
    End Function
    Public Shared Function getDivineDomainFromString(str As String) As divineDomain
        Return getEnumFromString(str, divineDomainArray)
    End Function

    Private Shared Function getEnumFromString(str As String, enumArray As Array) As [Enum]
        For Each item In enumArray
            Dim itemStr As String = item.ToString.ToLower

            If itemStr = str.ToLower Then Return item
            If stripS(itemStr) = str.ToLower Then Return item
        Next
        Return Nothing
    End Function
End Class
