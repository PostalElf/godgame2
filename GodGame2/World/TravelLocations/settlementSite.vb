Public Class settlementSite
    Inherits travelLocation

    Public Overrides Function ToString() As String
        Return "[Songstone] " & name
    End Function

    Friend Function settle(ByRef empire As empire) As settlement
        Dim settlement As New settlement(empire, shard)
        With settlement
            If name Is Nothing Then .name = getRandomSettlementName() Else .name = name
            For Each building In empire.availableBuildings
                .addAvailableBuilding(building)
            Next
        End With
        report.Add("The " & empire.name & " establishes the new settlement of " & settlement.name & " on " & shard.name & " Shard.", reportQueueType.SettlementNew, empire)
        Return settlement
    End Function
    Private Shared Property preSettlementNames As New List(Of String)
    Private Shared Property sufSettlementNames As New List(Of String)
    Friend Shared Function getRandomSettlementName() As String
        If preSettlementNames.Count = 0 Then preSettlementNames = fileget("data/preSettlementNames.txt")
        If sufSettlementNames.Count = 0 Then sufSettlementNames = fileget("data/sufSettlementNames.txt")

        Dim pre As Integer = rng.Next(preSettlementNames.Count)
        Dim suf As Integer = rng.Next(sufSettlementNames.Count)

        getRandomSettlementName = preSettlementNames(pre) & sufSettlementNames(suf)

        preSettlementNames.RemoveAt(pre)
        sufSettlementNames.RemoveAt(suf)
    End Function
End Class
