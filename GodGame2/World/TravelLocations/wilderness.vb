Public Class wilderness
    Inherits travelLocation

    Public Overrides Function ToString() As String
        Return "[" & type.ToString & "] " & name
    End Function
    Public Sub New(Optional wildernessType As wildernessType = Nothing)
        type = wildernessType
    End Sub

    Friend Property type As wildernessType

    Private Shared Property wildernessPreNames As New Dictionary(Of wildernessType, List(Of String))
    Private Shared Property wildernessMidNames As New Dictionary(Of wildernessType, List(Of String))
    Private Shared Property wildernessSufNames As New Dictionary(Of wildernessType, List(Of String))
    Friend Shared Function getRandomWildernessName(wilderness As wildernessType) As String
        If wildernessPreNames.ContainsKey(wilderness) = False Then wildernessPreNames.Add(wilderness, New List(Of String))
        If wildernessPreNames(wilderness).Count = 0 Then wildernessPreNames(wilderness) = filegetBracket("data/preWildernessNames.txt", wilderness.ToString)
        If wildernessMidNames.ContainsKey(wilderness) = False Then wildernessMidNames.Add(wilderness, New List(Of String))
        If wildernessMidNames(wilderness).Count = 0 Then wildernessMidNames(wilderness) = filegetBracket("data/midWildernessNames.txt", wilderness.ToString)
        If wildernessSufNames.ContainsKey(wilderness) = False Then wildernessSufNames.Add(wilderness, New List(Of String))
        If wildernessSufNames(wilderness).Count = 0 Then wildernessSufNames(wilderness) = filegetBracket("data/sufWildernessNames.txt", wilderness.ToString)

        Dim preRoll As Integer = rng.Next(wildernessPreNames(wilderness).Count)
        Dim prefix As String = wildernessPreNames(wilderness)(preRoll)
        wildernessPreNames(wilderness).RemoveAt(preRoll)

        Dim midRoll As Integer = rng.Next(wildernessMidNames(wilderness).Count)
        Dim mid As String = wildernessMidNames(wilderness)(midRoll)
        wildernessMidNames(wilderness).RemoveAt(midRoll)

        Dim sufRoll As Integer = rng.Next(wildernessSufNames(wilderness).Count)
        Dim suffix As String = wildernessSufNames(wilderness)(sufRoll)
        wildernessSufNames(wilderness).RemoveAt(sufRoll)

        Return prefix & mid & suffix
    End Function
End Class

Public Enum wildernessType
    Forest = 1
    Mountain
    Lake
    Swamp
    Plains
End Enum
