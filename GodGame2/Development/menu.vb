Public Class menu
    Friend Shared Function getListChoice(objList As System.Collections.Generic.IEnumerable(Of Object), indent As Integer, Optional str As String = "", Optional prompt As String = "> ") As Object
        If objList.Count = 0 Then Return Nothing

        Dim ind As String = vbSpace(indent)
        If str <> "" Then
            Console.WriteLine(str)
        End If

        For n = 0 To objList.Count - 1
            Dim obj As Object = objList(n)
            Console.WriteLine(ind & n + 1 & ") " & obj.ToString)
        Next

        While True
            Console.WriteLine()
            Console.Write(ind & prompt)
            Dim input As String = Console.ReadLine

            If IsNumeric(input) = True Then
                Dim num As Integer = CInt(input) - 1
                If num > objList.Count OrElse num < -1 Then
                    Console.WriteLine("Invalid input!")
                ElseIf num = -1 Then
                    'escape clause when user input 0
                    Return Nothing
                Else
                    Return objList(num)
                End If
            Else
                Console.WriteLine("Invalid input!")
            End If
        End While

        Return Nothing
    End Function
    Friend Shared Function getListChoice(objList As Dictionary(Of Char, String), indent As Integer, Optional str As String = "", Optional prompt As String = "> ") As Char
        If objList.Count = 0 Then Return Nothing

        Dim ind As String = vbSpace(indent)
        If str <> "" Then Console.WriteLine(str)

        For Each kvp In objList
            Console.WriteLine(ind & kvp.Key & ") " & kvp.Value)
        Next

        While True
            Console.WriteLine()
            Console.Write(ind & prompt)
            Dim input As ConsoleKeyInfo = Console.ReadKey

            If objList.ContainsKey(input.KeyChar) OrElse objList.ContainsKey(Char.ToUpperInvariant(input.KeyChar)) _
                OrElse input.Key = ConsoleKey.Spacebar OrElse input.Key = ConsoleKey.Enter _
                Then
                Return Char.ToLowerInvariant(input.KeyChar)
            Else
                Console.WriteLine()
                Console.WriteLine("Invalid input!")
            End If
        End While

        Return Nothing
    End Function
    Friend Shared Function confirmChoice(indent As Integer, Optional str As String = "Are you sure? ") As Boolean
        Dim ind As String = vbSpace(indent)

        Console.Write(ind & str)
        Dim input As ConsoleKeyInfo = Console.ReadKey()
        If input.Key = ConsoleKey.Y Then Return True Else Return False
    End Function
    Friend Shared Function getNumInput(indent As Integer, min As Integer, max As Integer, str As String) As Integer
        Dim input As String = ""
        Dim ind As String = vbSpace(indent)

        While True
            Console.Write(ind & str)
            input = Console.ReadLine
            If IsNumeric(input) = True Then
                Dim num As Integer = CInt(input)
                If num >= min AndAlso num <= max Then Return num Else Console.WriteLine(ind & "Number must be between " & min & " and " & max & ".")
            Else
                Console.WriteLine(ind & "Numbers only please!")
            End If
        End While
        Return Nothing
    End Function
    Friend Shared Function getCharInput(objList As List(Of Char), str As String) As Char
        While True
            Console.Write(str)
            Dim input As ConsoleKeyInfo = Console.ReadKey

            If input.Key = ConsoleKey.Escape Then
                Return Nothing
            ElseIf objList.Contains(Char.ToLower(input.KeyChar)) OrElse objList.Contains(Char.ToUpper(input.KeyChar)) Then
                Return input.KeyChar
            Else
                Console.WriteLine()
                Console.WriteLine("Invalid input.")
            End If
        End While
        Return Nothing
    End Function

    Friend Shared Function getFateDestinyChoice() As divinePower
        Console.Write("Spend Fate or Destiny?  ")
        Dim input As ConsoleKeyInfo = Console.ReadKey

        Console.WriteLine()

        If input.Key = ConsoleKey.F Then
            Return divinePower.Fate
        ElseIf input.Key = ConsoleKey.D Then
            Return divinePower.Destiny
        Else
            Return Nothing
        End If
    End Function


    Friend Shared Function heroEquip(hero As hero, item As item) As item
        'returns item that player chooses

        Console.WriteLine(hero.name & " has received " & item.name & " (" & item.equipmentSlot & ").")
        item.consoleReport(1)

        If hero.equipment(item.equipmentSlot) Is Nothing = False Then
            Console.WriteLine()
            Console.WriteLine("Current " & item.equipmentSlot & ":")

            Dim oldItem As item = hero.equipment(item.equipmentSlot)
            oldItem.consoleReport(1)
            Console.WriteLine()

            Dim choice As Boolean = confirmChoice(0, "Replace " & oldItem.name & "? ")
            Console.WriteLine()
            If choice = True Then
                'replace old equipment
                oldItem.modifiers.Clear()
                oldItem.hero = Nothing
                Return item
            Else
                'do not replace
                Return oldItem
            End If
        Else
            Return item
        End If
    End Function
End Class
