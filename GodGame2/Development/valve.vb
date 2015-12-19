Imports System.IO
Imports Microsoft.VisualBasic.FileIO

Module valve
    Public rng As New Random

    Public Function vbSpace(Optional times As Integer = 1) As String
        If times = 0 Then Return Nothing

        Dim total As String = Nothing
        For n = 1 To times
            total &= "   "
        Next
        Return total
    End Function
    Public Function fakeTab(word As String, totalLength As Integer)
        Dim spaceCount As Integer = totalLength - word.Length
        Dim spaces As String = ""
        For n = 1 To spaceCount
            spaces &= " "
        Next
        Return word & spaces
    End Function

    Public Function constrain(value As Integer, Optional minValue As Integer = 1, Optional maxValue As Integer = 100) As Integer
        Dim total As Integer = value
        If total < minValue Then total = minValue
        If total > maxValue Then total = maxValue
        Return total
    End Function
    Public Function circular(value As Integer, Optional minValue As Integer = 1, Optional maxValue As Integer = 4) As Integer
        Dim total As Integer = value
        While total < minValue OrElse total > maxValue
            If total < minValue Then total += maxValue
            If total > maxValue Then total -= maxValue
        End While
        Return total
    End Function

    Public Function sign(value As Decimal) As String
        If value < 0 Then Return "" Else Return "+"
    End Function
    Public Function withSign(value As Decimal) As String
        Return sign(value) & value
    End Function
    Public Function withCommas(inputList As List(Of String)) As String
        Dim total As String = ""
        For n = 0 To inputList.Count - 1
            total &= inputList(n)
            If n < inputList.Count - 1 Then total &= ", "
        Next
        Return total
    End Function

    Public Function percentRoll(probability As Integer) As Boolean
        Dim roll As Integer = rng.Next(1, 101)
        If roll <= probability Then Return True Else Return False
    End Function
    Public Function lumpyRng(min As Integer, max As Integer)
        'min is inclusive while max is exclusive

        Dim total As Integer = 0
        For n = 1 To 3
            total += rng.Next(min, max)
        Next
        Return Int(total / 3)
    End Function
    Public Function rollDice(dice As String) As Integer
        '3d6
        Dim diceText As String() = dice.Split("d")
        If IsNumeric(diceText(0)) = False OrElse IsNumeric(diceText(1)) = False Then Return 0

        Dim numOfDice As Integer = CInt(diceText(0))
        Dim diceFace As Integer = CInt(diceText(1))
        Dim total As Integer = 0
        For n = 1 To numOfDice
            total += rng.Next(1, diceFace + 1)
        Next
        Return total
    End Function
    Public Function cascadingRng(value As Integer, min As Integer, max As Integer) As Integer
        'returns probability value (0-100) that's lower the closer it gets to min

        Dim range As Integer = max - min
        Return Int(range / 100 * value)
    End Function
    Public Function coinFlip() As Boolean
        Randomize()
        If Int(Rnd() * 2) + 1 = 1 Then Return True Else Return False
    End Function
    Public Function pythogoras(xy1 As xy, xy2 As xy) As Integer
        Dim x As Integer = Math.Abs(xy1.x - xy2.x)
        Dim y As Integer = Math.Abs(xy1.y - xy2.y)
        Return Math.Round(Math.Sqrt(x * x + y * y))
    End Function

    Public Function getLiteral(value As Integer, Optional sigDigits As Integer = 2) As String
        Dim str As String = ""
        For n = 1 To sigDigits
            str = str & "0"
        Next
        str = str & value

        Dim characters() As Char = StrReverse(str)
        str = Nothing
        For n = 0 To sigDigits - 1
            str = str & characters(n)
        Next

        Return StrReverse(str)
    End Function
    Public Function joinString(stringQueue As Queue(Of String)) As String
        Dim total As String = ""
        While stringQueue.Count > 0
            total &= stringQueue.Dequeue
        End While
        Return total
    End Function
    Public Function writeDash(n As Integer) As String
        Dim str As String = ""
        For count = 0 To n
            str &= ("-")
        Next
        Return str
    End Function
    Public Function stripS(str As String) As String
        If str(str.Count - 1) = "s" Then
            Dim split As String = str.TrimEnd("s")
            Return split
        Else
            Return str
        End If
    End Function
    Public Function aOrAn(nextWord As String) As String
        Dim vowels As String = "aeiou"
        For Each c As Char In vowels
            If nextWord.ToLower.StartsWith(c) Then Return "an " & nextWord
        Next
        Return "a " & nextWord
    End Function

    Public Function fileget(pathname As String) As List(Of String)
        Dim templist As New List(Of String)

        Try
            Dim sr As New StreamReader(pathname)
            Do While sr.Peek <> -1
                Dim line As String = sr.ReadLine
                templist.Add(line)
            Loop
        Catch ex As Exception
            MsgBox("Invalid pathname")
            For count As Integer = 1 To 20
                templist.Add(count)
            Next
        End Try

        Return templist
    End Function
    Public Function filegetBracket(pathname As String, bracketString As String) As List(Of String)
        Dim total As New List(Of String)
        Try
            Dim line As String
            Using sr As New StreamReader(pathname)
                While sr.Peek <> -1
                    line = sr.ReadLine
                    If line = "[" & bracketString & "]" Then
                        'found bracketString
                        'keep reading until next bracket
                        While sr.Peek <> -1
                            line = sr.ReadLine
                            If line.StartsWith("[") Then
                                'reached next bracketed section, stop searching
                                Return total
                            Else
                                If line <> "" Then total.Add(line)
                            End If
                        End While
                    End If
                End While
            End Using
        Catch ex As Exception
            MsgBox(ex.ToString)
            Return Nothing
        End Try
        Return total
    End Function
    Public Function csvFileget(pathname As String) As List(Of String())
        Dim total As New List(Of String())

        Using parser As New TextFieldParser(pathname)
            parser.SetDelimiters(",")
            parser.HasFieldsEnclosedInQuotes = True

            'skip header
            parser.ReadLine()

            While parser.EndOfData = False
                Dim currentLine As String() = parser.ReadFields
                If currentLine(0) <> "" Then total.Add(currentLine)
            End While
        End Using

        Return total
    End Function
End Module


Public Class range
    Public Property min As Integer
    Public Property max As Integer

    Public Sub New(_min As Integer, _max As Integer)
        min = _min
        max = _max
    End Sub
    Public Overrides Function ToString() As String
        If min = max Then Return min Else Return min & "-" & max
    End Function

    Public Function roll() As Integer
        Return rng.Next(min, max + 1)
    End Function
    Public Function isWithin(value As Integer) As Boolean
        If value >= min AndAlso value <= max Then Return True Else Return False
    End Function
End Class

Public Class xy
    Public Property x As Integer
    Public Property y As Integer

    Public Sub New()

    End Sub
    Public Sub New(aX As Integer, aY As Integer)
        x = aX
        y = aY
    End Sub
    Public Overrides Function ToString() As String
        Return x & "," & y
    End Function
End Class

Public Class rollingCounter
    Private Property value As Integer

    Public Sub New(aValue As Integer)
        value = aValue
    End Sub
    Public Function Tick() As Integer
        Tick = value
        value += 1
    End Function
    Public Function Last() As Integer
        Return value - 1
    End Function
End Class