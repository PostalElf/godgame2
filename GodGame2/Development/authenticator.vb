Imports System.IO
Imports System.Text
Imports System.Security.Cryptography

Public Class authenticator
    Friend Shared Sub generateHash(pathname As String)
        Dim rawData As String = ""
        Using reader As New StreamReader(pathname)
            rawData = reader.ReadToEnd
            reader.Close()
        End Using

        Dim hash As String = ""
        Using md5hash As MD5 = MD5.Create
            hash = getMd5Hash(md5hash, rawData)
        End Using

        Dim newPathname As String = getHashPathname(pathname)
        Using writer As New StreamWriter(newPathname)
            writer.WriteLine(hash)
            writer.Close()
        End Using
    End Sub
    Friend Shared Function checkHash(pathname As String) As Boolean
        Try
            Dim rawData As String = ""
            Using reader As New StreamReader(pathname)
                rawData = reader.ReadToEnd
                reader.Close()
            End Using

            Dim hash As String = ""
            Dim newPathname As String = getHashPathname(pathname)
            Using reader As New StreamReader(newPathname)
                hash = reader.ReadLine
                reader.Close()
            End Using
            hash.Trim()

            Using md5hash As MD5 = MD5.Create
                Return VerifyMd5Hash(md5hash, rawData, hash)
            End Using
        Catch ex As Exception
            MsgBox("CheckHash Error: " & ex.Message)
            Return Nothing
        End Try
    End Function

    Private Shared Function getHashPathname(pathname As String) As String
        Dim directoryAndFilename As String() = pathname.Split("/")
        Dim filename As String = directoryAndFilename(1)
        Dim newFilename As String = filename.Split(".")(0)
        Dim newPathname As String = "hash/" & newFilename & ".md5"

        Return newPathname
    End Function
    Private Shared Function getMd5Hash(ByVal md5Hash As MD5, ByVal input As String)
        ' Convert the input string to a byte array and compute the hash.
        Dim data As Byte() = md5Hash.ComputeHash(Encoding.UTF8.GetBytes(input))

        ' Create a new Stringbuilder to collect the bytes and create a string.
        Dim sBuilder As New StringBuilder()

        ' Loop through each byte of the hashed data and format each one as a hexadecimal string.
        Dim i As Integer
        For i = 0 To data.Length - 1
            sBuilder.Append(data(i).ToString("x2"))
        Next i

        ' Return the hexadecimal string.
        Return sBuilder.ToString()
    End Function
    Private Shared Function VerifyMd5Hash(ByVal md5Hash As MD5, ByVal input As String, ByVal hash As String) As Boolean
        ' Hash the input.
        Dim hashOfInput As String = GetMd5Hash(md5Hash, input)

        ' Compare
        Dim result As Integer = String.Compare(hash, hashOfInput)
        If result = 0 Then Return True Else Return False
    End Function

    Friend Shared Function spellcheckFiles() As Boolean
        'makes sure that there are no accidental typos that lead to invalid files
        Dim total As New List(Of String())


        'building
        total.Clear()
        total = csvFileget(constants.pathnameBuilding)
        For Each line In total
            Dim n As New rollingCounter(0)
            Dim id As Integer = CInt(line(n.Tick))
            Dim name As String = line(n.Tick)
            Dim cost As Integer = CInt(line(n.Tick))
            Dim requiredBuildingID As Integer = CInt(line(n.Tick))

            While n.Tick < line.Count AndAlso line(n.Last) <> ""
                Dim modifier As New modifier(Nothing, name, line(n.Last))
                If modifier.invalidString = True Then Return False
            End While
        Next


        'science
        total.Clear()
        total = csvFileget(constants.pathnameScience)
        For Each line In total
            Dim i As rollingCounter = New rollingCounter(0)
            Dim name As String = line(i.Tick)
            Dim tier As Integer = CInt(line(i.Tick))
            Dim sIsKeyTechTo As Integer = CInt(line(i.Tick))
            Dim sCost As Integer = CInt(line(i.Tick))
            Dim sDescription As String = line(i.Tick)

            While i.Tick < line.Count AndAlso line(i.Last) <> ""
                Dim modifier As New modifier(Nothing, name, line(i.Last))
                If modifier.invalidString = True Then Return False
            End While
        Next


        'culture
        total.Clear()
        total = csvFileget(constants.pathnameCulture)
        For Each line In total
            Dim i As rollingCounter = New rollingCounter(0)
            Dim name As String = line(i.Tick)
            Dim tier As Integer = CInt(line(i.Tick))
            Dim sIsKeyTechTo As Integer = CInt(line(i.Tick))
            Dim sCost As Integer = CInt(line(i.Tick))
            Dim sDescription As String = line(i.Tick)

            While i.Tick < line.Count AndAlso line(i.Last) <> ""
                Dim modifier As New modifier(Nothing, name, line(i.Last))
                If modifier.invalidString = True Then Return False
            End While
        Next


        'zeitgeist
        total.Clear()
        total = csvFileget(constants.pathnameZeitgeist)
        For Each line In total
            Dim n As New rollingCounter(0)
            Dim id As Integer = CInt(line(n.Tick))
            Dim name As String = line(n.Tick)
            Dim description As String = line(n.Tick)

            Dim split As String() = line(n.Tick).Split(" ")
            For i = 0 To split.Count - 1 Step 2
                Dim shapeStr As String = split(i)
                Dim shape As philosophyShape = constants.getPhilosophyShapeFromString(shapeStr)
                Dim value As Integer = split(i + 1)
                If value = 0 OrElse shape = Nothing Then Return False
            Next

            While n.Tick < line.Count AndAlso line(n.Last) <> ""
                Dim modifier As New modifier(Nothing, name, line(n.Last))
                If modifier.invalidString = True Then Return False
            End While
        Next


        'philosophy
        total.Clear()
        total = csvFileget(constants.pathnamePhilosophy)
        For Each line In total
            Dim n As New rollingCounter(0)
            Dim id As Integer = CInt(line(n.Tick))
            Dim name As String = line(n.Tick)
            Dim shape As philosophyShape = constants.getPhilosophyShapeFromString(line(n.Tick))
            If shape = Nothing Then Return False

            While n.Tick < line.Count AndAlso line(n.Last) <> ""
                Dim modifier As New modifier(Nothing, name, line(n.Last))
                If modifier.invalidString = True Then Return False
            End While
        Next


        'threat


        'challenges


        'clear
        Return True
    End Function
End Class
