Attribute VB_Name = "Module11"
Sub calcAvg()
    Dim familyNameCount As Integer
    Dim familyName(0 To 10000) As String
    Dim father(0 To 10000) As String
    Dim mother(0 To 10000) As String
    Dim fatherCount As Integer
    Dim motherCount As Integer
    Dim rowcount As Integer
    Dim columnNeeded As Integer
    Dim i As Integer
    Dim j As Integer
    Dim k As Integer
    Dim l As Integer
    Dim secondaryTableOffset As Integer
    Dim plantedTotal As Long
    Dim overallPlantedTotal As Long
    Dim psaTotal As Long
    Dim overallPsaTotal As Long
    Dim loopRowCount As Integer
    Dim loopcolumncount As Integer
    Dim actualRowCount As Integer
    Dim actualcolumncount As Integer
    Dim threshold As Double
    
    familyName(0) = Cells(2, 2)
    rowcount = rowcount + 2
    familyNameCount = familyNameCount + 1
    i = 3
    Do While Cells(i, 2) <> ""
breakoutMistake:
        If Cells(i, 2).Value <> familyName(familyNameCount - 1) Then
            familyName(familyNameCount) = Cells(i, 2).Value
            familyNameCount = familyNameCount + 1
        End If
        i = i + 1
        rowcount = rowcount + 1
    Loop
    If Cells(i, 11).Value <> "" Then
        Cells(i, 2).Value = ("PH_" & i)
        GoTo breakoutMistake
    End If
    Cells((rowcount + j + 2), 1).Value = "Mother"
    Cells((rowcount + j + 2), 2).Value = "Father"
    Cells((rowcount + j + 2), 3).Value = "Planted"
    Cells((rowcount + j + 2), 4).Value = "PSA death"
    Cells((rowcount + j + 2), 5).Value = "Percent alive"
    Cells((rowcount + j + 2), 7).Value = "Female\Male"
    overallPsaTotal = 0
    overallPlantedTotal = 0
    For j = 0 To familyNameCount
        plantedTotal = 0
        psaTotal = 0
        For i = 1 To (rowcount + 1)
            If familyName(j) = Cells(i, 2).Value Then
                plantedTotal = plantedTotal + Cells(i, 11).Value
                psaTotal = psaTotal + Cells(i, 13).Value
            End If
        Next i
        
        Cells((rowcount + j + 3), 3).Value = plantedTotal
        overallPlantedTotal = overallPlantedTotal + plantedTotal
        Cells((rowcount + j + 3), 4).Value = psaTotal
        overallPsaTotal = overallPsaTotal + psaTotal
        Cells((rowcount + j + 3), 5).Value = 100 - ((psaTotal / plantedTotal) * 100)
        
        Cells((rowcount + j + 5), 3).Value = overallPlantedTotal
        Cells((rowcount + j + 5), 4).Value = overallPsaTotal
        Cells((rowcount + j + 5), 5).Value = 100 - ((overallPsaTotal / overallPlantedTotal) * 100)
        threshold = Cells((rowcount + j + 5), 5).Value
        
        Cells((rowcount + j + 3), 5).Value = Format(Cells((rowcount + j + 3), 5).Value, "0.00")
        If Cells((rowcount + j + 3), 5).Value < threshold Then
            Cells((rowcount + j + 3), 5).Interior.Color = RGB(195, 60, 60)
        End If
        If Cells((rowcount + j + 3), 5).Value >= threshold Then
            Cells((rowcount + j + 3), 5).Interior.Color = RGB(60, 195, 60)
        End If
        If Cells((rowcount + j + 3), 3).Value < 40 Then
            Cells((rowcount + j + 3), 5).Interior.Color = RGB(255, 255, 255)
        End If
        If j = (familyNameCount - 1) Then Exit For
    Next j
    Cells((rowcount + j + 4), 3).Value = ""
    Cells((rowcount + j + 4), 4).Value = ""
    Cells((rowcount + j + 4), 5).Value = ""
    Cells((rowcount + j + 3), 5).Value = Format(Cells((rowcount + j + 3), 5).Value, "0.00")
        If Cells((rowcount + j + 5), 5).Value < threshold Then
            Cells((rowcount + j + 5), 5).Interior.Color = RGB(195, 60, 60)
        End If
        If Cells((rowcount + j + 5), 5).Value >= threshold Then
            Cells((rowcount + j + 5), 5).Interior.Color = RGB(60, 195, 60)
        End If
        
    'derive mother and father names from the table
    
    fatherCount = 0
    fatherFound = 0
    motherCount = 0
    motherfound = 0
    For j = 0 To familyNameCount
        Dim parents As Variant
        If Cells((rowcount + j + 3), 2).Value = "" Then
            Cells((rowcount + j + 3), 2).Value = onlyDigits(familyName(j))
            father(fatherCount) = onlyDigits(familyName(j))
            fatherCount = fatherCount + 1
        End If
        parents = Split(familyName(j), "_")
        For a = 0 To UBound(parents)
            If a = 0 Then
                mother(motherCount) = onlyDigits(familyName(j))
                motherCount = motherCount + 1
                Cells((rowcount + j + 3), 1).Value = parents(a)
                motherCount = motherCount + 1
            End If
            If a = 1 Then
                Cells((rowcount + j + 3), 2).Value = parents(a)
                fatherCount = fatherCount + 1
            End If
        Next a
        If j = (familyNameCount - 1) Then Exit For
    Next j
    Call clear(rowcount)
    'use those names and indexed locations to derive a 2D table with averages, colour coat also
    Cells((rowcount + 3), 7).Value = Cells((rowcount + 3), 1).Value
    motherfound = motherfound + 1
    Cells((rowcount + 2), 8).Value = Cells((rowcount + 3), 2).Value
    fatherFound = fatherFound + 1
    For i = 0 To rowcount
        Cells((rowcount + 3 + i), 7).Value = Cells((rowcount + 3 + i), 1).Value
        Cells((rowcount + 3 + i), 6).Value = Cells((rowcount + 3 + i), 2).Value
    Next i
    Call removeDupsMother(rowcount)
    Call sortFather(rowcount)
    
    loopRowCount = (Range("G" & (rowcount + 3)).End(xlDown).Row) - (rowcount + 3)
    loopcolumncount = (Range("F" & (rowcount + 3)).End(xlDown).Row) - (rowcount + 3)
    
    For i = 0 To loopcolumncount
        Cells((rowcount + loopRowCount + 5), 8 + i).Value = 0
        Cells((rowcount + loopRowCount + 6), 8 + i).Value = 0
        Cells((rowcount + loopRowCount + 7), 8 + i).Value = 0
    Next i
    For i = 0 To loopRowCount
        Cells((rowcount + 3 + i), loopcolumncount + 10).Value = 0
        Cells((rowcount + 3 + i), loopcolumncount + 11).Value = 0
        Cells((rowcount + 3 + i), loopcolumncount + 12).Value = 0
    Next i
    
    For j = 0 To rowcount - 2
        For i = 0 To loopRowCount
    'find the row for 2D input
            If (Cells((rowcount + 3 + i), 7).Value = Cells((rowcount + 3 + j), 1).Value) Then
                actualRowCount = i
            End If
        Next i
        'find the column for 2D input
        For k = 0 To loopcolumncount
            If (Cells((rowcount + 3 + k), 6).Value = Cells((rowcount + 3 + j), 2).Value) Then
                actualcolumncount = k
            End If
        Next k
        
        Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Value = Cells((rowcount + 3 + j), 5).Value
        Cells((rowcount + loopRowCount + 5), 7).Value = "Planted"
        Cells((rowcount + loopRowCount + 6), 7).Value = "PSA Death"
        Cells((rowcount + loopRowCount + 7), 7).Value = "Average Alive"
        Cells((rowcount + 2), loopcolumncount + 10).Value = "Planted"
        Cells((rowcount + 2), loopcolumncount + 11).Value = "PSA Death"
        Cells((rowcount + 2), loopcolumncount + 12).Value = "Average Alive"
        
        Cells((rowcount + loopRowCount + 6), 8 + actualcolumncount).Value = Cells((rowcount + loopRowCount + 6), 8 + actualcolumncount).Value + Cells((rowcount + 3 + j), 4).Value
        Cells((rowcount + loopRowCount + 5), 8 + actualcolumncount).Value = Cells((rowcount + loopRowCount + 5), 8 + actualcolumncount).Value + Cells((rowcount + 3 + j), 3).Value
        Cells((rowcount + loopRowCount + 7), 8 + actualcolumncount).Value = 100 - ((Cells((rowcount + loopRowCount + 6), 8 + actualcolumncount).Value / Cells((rowcount + loopRowCount + 5), 8 + actualcolumncount).Value) * 100)
        
        If Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Value < threshold Then
            Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Interior.Color = RGB(195, 60, 60)
        End If
        If Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Value >= threshold Then
            Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Interior.Color = RGB(60, 195, 60)
        End If
        If Cells((rowcount + 3 + j), 3).Value < 40 Then
            Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Interior.Color = RGB(255, 255, 255)
            'GoTo under40success
        End If
        
        'deletes last value?
        If Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Value = "" Then
            Cells((rowcount + 3 + actualRowCount), 8 + actualcolumncount).Interior.Color = xlNone
            Exit For
        End If
'under40success:
    Next j
    'per column colouration of average over per father
    For i = 0 To loopcolumncount
        If Cells((rowcount + loopRowCount + 5), 8 + i).Value < 40 Then
            Cells((rowcount + loopRowCount + 7), 8 + i).Interior.Color = RGB(255, 255, 255)
            GoTo under40successcolumn
        End If
        If Cells((rowcount + loopRowCount + 7), 8 + i).Value < threshold Then
            Cells((rowcount + loopRowCount + 7), 8 + i).Interior.Color = RGB(195, 60, 60)
        End If
        If Cells((rowcount + loopRowCount + 7), 8 + i).Value >= threshold Then
            Cells((rowcount + loopRowCount + 7), 8 + i).Interior.Color = RGB(60, 195, 60)
        End If
under40successcolumn:
        Cells((rowcount + loopRowCount + 7), 8 + i).Value = Format(Cells((rowcount + loopRowCount + 7), 8 + i).Value, "0.00")

    Next i
    'cheating a little and grabbing data from first table to calculate the alive rate for mothers
    l = 0
    For i = 0 To loopRowCount
        For j = 0 To loopcolumncount
            If Cells((rowcount + i + 3), 8 + j).Value <> "" Then
                For k = 0 To rowcount
                    If Cells((rowcount + i + 3), 8 + j).Value = Cells((rowcount + k + 3), 5).Value Then
                          Cells((rowcount + i + 3), loopcolumncount + 10).Value = Cells((rowcount + i + 3), loopcolumncount + 10).Value + Cells((rowcount + l + 3), 3).Value
                          Cells((rowcount + i + 3), loopcolumncount + 11).Value = Cells((rowcount + i + 3), loopcolumncount + 11).Value + Cells((rowcount + l + 3), 4).Value
                          Cells((rowcount + i + 3), loopcolumncount + 12).Value = (100 - ((Cells((rowcount + i + 3), loopcolumncount + 11).Value) / (Cells((rowcount + i + 3), loopcolumncount + 10).Value)) * 100)
                          Cells((rowcount + i + 3), loopcolumncount + 12).Value = Format(Cells((rowcount + i + 3), loopcolumncount + 12).Value, "0.00")
                        If Cells((rowcount + i + 3), loopcolumncount + 12).Value < threshold Then
                            Cells((rowcount + i + 3), loopcolumncount + 12).Interior.Color = RGB(195, 60, 60)
                        End If
                        If Cells((rowcount + i + 3), loopcolumncount + 12).Value >= threshold Then
                            Cells((rowcount + i + 3), loopcolumncount + 12).Interior.Color = RGB(60, 195, 60)
                        End If
                        If Cells((rowcount + i + 3), loopcolumncount + 10).Value < 40 Then
                            Cells((rowcount + i + 3), loopcolumncount + 12).Interior.Color = RGB(255, 255, 255)
                        End If
                        l = l + 1
                    If Cells((rowcount + i + 3), 8 + j).Value = Cells((rowcount + k + 3), 5).Value Then Exit For
                    End If
                Next k
            End If
        Next j
    Next i
    For i = 0 To loopcolumncount
        Cells((rowcount + 3 + i), 6).Value = ""
    Next i
    'you did it!
    MsgBox ("Execution success. Thanks for using ""Shiny Sorter"". v1.0 Beta (hotfix 2 build 20161121/1621), an Excel Software by Harrison Crane. (Plant and Food Research 2016)")
End Sub
'place your cursor into this part and click run to remove numbers from the first row you select, output cleanup
Private Sub clear(rowcount As Integer)
s = Array("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
For Each r In Range("A" & (rowcount + 3) & ":" & "A" & ((rowcount * 2) + 3))
v = r.Value
For i = 0 To 9
v = Replace(v, s(i), "")
Next
r.Value = v
Next
End Sub

Private Sub removeDupsMother(rowcount As Integer)
ActiveSheet.Range("G" & (rowcount + 3) & ":G" & ((rowcount * 2) + 3) & "").RemoveDuplicates Columns:=1, Header:=xlNo
End Sub

Private Sub sortFather(rowcount As Integer)
Range("F" & (rowcount + 3) & ":F" & ((rowcount * 2) + 3) & "").Sort Key1:=Range("F" & (rowcount + 3) & ":F" & ((rowcount * 2) + 3) & ""), Order1:=xlAscending
ActiveSheet.Range("F" & (rowcount + 3) & ":F" & ((rowcount * 2) + 3) & "").RemoveDuplicates Columns:=1, Header:=xlNo
For i = 0 To rowcount
    Cells((rowcount + 2), (8 + i)).Value = Cells((rowcount + 3 + i), 6).Value
Next i
End Sub


Function onlyDigits(s As String) As String
    ' Variables needed (remember to use "option explicit").   '
    Dim retval As String    ' This is the return string.      '
    Dim i As Integer        ' Counter for character position. '

    ' Initialise return string to empty                       '
    retval = ""

    ' For every character in input string, copy digits to     '
    '   return string.                                        '
    For i = 1 To Len(s)
        If Mid(s, i, 1) >= "0" And Mid(s, i, 1) <= "9" Then
            retval = retval + Mid(s, i, 1)
        End If
    Next

    ' Then return the return string.                          '
    onlyDigits = retval
End Function
