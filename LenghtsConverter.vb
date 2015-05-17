
Option Strict On





Public Class Form1



    Private Sub btnconvert_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnconvert.Click
        Dim inch As Double
        Dim inch2 As Double = 0
        Dim inch3 As Double
        Dim inch4 As Double
        Dim inch5 As Double
        Dim foot2 As Double
        Dim foot3 As Double
        Dim foot4 As Double
        Dim foot5 As Double
        Dim yard2 As Double = 0
        Dim yard4 As Double
        Dim yard3 As Double
        Dim yard5 As Double
        Dim yard9 As Double
        Dim mile As Double
        Dim input As Double


        Dim foot As Double = 0
        Dim yard As Double = 0

        Dim meter As Double = 0
        Dim meter2 As Double = 0
        Dim meter3 As Double
        Dim meter4 As Double
        Dim meter5 As Double
        Dim mile2 As Double = 0
        Dim mile3 As Double
        Dim mile4 As Double
        Dim mile5 As Double



        Dim play As Double
        Dim play2 As String
        Dim play3 As String
        Dim play4 As String
        Dim play5 As String
        Dim output As Double

        If txtinput.Text = "" Then
            MsgBox("please enter a value and select appropriate unit", MsgBoxStyle.Information)

        End If

        input = CDbl(txtinput.Text)

        If input <= 0 Then

            MsgBox("please enter a positive value", MsgBoxStyle.Information)

        End If


        Dim play6() As Double = {0, 0.833, 1 / 0.0277777778, 0.0254, 1 / 63360}


        If LstboxselectA.SelectedIndex = 0 = True Then

            If lstboxselectB.SelectedIndex = 0 = True Then
                inch = CDbl(txtinput.Text)
                txtoutput.Text = CStr(play6(0))
                MsgBox("you cannot convert to same units")


            End If


            If (LstboxselectA.SelectedIndex = 1) Or (lstboxselectB.SelectedIndex = 1 = True) Then

                foot2 = CInt(txtinput.Text)
                txtoutput.Text = CStr(foot2 * play6(1))

            End If

            If (LstboxselectA.SelectedIndex = 1) Or (lstboxselectB.SelectedIndex = 2 = True) Then
                yard9 = CInt(txtinput.Text)
                txtoutput.Text = CStr(yard9 / 0.0277777778)

            End If

            If (LstboxselectA.SelectedIndex = 1) Or (lstboxselectB.SelectedIndex = 3 = True) Then
                meter = CInt(txtinput.Text)
                txtoutput.Text = CStr(meter * play6(3))

            End If

            If (LstboxselectA.SelectedIndex = 1) Or (lstboxselectB.SelectedIndex = 4 = True) Then
                mile = CInt(txtinput.Text)
                txtoutput.Text = CStr(mile * play6(4))

            End If

        End If





        calculate()




        calculate2()

        calculate3()


        calculate4()





       

    End Sub





    Private Sub calculate()



        

        Dim inch As Double = 0
        Dim inch2 As Double = 0
        Dim inch3 As Double
        Dim inch4 As Double
        Dim inch5 As Double
        Dim foot2 As Double
        Dim foot3 As Double
        Dim foot4 As Double
        Dim foot5 As Double
        Dim yard2 As Double
        Dim yard4 As Double
        Dim yard3 As Double
        Dim yard5 As Double

        Dim foot As Double = 0
        Dim yard As Double = 0

        Dim meter As Double = 0
        Dim meter2 As Double
        Dim meter3 As Double = 0.09144
        Dim meter4 As Double
        Dim meter5 As Double
        Dim mile2 As Double
        Dim mile3 As Double
        Dim mile4 As Double
        Dim mile5 As Double
        Dim mini As Double


        Dim mile As Double = 0

        Dim play As String
        Dim play2 As String
        Dim play3 As String
        Dim play4 As String




        Dim play5() As Double = {1 / 12, 0, 1 / 3, 1 / 3.28155, 1 / 5280}

        'Dim result As Double = lstboxselectB.SelectedIndex






        If LstboxselectA.SelectedIndex = 1 = True Then




            If lstboxselectB.SelectedIndex = 0 = True Then
                inch2 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(inch2 / 12)

            End If

            If lstboxselectB.SelectedIndex = 1 = True Then
                txtoutput.Text = CStr(play5(1))
                MsgBox("you cannot convert to same units")

            End If

            If lstboxselectB.SelectedIndex = 2 = True Then
                yard2 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(yard2 * play5(2))




            End If

            If lstboxselectB.SelectedIndex = 3 = True Then
                meter2 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(meter3 * play5(3))

            End If

            If lstboxselectB.SelectedIndex = 4 = True Then
                mile2 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(mile2 * play5(4))

            End If


        End If

        

    End Sub

    Private Sub calculate2()







        Dim inch As Double = 0
        Dim inch2 As Double = 0
        Dim inch3 As Double

        Dim inch4 As Double
        Dim inch5 As Double
        Dim foot2 As Double
        Dim foot3 As Double

        Dim foot4 As Double
        Dim foot5 As Double
        Dim yard2 As Double = 0
        Dim yard4 As Double
        Dim yard3 As Double
        Dim yard5 As Double

        Dim foot As Double = 0
        Dim yard As Double = 0

        Dim meter As Double = 0
        Dim meter2 As Double = 0
        Dim meter3 As Double
        Dim meter5 As Double
        Dim mile2 As Double = 0
        Dim mile3 As Double
        Dim mile4 As Double
        Dim mile5 As Double
        Dim mini As Double

        Dim play4() As Double = {36, 3, 0, 0.9144, 0.00568181818}

        If LstboxselectA.SelectedIndex = 2 = True Then

            If lstboxselectB.SelectedIndex = 0 = True Then
                inch3 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(inch3 * play4(0))

            End If

            If lstboxselectB.SelectedIndex = 1 = True Then
                foot3 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(foot3 * play4(1))

            End If

            If lstboxselectB.SelectedIndex = 2 = True Then
                txtoutput.Text = CStr(play4(2))
                MsgBox("you cannot convert to same units")

            End If

            If lstboxselectB.SelectedIndex = 3 = True Then
                meter3 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(meter3 * play4(3))

            End If

            If lstboxselectB.SelectedIndex = 4 = True Then
                mile3 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(mile3 * play4(4))

            End If

        End If



       
    End Sub

    Private Sub calculate3()

        Dim inch4 As Double
        Dim foot4 As Double
        Dim yard4 As Double
        Dim meter4 As Double
        Dim mile4 As Double
        Dim mile5 As Double
        Dim play As Double

        Dim play3() As Double = {39.3700787, 3.2808399, 1.0936133, 0, 0.000621371192}

        If LstboxselectA.SelectedIndex = 3 = True Then

            If lstboxselectB.SelectedIndex = 0 = True Then
                inch4 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(inch4 * play3(0))

            End If

            If lstboxselectB.SelectedIndex = 1 = True Then
                foot4 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(foot4 * play3(1))

            End If

            If lstboxselectB.SelectedIndex = 2 = True Then
                yard4 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(yard4 * play3(2))


            End If

            If lstboxselectB.SelectedIndex = 3 = True Then
                txtoutput.Text = CStr(play3(3))
                MsgBox("you cannot convert to same units")


            End If

            If lstboxselectB.SelectedIndex = 4 = True Then
                mile4 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(mile4 * play3(4))

            End If

        End If


    End Sub


    Private Sub calculate4()


        Dim inch5 As Double
        Dim foot5 As Double
        Dim yard5 As Double
        Dim meter5 As Double
        Dim mile5 As Double

        Dim play2() As Double = {63360, 5280, 1760, 1609.344, 0}

        If LstboxselectA.SelectedIndex = 4 = True Then

            If lstboxselectB.SelectedIndex = 0 = True Then
                inch5 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(inch5 * play2(0))

            End If

            If lstboxselectB.SelectedIndex = 1 = True Then
                foot5 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(foot5 * play2(1))


            End If

            If lstboxselectB.SelectedIndex = 2 = True Then
                yard5 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(yard5 * play2(2))

            End If

            If lstboxselectB.SelectedIndex = 3 = True Then
                meter5 = CDbl(txtinput.Text)
                txtoutput.Text = CStr(meter5 * play2(3))

            End If

            If lstboxselectB.SelectedIndex = 4 = True Then
                txtoutput.Text = CStr(play2(4))
                MsgBox("you cannot convert to same units")

            End If

        End If





    End Sub


End Class
