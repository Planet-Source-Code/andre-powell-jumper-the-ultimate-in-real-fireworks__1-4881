Attribute VB_Name = "Module1"
Option Explicit

' --------- Water Sim ----------
Global xyc(5000, 3) As Integer
Global Bxyc(5000, 4) As Integer

Global NumSand%, NumBub%
Global done%
' ------------------------------

' -------- Bouncer -------------
Global ox(500) As Integer
Global oy(500) As Integer
Global xC(500) As Long
Global xDead(500) As Integer
Global xType(500) As Integer

Global xPos(500) As Integer         '// current x position
Global yPos(500) As Integer         '// current y position
Global xPosInit(500) As Integer     '// initial x position
Global yPosInit(500) As Integer     '//  initial y position
Global xVelocInit(500) As Integer   '// x initial velocity
Global yVeloc(500) As Integer
Global nGravity(500) As Integer     '// acceleration factor
Global nNoBalls As Integer         '// Number of balls

' ------------------------------

Global Const BOUNCER = 1
Global Const WATER = 2
Global Const DOTS = 3
Global nWhich As Integer

Sub BigExplosion(X As Single, Y As Single, Count As Integer)
    Explosion X, Y, Count, QBColor(14)
    Explosion X, Y, Count, QBColor(15)
    Explosion X, Y, Count, QBColor(12)
    Explosion X, Y, Count, QBColor(15)
End Sub

Sub Explosion(X As Single, Y As Single, nCount As Integer, nColor As Long)
Dim nI As Integer
Dim explo As Integer
Dim nBall As Integer

    explo = 20
    nBall = 1
    For nI = 1 To nCount
        Do
            nBall = nBall + 1
        Loop While xType(nBall) <> 0
        
        'xC(nI) = RGB(Int(Rnd * 254), Int(Rnd * 254), Int(Rnd * 254))
        xC(nBall) = nColor 'QBColor(14)
        xPos(nBall) = X
        yPos(nBall) = Y
        'xPos(nBall) = Form1.Width / 2
        'yPos(nBall) = Form1.Height / 2
        nGravity(nBall) = (Rnd * 50) + 1
        xVelocInit(nBall) = (Rnd * 200) + 1
        If (Rnd * 50) > 25 Then
            xVelocInit(nBall) = -xVelocInit(nBall)
        End If
        yVeloc(nBall) = (Rnd * 200) + 1
    
        'If (Rnd * 50) > 25 Then
            yVeloc(nBall) = -yVeloc(nBall)
        'End If
    
        If Rnd * 20 > 10 Then
            xType(nBall) = 2 ' Flame
        Else
            xType(nBall) = 3 ' Dot
        End If
        If nBall > nNoBalls Then
            nNoBalls = nBall
        End If
    Next
    'nNoBalls = nNoBalls + explo

End Sub

'
Sub Init()
    Dim i%, j%, mult%
    Dim CX, CY, Msg, xPos, yPos ' Declare variables.

    NumSand = 250
    NumBub = 50

    mult = 4

    Form1.ScaleMode = 3               ' Set ScaleMode to pixels.
    Form1.DrawWidth = 4               ' Set DrawWidth.
    Form1.ForeColor = QBColor(4)      ' Set background to red.
'    backColor = QBColor(0)
    Form1.FontSize = 24               ' Set point size.
    CX = Form1.ScaleWidth / 2         ' Get horizontal center.
    CY = Form1.ScaleHeight / 2        ' Get vertical center.
    For i% = 1 To NumSand
        xyc(i%, 1) = Form1.DrawWidth * Int(Rnd * ((Form1.ScaleWidth / Form1.DrawWidth) - 1))
        j = 0
        Do
            j = j + Form1.DrawWidth
            If j > 20000 Then
                j = 100
                Exit Do
            End If
        Loop While Form1.Point(xyc(i%, 1), j) <> 0
        xyc(i%, 2) = j
        xyc(i%, 3) = Int(Rnd * 13) + 1
        Form1.ForeColor = QBColor(xyc(i%, 3))  ' Set foreground color.
        Form1.PSet (xyc(i%, 1), j), QBColor(xyc(i%, 3))
        DoEvents
    Next

    For i% = 1 To NumBub
        Bxyc(i%, 1) = Form1.DrawWidth * Int(Rnd * ((Form1.ScaleWidth / Form1.DrawWidth) - 1))
        j = Int(Form1.ScaleHeight / Form1.DrawWidth) * Form1.DrawWidth
        Do
            j = j - Form1.DrawWidth
            If j < 10 Then
                j = 5
                Exit Do
            End If
        Loop While Form1.Point(Bxyc(i%, 1), j) <> 0  ' White
        Bxyc(i%, 2) = j     ' Y axis start pos
        Bxyc(i%, 3) = 14    ' color to yellow
        Bxyc(i%, 4) = 0     ' Auto level direction
        Form1.ForeColor = QBColor(Bxyc(i%, 3))  ' Set foreground color.
        Form1.PSet (Bxyc(i%, 1), j), QBColor(Bxyc(i%, 3))
        DoEvents
    Next

End Sub

Sub InitLines()
Dim nI As Integer
    nNoBalls = 20
    For nI = 1 To nNoBalls
        xC(nI) = RGB(Int(Rnd * 254), Int(Rnd * 254), Int(Rnd * 254))
        xPos(nI) = Rnd * Form1.Width
        yPos(nI) = Rnd * Form1.Height
        xPos(nI) = Form1.Width / 2
        yPos(nI) = Form1.Height / 2
        nGravity(nI) = (Rnd * 50) + 1
        xVelocInit(nI) = (Rnd * 200) + 1
        If (Rnd * 50) > 25 Then
            xVelocInit(nI) = -xVelocInit(nI)
        End If
        yVeloc(nI) = (Rnd * 200) + 1

        'If (Rnd * 50) > 25 Then
            yVeloc(nI) = -yVeloc(nI)
        'End If
        xType(nI) = 1

    Next
    Form1.Timer1.Enabled = True

End Sub

Sub MoveBalls()
    Dim nI As Integer
    DoEvents
    For nI = 1 To nNoBalls
        If yVeloc(nI) > 0 Then
            If yPos(nI) >= Form1.Height - Form1.p1(nI - 1).Height Then
                yVeloc(nI) = -yVeloc(nI)
            End If

            If xVelocInit(nI) > 0 Then
                If xPos(nI) >= Form1.Width - Form1.p1(nI - 1).Width Then
                    xVelocInit(nI) = -xVelocInit(nI)
                Else
                    If xPos(nI) <= 0 Then
                        xVelocInit(nI) = -xVelocInit(nI)
                    End If
                End If
            Else
                If xPos(nI) <= 0 Then
                    xVelocInit(nI) = -xVelocInit(nI)
                End If
            End If
        Else
        End If

        
        If yPos(nI) + Form1.p1(nI - 1).Height > Form1.Height Then
            xDead(nI) = xDead(nI) + 1
            If xDead(nI) = 4 Then
                yVeloc(nI) = 0
                yPos(nI) = yPosInit(nI)
            End If
        Else
            xDead(nI) = 0
        End If
'                If yVeloc(nI) < 7 And yVeloc(nI) > -7 Then
'                    xDead(nI) = xDead(nI) + 1
'                    If xDead(nI) = 3 Then
'                        yVeloc(nI) = 0
'                        yPos(nI) = yPosInit(nI)
'                    End If
'                Else
'                    xDead(nI) = 0
'                End If

        'Else
        yVeloc(nI) = yVeloc(nI) + nGravity(nI)
        xPos(nI) = xPos(nI) + xVelocInit(nI)
        yPos(nI) = yPos(nI) + yVeloc(nI)
        Form1.p1(nI - 1).Left = xPos(nI)
        Form1.p1(nI - 1).Top = yPos(nI)
        'End If
    Next

End Sub

Sub MoveDots()
    Dim nI As Integer
    Dim xx As Single
    Dim yy As Single

    Dim nActive As Integer

    nActive = 0
    Form1.DrawWidth = 2
    DoEvents
    For nI = 1 To nNoBalls
        DoEvents
        If xType(nI) <> 0 Then
            nActive = nActive + 1
                        'If yVeloc(nI) > 0 Then
            If yPos(nI) >= Form1.Height Then
                yVeloc(nI) = -yVeloc(nI)
                If xType(nI) = 2 Or xType(nI) = 3 Then
                    xType(nI) = 0
                Else
                    If Rnd * 20 > 15 Then
                        Explosion 0 + xPos(nI), Form1.Height - 10, 4, QBColor(14)
                        xType(nI) = 0
                    End If

                End If
            End If
    
            If xVelocInit(nI) > 0 Then
                If xPos(nI) >= Form1.Width Then
                    xVelocInit(nI) = -xVelocInit(nI)
                Else
                    If xPos(nI) <= 0 Then
                        xVelocInit(nI) = -xVelocInit(nI)
                    End If
                End If
            Else
                If xPos(nI) <= 0 Then
                    xVelocInit(nI) = -xVelocInit(nI)
                End If
            End If
                            'Else
                
                            'End If
    
            If yPos(nI) <= 0 Then
                yVeloc(nI) = -yVeloc(nI)
            End If
            
            If yPos(nI) > Form1.Height Then
                xDead(nI) = xDead(nI) + 1
                If xDead(nI) = 4 Then
                    yVeloc(nI) = 0
                    yPos(nI) = yPosInit(nI)
                End If
            Else
                xDead(nI) = 0
            End If
    
            'Form1.PSet (xPos(nI), yPos(nI)), 0
    
            
            If (0& + ox(nI) + oy(nI)) <> 0 Then
                xx = xPos(nI)
                yy = yPos(nI)
        
                yVeloc(nI) = yVeloc(nI) + nGravity(nI)
                xPos(nI) = xPos(nI) + xVelocInit(nI)
                yPos(nI) = yPos(nI) + yVeloc(nI)
                    
                If xType(nI) = 3 Then
                    Form1.PSet (ox(nI), oy(nI)), QBColor(0)
                Else
                    Form1.Line (xx, yy)-(ox(nI), oy(nI)), QBColor(0)
                End If

                ox(nI) = xx
                oy(nI) = yy
                If xType(nI) = 0 Then
                    Form1.Line (xPos(nI), yPos(nI))-(xx, yy), 0
                Else
                    If xType(nI) = 3 Then
                        Form1.PSet (xx, yy), xC(nI)
                    Else
                        Form1.Line (xPos(nI), yPos(nI))-(xx, yy), xC(nI)
                    End If
                End If


            Else
                ox(nI) = xPos(nI)
                oy(nI) = yPos(nI)
    
                yVeloc(nI) = yVeloc(nI) + nGravity(nI)
                xPos(nI) = xPos(nI) + xVelocInit(nI)
                yPos(nI) = yPos(nI) + yVeloc(nI)
    
            End If
            'Form1.PSet (xPos(nI), yPos(nI)), xC(nI)

            If xType(nI) = 1 Then
            If Rnd * 300 > 298 Then
                Explosion 0 + xPos(nI), 0 + yPos(nI), 10, QBColor(14)
            End If
            End If

        End If
    Next

    If nActive = 0 Then
        InitLines
    End If
'    nI = 1
'    Do
'
'    Loop While xType(nI) <> 0

End Sub

Sub Moveit()

Dim X%, Y%, i%, j%, c%
    j% = 1  ' bub counter
    Dim b%  ' if bub moved
    done = 0
    b = 0
    For i% = 1 To NumSand
        X = xyc(i%, 1)
        Y = xyc(i%, 2)
        c = xyc(i%, 3)

       If Y < Form1.ScaleHeight - 1 Then

        If Form1.Point(X, Y + Form1.DrawWidth) = 0 Then  ' Down
                xyc(i%, 2) = Y + Form1.DrawWidth
                Form1.PSet (X, xyc(i%, 2)), QBColor(c)
                Form1.PSet (X, Y), 0
                done = 1
        Else
            If Rnd * 100 > 50 Then
                If Form1.Point(X + Form1.DrawWidth, Y + Form1.DrawWidth) = 0 Then ' Down right
                    xyc(i%, 2) = Y + Form1.DrawWidth
                    xyc(i%, 1) = X + Form1.DrawWidth
                    Form1.PSet (X + Form1.DrawWidth, xyc(i%, 2)), QBColor(c)
                    Form1.PSet (X, Y), 0
                    done = 1
                End If
            Else
                If Form1.Point(X - Form1.DrawWidth, Y + Form1.DrawWidth) = 0 Then ' Down left
                    xyc(i%, 2) = Y + Form1.DrawWidth
                    xyc(i%, 1) = X - Form1.DrawWidth
                    Form1.PSet (X - Form1.DrawWidth, xyc(i%, 2)), QBColor(c)
                    Form1.PSet (X, Y), 0
                    done = 1
                End If
            End If
        End If

        DoEvents
       End If
' -------------------------------------------- bub
        X = Bxyc(j%, 1)
        Y = Bxyc(j%, 2)
        c = 14

       If Y > 1 Then    ' If not at top.

        If Form1.Point(X, Y - Form1.DrawWidth) = 0 Then ' up
            Bxyc(j%, 2) = Y - Form1.DrawWidth
            Form1.PSet (X, Bxyc(j%, 2)), QBColor(c)
            Form1.PSet (X, Y), 0
            Bxyc(j, 4) = 0
            b = 1

        Else
            If Rnd * 100 > 50 Then  ' up Left or up Right
                If Form1.Point(X + Form1.DrawWidth, Y - Form1.DrawWidth) = 0 Then ' up right
                    Bxyc(j%, 2) = Y - Form1.DrawWidth
                    Bxyc(j%, 1) = X + Form1.DrawWidth
                    Form1.PSet (X + Form1.DrawWidth, Bxyc(j%, 2)), QBColor(c)
                    Form1.PSet (X, Y), 0
                    Bxyc(j, 4) = 0
                    b = 1
                End If
            Else
                If Form1.Point(X - Form1.DrawWidth, Y - Form1.DrawWidth) = 0 Then ' up left
                    Bxyc(j%, 2) = Y - Form1.DrawWidth
                    Bxyc(j%, 1) = X - Form1.DrawWidth
                    Form1.PSet (X - Form1.DrawWidth, Bxyc(j%, 2)), QBColor(c)
                    Form1.PSet (X, Y), 0
                    Bxyc(j, 4) = 0
                    b = 1
                End If
            End If
        End If

        If b = 0 Then   ' Bub not moved, so go left or right.
            If Bxyc(j%, 4) = 0 Then
                If Rnd * 100 > 50 Then
                    Bxyc(j%, 4) = 2
                Else
                    Bxyc(j%, 4) = 1
                End If
            End If
            If Bxyc(j, 4) = 1 Then  ' left

                If Form1.Point(X - Form1.DrawWidth, Y) = 0 And X > 2 Then
                    Bxyc(j%, 1) = X - Form1.DrawWidth
                    Form1.PSet (X - Form1.DrawWidth, Y), QBColor(c)
                    Form1.PSet (X, Y), 0
                    b = 1
                Else
                    Bxyc(j, 4) = 2
                End If

            Else                    ' right

                If Form1.Point(X + Form1.DrawWidth, Y) = 0 And X < Form1.ScaleWidth - 5 Then
                    Bxyc(j%, 1) = X + Form1.DrawWidth
                    Form1.PSet (X + Form1.DrawWidth, Y), QBColor(c)
                    Form1.PSet (X, Y), 0
                    b = 1
                Else
                    Bxyc(j, 4) = 1
                End If


            End If
        End If


'        DoEvents
       End If
       j% = j% + 1
        If j% > NumBub Then
            j% = 1
        End If
    Next

End Sub

