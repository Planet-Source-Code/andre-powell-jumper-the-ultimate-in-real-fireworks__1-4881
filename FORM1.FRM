VERSION 4.00
Begin VB.Form Form1 
   Appearance      =   0  'Flat
   BackColor       =   &H00000000&
   BorderStyle     =   0  'None
   ClientHeight    =   4320
   ClientLeft      =   4020
   ClientTop       =   2130
   ClientWidth     =   3150
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   BeginProperty Font 
      name            =   "MS Sans Serif"
      charset         =   0
      weight          =   700
      size            =   8.25
      underline       =   0   'False
      italic          =   0   'False
      strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   Height          =   4725
   Left            =   3960
   LinkTopic       =   "Form1"
   ScaleHeight     =   4320
   ScaleWidth      =   3150
   ShowInTaskbar   =   0   'False
   Top             =   1785
   Width           =   3270
   WindowState     =   2  'Maximized
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   8
      Left            =   9720
      Picture         =   "FORM1.frx":0000
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   8
      Top             =   1320
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   7
      Left            =   10080
      Picture         =   "FORM1.frx":03E2
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   7
      Top             =   1920
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   6
      Left            =   10110
      Picture         =   "FORM1.frx":07C4
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   6
      Top             =   2610
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   5
      Left            =   9330
      Picture         =   "FORM1.frx":0BA6
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   5
      Top             =   3270
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   4
      Left            =   7920
      Picture         =   "FORM1.frx":0F88
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   4
      Top             =   3195
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   3
      Left            =   7575
      Picture         =   "FORM1.frx":136A
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   3
      Top             =   2325
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   2
      Left            =   7890
      Picture         =   "FORM1.frx":174C
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   2
      Top             =   1260
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   3360
      Top             =   60
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   1
      Left            =   9015
      Picture         =   "FORM1.frx":1B2E
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   1
      Top             =   870
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.PictureBox p1 
      Appearance      =   0  'Flat
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      ClipControls    =   0   'False
      ForeColor       =   &H80000008&
      Height          =   570
      Index           =   0
      Left            =   8775
      Picture         =   "FORM1.frx":1F10
      ScaleHeight     =   540
      ScaleWidth      =   630
      TabIndex        =   0
      Top             =   2070
      Visible         =   0   'False
      Width           =   660
   End
End
Attribute VB_Name = "Form1"
Attribute VB_Creatable = False
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_DblClick()
    End
End Sub

Private Sub Form_Load()
    Dim nI As Integer
    Dim xPosOld As Integer
    Dim yPosOld As Integer
    
    If App.PrevInstance <> 0 Then
        End
    End If
    Show

    nWhich = BOUNCER
    nWhich = DOTS
    'nWhich = Water

    Select Case nWhich
    Case BOUNCER
        nNoBalls = 9
            For nI = 1 To nNoBalls
                xPos(nI) = Rnd * Form1.Width  'p1(nI - 1).Left
                yPos(nI) = Rnd * Form1.Height 'p1(nI - 1).Top
                nGravity(nI) = (Rnd * 10) + 1
                xVelocInit(nI) = (Rnd * 50) + 1
                yVeloc(nI) = 0
            Next
            Timer1.Enabled = True
    Case DOTS
        InitLines

    Case WATER
        Dim i%
        i% = 5
        Init
        Do
            done = 0
            Do
                Moveit
                DoEvents
            Loop While done <> 0
        i% = i% - 1
        Init
        Loop While i% <> 0
    End Select
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
Dim nI As Integer
Dim explo As Integer
Dim nBall As Integer

If Button And 1 = 1 Then
    BigExplosion X, Y, 10
End If

End Sub

Private Sub Form_Unload(Cancel As Integer)
    Unload Me
    End
End Sub

Private Sub Timer1_Timer()
    Select Case nWhich
    Case BOUNCER
        MoveBalls
    Case WATER
    Case DOTS
        MoveDots
        If Rnd * 200 > 195 Then
            BigExplosion Form1.Width * Rnd, Form1.Height * Rnd, 10
        End If
    End Select
End Sub

