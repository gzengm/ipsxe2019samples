'==============================================================
'
' SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE 
' LICENSE AGREEMENT,
' http://software.intel.com/en-us/articles/intel-sample-source-
' code-license-agreement/
'
' Copyright 2016 Intel Corporation
'
' THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR 
' IMPLIED, INCLUDING BUT NOT LIMITED TO ANY IMPLIED WARRANTY OF 
' MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, NON-
' INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
'
' =============================================================

Option Strict Off
Option Explicit On 
Imports System.Runtime.InteropServices ' Required for using MarshalAs
Friend Class frmArray
    Inherits System.Windows.Forms.Form
#Region "Windows Form Designer generated code "
    Public Sub New()
        MyBase.New()
        If m_vb6FormDefInstance Is Nothing Then
            If m_InitializingDefInstance Then
                m_vb6FormDefInstance = Me
            Else
                Try
                    'For the start-up form, the first instance created is the default instance.
                    If System.Reflection.Assembly.GetExecutingAssembly.EntryPoint.DeclaringType Is Me.GetType Then
                        m_vb6FormDefInstance = Me
                    End If
                Catch
                End Try
            End If
        End If
        'This call is required by the Windows Form Designer.
        InitializeComponent()
    End Sub
    'Form overrides dispose to clean up the component list.
    Protected Overloads Overrides Sub Dispose(ByVal Disposing As Boolean)
        If Disposing Then
            If Not components Is Nothing Then
                components.Dispose()
            End If
        End If
        MyBase.Dispose(Disposing)
    End Sub
    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer
    Public ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lstReturn As System.Windows.Forms.ListBox
    Public WithEvents lstOutput As System.Windows.Forms.ListBox
    Public WithEvents cmdPushMe As System.Windows.Forms.Button
    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.
    'Do not modify it using the code editor.
    Public WithEvents lblFromFtn As System.Windows.Forms.Label
    Public WithEvents lblToFtn As System.Windows.Forms.Label
    <System.Diagnostics.DebuggerStepThrough()> Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.lstReturn = New System.Windows.Forms.ListBox
        Me.lstOutput = New System.Windows.Forms.ListBox
        Me.cmdPushMe = New System.Windows.Forms.Button
        Me.lblFromFtn = New System.Windows.Forms.Label
        Me.lblToFtn = New System.Windows.Forms.Label
        Me.SuspendLayout()
        '
        'lstReturn
        '
        Me.lstReturn.BackColor = System.Drawing.SystemColors.Window
        Me.lstReturn.ColumnWidth = 149
        Me.lstReturn.Cursor = System.Windows.Forms.Cursors.Default
        Me.lstReturn.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lstReturn.ForeColor = System.Drawing.SystemColors.WindowText
        Me.lstReturn.ItemHeight = 14
        Me.lstReturn.Location = New System.Drawing.Point(72, 176)
        Me.lstReturn.MultiColumn = True
        Me.lstReturn.Name = "lstReturn"
        Me.lstReturn.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.lstReturn.Size = New System.Drawing.Size(297, 60)
        Me.lstReturn.TabIndex = 4
        '
        'lstOutput
        '
        Me.lstOutput.BackColor = System.Drawing.SystemColors.Window
        Me.lstOutput.ColumnWidth = 149
        Me.lstOutput.Cursor = System.Windows.Forms.Cursors.Default
        Me.lstOutput.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lstOutput.ForeColor = System.Drawing.SystemColors.WindowText
        Me.lstOutput.ItemHeight = 14
        Me.lstOutput.Location = New System.Drawing.Point(72, 88)
        Me.lstOutput.MultiColumn = True
        Me.lstOutput.Name = "lstOutput"
        Me.lstOutput.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.lstOutput.Size = New System.Drawing.Size(297, 60)
        Me.lstOutput.TabIndex = 3
        '
        'cmdPushMe
        '
        Me.cmdPushMe.BackColor = System.Drawing.SystemColors.Control
        Me.cmdPushMe.Cursor = System.Windows.Forms.Cursors.Default
        Me.cmdPushMe.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.cmdPushMe.ForeColor = System.Drawing.SystemColors.ControlText
        Me.cmdPushMe.Location = New System.Drawing.Point(144, 16)
        Me.cmdPushMe.Name = "cmdPushMe"
        Me.cmdPushMe.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.cmdPushMe.Size = New System.Drawing.Size(113, 49)
        Me.cmdPushMe.TabIndex = 0
        Me.cmdPushMe.Text = "Push Me"
        '
        'lblFromFtn
        '
        Me.lblFromFtn.BackColor = System.Drawing.SystemColors.Control
        Me.lblFromFtn.Cursor = System.Windows.Forms.Cursors.Default
        Me.lblFromFtn.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblFromFtn.ForeColor = System.Drawing.SystemColors.ControlText
        Me.lblFromFtn.Location = New System.Drawing.Point(16, 176)
        Me.lblFromFtn.Name = "lblFromFtn"
        Me.lblFromFtn.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.lblFromFtn.Size = New System.Drawing.Size(49, 32)
        Me.lblFromFtn.TabIndex = 2
        Me.lblFromFtn.Text = "From Fortran"
        '
        'lblToFtn
        '
        Me.lblToFtn.BackColor = System.Drawing.SystemColors.Control
        Me.lblToFtn.Cursor = System.Windows.Forms.Cursors.Default
        Me.lblToFtn.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.lblToFtn.ForeColor = System.Drawing.SystemColors.ControlText
        Me.lblToFtn.Location = New System.Drawing.Point(16, 96)
        Me.lblToFtn.Name = "lblToFtn"
        Me.lblToFtn.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.lblToFtn.Size = New System.Drawing.Size(41, 32)
        Me.lblToFtn.TabIndex = 1
        Me.lblToFtn.Text = "To Fortran"
        '
        'frmArray
        '
        Me.AutoScaleBaseSize = New System.Drawing.Size(5, 13)
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.ClientSize = New System.Drawing.Size(389, 285)
        Me.Controls.Add(Me.lstReturn)
        Me.Controls.Add(Me.lstOutput)
        Me.Controls.Add(Me.cmdPushMe)
        Me.Controls.Add(Me.lblFromFtn)
        Me.Controls.Add(Me.lblToFtn)
        Me.Cursor = System.Windows.Forms.Cursors.Default
        Me.Font = New System.Drawing.Font("Arial", 8.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Location = New System.Drawing.Point(239, 140)
        Me.Name = "frmArray"
        Me.RightToLeft = System.Windows.Forms.RightToLeft.No
        Me.StartPosition = System.Windows.Forms.FormStartPosition.Manual
        Me.Text = "Arrays to Fortran"
        Me.ResumeLayout(False)

    End Sub
#End Region
#Region "Upgrade Support "
    Private Shared m_vb6FormDefInstance As frmArray
    Private Shared m_InitializingDefInstance As Boolean
    Public Shared Property DefInstance() As frmArray
        Get
            If m_vb6FormDefInstance Is Nothing OrElse m_vb6FormDefInstance.IsDisposed Then
                m_InitializingDefInstance = True
                m_vb6FormDefInstance = New frmArray
                m_InitializingDefInstance = False
            End If
            DefInstance = m_vb6FormDefInstance
        End Get
        Set(ByVal Value As frmArray)
            m_vb6FormDefInstance = Value
        End Set
    End Property
#End Region

    ' Declare the Fortran routine.  The name of the routine is case-sensitive
    ' and the path to the DLL is explicit.  When run from Visual Studio, the DLL
    ' is looked for in the BIN subfolder of the VB.NET project
    '
    ' By default, VB.NET would simply pass the data to the DLL without any bounds
    ' information (and no chance of rewriting the strings).  So we use the MarshalAs
    ' attribute to tell it to pass a SafeArray.  If you were passing numeric types,
    ' you could just pass the data and access it as a normal array.
    '
    Private Declare Sub ForCall Lib "ArrayOfVBStr.dll" _
    (<MarshalAs(UnmanagedType.SafeArray)> ByRef array1(,) As String)

    Private myarray(2, 3) As String

    Private Sub cmdPushMe_Click(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles cmdPushMe.Click
        'Dim myarray(2, 3) As String
        cmdPushMe.Enabled = False


        ' Call the Fortran routine
        Call ForCall(myarray)

        ' Fill the second listbox with what Fortran stored into the array
        lstReturn.Items.Add((myarray(0, 0)))
        lstReturn.Items.Add((myarray(0, 1)))
        lstReturn.Items.Add((myarray(0, 2)))
        lstReturn.Items.Add((myarray(0, 3)))
        lstReturn.Items.Add((myarray(1, 0)))
        lstReturn.Items.Add((myarray(1, 1)))
        lstReturn.Items.Add((myarray(1, 2)))
        lstReturn.Items.Add((myarray(1, 3)))
        lstReturn.Items.Add((myarray(2, 0)))
        lstReturn.Items.Add((myarray(2, 1)))
        lstReturn.Items.Add((myarray(2, 2)))
        lstReturn.Items.Add((myarray(2, 3)))

    End Sub

    Private Sub lstOutput_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles lstOutput.SelectedIndexChanged
        Dim i As Short
        For i = 0 To lstOutput.Items.Count - 1
            If lstOutput.GetSelected(i) = True Then
                lstOutput.SetSelected(i, False)
            End If
        Next i
    End Sub
    Private Sub lstReturn_SelectedIndexChanged(ByVal eventSender As System.Object, ByVal eventArgs As System.EventArgs) Handles lstReturn.SelectedIndexChanged
        Dim i As Short
        For i = 0 To lstReturn.Items.Count - 1
            If lstReturn.GetSelected(i) = True Then
                lstReturn.SetSelected(i, False)
            End If
        Next i
    End Sub

    Private Sub frmArray_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        ' Fill the array with some values
        '
        myarray(0, 0) = "One"
        myarray(0, 1) = "Two"
        myarray(0, 2) = "Three"
        myarray(0, 3) = "Four"
        myarray(1, 0) = "Five"
        myarray(1, 1) = "Six"
        myarray(1, 2) = "Seven"
        myarray(1, 3) = "Eight"
        myarray(2, 0) = "Nine"
        myarray(2, 1) = "Ten"
        myarray(2, 2) = "Eleven"
        myarray(2, 3) = "Twelve"

        ' Fill in the first listbox showing what we're sending to Fortran
        lstOutput.Items.Add(myarray(0, 0))
        lstOutput.Items.Add(myarray(0, 1))
        lstOutput.Items.Add(myarray(0, 2))
        lstOutput.Items.Add(myarray(0, 3))
        lstOutput.Items.Add(myarray(1, 0))
        lstOutput.Items.Add(myarray(1, 1))
        lstOutput.Items.Add(myarray(1, 2))
        lstOutput.Items.Add(myarray(1, 3))
        lstOutput.Items.Add(myarray(2, 0))
        lstOutput.Items.Add(myarray(2, 1))
        lstOutput.Items.Add(myarray(2, 2))
        lstOutput.Items.Add(myarray(2, 3))

    End Sub
End Class