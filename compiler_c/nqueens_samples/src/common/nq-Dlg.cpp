//==============================================================
//
// SAMPLE SOURCE CODE - SUBJECT TO THE TERMS OF SAMPLE CODE LICENSE AGREEMENT,
// http://software.intel.com/en-us/articles/intel-sample-source-code-license-agreement/
//
// Copyright Intel Corporation
//
// THIS FILE IS PROVIDED "AS IS" WITH NO WARRANTIES, EXPRESS OR IMPLIED, INCLUDING BUT
// NOT LIMITED TO ANY IMPLIED WARRANTY OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE, NON-INFRINGEMENT OF INTELLECTUAL PROPERTY RIGHTS.
//
// =============================================================
// nq-Dlg.cpp : implementation file

#include "stdafx.h"
#include "nq_main.h"
#include "nq-Dlg.h"
#include "nq-serial.h"
#include "nq.h"

#ifdef _DEBUG
#define new DEBUG_NEW
#endif

int g_nSolutions=0;
int g_nsize; 

// CAboutDlg dialog used for App About

class CAboutDlg : public CDialog
{
public:
	CAboutDlg();

// Dialog Data
	enum { IDD = IDD_ABOUTBOX };

	protected:
	virtual void DoDataExchange(CDataExchange* pDX);    // DDX/DDV support

// Implementation
protected:
	DECLARE_MESSAGE_MAP()
};

CAboutDlg::CAboutDlg() : CDialog(CAboutDlg::IDD)
{
}

void CAboutDlg::DoDataExchange(CDataExchange* pDX)
{
    CDialog::DoDataExchange(pDX);
}

BEGIN_MESSAGE_MAP(CAboutDlg, CDialog)
END_MESSAGE_MAP()


// CnqMainDlg dialog


CnqMainDlg::CnqMainDlg(CWnd* pParent /*=NULL*/)
    : CDialog(CnqMainDlg::IDD, pParent)
{
    m_hIcon = AfxGetApp()->LoadIcon(IDR_MAINFRAME);
}

void CnqMainDlg::DoDataExchange(CDataExchange* pDX)
{
    CDialog::DoDataExchange(pDX);
    DDX_Control(pDX, IDC_EDIT_OUTMSG, _ctrlEditOutMsg);
	DDX_Control(pDX, IDC_EDIT_EXP_OUTPUT, _ctrlEditOutMsgSerial);
}

BEGIN_MESSAGE_MAP(CnqMainDlg, CDialog)
	ON_WM_SYSCOMMAND()
	ON_WM_PAINT()
	ON_WM_QUERYDRAGICON()
	//}}AFX_MSG_MAP
	ON_BN_CLICKED(IDOK, &CnqMainDlg::OnBnClickedOk)
	ON_BN_CLICKED(IDC_BUT_SERIAL, &CnqMainDlg::OnBnClickedButSerial)
	ON_BN_CLICKED(IDC_BUTTON1, &CnqMainDlg::OnBnClickedButton1)
	ON_BN_CLICKED(IDC_BUTCLEAR, &CnqMainDlg::OnBnClickedButclear)
END_MESSAGE_MAP()


// CnqMainDlg message handlers

BOOL CnqMainDlg::OnInitDialog()
{
	CDialog::OnInitDialog();

	// Add "About..." menu item to system menu.

	// IDM_ABOUTBOX must be in the system command range.
	ASSERT((IDM_ABOUTBOX & 0xFFF0) == IDM_ABOUTBOX);
	ASSERT(IDM_ABOUTBOX < 0xF000);

	CMenu* pSysMenu = GetSystemMenu(FALSE);
	if (pSysMenu != NULL)
	{
		CString strAboutMenu;
		strAboutMenu.LoadString(IDS_ABOUTBOX);
		if (!strAboutMenu.IsEmpty())
		{
			pSysMenu->AppendMenu(MF_SEPARATOR);
			pSysMenu->AppendMenu(MF_STRING, IDM_ABOUTBOX, strAboutMenu);
		}
	}

	// Set the icon for this dialog.  The framework does this automatically
	//  when the application's main window is not a dialog
	SetIcon(m_hIcon, TRUE);			// Set big icon
	SetIcon(m_hIcon, FALSE);		// Set small icon

	// Add extra initialization here
	{
		// disable "serial button" if this project is serial.
#ifdef __NQ_SERIAL
		CnqMainDlg::GetDlgItem(IDC_BUT_SERIAL)->EnableWindow(FALSE);
#endif
		CString strCpuInfo;

		// print system info in edit control: IDC_EDIT_SYSINFO
		SYSTEM_INFO sysInfo;
		GetSystemInfo(&sysInfo);
		
		strCpuInfo = "Processor Type - "; 
		switch (sysInfo.wProcessorArchitecture) {
			case PROCESSOR_ARCHITECTURE_AMD64: strCpuInfo += "x64\r\n"; break;
			case PROCESSOR_ARCHITECTURE_INTEL: strCpuInfo += "x86\r\n"; break; 
			case PROCESSOR_ARCHITECTURE_IA64:  strCpuInfo += "IA64\r\n"; break;
			default: 
				 strCpuInfo += "unknown\r\n"; break;
		}

		strCpuInfo.AppendFormat(L"Processor# - %d", 
			sysInfo.dwNumberOfProcessors); 
		
		this->SetDlgItemText(IDC_EDIT_SYSINFO, strCpuInfo);
	}

	return TRUE;  // return TRUE  unless you set the focus to a control
}

void CnqMainDlg::OnSysCommand(UINT nID, LPARAM lParam)
{
	if ((nID & 0xFFF0) == IDM_ABOUTBOX)
	{
		CAboutDlg dlgAbout;
		dlgAbout.DoModal();
	}
	else
	{
		CDialog::OnSysCommand(nID, lParam);
	}
}

// If you add a minimize button to your dialog, you will need the code below
//  to draw the icon.  For MFC applications using the document/view model,
//  this is automatically done for you by the framework.

void CnqMainDlg::OnPaint()
{
	if (IsIconic())
	{
		CPaintDC dc(this); // device context for painting

		SendMessage(WM_ICONERASEBKGND, reinterpret_cast<WPARAM>(dc.GetSafeHdc()), 0);

		// Center icon in client rectangle
		int cxIcon = GetSystemMetrics(SM_CXICON);
		int cyIcon = GetSystemMetrics(SM_CYICON);
		CRect rect;
		GetClientRect(&rect);
		int x = (rect.Width() - cxIcon + 1) / 2;
		int y = (rect.Height() - cyIcon + 1) / 2;

		// Draw the icon
		dc.DrawIcon(x, y, m_hIcon);
	}
	else
	{
		CDialog::OnPaint();
	}
}

// The system calls this function to obtain the cursor to display while the user drags
//  the minimized window.
HCURSOR CnqMainDlg::OnQueryDragIcon()
{
	return static_cast<HCURSOR>(m_hIcon);
}


void CnqMainDlg::OnBnClickedOk()
{
	// calculate and display output, do not call OnOK();
	CString strRet; 
	BOOL bSucceeded;

	int nSize = 0;
	nSize = CnqMainDlg::GetDlgItemInt(IDC_EDIT_SIZE, &bSucceeded, 1); 

	if (nSize <= 0) 
		strRet = "Bad data; \nEnter a number > 0\r\n\r\n";
	else 
	{
		// set cursor to hour glass
		CWinApp* pApp = AfxGetApp( ); 
		pApp->BeginWaitCursor();

#ifdef __NQ_SERIAL
		strRet = nqSerialGetSolutions(nSize);
#else
		strRet = nqGetSolutions(nSize);
#endif; 
		// reset hour-glass cursor back to original
		pApp->EndWaitCursor();
	}

	// append the msg to edit control: outmsg
	_ctrlEditOutMsg.ReplaceSel(strRet);
	_ctrlEditOutMsg.SetRedraw();
}

void CnqMainDlg::OnBnClickedButSerial()
{
	int nSize = 0;
	CString strRet; 
	BOOL bSucceeded;

	nSize = CnqMainDlg::GetDlgItemInt(IDC_EDIT_SIZE, &bSucceeded, 1); 

	if (nSize <= 0) 
		strRet = "Bad data; \nEnter a number > 0\r\n\r\n";
	else 
	{
		// set cursor to hour glass
		CWinApp* pApp = AfxGetApp( ); 
		pApp->BeginWaitCursor();

		// get solutions from serial
		strRet = nqSerialGetSolutions(nSize);

		// reset hour-glass cursor back to original
		pApp->EndWaitCursor();
	}

	// append the msg to edit control: outmsg
	_ctrlEditOutMsgSerial.ReplaceSel(strRet);
	_ctrlEditOutMsgSerial.SetRedraw();
}

void CnqMainDlg::OnBnClickedButton1()
{
	// append the msg to edit control: outmsg
	CString strRet("Board size is a number, the size of the Chess board. \r\nIt's recommended to be less than 12.\r\n\r\n"); 
	_ctrlEditOutMsg.ReplaceSel(strRet);
	_ctrlEditOutMsg.SetRedraw();
}

void CnqMainDlg::OnBnClickedButclear()
{
	// Clear output text in both output boxes. 
	_ctrlEditOutMsg.SetWindowText(NULL);

	_ctrlEditOutMsgSerial.SetWindowText(NULL);
}
