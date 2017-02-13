unit NameWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TNameWindow }

  TNameWindow = class(TForm)
    m_nameEdit: TEdit;
    m_nameLabel: TLabel;
    m_okButton: TButton;
    m_cancelButton: TButton;
    m_groupBox: TGroupBox;
    m_buttonPanel: TPanel;
    procedure m_cancelButtonClick(Sender: TObject);
    procedure m_okButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    function GetText: string;
  end;


implementation

{$R *.lfm}

{ TNameWindow }

procedure TNameWindow.m_okButtonClick(Sender: TObject);
begin
  if GetText = '' then begin
    MessageDlg('Warning', 'Name is empty - enter something!', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    FocusControl(m_nameEdit);
    Exit;
  end;
  Close;
end;

procedure TNameWindow.m_cancelButtonClick(Sender: TObject);
begin
  m_nameEdit.Text := '';
  Close;
end;

function TNameWindow.GetText: string;
begin
  GetText := Trim(m_nameEdit.Text);
end;

end.

