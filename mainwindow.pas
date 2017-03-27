unit MainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, StdCtrls, Spin, Buttons, ValEdit, Grids, LCLType, Menus, Clipbrd,
  Etcd, Rest, Types, Utils;

type

  { TMainForm }

  TMainForm = class(TForm)
    m_treePasteItem: TMenuItem;
    m_treeSeparator: TMenuItem;
    m_treeBranchItem: TMenuItem;
    m_valueCopyItem: TMenuItem;
    m_valuePopup: TPopupMenu;
    m_treeRemoveItem: TMenuItem;
    m_treeCopyItem: TMenuItem;
    m_treeImages: TImageList;
    m_treeAddButton: TBitBtn;
    m_connectButton: TBitBtn;
    m_disconnectButton: TBitBtn;
    m_serverEdit: TComboBox;
    m_serverLabel: TLabel;
    m_connectionGroup: TGroupBox;
    m_portLabel: TLabel;
    m_valueRemoveItem: TMenuItem;
    m_valueAddButton: TBitBtn;
    m_valueButtonPanel: TPanel;
    m_valueRefreshButton: TBitBtn;
    m_treeRemoveButton: TBitBtn;
    m_treeRefreshButton: TBitBtn;
    m_valueRemoveButton: TBitBtn;
    m_valueGroup: TGroupBox;
    m_tree: TTreeView;
    m_treeGroup: TGroupBox;
    m_statusBar: TStatusBar;
    m_splitter: TSplitter;
    m_portEdit: TSpinEdit;
    m_treeButtonPanel: TPanel;
    m_values: TValueListEditor;
    m_treePopup: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure m_treeBranchItemClick(Sender: TObject);
    procedure m_treeContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure m_treeCopyItemClick(Sender: TObject);
    procedure m_connectButtonClick(Sender: TObject);
    procedure m_disconnectButtonClick(Sender: TObject);
    procedure m_serverEditKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure m_serverEditSelect(Sender: TObject);
    procedure m_treeAddButtonClick(Sender: TObject);
    procedure m_treeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure m_treePopupPopup(Sender: TObject);
    procedure m_treeRemoveItemClick(Sender: TObject);
    procedure m_treeRefreshButtonClick(Sender: TObject);
    procedure m_treeRemoveButtonClick(Sender: TObject);
    procedure m_treeSelectionChanged(Sender: TObject);
    procedure m_valueAddButtonClick(Sender: TObject);
    procedure m_valueCopyItemClick(Sender: TObject);
    procedure m_valueRefreshButtonClick(Sender: TObject);
    procedure m_valueRemoveButtonClick(Sender: TObject);
    procedure m_valueRemoveItemClick(Sender: TObject);
    procedure m_valuesContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure m_valuesEditingDone(Sender: TObject);
    procedure m_valuesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure m_valuesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    RestClient: TEtcdRestClient;
    procedure AddTreeItems(TreeNode: TTreeNode; EtcdNode: TEtcdNode);
    procedure AddValueItems(EtcdNode: TEtcdNode);
  public
    procedure Disconnect;
    procedure Connect;
    procedure LoadValues;
    procedure LoadMRU;
    procedure SaveMRU;
  end;

var
  MainForm: TMainForm;

implementation

uses NameWindow;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.AddTreeItems(TreeNode: TTreeNode; EtcdNode: TEtcdNode);
var i : integer;
    SubNode: TEtcdNode;
    SubTreeNode: TTreeNode;
begin
  for i := 0 to EtcdNode.NodeCount - 1 do begin
    SubNode := EtcdNode.Nodes[i];
    SubTreeNode := m_tree.Items.AddChildObject(TreeNode, SubNode.ShortName, SubNode);
    SubTreeNode.StateIndex := 0;
    AddTreeItems(SubTreeNode, SubNode);
  end;
end;

procedure TMainForm.AddValueItems(EtcdNode: TEtcdNode);
var i : integer;
    value: TEtcdValue;
begin
  m_values.Clear;
  for i := 0 to EtcdNode.ValueCount - 1 do begin
    value := EtcdNode.Values[i];
    m_values.InsertRow(value.ShortKey, value.Value, true);
    m_values.Objects[0, i+1] := value;
  end;
  m_valueButtonPanel.Enabled := true;
  if EtcdNode.ValueCount > 0 then
    m_values.Options := m_values.Options + [goEditing]
  else
    m_values.Options := m_values.Options - [goEditing];
  m_values.Row := 1;
  m_values.Col := 1;
end;

procedure TMainForm.Disconnect;
begin
  m_tree.Items.Clear;
  m_values.Clear;
  m_values.RowCount := 1;
  m_values.Options := m_values.Options - [goEditing];
  m_treeButtonPanel.Enabled := false;
  m_valueButtonPanel.Enabled := false;
  m_connectButton.Enabled := true;
  m_disconnectButton.Enabled := false;
  if Assigned(RestClient) then begin
    FreeAndNil(RestClient);
  end;
end;

procedure TMainForm.Connect;
var EtcdNode: TEtcdNode;
    TreeNode: TTreeNode;
begin
  m_tree.Items.Clear;
  m_values.Clear;
  m_valueButtonPanel.Enabled := false;
  try
    RestClient := TEtcdRestClient.Create(m_serverEdit.Text, m_portEdit.Value);
    EtcdNode := LoadEtcdFolders(RestClient.LoadFolders);
    m_treeButtonPanel.Enabled := true;
    m_connectButton.Enabled := false;
    m_disconnectButton.Enabled := true;
    if EtcdNode <> Nil then begin
      TreeNode := m_tree.Items.AddObject(Nil, EtcdNode.ShortName, EtcdNode);
      TreeNode.StateIndex := 0;
      AddTreeItems(TreeNode, EtcdNode);
    end;
    if not m_serverEdit.Items.IndexOf(m_serverEdit.Text) = 0 then begin
      m_serverEdit.Items.Add(m_serverEdit.Text);
      SaveMRU;
    end;
  except
    on E: Exception do begin
      MessageDlg('Error', 'Error occured: ' + E.Message, mtError, [mbOK], 0);
      Disconnect;
    end;
  end;
end;

procedure TMainForm.m_connectButtonClick(Sender: TObject);
begin
  Connect;
end;

procedure TMainForm.m_disconnectButtonClick(Sender: TObject);
begin
  Disconnect;
end;

procedure TMainForm.m_serverEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 13 then
    Connect;
end;

procedure TMainForm.m_serverEditSelect(Sender: TObject);
begin
  Connect;
end;

procedure TMainForm.m_treeAddButtonClick(Sender: TObject);
var ActiveNode: TEtcdNode;
    NodeName: string;
begin
  if Assigned(m_tree.Selected) and Assigned(RestClient) then begin
    ActiveNode := TEtcdNode(m_tree.Selected.Data);
    with TNameWindow.Create(self) do try
      ShowModal;
      NodeName := GetText;
    finally
      Free;
    end;
    if NodeName <> '' then try
      RestClient.CreateFolder(ActiveNode.Name, NodeName);
      Connect;
    except
      on E: Exception do
      MessageDlg('Error', 'Error occured: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.m_treeKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) or (ssAlt in Shift) then
    exit;
  case Key of
    VK_DELETE: begin
      m_treeRemoveButtonClick(Nil);
      Key := 0;
    end;
    VK_INSERT: begin
      m_treeAddButtonClick(Nil);
      Key := 0;
    end;
  end;
end;

procedure TMainForm.m_treePopupPopup(Sender: TObject);
begin
  m_treePasteItem.Enabled := IsJson(Clipboard.AsText);
end;

procedure TMainForm.m_treeRemoveItemClick(Sender: TObject);
begin
  m_treeRemoveButtonClick(Sender);
end;

procedure TMainForm.m_treeRefreshButtonClick(Sender: TObject);
begin
  Connect;
end;

procedure TMainForm.m_treeRemoveButtonClick(Sender: TObject);
var ActiveNode: TEtcdNode;
begin
  if Assigned(m_tree.Selected) and Assigned(RestClient) then begin
    ActiveNode := TEtcdNode(m_tree.Selected.Data);
    if MessageDlg('Confirm', 'Really delete ' + ActiveNode.Name + '?',
       TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0) = mrYes then try
      RestClient.DeleteFolder(ActiveNode.Name);
      Connect;
    except
      on E: Exception do
      MessageDlg('Error', 'Error occured: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.m_treeSelectionChanged(Sender: TObject);
begin
  LoadValues;
end;

procedure TMainForm.m_valueAddButtonClick(Sender: TObject);
var ActiveNode: TEtcdNode;
    KeyName: string;
begin
  if Assigned(m_tree.Selected) and Assigned(RestClient) then begin
    ActiveNode := TEtcdNode(m_tree.Selected.Data);
    with TNameWindow.Create(self) do try
      ShowModal;
      KeyName := GetText;
    finally
      Free;
    end;
    if KeyName <> '' then try
      RestClient.CreateKey(ActiveNode.Name, KeyName);
      LoadValues;
    except
      on E: Exception do
      MessageDlg('Error', 'Error occured: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.m_valueCopyItemClick(Sender: TObject);
var ActiveValue: TEtcdValue;
begin
  if (m_values.Row > 0) and (m_values.Keys[m_values.Row] <> '') then begin
    ActiveValue := TEtcdValue(m_values.Objects[0, m_values.Row]);
    Clipboard.AsText := ActiveValue.Key;
  end;
end;

procedure TMainForm.m_valueRefreshButtonClick(Sender: TObject);
begin
  LoadValues;
end;

procedure TMainForm.m_valueRemoveButtonClick(Sender: TObject);
var ActiveNode: TEtcdNode;
    ActiveValue: TEtcdValue;
begin
  if Assigned(m_tree.Selected) and Assigned(RestClient) and (m_values.Row > 0)
     and (m_values.Keys[m_values.Row] <> '') then begin
    ActiveNode := TEtcdNode(m_tree.Selected.Data);
    ActiveValue := TEtcdValue(m_values.Objects[0, m_values.Row]);
    if MessageDlg('Confirm', 'Really delete key ' + ActiveValue.ShortKey +
       ' in ' + ActiveNode.Name + '?',
       TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0) = mrYes then try
      RestClient.DeleteKey(ActiveValue.Key);
      LoadValues;
    except
      on E: Exception do
      MessageDlg('Error', 'Error occured: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.m_valueRemoveItemClick(Sender: TObject);
begin
  m_valueRemoveButtonClick(Sender);
end;

procedure TMainForm.m_valuesContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
var GridCoord: TGridCoord;
begin
  GridCoord := m_values.MouseCoord(MousePos.X, MousePos.Y);
  if not Assigned(RestClient) or not (m_values.Row > 0) or not(GridCoord.Y > 0)
    or not (m_values.Keys[m_values.Row] <> '') then Handled := true;
end;

procedure TMainForm.m_valuesEditingDone(Sender: TObject);
var ActiveValue: TEtcdValue;
    NewValue: string;
begin
  if Assigned(RestClient) and (m_values.Row > 0)
     and (m_values.Keys[m_values.Row] <> '') then begin
    ActiveValue := TEtcdValue(m_values.Objects[0, m_values.Row]);
    NewValue := m_values.Values[ActiveValue.ShortKey];
    try
      RestClient.SetKeyValue(ActiveValue.Key, NewValue);
    except
      on E: Exception do
      MessageDlg('Error', 'Error occured: ' + E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMainForm.m_valuesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ssShift in Shift) or (ssCtrl in Shift) or (ssAlt in Shift) then
    exit;
  case Key of
    VK_DELETE: begin
      m_valueRemoveButtonClick(Nil);
      Key := 0;
    end;
    VK_INSERT: begin
      m_valueAddButtonClick(Nil);
      Key := 0;
    end;
  end;
end;

procedure TMainForm.m_valuesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var GridCoord: TGridCoord;
begin
  if Button = mbRight then begin
    GridCoord := m_values.MouseCoord(X, Y);
    m_values.Row := GridCoord.Y;
    m_values.Col := GridCoord.X;
  end;
end;

procedure TMainForm.LoadValues;
var ActiveNode: TEtcdNode;
begin
  if Assigned(m_tree.Selected) and Assigned(RestClient) then begin
    ActiveNode := TEtcdNode(m_tree.Selected.Data);
    try
      LoadEtcdValues(ActiveNode, RestClient.LoadValuesInFolder(ActiveNode.Name));
      AddValueItems(ActiveNode);
    except
      on E: Exception do begin
        m_values.Clear;
        MessageDlg('Error', 'Error occured: ' + E.Message, mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Disconnect;
  LoadMRU;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  m_values.ColWidths[0] := Round(m_values.Width * 0.3);
  m_values.ColWidths[1] := Round(m_values.Width * 0.6);
end;

procedure TMainForm.m_treeBranchItemClick(Sender: TObject);
var ActiveNode: TEtcdNode;
    str: string;
begin
  if Assigned(m_tree.Selected) then begin
    ActiveNode := TEtcdNode(m_tree.Selected.Data);
    str := ActiveNode.AsJson;
    ShowMessage(str);
  end;
end;

procedure TMainForm.m_treeContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin
  if not Assigned(m_tree.Selected) then Handled := true;
end;

procedure TMainForm.m_treeCopyItemClick(Sender: TObject);
var ActiveNode: TEtcdNode;
begin
  if Assigned(m_tree.Selected) then begin
    ActiveNode := TEtcdNode(m_tree.Selected.Data);
    Clipboard.AsText := ActiveNode.Name;
  end;
end;

procedure TMainForm.LoadMRU;
var f: TextFile;
    s: string;
begin
  if not FileExists(GetUserDir + '.etcd_browser') then
    Exit;
  AssignFile(f, GetUserDir + '.etcd_browser');
  try
    Reset(f);
    while not Eof(f) do begin
      ReadLn(f, s);
      m_serverEdit.Items.Add(s);
    end;
    CloseFile(f);
  finally
  end;
end;

procedure TMainForm.SaveMRU;
var f : TextFile;
    i : integer;
begin
  AssignFile(f, GetUserDir + '.etcd_browser');
  Rewrite(f);
  for i := 1 to m_serverEdit.Items.Count do begin
    WriteLn(f, m_serverEdit.Items.Strings[i-1]);
  end;
  CloseFile(f);
end;

end.

