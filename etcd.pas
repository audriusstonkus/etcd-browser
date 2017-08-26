unit Etcd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser;

type TEtcdValue = class(TObject)
  private
    m_key, m_value: string;
    function GetKey: string;
  public
    property Key: string read m_key write m_key;
    property Value: string read m_value write m_value;
    property ShortKey: string read GetKey;
    function AsJson: string;
end;

type TEtcdNode = class(TObject)
  private
    m_name : string;
    m_nodes: array of TEtcdNode;
    m_values: array of TEtcdValue;
    m_data: TJsonObject;
    function GetNode(index: integer) : TEtcdNode;
    function GetNodeCount: integer;
    function GetValue(index: integer) : TEtcdValue;
    function GetValueCount: integer;
    function GetName: string;
  public
    property Name: string read m_name write m_name;
    property Data: TJsonObject read m_data write m_data;
    property ShortName: string read GetName;
    property Nodes[Index: Integer]: TEtcdNode read GetNode;
    property NodeCount: integer read GetNodeCount;
    property Values[Index: Integer]: TEtcdValue read GetValue;
    property ValueCount: integer read GetValueCount;
    constructor Create;
    procedure AddNode(node: TEtcdNode);
    procedure AddValue(value: TEtcdValue);
    procedure ClearValues;
    function AsJson: string;
    procedure Rename(NewName: String);
end;

function LoadEtcdFolders(Data: TJSONData): TEtcdNode;
procedure LoadEtcdValues(Node: TEtcdNode; Data: TJSONData);
function ParseEtcdSubTree(ParentNode: TEtcdNode; NodeName: string;
  StrJson: string) : TEtcdNode;
function ParseEtcdValue(StrJson: string): TEtcdValue;

implementation
uses Dialogs;

constructor TEtcdNode.Create;
begin
  SetLength(m_nodes, 0);
  SetLength(m_values, 0);
end;

procedure TEtcdNode.AddNode(node: TEtcdNode);
var len, i, j: integer;
    found: boolean;
    new_nodes: array of TEtcdNode;
begin
  len := Length(m_nodes);
  SetLength(new_nodes, len + 1);
  i := 0;
  j := 0;
  found := false;
  while i < len do begin
    if not found and (CompareText(m_nodes[i].Name, node.Name) > 0) then begin
      found := true;
      new_nodes[j] := node;
    end
    else begin
      new_nodes[j] := m_nodes[i];
      i := i + 1;
    end;
    j := j + 1;
  end;
  if not found then
    new_nodes[len] := node;
  SetLength(m_nodes, 0);
  m_nodes := new_nodes;
end;

procedure TEtcdNode.AddValue(value: TEtcdValue);
var len, i, j: integer;
    found: boolean;
    new_values: array of TEtcdValue;
begin
  len := Length(m_values);
  SetLength(new_values, len + 1);
  i := 0;
  j := 0;
  found := false;
  while i < len do begin
    if not found and (CompareText(m_values[i].Key, value.Key) > 0) then begin
      found := true;
      new_values[j] := value;
    end
    else begin
      new_values[j] := values[i];
      i := i + 1;
    end;
    j := j + 1;
  end;
  if not found then
    new_values[len] := value;
  SetLength(m_values, 0);
  m_values := new_values;
end;

function TEtcdNode.GetNode(index: integer) : TEtcdNode;
begin
  GetNode := m_nodes[index];
end;

function TEtcdNode.GetNodeCount: integer;
begin
  GetNodeCount := Length(m_nodes);
end;

function TEtcdNode.GetValue(index: integer) : TEtcdValue;
begin
  GetValue := m_values[index];
end;

function TEtcdNode.GetValueCount: integer;
begin
  GetValueCount := Length(m_values);
end;

function TEtcdNode.GetName: string;
var i: integer;
begin
  i := LastDelimiter('/', m_name);
  GetName := Copy(m_name, i, Length(m_name) - i + 1);
end;

procedure TEtcdNode.ClearValues;
begin
  SetLength(m_values, 0);
end;

function TEtcdNode.AsJson: string;
var str: string;
begin
  str := Data.AsJson;
  str := str.Replace('\/', '/');
  Result := str;
end;

procedure TEtcdNode.Rename(NewName: String);
var i: integer;
    SubNode: TEtcdNode;
    Value: TEtcdValue;
begin
  for i := 0 to NodeCount - 1 do begin
    SubNode := Nodes[i];
    SubNode.Rename(NewName + SubNode.GetName);
  end;
  for i := 0 to ValueCount -1 do begin
    Value := Values[i];
    Value.m_key := NewName + '/' + Value.GetKey;
  end;
  m_name := NewName;
end;

function TEtcdValue.GetKey: string;
var i: integer;
begin
  i := LastDelimiter('/', m_key);
  GetKey := Copy(m_key, i + 1, Length(m_key) - i);
end;

function TEtcdValue.AsJson: string;
begin
  AsJson := Format('{"key": "%s", "value": "%s"}', [
         StringReplace(GetKey, '"', '\"', [rfReplaceAll]),
         StringReplace(value, '"', '\"', [rfReplaceAll])
         ]);
end;

function ParseValue(JsonObject: TJSONObject): TEtcdValue;
var Value: TEtcdValue;
begin
  Value := TEtcdValue.Create;
  Value.Key := JsonObject.Get('key', '');
  Value.Value := JsonObject.Get('value', '');
  ParseValue := Value;
end;

function ParseNode(JsonObject: TJSONObject): TEtcdNode;
var Node, SubNode: TEtcdNode;
    IsDir: Boolean;
    Name: string;
    SubNodes, EmptyArray: TJSONArray;
    i : integer;
begin
  EmptyArray := Nil;
  IsDir := JsonObject.Get('dir', False);
  Name := JsonObject.Get('key', '/');
  SubNodes := JsonObject.Get('nodes', EmptyArray);
  if not IsDir then begin
    ParseNode := Nil;
  end
  else begin
    Node := TEtcdNode.Create;
    Node.Name := Name;
    Node.Data := JsonObject;
    if Subnodes <> Nil then begin
      for i := 0 to SubNodes.Count - 1 do begin
        SubNode := ParseNode(SubNodes.Objects[i]);
        if SubNode <> Nil then begin
          Node.AddNode(SubNode);
        end
        else begin
          Node.AddValue(ParseValue(SubNodes.Objects[i]));
        end;
      end;
    end;
    ParseNode := Node;
  end;
end;

function LoadEtcdFolders(Data: TJSONData): TEtcdNode;
var JsonObject, RootObject, EmptyObject : TJSONObject;
begin
  EmptyObject := Nil;
  JsonObject := TJSONObject(Data);
  RootObject := JsonObject.Get('node', EmptyObject);
  LoadEtcdFolders := ParseNode(RootObject);
end;

procedure LoadEtcdValues(Node: TEtcdNode; Data: TJSONData);
var JsonObject, RootObject, EmptyObject, SubNode : TJSONObject;
    SubNodes, EmptyArray: TJSONArray;
    i : integer;
begin
  EmptyObject := Nil;
  EmptyArray := Nil;
  Node.ClearValues;
  JsonObject := TJSONObject(Data);
  RootObject := JsonObject.Get('node', EmptyObject);
  SubNodes := RootObject.Get('nodes', EmptyArray);
  if SubNodes <> Nil then begin
    for i := 0 to SubNodes.Count - 1 do begin
      SubNode := SubNodes.Objects[i];
      if not SubNode.Get('dir', False) then begin
        Node.AddValue(ParseValue(SubNode));
      end;
    end;
  end;
end;

function ParseEtcdSubTree(ParentNode: TEtcdNode; NodeName: string;
  StrJson: string): TEtcdNode;
var Data: TJsonData;
    SubNode: TEtcdNode;
    NewBase: string;
begin
  Data := GetJson('{node: ' + StrJson + '}');
  SubNode := LoadEtcdFolders(data);
  NewBase := ParentNode.m_name + '/' + NodeName;
  SubNode.Rename(NewBase);
  ParseEtcdSubTree := SubNode;
end;

function ParseEtcdValue(StrJson: string): TEtcdValue;
var Data: TJsonData;
begin
  Data := GetJson(StrJson);
  ParseEtcdValue := ParseValue(TJSONObject(Data));
end;

end.

