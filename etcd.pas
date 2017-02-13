unit Etcd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Utils, fphttpclient, fpjson, jsonparser;

type TEtcdValue = class(TObject)
  private
    m_key, m_value: string;
    function GetKey: string;
  public
    property Key: string read m_key write m_key;
    property Value: string read m_value write m_value;
    property ShortKey: string read GetKey;
end;

type TEtcdNode = class(TObject)
  private
    m_name : string;
    m_nodes: array of TEtcdNode;
    m_values: array of TEtcdValue;
    function GetNode(index: integer) : TEtcdNode;
    function GetNodeCount: integer;
    function GetValue(index: integer) : TEtcdValue;
    function GetValueCount: integer;
    function GetName: string;
  public
    property Name: string read m_name write m_name;
    property ShortName: string read GetName;
    property Nodes[Index: Integer]: TEtcdNode read GetNode;
    property NodeCount: integer read GetNodeCount;
    property Values[Index: Integer]: TEtcdValue read GetValue;
    property ValueCount: integer read GetValueCount;
    constructor Create;
    procedure AddNode(node: TEtcdNode);
    procedure AddValue(value: TEtcdValue);
    procedure ClearValues;
end;

function LoadEtcdFolders(Data: TJSONData): TEtcdNode;
procedure LoadEtcdValues(Node: TEtcdNode; Data: TJSONData);

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
var i: integer;
begin
  SetLength(m_values, 0);
end;

function TEtcdValue.GetKey: string;
var i: integer;
begin
  i := LastDelimiter('/', m_key);
  GetKey := Copy(m_key, i + 1, Length(m_key) - i);
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
    if Subnodes <> Nil then begin
      for i := 0 to SubNodes.Count - 1 do begin
        SubNode := ParseNode(SubNodes.Objects[i]);
        if SubNode <> Nil then
          Node.AddNode(SubNode);
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
    Value: TEtcdValue;
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
        Value := TEtcdValue.Create;
        Value.Key := SubNode.Get('key', '');
        Value.Value := SubNode.Get('value', '');
        Node.AddValue(Value);
      end;
    end;
  end;
end;

end.
