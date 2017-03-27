unit Rest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson;

const
  ETCD_PROTOCOL = 'http';
  ETCD_PREFIX = '/v2/keys';
  ETCD_TIMEOUT = 1000;

type TEtcdRestClient = class(TObject)
  private
    BaseUrl: string;
    function SafeName(Name: string): string;
  public
    constructor Create(Server: string; Port: integer);
    destructor Destroy; override;
    function LoadFolders(Path: string = ''): TJSONData;
    function LoadValuesInFolder(Node: string): TJSONData;
    procedure CreateFolder(Path: string; Name: string);
    procedure DeleteFolder(Path: string);
    procedure CreateKey(Path: string; Name: string);
    procedure DeleteKey(Path: string);
    procedure SetKeyValue(Key: string; Value: string);
end;

implementation

constructor TEtcdRestClient.Create(Server: string; Port: integer);
begin
  BaseUrl := ETCD_PROTOCOL + '://' + Server + ':' + IntToStr(Port) + ETCD_PREFIX;
end;

destructor TEtcdRestClient.Destroy;
begin
  inherited;
end;

function TEtcdRestClient.SafeName(Name: string): string;
begin
  Name := StringReplace(Name, '/', '', [rfReplaceAll]);
  Name := StringReplace(Name, '"', '', [rfReplaceAll]);
  Name := StringReplace(Name, '''', '', [rfReplaceAll]);
  Result := Name;
end;

function TEtcdRestClient.LoadFolders(Path: string = ''): TJSONData;
var StrJson: string;
begin
  with TFPHTTPClient.Create(Nil) do try
    IOTimeout := ETCD_TIMEOUT;
    StrJson := Get(BaseUrl + Path + '?recursive=true');
    LoadFolders := GetJSON(StrJson);
  finally
    Free;
  end;
end;

function TEtcdRestClient.LoadValuesInFolder(Node: string): TJSONData;
var StrJson: string;
begin
  with TFPHTTPClient.Create(Nil) do try
    IOTimeout := ETCD_TIMEOUT;
    StrJson := Get(BaseUrl + Node);
    LoadValuesInFolder := GetJSON(StrJson);
  finally
    Free;
  end;
end;

procedure TEtcdRestClient.CreateFolder(Path: string; Name: string);
var FullPath, Response: string;
    data: TStringStream;
begin
  FullPath := '/' + SafeName(Name);
  if Path <> '/' then
    FullPath := Path + FullPath;
  data := TStringStream.Create('');
  data.WriteString('dir=true');
  data.Seek(0, 0);
  with TFPHTTPClient.Create(Nil) do try
    IOTimeout := ETCD_TIMEOUT;
    AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    RequestBody := data;
    Response := Put(BaseUrl + FullPath);
    if ResponseStatusCode >= 300 then
      raise Exception.Create(IntToStr(ResponseStatusCode) + ' ' + Response);
  finally
    Free;
  end;
end;

procedure TEtcdRestClient.DeleteFolder(Path: string);
var FullPath, Response: string;
begin
  FullPath := Path + '?dir=true&recursive=true';
  with TFPHTTPClient.Create(Nil) do try
    IOTimeout := ETCD_TIMEOUT;
    Response := Delete(BaseUrl + FullPath);
    if ResponseStatusCode >= 300 then
      raise Exception.Create(IntToStr(ResponseStatusCode) + ' ' + Response);
  finally
    Free;
  end;
end;

procedure TEtcdRestClient.CreateKey(Path: string; Name: string);
var FullPath, Response: string;
begin
  FullPath := '/' + SafeName(Name);
  if Path <> '/' then
    FullPath := Path + FullPath;
  with TFPHTTPClient.Create(Nil) do try
    IOTimeout := ETCD_TIMEOUT;
    Response := Put(BaseUrl + FullPath);
    if ResponseStatusCode >= 300 then
      raise Exception.Create(IntToStr(ResponseStatusCode) + ' ' + Response);
  finally
    Free;
  end;
end;

procedure TEtcdRestClient.DeleteKey(Path: string);
var Response: string;
begin
  with TFPHTTPClient.Create(Nil) do try
    IOTimeout := ETCD_TIMEOUT;
    Response := Delete(BaseUrl + Path);
    if ResponseStatusCode >= 300 then
      raise Exception.Create(IntToStr(ResponseStatusCode) + ' ' + Response);
  finally
    Free;
  end;
end;

procedure TEtcdRestClient.SetKeyValue(Key: string; Value: string);
var Response: string;
    data: TStringStream;
begin
  data := TStringStream.Create('');
  data.WriteString('value=' + EncodeURLElement(Value) + '');
  data.Seek(0, 0);
  with TFPHTTPClient.Create(Nil) do try
    IOTimeout := ETCD_TIMEOUT;
    RequestBody := data;
    AddHeader('Content-Type', 'application/x-www-form-urlencoded');
    Response := Put(BaseUrl + Key);
    if ResponseStatusCode >= 300 then
      raise Exception.Create(IntToStr(ResponseStatusCode) + ' ' + Response);
  finally
    Free;
  end;
end;

end.

