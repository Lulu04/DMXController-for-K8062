unit u_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  ExtCtrls, ComCtrls, u_k8062, frame_cursor;

type

  { TForm1 }

  TForm1 = class(TForm)
    CBDevice: TComboBox;
    CBBank: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    SpeedButton1: TSpeedButton;
    procedure CBBankSelect(Sender: TObject);
    procedure CBDeviceSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FK8062Manager: TK8062Manager;
    function BankAdress: word;
    procedure UpdateDeviceComboBox;
    function SelectedDevice: TVelleman_K8062D_Device;
    procedure UpdateCursorsBounds;
    procedure UpdateCursorAdress;
    procedure UpdateTargetDevice;
    procedure UpdateCursorValue;
  private
    FCursors: array[0..15] of TFrameCursor;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var i: integer;
begin
  FK8062Manager:=TK8062Manager.Create;
  FK8062Manager.LookForDevice;

  for i:=0 to High(FCursors) do begin
    FCursors[i] := TFrameCursor.Create(Self);
    FCursors[i].Name:='FrameCursor'+(i+1).ToString;
    FCursors[i].Parent := Panel1;
  end;
end;

procedure TForm1.CBDeviceSelect(Sender: TObject);
begin
  if SelectedDevice<>NIL then begin
    SelectedDevice.StartDevice;
    SelectedDevice.ChannelCount:=512;
  end;
  UpdateTargetDevice;
end;

procedure TForm1.CBBankSelect(Sender: TObject);
begin
  UpdateCursorAdress;
  UpdateCursorValue;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FK8062Manager.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  UpdateCursorsBounds;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  label2.Caption := FK8062Manager.DeviceCount.ToString;
  UpdateDeviceComboBox;
  UpdateCursorsBounds;
  UpdateCursorAdress;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  FK8062Manager.LookForDevice;
  if FK8062Manager.DeviceCount>0
    then ShowMessage('Found '+FK8062Manager.DeviceCount.ToString+' K8062 device')
    else ShowMessage('No device found...');
  UpdateDeviceComboBox;
end;

function TForm1.BankAdress: word;
begin
  Result := CBBank.ItemIndex*16+1;
end;

procedure TForm1.UpdateDeviceComboBox;
var i: integer;
begin
  CBDevice.Clear;
  for i:=1 to FK8062Manager.DeviceCount do
    CBDevice.Items.Add(i.ToString+' - USB K8062');
end;

function TForm1.SelectedDevice: TVelleman_K8062D_Device;
begin
  Result := FK8062Manager.Devices[CBDevice.ItemIndex];
end;

procedure TForm1.UpdateCursorsBounds;
var i: integer;
  xx, cursorWidth, Hmargin: single;
begin
  Hmargin:=Panel1.ClientWidth/(Length(FCursors)-1)*0.2;
  cursorWidth:=(Panel1.ClientWidth-Hmargin*(Length(FCursors)-1))/Length(FCursors);
  xx:=0;
  for i:=0 to High(FCursors) do begin
    FCursors[i].SetBounds(Round(xx), 0, Round(cursorWidth-1), Panel1.ClientHeight-1);
    xx:=xx+cursorWidth+Hmargin;
  end;
end;

procedure TForm1.UpdateCursorAdress;
var i: integer;
begin
  for i:=0 to High(FCursors) do
    FCursors[i].Adress:=BankAdress+i;
end;

procedure TForm1.UpdateTargetDevice;
var i: integer;
begin
  for i:=0 to High(FCursors) do
    FCursors[i].SetTargetDevice(SelectedDevice);
end;

procedure TForm1.UpdateCursorValue;
var i: integer;
begin
  if SelectedDevice=NIL then exit;
  for i:=0 to High(FCursors) do
    FCursors[i].Value:=SelectedDevice.Channels[BankAdress+i];
end;

end.

