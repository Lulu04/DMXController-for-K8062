unit frame_cursor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls,
  LCLType,
  u_k8062;

type

  { TFrameCursor }

  TFrameCursor = class(TFrame)
    Label1: TLabel;
    Label2: TLabel;
    TB: TTrackBar;
    procedure TBChange(Sender: TObject);
  private
    FTargetDevice: TVelleman_K8062D_Device;
    FAdress: word;
    function GetValue: byte;
    procedure SetAdress(AValue: Word);
    procedure SetValue(AValue: byte);

  public
   // constructor Create( TheOwner: TComponent ); override;
    procedure EraseBackground(DC: HDC); override;

    procedure SetTargetDevice( aTarget: TVelleman_K8062D_Device );

    property Value: byte read GetValue write SetValue;
    property Adress: Word read FAdress Write SetAdress;
  end;

implementation
uses Graphics;

{$R *.lfm}

{ TFrameCursor }

procedure TFrameCursor.TBChange(Sender: TObject);
begin
  Label1.Caption:=TB.Position.ToString;
  if FTargetDevice<>NIL
    then FTargetDevice.Channels[FAdress]:=TB.Position;
end;

function TFrameCursor.GetValue: byte;
begin
  Result := byte(TB.Position);
end;

procedure TFrameCursor.SetAdress(AValue: Word);
begin
  Label2.Caption:=AValue.ToString;
  FAdress:=AValue;
end;

procedure TFrameCursor.SetValue(AValue: byte);
begin
  TB.Position:=AValue;
  Label1.Caption:=AValue.ToString;
end;

procedure TFrameCursor.EraseBackground(DC: HDC);
begin
  // do nothing here
end;

procedure TFrameCursor.SetTargetDevice(aTarget: TVelleman_K8062D_Device);
begin
  FTargetDevice:=aTarget;
end;

end.

