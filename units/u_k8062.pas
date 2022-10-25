unit u_k8062;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  hidapi;



const

  K8062_VENDOR_ID = $10cf;
  K8062_PRODUCT_ID = $8062;

  DEFAULT_CHANNELCOUNT = 24; // channels sent to the device when application start
                             // must be between 24 and 512


  // library filename
  {$ifdef MSWINDOWS}
    {$ifdef CPU386}
      LIBHIDAPI = 'i386-win32\hidapi.dll';
    {$endif}
    {$ifdef CPU64}
      LIBHIDAPI = 'x86_64-win64\hidapi.dll';
    {$endif}
  {$endif}

  {$ifdef LINUX}
  {$ifdef CPU386}
    LIBHIDAPI = 'i386-linux/libhidapi-libusb.so.0.12.0';
  {$endif}
  {$ifdef CPU64}
    LIBHIDAPI = 'x86_64-linux/libhidapi-libusb.so.0.0.0';
  {$endif}
  {$endif}


type

  { TK8062Thread }

  TK8062Thread=class(TThread)
  private
    FPeriodMS: integer;
    FHidDevice: PHidDevice;
    Buf_tx: array [0..64] of byte;
    FDmxDataOut: array[0..523] of byte;
    procedure DoWrite;
    procedure SendDmx;
  protected
    procedure Execute; override;
  public
    ChannelCount: integer;
    Constructor Create(aPeriodMs: integer; aStart: boolean; aHidDevice: PHidDevice);
    procedure SetChannel(aAdr: integer; aLevel: byte);
  end;


  { TVelleman_K8062D_Device }

  TVelleman_K8062D_Device=class
  private
    FK8062Thread: TK8062Thread;
    FHidDevice: PHidDevice;
    FChannelCount: integer;
    FDmxBuffer: array[1..512] of byte;
    function GetChannel(index: integer): byte;
    function GetChannelCount: integer;
    procedure SetChannel(index: integer; AValue: byte);
    procedure SetChannelCount(AValue: integer);
  protected
    FUsbPath: string;
  public
    constructor Create;
    destructor Destroy; override;

    function StartDevice: boolean;
    procedure StopDevice;

    // set/retrieve a channel's value. index is [1..512]
    property Channels[index:integer]: byte read GetChannel write SetChannel;

    // set/retrieve the number of channels sent to the device
    property ChannelCount: integer read GetChannelCount write SetChannelCount;
  end;


  { TK8062Manager }

  TK8062Manager=class
  private
    FReady: boolean;
    FList: TList;
    function GetDevice(index: integer): TVelleman_K8062D_Device;
    function GetDeviceCount: integer;
    procedure RegisterDevice( aPath: string );
    procedure FreeAll;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LookForDevice;

    property Ready: boolean read FReady;
    property DeviceCount: integer read GetDeviceCount;
    property Devices[index:integer]: TVelleman_K8062D_Device read GetDevice;
  end;

implementation

{ TVelleman_K8062D_Device }

constructor TVelleman_K8062D_Device.Create;
begin
  FChannelCount := DEFAULT_CHANNELCOUNT;
end;

destructor TVelleman_K8062D_Device.Destroy;
begin
  StopDevice;
  inherited Destroy;
end;

function TVelleman_K8062D_Device.StartDevice: boolean;
begin
  if FHidDevice = NIL then begin
    FHidDevice := THidDevice.OpenPath(FUsbPath);
    if FHidDevice = NIL then begin
      Result := FALSE;
      exit;
    end;
    if FK8062Thread=NIL then begin
      FK8062Thread:=TK8062Thread.Create(25, FALSE, FHidDevice);
      FK8062Thread.ChannelCount:= FChannelCount;
      FK8062Thread.Start;
    end;
  end;
  Result := TRUE;
end;

procedure TVelleman_K8062D_Device.StopDevice;
begin
  if FK8062Thread<>NIL then begin
    FK8062Thread.Terminate;
    FK8062Thread.WaitFor;
    FK8062Thread.Free;
    FK8062Thread:=NIL;
  end;

  if FHidDevice<>NIL then begin
    FHidDevice^.Close;
    FHidDevice:=NIL;
  end;
end;

function TVelleman_K8062D_Device.GetChannelCount: integer;
begin
  Result := FChannelCount;
end;

function TVelleman_K8062D_Device.GetChannel(index: integer): byte;
begin
  if (index<1) or (index>512) then begin
    Raise Exception.Create('Invalid DMX adress: attempt to read at dmx adress '+index.ToString);
    exit;
  end;
  Result:=FDmxBuffer[index];
end;

procedure TVelleman_K8062D_Device.SetChannel(index: integer; AValue: byte);
begin
  if (index<1) or (index>512) then begin
    Raise Exception.Create('Invalid DMX adress: attempt to write at dmx adress '+index.ToString);
    exit;
  end;
  FDmxBuffer[index]:=AValue;
  if FK8062Thread<>NIL
    then FK8062Thread.SetChannel(index, AValue);
end;

procedure TVelleman_K8062D_Device.SetChannelCount(AValue: integer);
begin
  if AValue<24 then AValue:=24;
  if AValue>512 then AValue:=512;

  if AValue = FChannelCount then exit;

  FChannelCount := AValue;
  if FK8062Thread<>NIL then FK8062Thread.ChannelCount:=AValue;
end;

{ TK8062Thread }

procedure TK8062Thread.DoWrite;
var Written: Cardinal;
  ToWrite: Cardinal;
begin
  Buf_tx[0]:=0;
  ToWrite:=9;
  Written:=FHidDevice^.Write(buf_tx, ToWrite);
end;

procedure TK8062Thread.SendDmx;
var i, max_ch, n: integer;
  function GetShareData(aIndex: integer): byte; inline;
  begin
    Result:=FDmxDataOut[aIndex];
  end;

begin
  Buf_tx[0]:=0;
  max_ch:=ChannelCount;
  if max_ch<8 then max_ch:=8;
  if max_ch>512 then max_ch:=512;
  i:=0;
  repeat
    inc(i);
  until (GetShareData(i+10)>0) or (i=100) or (i=max_ch-5);
  Buf_tx[1]:=4;    //send start code + 6 bytes of data
  Buf_tx[2]:=i;    //number of zeroes (incl. start code = 0)
  Buf_tx[3]:=GetShareData(i+10);
  Buf_tx[4]:=GetShareData(i+11);
  Buf_tx[5]:=GetShareData(i+12);
  Buf_tx[6]:=GetShareData(i+13);
  Buf_tx[7]:=GetShareData(i+14);
  Buf_tx[8]:=GetShareData(i+15);
  DoWrite;
  i:=i+6;
  repeat
    if max_ch-i<6 then
    begin
      Buf_tx[1]:=3;    //send one byte of data
      Buf_tx[2]:=GetShareData(i+10);
      DoWrite;
      inc(i);
    end;
    if (max_ch-i>=6) and (GetShareData(i+10)>0) then
    begin
      Buf_tx[1]:=2;    //send 7 bytes of data
      Buf_tx[2]:=GetShareData(i+10);
      Buf_tx[3]:=GetShareData(i+11);
      Buf_tx[4]:=GetShareData(i+12);
      Buf_tx[5]:=GetShareData(i+13);
      Buf_tx[6]:=GetShareData(i+14);
      Buf_tx[7]:=GetShareData(i+15);
      Buf_tx[8]:=GetShareData(i+16);
      DoWrite;
      i:=i+7;
    end;
    if (max_ch-i>=6) and (GetShareData(i+10)=0) then
    begin
      n:=0;
      repeat
        inc(i);
        inc(n);
      until (GetShareData(i+10)>0) or (n=100) or (i>=max_ch-6);
      Buf_tx[1]:=5;    //send n zeroes + 6 bytes of data
      Buf_tx[2]:=n;    //number of zeroes to send
      Buf_tx[3]:=GetShareData(i+10);
      Buf_tx[4]:=GetShareData(i+11);
      Buf_tx[5]:=GetShareData(i+12);
      Buf_tx[6]:=GetShareData(i+13);
      Buf_tx[7]:=GetShareData(i+14);
      Buf_tx[8]:=GetShareData(i+15);
      DoWrite;
      i:=i+6;
    end;
  until i>max_ch;
end;

procedure TK8062Thread.Execute;
var TOrigin, TNow, DeltaT: QWord;
begin
  TOrigin:=GetTickCount64;
  while not Terminated do begin
    TNow:=GetTickCount64;
    DeltaT := TNow-TOrigin;
    if DeltaT>=FPeriodMS then begin
      SendDmx;
      TOrigin+=FPeriodMS;
    end
    else sleep(1);
   end;
end;

constructor TK8062Thread.Create(aPeriodMs: integer; aStart: boolean;
  aHidDevice: PHidDevice);
begin
  inherited Create(TRUE);
  Priority:=tpHighest;//tpNormal;
  FPeriodMS:=aPeriodMs;
  FHidDevice:=aHidDevice;
  ChannelCount:=512;
  if aStart then Start;
end;

procedure TK8062Thread.SetChannel(aAdr: integer; aLevel: byte);
begin
  FDmxDataOut[aAdr+10] := aLevel;
end;

{ TK8062Manager }

procedure TK8062Manager.RegisterDevice(aPath: string);
var o: TVelleman_K8062D_Device;
begin
  o := TVelleman_K8062D_Device.Create;
  o.FUsbPath:=aPath;
  FList.Add(o);
end;

function TK8062Manager.GetDeviceCount: integer;
begin
  Result := FList.Count;
end;

function TK8062Manager.GetDevice(index: integer): TVelleman_K8062D_Device;
begin
  if (index<0) or (index>=FList.Count)
    then Result := NIL
    else Result := TVelleman_K8062D_Device(FList.Items[index]);
end;

procedure TK8062Manager.FreeAll;
var i: integer;
begin
  for i:=0 to FList.Count-1 do
    TVelleman_K8062D_Device(FList.Items[i]).Free;
  FList.Clear;
end;

constructor TK8062Manager.Create;
begin
  FReady := HidInit(ConcatPaths([ExtractFilePath(ParamStr(0)), LIBHIDAPI]))>0;
  FList := TList.Create;
end;

destructor TK8062Manager.Destroy;
begin
  FreeAll;
  FList.Free;
  HidExit;
  inherited Destroy;
end;

procedure TK8062Manager.LookForDevice;
var LinkedList, Item: PHidDeviceInfo;
begin
  FreeAll;
  LinkedList := THidDeviceInfo.Enumerate(K8062_VENDOR_ID, K8062_PRODUCT_ID);

  Item := LinkedList;
  while Assigned(Item) do begin
    RegisterDevice( Item^.Path );
    Item := Item^.Next;
  end;

  if Assigned(LinkedList)
    then LinkedList^.Free;
end;

end.

