program BridgeExample;

{$APPTYPE CONSOLE}

uses
  System.SysUtils;

type
  IBridgeDevice = interface
    ['{0E572EDB-60FA-43D8-AFC2-D5F9B35554E0}']
    function PowerOn: string;
    function Mute: string;
  end;

  TTvDevice = class(TInterfacedObject, IBridgeDevice)
  public
    function PowerOn: string;
    function Mute: string;
  end;

  TRadioDevice = class(TInterfacedObject, IBridgeDevice)
  public
    function PowerOn: string;
    function Mute: string;
  end;

  TBasicRemote = class
  private
    FDevice: IBridgeDevice;
  public
    constructor Create(const ADevice: IBridgeDevice);
    function Activate: string;
  end;

  TMuteRemote = class
  private
    FDevice: IBridgeDevice;
  public
    constructor Create(const ADevice: IBridgeDevice);
    function Activate: string;
  end;

function TTvDevice.PowerOn: string;
begin
  Result := 'TV:on';
end;

function TTvDevice.Mute: string;
begin
  Result := 'TV:muted';
end;

function TRadioDevice.PowerOn: string;
begin
  Result := 'Radio:on';
end;

function TRadioDevice.Mute: string;
begin
  Result := 'Radio:muted';
end;

constructor TBasicRemote.Create(const ADevice: IBridgeDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;

function TBasicRemote.Activate: string;
begin
  Result := FDevice.PowerOn;
end;

constructor TMuteRemote.Create(const ADevice: IBridgeDevice);
begin
  inherited Create;
  FDevice := ADevice;
end;

function TMuteRemote.Activate: string;
begin
  Result := FDevice.Mute;
end;

procedure PrintResults(const Tv, Radio: IBridgeDevice);
var
  Basic: TBasicRemote;
  Muting: TMuteRemote;
begin
  Basic := TBasicRemote.Create(Tv);
  try
    Writeln('basic-tv=' + Basic.Activate);
  finally
    Basic.Free;
  end;

  Basic := TBasicRemote.Create(Radio);
  try
    Writeln('basic-radio=' + Basic.Activate);
  finally
    Basic.Free;
  end;

  Muting := TMuteRemote.Create(Tv);
  try
    Writeln('mute-tv=' + Muting.Activate);
  finally
    Muting.Free;
  end;

  Muting := TMuteRemote.Create(Radio);
  try
    Writeln('mute-radio=' + Muting.Activate);
  finally
    Muting.Free;
  end;
end;

var
  Tv: IBridgeDevice;
  Radio: IBridgeDevice;
begin
  Tv := TTvDevice.Create;
  Radio := TRadioDevice.Create;
  PrintResults(Tv, Radio);
end.
