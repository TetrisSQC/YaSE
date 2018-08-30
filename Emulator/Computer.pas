unit Computer;

interface

uses Classes;

const
  BORDERWIDTH = 20;

  // Screen things
  nPixelsWide = 256;
  nPixelsHigh = 192;
  nCharsWide = 32;
  nCharsHigh = 24;
  FirstAttr = nPixelsHigh * nCharsWide;
  // lastAttr = FirstAttr + (nCharsHigh * nCharsWide);
  nRomStart = 8 + 7; // First Rom in Data
  nMF128start = nRomStart + 4; // MF128 Rom

  IID_IHardware: TGuid = '{9E1F5CF8-6F80-4BDA-9414-F39380E1AEA0}';

type
  TRAMPage = Array [0 .. 16383] of Byte;
  TRAMPages = array [0 .. 19] of TRAMPage;
  PRAMPages = ^TRAMPages;

  TPageMap = array [0 .. 4] of Integer;

  TJoystick = record
    up, down, left, right, fire: Word;
  end;

  TPair = record
    case boolean of
      false:
        (l, h: Byte);
      true:
        (W: Word);
  end;

  TQuadruple = record
    case Byte of
      0:
        (b1, b2, b3, b4: Byte);
      1:
        (w1, w2: Word);
      2:
        (q: longint);
  end;

  TZ80Register = class // Main Z80 registers
    af, bc, de, hl: TPair;
    ix, iy: TPair;
    sp, pc: TPair;
    ir: TPair;
    rtemp, im: Byte;
    AF2, BC2, DE2, HL2: TPair; { == AF' BC' DE' HL' }
    iff1, iff2: Byte;
    halt: boolean;
    bit7_r: byte;

    InterruptCounter: Integer;

    procedure Reset;
    procedure Assign(const ARegister: TZ80Register);
  end;

  TRenderNotify = procedure(Sender: TObject; const AImage, ABorder: PByte;
    const AWidth, AHeight, AScanline: Integer) of object;

  IHardware = interface
    procedure InByte(const Addr: Word; out Value: Byte);
    procedure OutByte(const Addr: Word; const Value: Byte);

    procedure PeekByte(const Addr: Word; out Value: Byte);
    procedure PokeByte(const Addr: Word; const Value: Byte);
    procedure PeekScreen(const Addr: Integer; out Color: Byte);

    procedure OnInterrupt;
    procedure Instruction(const Opcode: Byte; const Counter: Integer);

    procedure SetPaintCallback(const ACallback: TRenderNotify);

    procedure SetModel(const AModel: Integer);
    function GetModel: Integer;
    function GetDeviceName(): String;

    procedure doKey(down: boolean; ascii: Word; mods: TShiftState);
    procedure CycleTick;

    function GetKempstonValue: Byte;
    procedure SetKempstonValue(const Value: Byte);

    procedure LoadFromFile(const AFilename: String);
    procedure SaveToFile(const AFilename: String);


    function GetPaused: boolean;
    procedure SetPaused(const AValue: boolean);

    function GetRegister: TZ80Register;

    procedure NMI;
    procedure Reset;
  end;

  TComputer = class(TInterfacedObject, IHardware)
  private
    // IInterface
    (*function QueryInterface(const IID: TGuid; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;*)
  protected
    FOnPaint: TRenderNotify;
    FOnBreakpointNotify: TNotifyEvent;
    FName: String;
    FJoystick: Byte;
    FPaused: boolean;

    procedure DoPaint; virtual; abstract;

    procedure InByte(const Addr: Word; out Value: Byte); virtual; abstract;
    procedure OutByte(const Addr: Word; const Value: Byte); virtual; abstract;

    procedure PeekByte(const Addr: Word; out Value: Byte); virtual; abstract;
    procedure PokeByte(const Addr: Word; const Value: Byte); virtual; abstract;
    procedure PeekScreen(const Addr: Integer; out Color: Byte);
      virtual; abstract;

    procedure OnInterrupt; virtual; abstract;
    procedure Instruction(const Opcode: Byte; const Counter: Integer); virtual; abstract;

    procedure SetPaintCallback(const ACallback: TRenderNotify);
      virtual; abstract;
    procedure SetModel(const AModel: Integer); virtual; abstract;
    function GetModel: Integer;virtual;abstract;

    procedure doKey(down: boolean; ascii: Word; mods: TShiftState);
      virtual; abstract;
    procedure CycleTick; virtual; abstract;
    procedure Reset; virtual; abstract;

    procedure LoadFromFile(const AFilename: String); virtual; abstract;
    procedure SaveToFile(const AFilename: String); virtual; abstract;

    function GetDeviceName(): String;
    function GetPaused: boolean;
    procedure SetPaused(const AValue: boolean);

    function GetKempstonValue: Byte;
    procedure SetKempstonValue(const Value: Byte);

    function GetRegister: TZ80Register;virtual;abstract;
    procedure NMI;virtual;
  end;

implementation

// IInterface
(*function TComputer.QueryInterface(const IID: TGuid; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
  begin
    Result := S_OK;
  end
  else
  begin
    Result := E_NOINTERFACE;
  end;
end;

function TComputer._AddRef: Integer;
begin
  Result := -1;
end;

function TComputer._Release: Integer;
begin
  Result := -1;
end;
*)

function TComputer.GetDeviceName(): String;
begin
  Result := FName;
end;

function TComputer.GetPaused: boolean;
begin
  Result := FPaused;
end;

procedure TComputer.SetPaused(const AValue: boolean);
begin
  FPaused := AValue;
end;


function TComputer.GetKempstonValue: Byte;
begin
  result := FJoystick
end;

procedure TComputer.SetKempstonValue(const Value: Byte);
begin
  FJoystick := Value;
end;

procedure TComputer.NMI;
begin

end;


{ TZ80Register }
procedure TZ80Register.Reset;
begin
  iff1 := 0;
  iff2 := 0;
  halt := false;
  af.W := 0;
  AF2.W := 0;
  hl.W := 0;
  HL2.W := 0;
  de.W := 0;
  DE2.W := 0;
  bc.W := 0;
  BC2.W := 0;
  ix.W := $FFFF;
  iy.W := $FFFF;

  im := 1;
  ir.W := 0;
  sp.W := 0;
  pc.W := 0;
end;

procedure TZ80Register.Assign(const ARegister: TZ80Register);
begin
  af.W := ARegister.af.W;
  bc.W := ARegister.bc.W;
  de.W := ARegister.de.W;
  hl.W := ARegister.hl.W;
  ix.W := ARegister.ix.W;
  iy.W := ARegister.iy.W;
  sp.W := ARegister.sp.W;
  pc.W := ARegister.pc.W;
  ir.W := ARegister.ir.W;
  rtemp := ARegister.rtemp;
  im := ARegister.im;

  AF2.W := ARegister.AF2.W;
  BC2.W := ARegister.BC2.W;
  DE2.W := ARegister.DE2.W;
  HL2.W := ARegister.HL2.W;

  iff1 := ARegister.iff1;
  iff2 := ARegister.iff2;
  halt := ARegister.halt;

  InterruptCounter := ARegister.InterruptCounter;
end;

end.
