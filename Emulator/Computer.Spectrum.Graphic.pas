unit Computer.Spectrum.Graphic;

interface

uses Computer, SysUtils, Classes;

type
  TGPU = class
  private
    FHardware: IHardware;
    FBitmap: PCardinal;
    FBorderContext: PCardinal;
    FSize: Cardinal;

    fHeight: integer;
    fWidth: integer;
    fFlashstat: Boolean;
    ftop, fleft: Word;
    fBorder: Word;
    fFlash: Boolean;
  public
    constructor Create(const AHardware: IHardware; const AUseBorder: Boolean);
    destructor Destroy; override;

    procedure Setborder(const Y: integer);
    procedure Updateline(const Y: integer);

    property Flash_Status: Boolean read fFlashstat write fFlashstat;
    property Border: Word read fBorder write fBorder;

    property Flash: Boolean read fFlash write fFlash;
    property Top: Word read ftop;
    property Bitmap: PCardinal read FBitmap;
    property BorderContext: PCardinal read FBorderContext;
    property Width: integer read fWidth;
    property Height: integer read fHeight;
  end;

function RenderScreen(const AData: PByteArray; const AContext: Pointer;
  const AWidth, AHeight, APitch: integer): Boolean;

implementation

type
  TRGB = record
    r: Byte;
    g: Byte;
    b: Byte;
  end;
  TPalette = Array [0 .. 15] of Cardinal;

const
  ColorTable: array [0 .. 15] of TRGB = ((r: 000; g: 000; b: 000), (r: 000;
    g: 000; b: 160), (r: 220; g: 000; b: 000), (r: 228; g: 000; b: 180),
    (r: 000; g: 212; b: 000), (r: 000; g: 212; b: 212), (r: 208; g: 208;
    b: 000), (r: 200; g: 200; b: 200), (r: 000; g: 000; b: 000), (r: 000;
    g: 000; b: 172), (r: 240; g: 000; b: 000), (r: 252; g: 000; b: 220),
    (r: 000; g: 240; b: 000), (r: 000; g: 252; b: 252), (r: 252; g: 252;
    b: 000), (r: 252; g: 252; b: 252));

var
  FSpeccyline: array [0 .. 191] of Word;
  FReverse_attr: array [0 .. 127] of Byte;
  FVga_h_nib: array [0 .. 15, 0 .. 127] of longint;
  FPalette: TPalette;

function RGBToColor(const RGB: TRGB): Cardinal; inline;
begin
{$IFNDEF POSIX}
  result := $FF000000 or RGB.r shl 16 + RGB.g shl 8 + RGB.b;
{$ELSE}
  result := $FF000000 or RGB.b shl 16 + RGB.g shl 8 + RGB.r;
{$ENDIF}
end;

procedure NibToColor(Dst: PCardinal; const Palette: TPalette;
  const Nib: longint); inline;
var
  qtemp: TQuadruple absolute Nib;
begin
  Dst^ := Palette[qtemp.b1];
  inc(Dst);
  Dst^ := Palette[qtemp.b2];
  inc(Dst);
  Dst^ := Palette[qtemp.b3];
  inc(Dst);
  Dst^ := Palette[qtemp.b4];
end;

procedure MakeTable;
var
  v, x, Y, z: integer;
  fore_attrib, back_attrib: array [0 .. 255] of Byte;
  qtemp: TQuadruple;
begin
  // Create Table
  v := 0;
  for x := 0 to 2 do
    for Y := 0 to 7 do
      for z := 0 to 7 do
      begin
        FSpeccyline[v] := x shl 11 + Y shl 5 + z shl 8;
        inc(v);
      end;

  for x := 0 to 255 do
  begin
    Y := x and 7; (* fore *)
    z := (x and 56) shr 3; (* back *)
    if Y = 0 then
      Y := Y or 8;
    if z = 0 then
      z := z or 8;

    if x and 64 > 0 then
    begin
      Y := Y or 8;
      z := z or 8;
    end;
    fore_attrib[x] := Y;
    back_attrib[x] := z;
  end;

  for x := 0 to 15 do
  begin
    for Y := 0 to 127 do
    begin
      if x and 1 > 0 then
        qtemp.b4 := fore_attrib[Y]
      else
        qtemp.b4 := back_attrib[Y];

      if x and 2 > 0 then
        qtemp.b3 := fore_attrib[Y]
      else
        qtemp.b3 := back_attrib[Y];

      if x and 4 > 0 then
        qtemp.b2 := fore_attrib[Y]
      else
        qtemp.b2 := back_attrib[Y];

      if x and 8 > 0 then
        qtemp.b1 := fore_attrib[Y]
      else
        qtemp.b1 := back_attrib[Y];
      FVga_h_nib[x][Y] := qtemp.Q;
    end;
  end;

  for x := 0 to 127 do
  begin
    Y := (x and 7) shr 3;
    z := (x and 56) shr 3;
    FReverse_attr[x] := (x and 64) or Y or z;
  end;

  for x := low(ColorTable) to high(ColorTable) do
    FPalette[x] := RGBToColor(ColorTable[x]);
end;

{ TGPU }
constructor TGPU.Create(const AHardware: IHardware; const AUseBorder: Boolean);
begin
  FHardware := AHardware;

  if AUseBorder then
  begin
    ftop := 32;
    fleft := 32;
  end
  else
  begin
    ftop := 0;
    fleft := 0;
  end;

  fHeight := 192 + ftop shl 1;
  fWidth := 256 + fleft shl 1;
  FSize := fWidth * fHeight * 4;
  Getmem(FBitmap, FSize);
  GetMem(FBorderContext, FSize div Cardinal(FWidth));
end;

destructor TGPU.Destroy;
begin
  FreeMem(FBitmap, FSize);
  FreeMem(FBorderContext, FSize div Cardinal(FWidth));
  inherited;
end;

procedure TGPU.Setborder(const Y: integer);

var
  ptr: PCardinal;
  color: Cardinal;
  i: integer;

begin
  ptr := FBitmap;
  inc(ptr, Y * fWidth);

  color := FPalette[Border];
  for i := 0 to 256 + fleft shl 1 - 1 do
  begin
    ptr^ := color;
    inc(ptr);
  end;
end;

procedure TGPU.Updateline(const Y: integer);

var
  Bitmap, colorPos: Word;
  x, color, attr, data: Byte;
  offs: PCardinal;
  ptr1, ptr2: PCardinal;
  scanline: PCardinal;
  col: Cardinal;
begin
  if not assigned(FBitmap) then
    exit;

  Scanline := FBorderContext; inc(Scanline, Y);
  Scanline^ := FPalette[FBorder];

  scanline := FBitmap;
  inc(scanline, (Y + ftop) * fWidth);

  if (fleft > 0) or (ftop > 0) then // Realborder
  begin
    if (Y < 0) or (Y > 191) then
    begin
      Setborder(Y + ftop);
      exit;
    end;
    col := FPalette[Border];
    ptr1 := scanline;
    ptr2 := scanline;
    inc(ptr2, 256 + fleft);
    for x := 0 to fleft - 1 do
    begin
      ptr1^ := col;
      inc(ptr1);
      ptr2^ := col;
      inc(ptr2);
    end;

  end;
  Bitmap := FSpeccyline[Y];
  offs := scanline;
  inc(offs, fleft);
  colorPos := 6144 or ((Y and 248) shl 2);

  for x := 0 to 31 do
  begin
    if assigned(FHardware) then
      FHardware.PeekScreen(colorPos, color);

    inc(colorPos);
    attr := color and 127;

    if Flash then
    begin
      if (color and 128 > 0) and (fFlashstat) then
        attr := FReverse_attr[attr];
    end;

    if assigned(FHardware) then
      FHardware.PeekScreen(Bitmap, data);

    inc(Bitmap);

    NibToColor(offs, FPalette, FVga_h_nib[(data and $F0) shr 4][attr]);
    inc(offs, 4);

    NibToColor(offs, FPalette, FVga_h_nib[data and $0F][attr]);
    inc(offs, 4);
  end;
end;

function RenderScreen(const AData: PByteArray; const AContext: Pointer;
  const AWidth, AHeight, APitch: integer): Boolean;
var
  x, Y: integer;
  bmpPos, colorPos: Word;
  data, color, attr: Byte;
  offs: PCardinal;
begin
  result := false;
  if (AWidth <> 256) and (AHeight <> 192) then
    exit;
  if (AData = nil) or (AContext = nil) then
    exit;

  for Y := 0 to 191 do
  begin
    offs := PCardinal(AContext);
    inc(PByte(offs), Y * APitch);

    bmpPos := FSpeccyline[Y];
    colorPos := 6144 or ((Y and 248) shl 2);

    for x := 0 to 31 do
    begin
      color := AData[colorPos];

      inc(colorPos);
      attr := color and 127;

      data := AData[bmpPos];

      inc(bmpPos);

      NibToColor(offs, FPalette, FVga_h_nib[(data and $F0) shr 4][attr]);
      inc(offs, 4);

      NibToColor(offs, FPalette, FVga_h_nib[data and $0F][attr]);
      inc(offs, 4);
    end;
  end;
  result := true;
end;

initialization

MakeTable;

finalization

end.
