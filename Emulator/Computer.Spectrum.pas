unit Computer.Spectrum;

interface

uses Classes, SysUtils, Computer,
  Computer.CPU.Z80, Computer.Spectrum.Graphic, Computer.Audio.AY8912,
  Computer.Spectrum.Disk, Computer.Spectrum.Tape, System.Audio;

const
  tmZX48 = 0;
  tmZX128 = 1;
  tmZXPlus2 = 2;
  tmZXPlus2a = 3;
  tmZXPlus3 = 4;
  tmPentagon = 5;
  tmScorpion = 6;
  tmTimex = 7;
  tmDidaktik = 8;

type
  TZXSpectrum = class(TComputer, IHardware)
  protected
    FCurrentModel: Integer;
    FEmulatedMF128: Boolean; // glEmulatedMF128

    FEnableAudio: Boolean;
    FStatesPerInterrupt: Integer;

    FCycleState: Integer;
    FCPU: TZ80CPU;
    FGlobalTStates: Integer;

    FGPU: TGPU;
    FGLVLine: Integer;

    FAY8912: TAY8912;
    FSoundRegister: Integer;
    // Contains currently indexed AY-3-8912 sound register

    FTape: TBaseTape;

    FBetaDisk: TBetaDisk;
    FLastOut7FFD: Integer;
    FLastOut1FFD: Integer;

    FInterruptDelay: Integer;
    FInterruptTimer: Integer;
    FDelayOverage: Integer;
    FBeeperVal: Integer;

    FRAMPage: TRAMPages;
    // (pages 0 to 7 are RAM, and pages 8 to 11 are ROM)

    FWaveAddTStates: Integer;
    FTstatesPerLine: Integer;

    FPageAt: TPageMap;
    FUseScreen: Integer;

    // Keypresses
    FKeyB_SPC: Integer;
    FKeyH_ENT: Integer;
    FKeyY_P: Integer;
    FKey6_0: Integer;
    FKey1_5: Integer;
    FKeyQ_T: Integer;
    FKeyA_G: Integer;
    FKeyCAPS_V: Integer;
    FKeyPortMask: Byte;
    // Mask used for reading keyboard port ($BF on Speccys, $1F on TC2048)

    function LoadRom(const Filename: String; const nROMPage: Integer): Boolean;

    function MF128inb(port: Word): Byte;
    function MF3inb(port: Word): Byte;
    procedure HookLoadTape();
    procedure HookSaveTape();
    function Is48KModel: Boolean;
  protected
    // IHardware
    procedure InByte(const addr: Word; out Value: Byte); override;
    procedure OutByte(const addr: Word; const Value: Byte); override;

    procedure PeekByte(const addr: Word; out Value: Byte); override;
    procedure PokeByte(const addr: Word; const val: Byte); override;
    procedure PeekScreen(const addr: Integer; out Color: Byte); override;

    procedure OnInterrupt; override;
    procedure Instruction(const Opcode: Byte; const Counter: Integer); override;
    procedure Reset; override;
    procedure CycleTick; override;
    procedure doKey(down: Boolean; ascii: Word; mods: TShiftState); override;

    procedure SetPaintCallback(const ACallback: TRenderNotify); override;
    procedure SetModel(const AModel: Integer); override;
    function GetModel: Integer; override;

    procedure NMI; override;

    function GetRegister: TZ80Register;override;

    procedure LoadSNA128Snap(const AStream: TStream);
    procedure ReadZ80V2orV3Snap(const AStream: TStream);
    procedure ReadZ80V1Snap(const AStream: TStream);
    procedure SaveSNA128Snap(const AStream: TStream);
    function CompressMemoryBlock(lBlock: Integer; pData: pByte): Integer;

    procedure DoPaint; override;

    procedure LoadZ80Snap(const AFilename: string);
    procedure LoadSNASnap(const AFilename: string);

    procedure SaveSNASnap(const AFilename: string);
    procedure SaveZ80Snap(const AFilename: string);
    procedure LoadFromFile(const AFilename: String); override;
    procedure SaveToFile(const AFilename: String); override;

  public
    constructor Create;
    destructor Destroy; override;

    procedure resetKeyboard;
  end;

implementation

uses Math, UConfigDir, System.IOUtils;

{ TZXSpectrum }

function TZXSpectrum.LoadRom(const Filename: String;
  const nROMPage: Integer): Boolean;
var
  Stream: TFileStream;
  ret: Integer;
  Name: String;
begin
  Result := false;
  Name := FileName;
  if not FileExists(Name) then
     Name := TPath.Combine(GetResourceDir, Filename);

  if not FileExists(Name) then
    exit;

  Stream := TFileStream.Create(Name, fmOpenRead);
  try
    ret := Stream.Read(FRAMPage[nROMPage], 16384);
    Result := (ret = 8192) or (ret = 16384);
  finally
    Stream.free;
  end;
end;

procedure TZXSpectrum.DoPaint;
begin
  if assigned(FOnPaint) then
    FOnPaint(self, pByte(FGPU.Bitmap), pByte(FGPU.BorderContext),
     FGPU.Width, FGPU.Height, FGPU.Width * 4);
end;

procedure TZXSpectrum.PeekByte(const addr: Word; out Value: Byte);
begin
  Value := FRAMPage[FPageAt[(addr and $C000) shr 14], (addr and $3FFF)];
end;

procedure TZXSpectrum.PokeByte(const addr: Word; const val: Byte);
var
  Page: Byte;
begin
  Page := FPageAt[(addr and $C000) shr 14];
  if Page < nRomstart then
    FRAMPage[Page, addr and $3FFF] := val;
end;

procedure TZXSpectrum.PeekScreen(const addr: Integer; out Color: Byte);
begin
  Color := FRAMPage[FUseScreen, addr];
end;

procedure TZXSpectrum.HookLoadTape;
var
  temp: Word;
begin
  if not assigned(FTape) or (not FTape.IsInserted) then
    exit;

  if (FTape.IsRealTime) then
    FTape.StartTape
  else
    with FCPU.Register do
    begin
      if FTape.LoadTAP(FCPU.Register, af2.h, ix.W, de.W) then
        af2.l := af2.l Or 64
      else
        af2.l := af2.l And 190;

      temp := af.W;
      af.W := af2.W;
      af2.W := temp;
      pc.W := 1506;
    end;
end;

function TZXSpectrum.Is48KModel: Boolean;
begin
  result := FCurrentModel in [tmZX48,tmTimex,tmDidaktik];
end;

procedure TZXSpectrum.HookSaveTape();
begin
  with FCPU.Register do
  begin
    If FTape.SaveTAP(FCPU.Register, af2.W, ix.W, de.W) Then
    begin
      inc(ix.W, de.W);
      de.W := 0;
    end;
    resetKeyboard;
    pc.W := 1342; // RET
  end;
end;

procedure TZXSpectrum.SetPaintCallback(const ACallback: TRenderNotify);
begin
  FOnPaint := ACallback;
end;

procedure TZXSpectrum.Instruction(const Opcode: Byte; const Counter: Integer);
var
  Beeper: Integer;
begin
  if assigned(FBetaDisk) then
    with FCPU.Register do
    begin // nromstart+3=TRDOS
      if (pc.W >= $4000) and (FPageAt[0] = nRomstart + 3) then
        FPageAt[0] := nRomstart + (FLastOut7FFD and 16) shr 4
        // Byte(FLastOut7FFD and 16 > 0) + nromstart
      else if (pc.W and $FF00 = $3D00) and (FPageAt[0] = nRomstart + 1) then
        FPageAt[0] := nRomstart + 3; // TRDOS
    end;

  with FCPU.Register do
  begin
    if (Opcode = 191) and (pc.W = 1387) Then
      HookLoadTape;
    If (Opcode = 8) and (pc.W = 1233) Then
      HookSaveTape;
  end;

  if assigned(FOnBreakpointNotify) then
    FOnBreakpointNotify(self);

  inc(FCycleState, Counter);
  if FCycleState >= FTstatesPerLine then
    with FGPU do
    begin
      FGLVLine := (FGLVLine + 1) mod (192 + top shl 1);
      FCycleState := 0;
      Updateline(FGLVLine - top);
    end;

  Beeper := FBeeperVal;
  If assigned(FTape) and (FTape.IsRealTime) and (FTape.IsPlaying) Then
  begin
    FTape.UpdateState(Counter);
    If FTape.EarBit = 64 Then
      Beeper := Beeper + 15;
  end;
  Audio.Add(Counter, FWaveAddTStates, Beeper, FAY8912)
end;

procedure TZXSpectrum.resetKeyboard;
begin
  FKeyB_SPC := $FF;
  FKeyH_ENT := $FF;
  FKeyY_P := $FF;
  FKey6_0 := $FF;
  FKey1_5 := $FF;
  FKeyQ_T := $FF;
  FKeyA_G := $FF;
  FKeyCAPS_V := $FF;
end;

procedure TZXSpectrum.Reset;
begin
  FCPU.Register.Reset;
  FLastOut7FFD := 0;
  FLastOut1FFD := 0;
  FGPU.Border := 7;
  FGLVLine := 0;

  FSoundRegister := 0;

end;

procedure TZXSpectrum.OnInterrupt;
var
  lSleep: Integer;
begin
  inc(FCPU.Register.interruptCounter);

  // Attribute flash (state alters every 1/2 second)
  if (FCPU.Register.interruptCounter mod 25) = 0 then
    FGPU.Flash_Status := not FGPU.Flash_Status;

  DoPaint;

  if FEnableAudio then
    Audio.Update;

  // Keep the emulation running at the correct speed by
  // adding a delay to ensure that interrupts are
  // generated at the correct frequency
  lSleep := FInterruptTimer - Integer(TThread.GetTickCount) + FDelayOverage;
  if lSleep < 0 then
  begin
    if FDelayOverage < -40 then
      FDelayOverage := -40
    else
      FDelayOverage := lSleep;
  end;

  if lSleep > 0 then
  begin
    Sleep(lSleep);
    FDelayOverage := FDelayOverage + (FInterruptDelay - lSleep);
    if FDelayOverage > 0 then
      FDelayOverage := 0;
  end;

  FInterruptTimer := Integer(TThread.GetTickCount) + FInterruptDelay;
end;

constructor TZXSpectrum.Create;
begin
  inherited;
  FEnableAudio := true;

  FCPU := TZ80CPU.Create(self);
  FGPU := TGPU.Create(self, true);

  FInterruptDelay := 20;
  FInterruptTimer := TThread.GetTickCount + 20;

  SetModel(tmZX48);
  Reset;
  resetKeyboard;
end;

destructor TZXSpectrum.Destroy;
begin
  FreeAndNil(FCPU);
  FreeAndNil(FGPU);
  FreeAndNil(FAY8912);
  FreeAndNil(FTape);
  inherited;
end;

function TZXSpectrum.MF128inb(port: Word): Byte;
begin
  case port and $FF of
    63: (* Mf 128 Page Out *)
      begin
        { outport(32765,last_7ffd); }
        FPageAt[0] := nRomstart + (FLastOut7FFD and 16) shr 4;
        Result := $FF;
      end;
    191: (* Mf 128 Page In *)
      begin
        FPageAt[0] := nMF128start;
        if FLastOut7FFD and 8 > 0 then
          Result := $FF
        else
          Result := $7F;
      end;
  else
    Result := 0;
  end;
end;

function TZXSpectrum.MF3inb(port: Word): Byte;
begin
  Result := 0;
  case port and $FF of
    191:
      if (FLastOut1FFD and 1 = 0) and (FPageAt[0] = nMF128start) then
        FPageAt[0] := nRomstart + (((FLastOut7FFD and 16) shr 4) or
          (FLastOut1FFD and 4) shr 1);
    63:
      case (port shr 8) and $FF of
        $7F:
          Result := FLastOut7FFD;
        $1F:
          Result := FLastOut1FFD;
      else
        if (FPageAt[0] <> nMF128start) and (FLastOut1FFD and 1 = 0) then
          FPageAt[0] := nMF128start;
      end;
  end;
end;

procedure TZXSpectrum.CycleTick;
var
  dummy: Byte;
begin
  if assigned(FBetaDisk) then
    FBetaDisk.diskVG($A, dummy);
  if (not FPaused) then
  begin
    // Execute some z80 code
    FCPU.Execute(FGlobalTStates, FStatesPerInterrupt);
  end;
  if assigned(FBetaDisk) then
    FBetaDisk.diskVG($B, dummy);
end;

procedure TZXSpectrum.doKey(down: Boolean; ascii: Word; mods: TShiftState);
var
  CAPS: Boolean;
  SYMB: Boolean;
  // Shift : Boolean;
begin

  CAPS := (ssShift in mods);
  SYMB := (ssCtrl in mods);

  // Change control versions of keys to lower case
  If (ascii >= 1) And (ascii <= $27) And SYMB Then
    ascii := ascii + Ord('a') - 1;

  If CAPS Then
    FKeyCAPS_V := (FKeyCAPS_V And (Not 1))
  Else
    FKeyCAPS_V := (FKeyCAPS_V Or 1);
  If SYMB Then
    FKeyB_SPC := (FKeyB_SPC And (Not 2))
  Else
    FKeyB_SPC := (FKeyB_SPC Or 2);

  Case ascii of
    8: // Backspace
      begin
        If down Then
        begin
          FKey6_0 := (FKey6_0 And (Not 1));
          FKeyCAPS_V := (FKeyCAPS_V And (Not 1));
        end
        Else
        begin
          FKey6_0 := (FKey6_0 Or 1);
          If Not CAPS Then
            FKeyCAPS_V := (FKeyCAPS_V Or 1);
        End;
      end;
    65: // A
      If down Then
        FKeyA_G := (FKeyA_G And (Not 1))
      Else
        FKeyA_G := (FKeyA_G Or 1);
    66: // B
      If down Then
        FKeyB_SPC := (FKeyB_SPC And (Not 16))
      Else
        FKeyB_SPC := (FKeyB_SPC Or 16);
    67: // C
      If down Then
        FKeyCAPS_V := (FKeyCAPS_V And (Not 8))
      Else
        FKeyCAPS_V := (FKeyCAPS_V Or 8);
    68: // D
      If down Then
        FKeyA_G := (FKeyA_G And (Not 4))
      Else
        FKeyA_G := (FKeyA_G Or 4);
    69: // E
      If down Then
        FKeyQ_T := (FKeyQ_T And (Not 4))
      Else
        FKeyQ_T := (FKeyQ_T Or 4);
    70: // F
      If down Then
        FKeyA_G := (FKeyA_G And (Not 8))
      Else
        FKeyA_G := (FKeyA_G Or 8);
    71: // G
      If down Then
        FKeyA_G := (FKeyA_G And (Not 16))
      Else
        FKeyA_G := (FKeyA_G Or 16);
    72: // H
      If down Then
        FKeyH_ENT := (FKeyH_ENT And (Not 16))
      Else
        FKeyH_ENT := (FKeyH_ENT Or 16);
    73: // I
      If down Then
        FKeyY_P := (FKeyY_P And (Not 4))
      Else
        FKeyY_P := (FKeyH_ENT Or 4);
    74: // J
      If down Then
        FKeyH_ENT := (FKeyH_ENT And (Not 8))
      Else
        FKeyH_ENT := (FKeyH_ENT Or 8);
    75: // K
      If down Then
        FKeyH_ENT := (FKeyH_ENT And (Not 4))
      Else
        FKeyH_ENT := (FKeyH_ENT Or 4);
    76: // L
      If down Then
        FKeyH_ENT := (FKeyH_ENT And (Not 2))
      Else
        FKeyH_ENT := (FKeyH_ENT Or 2);
    77: // M
      If down Then
        FKeyB_SPC := (FKeyB_SPC And (Not 4))
      Else
        FKeyB_SPC := (FKeyB_SPC Or 4);
    78: // N
      If down Then
        FKeyB_SPC := (FKeyB_SPC And (Not 8))
      Else
        FKeyB_SPC := (FKeyB_SPC Or 8);
    79: // O
      If down Then
        FKeyY_P := (FKeyY_P And (Not 2))
      Else
        FKeyY_P := (FKeyY_P Or 2);
    80: // P
      If down Then
        FKeyY_P := (FKeyY_P And (Not 1))
      Else
        FKeyY_P := (FKeyY_P Or 1);
    81: // Q
      If down Then
        FKeyQ_T := (FKeyQ_T And (Not 1))
      Else
        FKeyQ_T := (FKeyQ_T Or 1);
    82: // R
      If down Then
        FKeyQ_T := (FKeyQ_T And (Not 8))
      Else
        FKeyQ_T := (FKeyQ_T Or 8);
    83: // S
      If down Then
        FKeyA_G := (FKeyA_G And (Not 2))
      Else
        FKeyA_G := (FKeyA_G Or 2);
    84: // T
      If down Then
        FKeyQ_T := (FKeyQ_T And (Not 16))
      Else
        FKeyQ_T := (FKeyQ_T Or 16);
    85: // U
      If down Then
        FKeyY_P := (FKeyY_P And (Not 8))
      Else
        FKeyY_P := (FKeyY_P Or 8);
    86: // V
      If down Then
        FKeyCAPS_V := (FKeyCAPS_V And (Not 16))
      Else
        FKeyCAPS_V := (FKeyCAPS_V Or 16);
    87: // W
      If down Then
        FKeyQ_T := (FKeyQ_T And (Not 2))
      Else
        FKeyQ_T := (FKeyQ_T Or 2);
    88: // X
      If down Then
        FKeyCAPS_V := (FKeyCAPS_V And (Not 4))
      Else
        FKeyCAPS_V := (FKeyCAPS_V Or 4);
    89: // Y
      If down Then
        FKeyY_P := (FKeyY_P And (Not 16))
      Else
        FKeyY_P := (FKeyY_P Or 16);
    90: // Z
      If down Then
        FKeyCAPS_V := (FKeyCAPS_V And (Not 2))
      Else
        FKeyCAPS_V := (FKeyCAPS_V Or 2);
    48: // 0
      If down Then
        FKey6_0 := (FKey6_0 And (Not 1))
      Else
        FKey6_0 := (FKey6_0 Or 1);
    49: // 1
      If down Then
        FKey1_5 := (FKey1_5 And (Not 1))
      Else
        FKey1_5 := (FKey1_5 Or 1);
    50: // 2
      If down Then
        FKey1_5 := (FKey1_5 And (Not 2))
      Else
        FKey1_5 := (FKey1_5 Or 2);
    51: // 3
      If down Then
        FKey1_5 := (FKey1_5 And (Not 4))
      Else
        FKey1_5 := (FKey1_5 Or 4);
    52: // 4
      If down Then
        FKey1_5 := (FKey1_5 And (Not 8))
      Else
        FKey1_5 := (FKey1_5 Or 8);
    53: // 5
      If down Then
        FKey1_5 := (FKey1_5 And (Not 16))
      Else
        FKey1_5 := (FKey1_5 Or 16);
    54: // 6
      If down Then
        FKey6_0 := (FKey6_0 And (Not 16))
      Else
        FKey6_0 := (FKey6_0 Or 16);
    55: // 7
      If down Then
        FKey6_0 := (FKey6_0 And (Not 8))
      Else
        FKey6_0 := (FKey6_0 Or 8);
    56: // 8
      If down Then
        FKey6_0 := (FKey6_0 And (Not 4))
      Else
        FKey6_0 := (FKey6_0 Or 4);
    57: // 9
      If down Then
        FKey6_0 := (FKey6_0 And (Not 2))
      Else
        FKey6_0 := (FKey6_0 Or 2);
    96: // FKeypad 0
      If down Then
        FKey6_0 := (FKey6_0 And (Not 1))
      Else
        FKey6_0 := (FKey6_0 Or 1);
    97: // FKeypad 1
      If down Then
        FKey1_5 := (FKey1_5 And (Not 1))
      Else
        FKey1_5 := (FKey1_5 Or 1);
    98: // FKeypad 2
      If down Then
        FKey1_5 := (FKey1_5 And (Not 2))
      Else
        FKey1_5 := (FKey1_5 Or 2);
    99: // FKeypad 3
      If down Then
        FKey1_5 := (FKey1_5 And (Not 4))
      Else
        FKey1_5 := (FKey1_5 Or 4);
    100: // FKeypad 4
      If down Then
        FKey1_5 := (FKey1_5 And (Not 8))
      Else
        FKey1_5 := (FKey1_5 Or 8);
    101: // FKeypad 5
      If down Then
        FKey1_5 := (FKey1_5 And (Not 16))
      Else
        FKey1_5 := (FKey1_5 Or 16);
    102: // FKeypad 6
      If down Then
        FKey6_0 := (FKey6_0 And (Not 16))
      Else
        FKey6_0 := (FKey6_0 Or 16);
    103: // FKeypad 7
      If down Then
        FKey6_0 := (FKey6_0 And (Not 8))
      Else
        FKey6_0 := (FKey6_0 Or 8);
    104: // FKeypad 8
      If down Then
        FKey6_0 := (FKey6_0 And (Not 4))
      Else
        FKey6_0 := (FKey6_0 Or 4);
    105: // FKeypad 9
      If down Then
        FKey6_0 := (FKey6_0 And (Not 2))
      Else
        FKey6_0 := (FKey6_0 Or 2);
    106: // FKeypad *
      begin
        If down Then
          FKeyB_SPC := (FKeyB_SPC And Not(18))
        Else
        begin
          If SYMB Then
            FKeyB_SPC := (FKeyB_SPC Or 16)
          Else
            FKeyB_SPC := (FKeyB_SPC Or 18);
        End;
      end;
    107: // FKeypad +
      begin
        If down Then
        begin
          FKeyH_ENT := (FKeyH_ENT And (Not 4));
          FKeyB_SPC := (FKeyB_SPC And (Not 2));
        end
        Else
        begin
          FKeyH_ENT := (FKeyH_ENT Or 4);
          If Not SYMB Then
            FKeyB_SPC := (FKeyB_SPC Or 2);
        End;
      end;
    109: // FKeypad -
      begin
        If down Then
        begin
          FKeyH_ENT := (FKeyH_ENT And (Not 8));
          FKeyB_SPC := (FKeyB_SPC And (Not 2));
        end
        Else
        begin
          FKeyH_ENT := (FKeyH_ENT Or 8);
          If Not SYMB Then
            FKeyB_SPC := (FKeyB_SPC Or 2);
        End;
      end;
    110: // FKeypad .
      begin
        If down Then
          FKeyB_SPC := (FKeyB_SPC And (Not 6))
        Else
        begin
          If SYMB Then
            FKeyB_SPC := (FKeyB_SPC Or 4)
          Else
            FKeyB_SPC := (FKeyB_SPC Or 6);
        End;
      end;
    111: // FKeypad /
      begin
        If down Then
        begin
          FKeyCAPS_V := (FKeyCAPS_V And (Not 16));
          FKeyB_SPC := (FKeyB_SPC And (Not 2));
        end
        Else
        begin
          FKeyCAPS_V := (FKeyCAPS_V Or 16);
          If Not SYMB Then
            FKeyB_SPC := (FKeyB_SPC Or 2);
        End;
      end;
    37: // Left
      begin
        If down Then
        begin
          FKey1_5 := (FKey1_5 And (Not 16));
          FKeyCAPS_V := (FKeyCAPS_V And (Not 1));
        end
        Else
        begin
          FKey1_5 := (FKey1_5 Or 16);
          If Not SYMB Then
            FKeyB_SPC := (FKeyB_SPC Or 2);
        End;
      end;
    38: // Up
      begin
        If down Then
        begin
          FKey6_0 := (FKey6_0 And (Not 8));
          FKeyCAPS_V := (FKeyCAPS_V And (Not 1));
        end
        Else
        begin
          FKey6_0 := (FKey6_0 Or 8);
          If Not CAPS Then
            FKeyCAPS_V := (FKeyCAPS_V Or 1);
        End;
      end;
    39: // Right
      begin
        If down Then
        begin
          FKey6_0 := (FKey6_0 And (Not 4));
          FKeyCAPS_V := (FKeyCAPS_V And (Not 1));
        end
        Else
        begin
          FKey6_0 := (FKey6_0 Or 4);
          If Not CAPS Then
            FKeyCAPS_V := (FKeyCAPS_V Or 1);
        End;
      end;
    40: // Down
      begin
        If down Then
        begin
          FKey6_0 := (FKey6_0 And (Not 16));
          FKeyCAPS_V := (FKeyCAPS_V And (Not 1));
        end
        Else
        begin
          FKey6_0 := (FKey6_0 Or 16);
          If Not CAPS Then
            FKeyCAPS_V := (FKeyCAPS_V Or 1);
        End;
      end;
    13: // RETURN
      If down Then
        FKeyH_ENT := (FKeyH_ENT And (Not 1))
      Else
        FKeyH_ENT := (FKeyH_ENT Or 1);
    32: // SPACE BAR
      If down Then
        FKeyB_SPC := (FKeyB_SPC And (Not 1))
      Else
        FKeyB_SPC := (FKeyB_SPC Or 1);
    187: // =/+ FKey
      begin
        If down Then
        begin
          If CAPS Then
            FKeyH_ENT := (FKeyH_ENT And (Not 4))
          Else
            FKeyH_ENT := (FKeyH_ENT And (Not 2));

          FKeyB_SPC := (FKeyB_SPC And (Not 2));
          FKeyCAPS_V := (FKeyCAPS_V Or 1);
        end
        Else
        begin
          FKeyH_ENT := (FKeyH_ENT Or 4);
          FKeyH_ENT := (FKeyH_ENT Or 2);
          FKeyB_SPC := (FKeyB_SPC Or 2);
        End;
      end;
    189: // -/_ FKey
      begin
        If down Then
        begin
          If CAPS Then
            FKey6_0 := (FKey6_0 And (Not 1))
          Else
            FKeyH_ENT := (FKeyH_ENT And (Not 8));

          FKeyB_SPC := (FKeyB_SPC And (Not 2));
          FKeyCAPS_V := (FKeyCAPS_V Or 1);
        end
        Else
        begin
          FKey6_0 := (FKey6_0 Or 1); // Release the Spectrum's '0' FKey
          FKeyH_ENT := (FKeyH_ENT Or 8); // Release the Spectrum's 'J' FKey
          FKeyB_SPC := (FKeyB_SPC Or 2); // Release the Symbol Shift FKey
        End;
      end;
    186: // ;/: FKeys
      begin
        If down Then
        begin
          If CAPS Then
            FKeyCAPS_V := (FKeyCAPS_V And (Not 2))
          Else
            FKeyY_P := (FKeyY_P And (Not 2));

          FKeyB_SPC := (FKeyB_SPC And (Not 2));
          FKeyCAPS_V := (FKeyCAPS_V Or 1);
        end
        Else
        begin
          FKeyCAPS_V := (FKeyCAPS_V Or 2);
          FKeyY_P := (FKeyY_P Or 2);
          FKeyB_SPC := (FKeyB_SPC Or 2);
        End;
      end;
  End;
End;



function TZXSpectrum.GetRegister: TZ80Register;
begin
  result := FCPU.Register;
end;

procedure TZXSpectrum.NMI;
begin
  // FLastOut1FFD := FLastOut1FFD or 2;
  FCPU.NMI(Z80_NMI);
end;

function TZXSpectrum.GetModel: Integer;
begin
  Result := FCurrentModel;
end;

(*
  Zx Spectrum 48 69888 ts Contention model 1
  Zx Spectrum 128/+2 70908 Ts Contention model 2
  Zx Spectrum 128/+2 70908 Ts Contention model 3
  Pentagon 71680 ts Contention model 3
  Scorpion 69888 ts  Contention model 1
*)

procedure TZXSpectrum.SetModel(const AModel: Integer);
begin
  FCurrentModel := AModel;
  FKeyPortMask := $BF;
  // Mask used for reading keyboard port ($BF on Speccys, $1F on TC2048)
  case AModel of
    tmZX48:
      begin
        // A 48K Spectrum has 69888 tstates per interrupt (3.50000 MHz)
        FName := 'ZX Spectrum 48K';
        FStatesPerInterrupt := 69888;
        FTstatesPerLine := 224;
        FWaveAddTStates := 158;
        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 1;
        FPageAt[3] := 2;
        FPageAt[4] := nRomstart;
        // load the ROM image into memory
        LoadRom('spectrum.rom', nRomstart);
      end;
    tmZX128:
      begin
        // A 128K Spectrum has 70908 tstates per interrupt (3.54690 MHz)

        FName := 'ZX Spectrum 128';
        FStatesPerInterrupt := 70908;
        FTstatesPerLine := 228;
        FWaveAddTStates := 160;

        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 2;
        FPageAt[3] := 0;
        FPageAt[4] := nRomstart;

        LoadRom('zx128_1.rom', nRomstart + 1);
        LoadRom('zx128_0.rom', nRomstart);
      end;
    tmZXPlus2:
      begin
        // A Spectrum +2 has 70908 tstates per interrupt (3.54690 MHz)
        FName := 'ZX Spectrum +2';
        FStatesPerInterrupt := 70908;
        FTstatesPerLine := 228;
        FWaveAddTStates := 160;
        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 2;
        FPageAt[3] := 0;
        FPageAt[4] := nRomstart;
        LoadRom('plus2_1.rom', nRomstart + 1);
        LoadRom('plus2_0.rom', nRomstart);
      end;
    tmZXPlus2a: // +2A
      begin
        FName := 'ZX Spectrum +2A';
        FStatesPerInterrupt := 70908;
        FTstatesPerLine := 228;
        FWaveAddTStates := 160;
        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 1;
        FPageAt[3] := 2;
        FPageAt[4] := nRomstart;

        LoadRom('plus2a_3.rom', nRomstart + 3);
        LoadRom('plus2a_2.rom', nRomstart + 2);
        LoadRom('plus2a_1.rom', nRomstart + 1);
        LoadRom('plus2a_0.rom', nRomstart);
      end;
    tmZXPlus3: // +3
      begin
        FName := 'ZX Spectrum +3';
        FStatesPerInterrupt := 70908;
        FTstatesPerLine := 228;
        FWaveAddTStates := 160;
        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 1;
        FPageAt[3] := 2;
        FPageAt[4] := nRomstart;

        LoadRom('plus3_3.rom', nRomstart + 3);
        LoadRom('plus3_2.rom', nRomstart + 2);
        LoadRom('plus3_1.rom', nRomstart + 1);
        LoadRom('plus3_0.rom', nRomstart);
      end;
    tmPentagon: // Pentagon
      begin
        FName := 'Pentagon 128k';
        FStatesPerInterrupt := 71680;
        FTstatesPerLine := 228;
        FWaveAddTStates := 160;
        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 2;
        FPageAt[3] := 0;
        FPageAt[4] := nRomstart;

        LoadRom('trdos.rom', nRomstart + 3);
        LoadRom('zx128_1.rom', nRomstart + 1);
        LoadRom('128tr.rom', nRomstart);
      end;
    tmScorpion: // Scorpion
      begin
        FName := 'Scorpion 256k';
        FStatesPerInterrupt := 70908; // 69888;
        FTstatesPerLine := 228;
        FWaveAddTStates := 160;
        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 2;
        FPageAt[3] := 0;
        FPageAt[4] := nRomstart;

        LoadRom('scorp3.rom', nRomstart + 3);
        LoadRom('scorp2.rom', nRomstart + 2);
        LoadRom('scorp1.rom', nRomstart + 1);
        LoadRom('scorp0.rom', nRomstart);
      end;
    tmTimex:
      begin
        FName := 'Timex TC2048';
        FStatesPerInterrupt := 69888;
        FWaveAddTStates := 158;

        // FMemPagingType := 0;
        FUseScreen := 5;
        FKeyPortMask := $1F;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 1;
        FPageAt[3] := 2;
        FPageAt[4] := nRomstart;

        FTstatesPerLine := 224;
        // FTStatesAtTop := -FTstatesPerInterrupt + 14336;
        // glTStatesAtBottom := -FTstatesPerInterrupt + 14336 + 43007;

        LoadRom('tc2048.rom', nRomstart);
      end;
    tmDidaktik:
      begin
        // A 48K Spectrum has 69888 tstates per interrupt (3.50000 MHz)
        FName := 'Didaktik';
        FStatesPerInterrupt := 69888;
        FTstatesPerLine := 224;
        FWaveAddTStates := 158;
        FUseScreen := 5;

        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 1;
        FPageAt[3] := 2;
        FPageAt[4] := nRomstart;
        // load the ROM image into memory
        LoadRom('didaktik 90.rom', nRomstart);
      end;

  end;

  FreeAndNil(FAY8912);
  FreeAndNil(FBetaDisk);

  if AModel in [tmPentagon, tmScorpion] then // Pentagon&Scorpion
    FBetaDisk := TBetaDisk.Create;

  if not Is48KModel then
    FAY8912 := TAY8912.Create(1773000, 22050, 8);

  if FEmulatedMF128 then
  begin
    if AModel in [tmZXPlus2a, tmZXPlus3] then
      LoadRom('mfplus3.rom', nMF128start)
    else
      LoadRom('mf128.rom', nMF128start);
  end;

  FGlobalTStates := -FStatesPerInterrupt;
end;

procedure TZXSpectrum.InByte(const addr: Word; out Value: Byte);
var
  i: Integer;
  port: TPair absolute addr;
begin
  Value := 255;

  if assigned(FBetaDisk) then
  begin // TRDOS
    if (FPageAt[0] = nRomstart + 3) and (port.l and 31 > 0) then
    begin
      i := (port.l shr 4) and $E;
      if i < 8 then
        FBetaDisk.diskVG(i, Value)
      else if i = $E then
        FBetaDisk.diskVG(8, Value);
      exit;
    end;
  end;

  case port.l of
    31:
      Value := FJoystick; (* Kempston *)
    63:
      begin
        if FCurrentModel = tmZXPlus3 then
          Value := MF3inb(port.W)
        else if FEmulatedMF128 then
          Value := MF128inb(port.l);
      end;
    191:
      begin
        if FCurrentModel = tmScorpion then (* Setup Page In *)
        begin
          FPageAt[0] := nRomstart + 3;
          if FLastOut7FFD and 8 > 0 then
            Value := $FF
          else
            Value := $7F;
        end
        else
        begin
          if FCurrentModel = tmZXPlus3 then
            Value := MF3inb(port.W)
          else if FEmulatedMF128 then
            Value := MF128inb(port.l);
        end;
      end;
    253:
      if assigned(FAY8912) then
        Value := FAY8912.Regs[FSoundRegister]
      else
        Value := 0;
    254:
      begin
        if port.h and 128 = 0 then
          Value := Value and FKeyB_SPC;
        if port.h and 064 = 0 then
          Value := Value and FKeyH_ENT;
        if port.h and 032 = 0 then
          Value := Value and FKeyY_P;
        if port.h and 016 = 0 then
          Value := Value and FKey6_0;
        if port.h and 008 = 0 then
          Value := Value and FKey1_5;
        if port.h and 004 = 0 then
          Value := Value and FKeyQ_T;
        if port.h and 002 = 0 then
          Value := Value and FKeyA_G;
        if port.h and 001 = 0 then
          Value := Value and FKeyCAPS_V;

        Value := (Value and FKeyPortMask);
        if assigned(FTape) and (FTape.IsRealTime) and (FTape.IsPlaying) then
          Value := Value or FTape.EarBit;
      end;
    255: (* Vertical Retrace (not on +3 or +2A) *)
      if not(FCurrentModel in [tmZXPlus2a, tmZXPlus3]) then
        Value := FGLVLine and 255;
  end;
end;

procedure TZXSpectrum.OutByte(const addr: Word; const Value: Byte);
var
  port: TPair absolute addr;
  bTemp: Byte;

  procedure plus3OutBankm128(const val: Byte);
  begin
    if (FLastOut7FFD = val) or (FLastOut7FFD and $20 > 0) then
      exit;
    if (val and 8 > 0) then
      FUseScreen := 7
    else
      FUseScreen := 5;

    if (FLastOut1FFD and 1 = 0) then
    begin
      FPageAt[1] := 5;
      FPageAt[2] := 2;
      FPageAt[3] := val and 7;
      if FPageAt[0] <> nRomstart + 4 { MF3 } then
        FPageAt[0] := nRomstart + (((val and 16) shr 4) or
          ((FLastOut1FFD and 4) shr 1));
    end;

    FLastOut7FFD := val;
  end;

  procedure plus3OutBank678(const val: Byte);
  begin
    if (FLastOut1FFD = val) or (FLastOut7FFD and $20 > 0) then
      exit;
    if (FLastOut1FFD and 1 = 0) then
    begin
      if FPageAt[0] <> nRomstart + 4 { MF3 } then
        FPageAt[0] := nRomstart + (((FLastOut7FFD and 16) shr 4) or
          ((val and 4) shr 1));
    end
    else
    begin
      case (val and 6) shr 1 of
        0:
          begin
            FPageAt[0] := 0;
            FPageAt[1] := 1;
            FPageAt[2] := 2;
            FPageAt[3] := 3;
          end;
        1:
          begin
            FPageAt[0] := 4;
            FPageAt[1] := 5;
            FPageAt[2] := 6;
            FPageAt[3] := 7;
          end;
        2:
          begin
            FPageAt[0] := 4;
            FPageAt[1] := 5;
            FPageAt[2] := 6;
            FPageAt[3] := 3;
          end;
        3:
          begin
            FPageAt[0] := 4;
            FPageAt[1] := 7;
            FPageAt[2] := 6;
            FPageAt[3] := 3;
          end;
      end;
    end;

    FLastOut1FFD := val;
  end;
(*
  procedure scorOutBankm128(const val: Byte);
  begin
  if (FLastOut7FFD and $20 > 0) then
  exit;
  if (val and 8 > 0) then
  FUseScreen := 7
  else
  FUseScreen := 5;

  FPageAt[3] := ((FLastOut1FFD and 16) shr 1) or (val and 7);

  if (FLastOut1FFD and 3 = 0) then
  FPageAt[0] := nromstart + ((val and 16) shr 4);

  FLastOut7FFD := val;
  end;

  procedure ScorOutBankmX(const val: Byte);
  begin
  case val and (1 or 2) of
  1:
  FPageAt[0] := 8;
  2:
  FPageAt[0] := nromstart + 2;
  3:;
  else
  FPageAt[0] := nromstart + (FLastOut7FFD and 16) shr 4;
  end;

  FPageAt[3] := ((val and 16) shr 1) or (FLastOut7FFD and 7);

  FLastOut1FFD := val;
  end;
*)

  procedure scorpion_update_memory();
  begin
    if (FLastOut7FFD and 8 > 0) then
      FUseScreen := 7
    else
      FUseScreen := 5;

    FPageAt[3] := (FLastOut7FFD and 7) or (FLastOut1FFD and $10) shr 1;
    // outputdebugstring(pchar(format('PageAt=%d, FLastOut7FFD=%d, ' +
    // 'FLastOut1FFD=%d', [FPageAt[3], FLastOut7FFD, FLastOut1FFD])));
    if ((FLastOut1FFD and 1) = 1) then
      FPageAt[0] := nRomstart + 8
    else
    begin
      if (FLastOut1FFD and $02) = $02 then
        FPageAt[0] := nRomstart + 2
      else
        FPageAt[0] := nRomstart + (FLastOut7FFD and 16) shr 4;
    end;
  end;

var
  OutByte: Byte;

begin
  OutByte := Value;

  if assigned(FBetaDisk) and (FPageAt[0] = nRomstart + 3) and (port.l and 31 > 0)
  then
  begin
    bTemp := (port.l shr 4);
    if bTemp < 8 then
      FBetaDisk.diskVG(bTemp, OutByte)
    else if bTemp = $F then
      FBetaDisk.diskVG(9, OutByte);
  end;

  if (port.l and $1) = 0 then
  begin
    FGPU.Border := (OutByte and $7);

    if ((OutByte and 16) > 0) then
      FBeeperVal := 31
    else
      FBeeperVal := 0;
  end;

  if (not Is48KModel) and (port.l = 253) then
  begin
    bTemp := port.h shr 6;
    case bTemp of
      0, 1:
        begin
          case FCurrentModel of
            tmZXPlus2a, tmZXPlus3:
              begin
                if bTemp = 0 then
                  plus3OutBank678(OutByte)
                else
                  plus3OutBankm128(OutByte);
              end;
            tmScorpion:
              begin
                if bTemp = 0 then
                begin
                  FLastOut1FFD := OutByte;
                  scorpion_update_memory();
                end
                else if (FLastOut7FFD <> OutByte) and (FLastOut7FFD and $20 = 0)
                then
                begin
                  FLastOut7FFD := OutByte;
                  scorpion_update_memory();
                end;
              end
          else
            begin
              FPageAt[3] := OutByte and 7;
              if (OutByte and 8) > 0 then
                FUseScreen := 7
              else
                FUseScreen := 5;
              if FPageAt[0] <> nRomstart + 4 then
                FPageAt[0] := nRomstart + (OutByte and 16) shr 4;

              FLastOut7FFD := OutByte;
            end;
          end;
        end;
      2:
        if assigned(FAY8912) then
          FAY8912.Regs[FSoundRegister] := OutByte;
      3:
        FSoundRegister := OutByte and $F;
    end;
  end;
end;

// Snapshot Loader

procedure TZXSpectrum.LoadSNA128Snap(const AStream: TStream);
var
  sData: array [0 .. 65535] of Byte;
  sTemp: array [0 .. 1] of Byte;
  lOut7FFD: Integer;
  lBank: Integer;
  lCounter: Integer;
begin
  // Read first three banks
  AStream.Read(sData, 49152);

  // PC
  AStream.Read(sTemp, 2);
  FCPU.Register.pc.W := 256 * sTemp[1] + sTemp[0];

  // Last out to 0x7FFD
  AStream.Read(sTemp, 1);
  lOut7FFD := sTemp[0];

  // Is TR-DOS paged? (ignored by vbSpec)
  AStream.Read(sTemp, 1);

  // Setup first three banks
  for lCounter := 0 to 16383 do
  begin
    FRAMPage[5, lCounter] := sData[lCounter];
    FRAMPage[2, lCounter] := sData[lCounter + 16384];
    FRAMPage[lOut7FFD and 7, lCounter] := sData[lCounter + 32768];
  end;

  lBank := 0;
  while lBank < 8 do
  begin
    AStream.Read(sData, 16384);
    if (lBank = 5) or (lBank = 2) or (lBank = (lOut7FFD and 7)) then
      lBank := lBank + 1;

    if lBank < 8 then
    begin
      for lCounter := 0 to 16383 do
        FRAMPage[lBank, lCounter] := sData[lCounter];

      lBank := lBank + 1;
    end;
  end;

  OutByte($7FFD, lOut7FFD);
end;

procedure TZXSpectrum.LoadSNASnap(const AFilename: string);
var
  Stream: TFileStream;
  sData: array [0 .. 65535] of Byte;
  iCounter: Integer;
  temp: Integer;
  glNewBorder: Byte;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    with FCPU.Register do
    begin
      Stream.Read(sData, 1);
      ir.h := sData[0];

      Stream.Read(sData, 2);
      HL2.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      DE2.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      BC2.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      af2.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      HL.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      de.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      BC.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      IY.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      ix.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 1);
      if (sData[0] and 4) = 4 then
      begin
        iff1 := 1;
        iff2 := 1;
      end
      else
      begin
        iff1 := 0;
        iff2 := 0;
      end;

      Stream.Read(sData, 1);
      ir.l := sData[0];
      RTemp := ir.l;

      Stream.Read(sData, 2);
      af.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 2);
      SP.W := 256 * sData[1] + sData[0];

      Stream.Read(sData, 1);
      im := sData[0];

      Stream.Read(sData, 1);
      // Border color
      glNewBorder := sData[0] and $7;

      temp := Stream.Size;
      if temp > 49180 then
      begin
        LoadSNA128Snap(Stream);

        // Set the initial border color
        FGPU.Border := glNewBorder;

        SetModel(tmZX128);
        resetKeyboard;
      end
      else
      begin
        // Load a 48K Snapshot file
        Stream.Read(sData, 49152);

        for iCounter := 0 to 16383 do
        begin
          FRAMPage[5, iCounter] := sData[iCounter];
          FRAMPage[1, iCounter] := sData[iCounter + 16384];
          FRAMPage[2, iCounter] := sData[iCounter + 32768];
        end;

        FGPU.Border := glNewBorder;

        SetModel(tmZX48);

        resetKeyboard;
        FCPU.pop(pc.W);
      end;
    end;
  finally
    Stream.free;
  end;
end;

procedure TZXSpectrum.ReadZ80V2orV3Snap(const AStream: TStream);
var
  lHeaderLen: Integer;
  sData: array [0 .. 65535] of Byte;
  lCounter: Integer;
  bHardwareSupported: Boolean;
  lMemPage: Integer;
  lBlockCounter: Integer;
  b128K: Boolean;
  lMemPos: Integer;
  lOutFFFD: Integer;
  sModel: string;
  Size: Integer;
begin
  with FCPU.Register do
  begin
    lCounter := 0;
    AStream.Read(sData, 2);
    lHeaderLen := 256 * sData[1] + sData[0];
    b128K := false;

    // offset 32 - 2 bytes - PC
    if lCounter < lHeaderLen then
    begin
      AStream.Read(sData, 2);
      pc.W := 256 * sData[1] + sData[0];
      lCounter := lCounter + 2;
    end;

    // offset 34 - 1 byte - hardware mode
    if lCounter < lHeaderLen then
    begin
      AStream.Read(sData, 1);
      case sData[0] of
        0: // 48K spectrum
          begin
            bHardwareSupported := true;
            SetModel(tmZX48);
          end;
        1:
          // 48K spectrum + Interface 1
          begin
            bHardwareSupported := true;
            SetModel(tmZX48);
          end;
        2: // SamROM
          begin
            sModel := 'SamROM';
            bHardwareSupported := false;
          end;
        3:
          begin
            if lHeaderLen = 23 then
            begin
              sModel := '128K Spectrum';
              bHardwareSupported := true;
              b128K := true;
              SetModel(tmZX128);
            end
            else
            begin
              sModel := '48K Spectrum + M.G.T.';
              bHardwareSupported := true;
              SetModel(tmZX48);
            end;
          end;
        4:
          begin
            if lHeaderLen = 23 then
            begin
              sModel := '128K Spectrum + Interface 1';
              bHardwareSupported := true;
              b128K := true;
              SetModel(tmZX128);
            end
            else
            begin
              sModel := '128K Spectrum';
              bHardwareSupported := true;
              b128K := true;
              SetModel(tmZX128);
            end;
          end;
        5:
          begin
            bHardwareSupported := true;
            sModel := '128K Spectrum + Interface 1';
            b128K := true;
            SetModel(tmZX128);
          end;
        6:
          begin
            bHardwareSupported := false;
            sModel := '128K Spectrum + M.G.T.';
            b128K := true;
            SetModel(tmZX128);
          end;
        7:
          begin
            bHardwareSupported := false;
            sModel := 'ZX Spectrum +3';
            b128K := true;
            SetModel(tmZXPlus3);
          end;
      else
        begin
          bHardwareSupported := false;
          sModel := 'Unknown hardware platform';
        end;
      end;

      if bHardwareSupported = false then
        exit;

      lCounter := lCounter + 1;
    end;

    // offset 35 - 1 byte - last out to 0x7FFD - not required for 48K spectrum
    if lCounter < lHeaderLen then
    begin
      if b128K then
      begin
        AStream.Read(sData, 1);
        OutByte($7FFD, sData[0]);
      end
      else
      begin
        AStream.Read(sData, 1);
        FPageAt[0] := nRomstart;
        FPageAt[1] := 5;
        FPageAt[2] := 1;
        FPageAt[3] := 2;
        FPageAt[4] := nRomstart;
      end;
      lCounter := lCounter + 1;
    end;

    // offset 36 - 1 byte - 0xFF if Interface 1 ROM is paged in
    if lCounter < lHeaderLen then
    begin
      AStream.Read(sData, 1);
      lCounter := lCounter + 1;

      // if sData[0] = 255 then 'This snapshot was saved with the Interface 1 ROM paged in. It may not run correctly.',
    end;

    // offset 37 - 1 byte (bit 0: 1=intR emulation on, bit 1: 1=LDIR emulation on
    if lCounter < lHeaderLen then
    begin
      AStream.Read(sData, 1);
      lCounter := lCounter + 1;
    end;

    // offset 38 - Last out to 0xFFFD (+2/+3 sound chip register number)
    lOutFFFD := 0;
    if lCounter < lHeaderLen then
    begin
      AStream.Read(sData, 1);
      lOutFFFD := sData[0];
      lCounter := lCounter + 1;
    end;

    // offset 39 - 16 bytes - contents of the sound chip registers
    if lCounter < lHeaderLen then
    begin
      AStream.Read(sData, 16);
      if b128K and assigned(FAY8912) then
        with FAY8912 do
        begin
          Regs[0] := sData[0];
          Regs[1] := sData[1];
          Regs[2] := sData[2];
          Regs[3] := sData[3];
          Regs[4] := sData[4];
          Regs[5] := sData[5];
          Regs[6] := sData[6];
          Regs[7] := sData[7];
          Regs[8] := sData[8];
          Regs[9] := sData[9];
          Regs[10] := sData[10];
          Regs[11] := sData[11];
          Regs[12] := sData[12];
          Regs[13] := sData[13];
          Regs[14] := sData[14];
          Regs[15] := sData[15];
        end;
      lCounter := lCounter + 16;
    end;

    if b128K then
      OutByte($FFFD, lOutFFFD);

    // read the remaining bytes of the header (we don't care what information they hold)
    if lCounter < lHeaderLen then
      AStream.Read(sData, lHeaderLen - lCounter);

    repeat
      // read a block
      AStream.Read(sData, 2);
      if AStream.Position = AStream.Size then
        break;
      lHeaderLen := 256 * sData[1] + sData[0];
      AStream.Read(sData, 1);
      case sData[0] of
        0: // Spectrum ROM
          if b128K then
            lMemPage := 9
          else
            lMemPage := 8;
        1:
          // Interface 1 ROM, or similar (we discard these blocks)
          lMemPage := -1;
        2: // 128K ROM (reset)
          if b128K then
            lMemPage := 8
          else
            lMemPage := -1;
        3:
          // Page 0 (not used by 48K Spectrum)
          if b128K then
            lMemPage := 0
          else
            lMemPage := -1;
        4:
          // Page 1 RAM at 0x8000
          lMemPage := 1;
        5: // Page 2 RAM at 0xC000
          lMemPage := 2;
        6: // Page 3 (not used by 48K Spectrum)
          if b128K then
            lMemPage := 3
          else
            lMemPage := -1;
        7:
          // Page 4 (not used by 48K Spectrum)
          if b128K then
            lMemPage := 4
          else
            lMemPage := -1;
        8:
          // Page 5 RAM at 0x4000
          lMemPage := 5;
        9: // Page 6 (not used by 48K Spectrum)
          if b128K then
            lMemPage := 6
          else
            lMemPage := -1;
        10:
          // Page 7 (not used by 48K Spectrum)
          if b128K then
            lMemPage := 7
          else
            lMemPage := -1;
        11: // Multiface ROM
          lMemPage := -1;
      else
        lMemPage := -1;
      end;

      Size := AStream.Read(sData, lHeaderLen);

      if lMemPage <> -1 then
      begin
        if lHeaderLen = $FFFF then
        begin
          // Not a compressed block, just copy it straight into RAM
          for lCounter := 0 to 16383 do
            FRAMPage[lMemPage, lCounter] := sData[lCounter];
        end
        else
        begin
          // Uncompress the block to memory
          lCounter := 0;
          lMemPos := 0;
          repeat
            if sData[lCounter] = $ED then
            begin
              if sData[lCounter + 1] = $ED then
              begin
                // This is an encoded block
                lCounter := lCounter + 2;
                lHeaderLen := sData[lCounter];
                lCounter := lCounter + 1;
                for lBlockCounter := 0 to lHeaderLen - 1 do
                  FRAMPage[lMemPage, lMemPos + lBlockCounter] :=
                    sData[lCounter];

                lMemPos := lMemPos + lHeaderLen;
              end
              else
              begin
                // Just a single ED, write it out
                FRAMPage[lMemPage, lMemPos] := $ED;
                lMemPos := lMemPos + 1;
              end;
            end
            else
            begin
              FRAMPage[lMemPage, lMemPos] := sData[lCounter];
              inc(lMemPos);
            end;
            inc(lCounter);
          until lCounter >= Size;
        end;
      end;
    until AStream.Position = AStream.Size;
  end;
end;

procedure TZXSpectrum.ReadZ80V1Snap(const AStream: TStream);
var
  lDataLen: Integer;
  sData: array [0 .. 65535] of Byte;
  lBlockLen: Integer;
  lCounter: Integer;
  lMemPos: Integer;
  lBlockCounter: Integer;
  Size: Integer;
begin
  lDataLen := AStream.Size - AStream.Position;

  // read the compressed data into sData
  Size := AStream.Read(sData, lDataLen);

  // Uncompress the block to memory
  lCounter := 0;
  lMemPos := 16384;
  with FCPU.Register do
    repeat
      if sData[lCounter] = $ED then
      begin
        if sData[lCounter + 1] = $ED then
        begin
          // This is an encoded block
          lCounter := lCounter + 2;
          lBlockLen := sData[lCounter];
          inc(lCounter);
          for lBlockCounter := 1 to lBlockLen do
          begin
            FRAMPage[FPageAt[lMemPos div 16384], lMemPos and 16383] :=
              sData[lCounter];
            inc(lMemPos);
          end
        end
        else
        begin
          // Just a single ED, write it out
          FRAMPage[FPageAt[lMemPos div 16384], lMemPos and 16383] := $ED;
          inc(lMemPos);
        end
      end
      else
      begin
        FRAMPage[FPageAt[lMemPos div 16384], lMemPos and 16383] :=
          sData[lCounter];
        inc(lMemPos);
      end;
      inc(lCounter);
    until lCounter > Size - 4;

  (*
    if (sData[lCounter - 1] <> 0)
    or (sData[lCounter] <> $ED)
    or (sData[lCounter + 1] <> $ED)
    or (sData[lCounter + 2] <> 0) then
    MessageDLG(
    'Error in compressed Z80 file. Block end marker 0x00EDED00 is not present.',
    mterror, [mbok], 0)
  *)
end;

procedure TZXSpectrum.LoadZ80Snap(const AFilename: string);
var
  Stream: TFileStream;
  sData: array [0 .. 65535] of Byte;
  iCounter: Integer;
  bCompressed: Boolean;
  glNewBorder: Byte;
begin
  Stream := TFileStream.Create(AFilename, fmOpenRead);
  try
    with FCPU.Register do
    begin
      FPageAt[0] := nRomstart;
      FPageAt[1] := 5;
      FPageAt[2] := 2;
      FPageAt[3] := 0;
      FPageAt[4] := nRomstart;

      if assigned(FAY8912) then
        FAY8912.Reset;

      // byte 0 - A register
      Stream.Read(sData, 1);
      af.h := sData[0];

      // byte 1 - F register
      Stream.Read(sData, 1);
      af.l := sData[0];

      // bytes 2 + 3 - BC register pair (C first, then B)
      Stream.Read(sData, 2);
      BC.W := 256 * sData[1] + sData[0];

      // bytes 4 + 5 - HL register pair
      Stream.Read(sData, 2);
      HL.W := 256 * sData[1] + sData[0];

      // bytes 6 + 7 - PC (this is zero for v2.x or v3.0 Z80 files)
      Stream.Read(sData, 2);
      pc.W := 256 * sData[1] + sData[0];

      // bytes 8 + 9 - SP
      Stream.Read(sData, 2);
      SP.W := 256 * sData[1] + sData[0];

      // byte 10 - Interrupt register
      Stream.Read(sData, 1);
      ir.h := sData[0];

      // byte 11 - Refresh register
      Stream.Read(sData, 1);
      ir.l := (sData[0] and 127);

      // byte 12 - bitfield
      Stream.Read(sData, 1);
      // if byte 12 = 255 then it must be treated as if it = 1, for compatibility with other emulators
      if sData[0] = 255 then
        sData[0] := 1;
      // bit 0 - bit 7 of R
      if (sData[0] and 1) = 1 then
        ir.l := ir.l or 128;
      RTemp := ir.l;

      // bits 1,2 and 3 - border color
      glNewBorder := (sData[0] and 14) div 2;
      // bit 4 - 1 if SamROM switched in (we don't care about this!)
      // bit 5 - if 1 and PC<>0 then the snapshot is compressed using the
      // rudimentary Z80 run-length encoding scheme
      bCompressed := false;
      if (sData[0] and $20) = $20 then
        bCompressed := true;
      // bits 6 + 7 - no meaning

      // bytes 13 + 14 - DE register pair
      Stream.Read(sData, 2);
      de.W := 256 * sData[1] + sData[0];

      // bytes 15 + 16 - BC' register pair
      Stream.Read(sData, 2);
      BC2.W := 256 * sData[1] + sData[0];

      // bytes 17 + 18 - DE' register pair
      Stream.Read(sData, 2);
      DE2.W := 256 * sData[1] + sData[0];

      // bytes 19 + 20 - HL' register pair
      Stream.Read(sData, 2);
      HL2.W := 256 * sData[1] + sData[0];

      // bytes 21 + 22 - AF' register pair (A first then F - not Z80 byte order!!)
      Stream.Read(sData, 2);
      af2.W := 256 * sData[0] + sData[1];

      // byte 23 + 24 - IY register pair
      Stream.Read(sData, 2);
      IY.W := 256 * sData[1] + sData[0];

      // byte 25 + 26 - IX register pair
      Stream.Read(sData, 2);
      ix.W := 256 * sData[1] + sData[0];

      // byte 27 - Interrupt flipflop (0=DI, else EI)
      Stream.Read(sData, 1);
      if sData[0] = 0 then
      begin
        iff1 := 0;
        iff2 := 0;
      end
      else
      begin
        iff1 := 1;
        iff2 := 1;
      end;
      // byte 28 - IFF2 (ignored)
      Stream.Read(sData, 1);

      // byte 29 - Interrupt mode (bits 2 - 7 contain info about joystick modes etc, which we ignore)
      Stream.Read(sData, 1);
      im := sData[0] and 3;

      if pc.W = 0 then
      begin
        // This is a V2 or V3 Z80 file
        ReadZ80V2orV3Snap(Stream);
      end
      else
      begin
        SetModel(tmZX48);
        // V1 .Z80 snapshots are all 48K
        // PC<>0, so lets check to see if this is a compressed V1 Z80 file
        if bCompressed then
        begin
          // Uncompress the RAM data
          ReadZ80V1Snap(Stream);
        end
        else
        begin
          // Uncompressed Z80 file
          Stream.Read(sData, 49152);

          // Copy the RAM data to addressable memory space
          for iCounter := 16384 to 65535 do
            FRAMPage[FPageAt[iCounter div 16384], iCounter and 16383] :=
              sData[iCounter - 16384];
        end;
      end;

      // Set the initial border color
      FGPU.Border := (glNewBorder and 7);
      resetKeyboard;
    end;
  finally
    Stream.free;
  end;
end;

procedure TZXSpectrum.SaveSNA128Snap(const AStream: TStream);
var
  lBank: Integer;
  lCounter: Integer;
var
  bDummy: Byte;
begin
  with FCPU.Register do
  begin
    AStream.Write(ir.h, 1);
    AStream.Write(HL2.W, 2);
    AStream.Write(DE2.W, 2);
    AStream.Write(BC2.W, 2);
    AStream.Write(af2.W, 2);

    AStream.Write(HL.W, 2);
    AStream.Write(de.W, 2);
    AStream.Write(BC.W, 2);
    AStream.Write(IY.W, 2);
    AStream.Write(ix.W, 2);

    // Interrupt flipflops
    if iff1 = 1 then
      bDummy := 4
    else
      bDummy := 0;
    AStream.Write(bDummy, 1);

    // R
    RTemp := RTemp and 127;
    bDummy := (ir.l and $80) or RTemp;
    AStream.Write(bDummy, 1);

    // AF
    AStream.Write(af.W, 2);

    // SP
    AStream.Write(SP.W, 2);

    // Interrupt Mode
    AStream.Write(im, 1);

    bDummy := FGPU.Border;
    AStream.Write(bDummy, 1);

    // Save the three currently-paged RAM banks
    for lCounter := 1 to 3 do
      AStream.Write(FRAMPage[FPageAt[lCounter]], 16384);

    // PC
    AStream.Write(pc.W, 2);

    // Last out to 0x7FFD
    AStream.Write(FLastOut7FFD, 1);

    // Is TR-DOS paged? (0=not paged, 1=paged)
    bDummy := Byte(assigned(FBetaDisk));
    AStream.Write(bDummy, 1);

    // Save the remaining RAM banks
    lBank := 0;
    while lBank < 8 do
    begin
      if (lBank <> FPageAt[1]) and (lBank <> FPageAt[2]) and
        (lBank <> FPageAt[3]) then
        AStream.Write(FRAMPage[lBank], 16384);
      inc(lBank);
    end;
  end;
end;

procedure TZXSpectrum.SaveSNASnap(const AFilename: string);
var
  Stream: TFileStream;
  lCounter: Integer;
  bDummy: Byte;
begin
  Stream := TFileStream.Create(AFilename, fmCreate);
  try
    with FCPU.Register do
    begin
      if FCurrentModel <> tmZX48 then
      begin
        // We're not running in 48K mode
        SaveSNA128Snap(Stream);
      end
      else
      begin
        FCPU.push(pc.W);

        Stream.Write(ir.h, 1);
        Stream.Write(HL2.W, 2);
        Stream.Write(DE2.W, 2);
        Stream.Write(BC2.W, 2);
        Stream.Write(af2.W, 2);

        Stream.Write(HL.W, 2);
        Stream.Write(de.W, 2);
        Stream.Write(BC.W, 2);
        Stream.Write(IY.W, 2);
        Stream.Write(ix.W, 2);

        // Interrupt flipflops
        if iff1 = 1 then
          bDummy := 4
        else
          bDummy := 0;
        Stream.Write(bDummy, 1);

        // R
        RTemp := RTemp and 127;
        bDummy := (ir.l and $80) or RTemp;
        Stream.Write(bDummy, 1);

        // AF
        Stream.Write(af.W, 2);

        // SP
        Stream.Write(SP.W, 2);

        // Interrupt Mode
        Stream.Write(im, 1);

        Stream.Write(FGPU.Border, 1);

        for lCounter := 0 to 16383 do
          Stream.Write(FRAMPage[FPageAt[1], lCounter], 1);
        for lCounter := 0 to 16383 do
          Stream.Write(FRAMPage[FPageAt[2], lCounter], 1);
        for lCounter := 0 to 16383 do
          Stream.Write(FRAMPage[FPageAt[3], lCounter], 1);

        FCPU.pop(pc.W);
      end;
    end;
  finally
    Stream.free;
  end;
end;

function TZXSpectrum.CompressMemoryBlock(lBlock: Integer; pData: pByte)
  : Integer;
var
  pDataStart: pByte;
  ii: Integer;
  lCounter: Integer;
  lCounter2: Integer;
  lByteCount: Integer;
begin
  pDataStart := pData;
  lCounter := 0;
  repeat
    if FRAMPage[lBlock, lCounter] = FRAMPage[lBlock, lCounter + 1] then
    begin
      // String of two or more bytes starting from lCounter
      // Might be a candidate for compression
      lCounter2 := lCounter;
      while (lCounter2 - lCounter) < 260 do
      begin
        if (lCounter2 >= 16383) then
          break;

        if FRAMPage[lBlock, lCounter2 + 1] = FRAMPage[lBlock, lCounter2] then
          lCounter2 := lCounter2 + 1
        else
          break;
      end;
      lByteCount := lCounter2 - lCounter + 1;
      lCounter := lCounter2 + 1;
      if lByteCount > 255 then
      begin
        lCounter := lCounter - (lByteCount - 255);
        lByteCount := 255;
      end;

      if lByteCount > 4 then
      begin
        pData^ := $ED;
        inc(pData);
        pData^ := $ED;
        inc(pData);
        pData^ := lByteCount;
        inc(pData);
        pData^ := FRAMPage[lBlock, lCounter - 1];
        inc(pData);
        if lCounter < 16384 then
        begin
          pData^ := FRAMPage[lBlock, lCounter];
          inc(pData);
          lCounter := lCounter + 1;
        end;
      end
      else
      begin
        if FRAMPage[lBlock, lCounter - 1] = $ED then
        begin
          // Strings of 2 or more ED bytes are always encoded
          pData^ := $ED;
          inc(pData);
          pData^ := $ED;
          inc(pData);
          pData^ := lByteCount;
          inc(pData);
          pData^ := FRAMPage[lBlock, lCounter - 1];
          inc(pData);
          if lCounter < 16384 then
          begin
            pData^ := FRAMPage[lBlock, lCounter];
            inc(pData);
            lCounter := lCounter + 1;
          end;
        end
        else
        begin
          // Strings of 4 or fewer bytes are not encoded
          for ii := 1 to lByteCount do
          begin
            pData^ := FRAMPage[lBlock, lCounter - 1];
            inc(pData);
          end;
        end;
      end;
    end
    else
    begin
      // A single byte, just write it out
      pData^ := FRAMPage[lBlock, lCounter];
      inc(pData);
      lCounter := lCounter + 1;
    end;
  until lCounter >= 16383;

  if lCounter = 16383 then
  begin;
    // Write out the final byte
    pData^ := FRAMPage[lBlock, 16383];
    inc(pData);
  end;

  lCounter2 := Integer(pData);
  lCounter := Integer(pDataStart);
  CompressMemoryBlock := lCounter2 - lCounter;
end;

procedure TZXSpectrum.SaveZ80Snap(const AFilename: string);
var
  Stream: TFileStream;
  sData: array [0 .. 65535] of Byte;
  lCounter: Integer;
  lBufSize: Integer;
  bDummy: Byte;
begin
  Stream := TFileStream.Create(AFilename, fmCreate);
  try
    with FCPU.Register do
    begin
      // Index0:A,F
      Stream.Write(af.W, 2);

      // Index2:BC
      Stream.Write(BC.W, 2);

      // Index4:HL
      Stream.Write(HL.W, 2);

      // Index6
      // Set PC to zero to indicate a v3.0 Z80 file
      bDummy := 0;
      Stream.Write(bDummy, 1);
      Stream.Write(bDummy, 1);

      // Index8
      // SP
      Stream.Write(SP.W, 2);

      // Index10
      // I
      Stream.Write(ir.h, 1);

      // Index11
      // R (7 bits)
      RTemp := RTemp and $7F;
      Stream.Write(RTemp, 1);

      // Index12
      // bitfield
      if ir.l and $80 = $80 then
        bDummy := 1
      else
        bDummy := 0;
      bDummy := bDummy or (FGPU.Border * 2);
      Stream.Write(bDummy, 1);

      // Index13
      // DE
      Stream.Write(de.W, 2);

      // Index15
      // BC'
      Stream.Write(BC2.W, 2);

      // Index17
      // DE'
      Stream.Write(DE2.W, 2);

      // Index19
      // HL'
      Stream.Write(HL2.W, 2);

      // Index21
      // AF'
      bDummy := af2.W div 256;
      Stream.Write(bDummy, 1);
      bDummy := af2.W and 255;
      Stream.Write(bDummy, 1);

      // Index23
      // IY
      Stream.Write(IY.W, 2);

      // Index25
      // IX
      Stream.Write(ix.W, 2);

      // Index27
      // Interrupt flipflops
      if iff1 = 1 then
      begin
        bDummy := 255;
        bDummy := 255;
      end
      else
      begin
        bDummy := 0;
        bDummy := 0;
      end;
      Stream.Write(bDummy, 1);
      Stream.Write(bDummy, 1);

      // Index29
      // Interrupt Mode
      Stream.Write(im, 1);

      // Index30
      // V2.01 info
      bDummy := 23;
      Stream.Write(bDummy, 1);
      bDummy := 0;
      Stream.Write(bDummy, 1);

      // Index32
      // PC
      Stream.Write(pc.W, 2);

      // Index34
      // Hardware mode
      if FCurrentModel = tmZX48 then
      begin
        bDummy := 0; // 48K Spectrum
        Stream.Write(bDummy, 1);
        Stream.Write(bDummy, 1);
      end
      else
      begin
        bDummy := 3; // 128K Spectrum
        Stream.Write(bDummy, 1);

        // Last out to 7FFD
        Stream.Write(FLastOut7FFD, 1);
      end;

      // Index36
      // IF.1 Paged in
      bDummy := 0;
      Stream.Write(bDummy, 1);

      // Index37
      // 1=R emulation on, 2=LDIR emulation on
      bDummy := 3;
      Stream.Write(bDummy, 1);

      // Index38
      if assigned(FAY8912) then
      begin
        bDummy := FSoundRegister;
        Stream.Write(bDummy, 1);
        bDummy := 0;
        Stream.Write(bDummy, 1);

        // Index39
        // AY-3-8912 register contents
        for lCounter := 0 to 15 do
        begin
          bDummy := FAY8912.Regs[lCounter];
          Stream.Write(bDummy, 1);
        end;
      end;

      // Index54
      if FCurrentModel = tmZX48 then
      begin
        // Block 1
        lBufSize := CompressMemoryBlock(FPageAt[1], addr(sData));

        // Buffer length
        Stream.Write(lBufSize, 2);

        // Index49
        // Block number
        bDummy := FPageAt[1] + 3;
        Stream.Write(bDummy, 1);
        Stream.Write(sData, lBufSize);

        // Block 2
        lBufSize := CompressMemoryBlock(FPageAt[2], addr(sData));
        // Buffer length
        Stream.Write(lBufSize, 2);
        // Block number
        bDummy := FPageAt[2] + 3;
        Stream.Write(bDummy, 1);
        Stream.Write(sData, lBufSize);

        // Block 3
        lBufSize := CompressMemoryBlock(FPageAt[3], addr(sData));
        // Buffer length
        Stream.Write(lBufSize, 2);
        // Block number
        bDummy := FPageAt[3] + 3;
        Stream.Write(bDummy, 1);
        Stream.Write(sData, lBufSize);
      end
      else
      begin
        for lCounter := 0 to 7 do
        begin
          lBufSize := CompressMemoryBlock(lCounter, addr(sData));
          // Buffer length
          Stream.Write(lBufSize, 2);
          // Block number
          bDummy := lCounter + 3;
          Stream.Write(bDummy, 1);
          Stream.Write(sData, lBufSize);
        end;
      end;
    end;
  finally
    Stream.free;
  end;
end;

procedure TZXSpectrum.LoadFromFile(const AFilename: String);
var
  FileName, ext: String;
begin
  FileName := AFileName;

  if not FileExists(AFilename) then
   Filename := TPath.Combine(GetResourceDir, AFilename);

  ext := Lowercase(ExtractFileExt(Filename));
  if ext = '.sna' then
    LoadSNASnap(Filename)
  else if ext = '.z80' then
    LoadZ80Snap(Filename)
  else
  begin
    FreeAndNil(FTape);
    FTape := TBaseTape.CreateTape(self, Filename);
  end;
end;

procedure TZXSpectrum.SaveToFile(const AFilename: String);
var
  ext: String;
begin
  ext := Lowercase(ExtractFileExt(AFilename));
  if ext = '.sna' then
    self.SaveSNASnap(AFilename)
  else if ext = '.z80' then
    self.SaveZ80Snap(AFilename)
end;

end.
