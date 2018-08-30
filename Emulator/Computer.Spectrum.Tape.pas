// http://kmp1.github.io/tzx.js/
{ .$Q+ }
unit Computer.Spectrum.Tape;

interface

uses Classes, SysUtils, Computer;

type
  TPosition = record
    start: Longint;
    ends: Longint;
  end;

  TTapeData = record
    typ: string;
    name: string;
    code: string;
    Real: TPosition;
    Data: TPosition;
  end;

  TTapeList = array [0 .. 100] of TTapeData;

  TTZXData = record
    CallCounter: Longint;
    NumCalls: Longint;
    CallByte: Longint;
    TotalTs: Longint;

    State: Longint;
    AimTStates: Longint;
    Pointer: Longint;
    CurBlockID: Longint;
    PulseLen: Longint;
    Sync1Len: Longint;
    Sync2Len: Longint;
    ZeroLen: Longint;
    OneLen: Longint;
    PauseLen: Longint;
    DataLen: Longint;
    ROMDataLen: Longint;
    UsedBits: Longint;
    Byte: Longint;
    DataPos: Longint;
    PulsesDone: Longint;
    PulseToneLen: Longint;
    BitLimit: Longint;
    BitCounter: Longint;
    LoopCounter: Longint;
    LoopPoint: Longint;
    CurBlock: Longint;

    BlockIsStandardTiming: Boolean;
  end;

  TBaseTape = class
  protected
    FHardware: IHardware;
    FBuffer: PByteArray;
    FBufferSize: Integer;
    FFileName: String;
    FIsPlaying: Boolean;
    FIsInserted: Boolean;
    FEarBit: Byte;
    FIsRealtime: Boolean;
  public
    constructor Create(const AHardware: IHardware);
    destructor Destroy; override;

    procedure OpenTape(const AFilename: String); virtual;
    procedure CloseTape; virtual;
    procedure StartTape(); virtual;
    procedure StopTape(); virtual;

    function SaveTAP(Registers: TZ80Register; lID: Byte; lStart, lLength: Word)
      : Boolean; virtual;

    function LoadTAP(Registers: TZ80Register; lID: Byte; lStart, lLength: Word)
      : Boolean; virtual;

    procedure UpdateState(const TapeTStates: Longint); virtual;

    class function CreateTape(const AHardware: IHardware;
      const AFilename: String): TBaseTape;

    property Filename: String read FFileName;
    property IsPlaying: Boolean read FIsPlaying;
    property IsInserted: Boolean read FIsInserted;
    property IsRealTime: Boolean read FIsRealtime;
    property EarBit: Byte read FEarBit;
  end;

  TTapeInfo = class(TBaseTape)
  private
    FTapelist: TTapeList;
    FCount: Byte;
    FStream: TStream;

    function GetData(index: Byte): TTapeData;

    procedure Skip_block(const length_wanted: Word; var CheckSum: Byte);
    procedure Read_block(start_ix: Word; const length: Word;
      var CheckSum: Byte);
    procedure Write_block(start_ix: Word; const length: Word;
      var CheckSum: Byte);
  public
    constructor Create(const AHardware: IHardware);
    destructor Destroy; override;

    function SaveTAP(Registers: TZ80Register; lID: Byte; lStart, lLength: Word)
      : Boolean; override;

    function LoadTAP(Registers: TZ80Register; lID: Byte; lStart, lLength: Word)
      : Boolean; override;

    procedure OpenTape(const AFilename: String); override;
    procedure CloseTape; override;

    property Count: Byte read FCount;
    property List[index: Byte]: TTapeData read GetData;
  end;

  TTZXInfo = class(TBaseTape)
  private
    FTZXOffsets: Array of Longint;
    FTZXBlockLength: Array of Longint;

    FTZXCallList: Array of Longint;

    FBitValues: Array [0 .. 7] of Byte;
    FHardware: IHardware;

    FTZXData: TTZXData;
    procedure SetCurTZXBlock(const BlockNum: Integer);
  public
    constructor Create(const AHardware: IHardware);

    procedure GetTZXBlockInfo(lBlockNum: Integer; out lType: Integer;
      out sText: string; out len: Integer);

    procedure StartTape(); override;
    procedure StopTape(); override;

    procedure UpdateState(const TapeTStates: Longint); override;

    procedure OpenTape(const AFilename: String); override;
    procedure CloseTape; override;
  end;

implementation

class function TBaseTape.CreateTape(const AHardware: IHardware;
  const AFilename: String): TBaseTape;
var
  ext: string;
begin
  ext := Lowercase(ExtractFileExt(AFilename));
  if ext = '.tzx' then
    result := TTZXInfo.Create(AHardware)
  else
    result := TTapeInfo.Create(AHardware);
  result.OpenTape(AFilename);
end;

constructor TBaseTape.Create(const AHardware: IHardware);
begin
  FHardware := AHardware;
  FIsRealtime := False;
end;

destructor TBaseTape.Destroy;
begin
  CloseTape;
  inherited;
end;

procedure TBaseTape.CloseTape;
begin
  if (FBufferSize <> 0) and (FBuffer <> nil) then
    FreeMem(FBuffer, FBufferSize);

  FBufferSize := 0;
  FBuffer := nil;
  FIsInserted := False;
  FIsPlaying := False;
end;

procedure TBaseTape.OpenTape(const AFilename: String);
begin
  CloseTape;
  FFileName := AFilename;
  FIsInserted := True;
end;

procedure TBaseTape.StartTape();
begin
  FIsPlaying := FIsInserted;
  FEarBit := 0;
end;

procedure TBaseTape.StopTape();
begin
  FIsPlaying := False;
end;

function TBaseTape.SaveTAP(Registers: TZ80Register; lID: Byte;
  lStart, lLength: Word): Boolean;
begin
  result := False;
end;

function TBaseTape.LoadTAP(Registers: TZ80Register; lID: Byte;
  lStart, lLength: Word): Boolean;
begin
  result := False;
end;

procedure TBaseTape.UpdateState(const TapeTStates: Longint);
begin
end;

constructor TTapeInfo.Create(const AHardware: IHardware);
begin
  inherited Create(AHardware);
end;

destructor TTapeInfo.Destroy;
begin
  inherited;
end;

procedure TTapeInfo.Skip_block(const length_wanted: Word; var CheckSum: Byte);
var
  i, numread: Integer;
begin
  numread := FStream.Read(FBuffer[0], length_wanted);
  for i := 0 to numread - 1 do
    CheckSum := CheckSum xor FBuffer[i];
end;

procedure TTapeInfo.Read_block(start_ix: Word; const length: Word;
  var CheckSum: Byte);
var
  i, numread: Integer;
begin
  numread := FStream.Read(FBuffer[0], length);
  if assigned(FHardware) then
    for i := 0 to numread - 1 do
    begin
      FHardware.PokeByte(start_ix, FBuffer[i]);
      CheckSum := CheckSum xor FBuffer[i];
      inc(start_ix);
    end;
end;

procedure TTapeInfo.Write_block(start_ix: Word; const length: Word;
  var CheckSum: Byte);
var
  i: Integer;
  value: Byte;
begin
  if assigned(FHardware) then
    For i := 0 To length - 1 do
    begin
      FHardware.PeekByte(start_ix, value);
      FStream.Write(value, 1);
      inc(start_ix);
      CheckSum := CheckSum Xor value;
    end;
end;

procedure TTapeInfo.CloseTape;
begin
  FreeAndNil(FStream);
end;

procedure TTapeInfo.OpenTape(const AFilename: String);
var
  j, i, tapfile: Byte;
  Temp: array [1 .. 10] of Byte;
  len: Word;
  counter: Longint;
begin
  inherited;

  FBufferSize := $FFFF;
  GetMem(FBuffer, FBufferSize);

  counter := 0;
  FCount := 0;
  CloseTape;

  if not fileexists(AFilename) then
    exit;

  if pos('.TAP', uppercase(AFilename)) > 0 then
    tapfile := 2
  else
    tapfile := 0;

  FStream := TFileStream.Create(AFilename, fmOpenRead);
  filemode := 0;

  while FStream.Position < FStream.Size do
    with FTapelist[FCount] do
    begin
      fillchar(FTapelist[FCount], sizeof(TTapeInfo), 0);

      FStream.Read(Temp, 2);
      Real.start := counter;
      inc(counter, 2);
      len := Temp[1] + Temp[2] shl 8 - Word(tapfile);
      Real.ends := len;

      FStream.Read(Temp, 1);
      inc(counter, len + 2);
      if (Temp[1] = 0) and (len = 17) then
      begin
        FStream.Read(Temp, 1);
        case Temp[1] of
          3:
            typ := 'Bytes';
          1:
            typ := 'Number array';
          2:
            typ := 'Character array';
          0:
            typ := 'Program';
        end;

        if Temp[1] < 4 then
        begin
          j := Temp[1];
          FStream.Read(Temp, 10);
          name := '';
          for i := 1 to 10 do
            name := name + chr(Temp[i]);
          case j of
            0:
              begin
                FStream.Read(Temp, 4);
                i := Temp[4] and $C0;
                if i = 0 then
                begin
                  code := 'LINE';
                  Data.start := Temp[3] + Word(Temp[4]) shl 8;
                end;
              end;
            3:
              begin
                FStream.Read(Temp, 4);
                code := 'CODE';
                Data.start := Temp[3] + Word(Temp[4]) shl 8;
                Data.ends := Temp[1] + Word(Temp[2]) shl 8;
              end;
          end;
        end;
      end;
      FStream.Position := counter;
      inc(FCount);
    end;
  FStream.Position := 0;
  filemode := 0;
end;

function TTapeInfo.GetData(index: Byte): TTapeData;
begin
  if index > FCount then
    index := FCount;
  result := FTapelist[index];
end;

function TTapeInfo.SaveTAP(Registers: TZ80Register; lID: Byte;
  lStart, lLength: Word): Boolean;
var
  lCheckSum: Byte;
  length: TPair;
begin
  if not assigned(FStream) then
  begin
    result := False;
    exit;
  end;
  FStream.Position := FStream.Size;
  length.W := lLength + 2;
  FStream.Write(length.l, 1);
  FStream.Write(length.h, 1);
  FStream.Write(lID, 1);

  lCheckSum := lID;
  Write_block(lStart, lLength, lCheckSum);
  FStream.Write(lCheckSum, 1);
  result := True;
End;

function TTapeInfo.LoadTAP(Registers: TZ80Register; lID: Byte;
  lStart, lLength: Word): Boolean;
var
  length: TPair;
  id_tape, CheckSum: Byte;
  CalcCheckSum: Byte;
begin
  if not assigned(FStream) then
  begin
    result := False;
    exit;
  end; (* fail, no file *)

  if FStream.Position = FStream.Size then
    FStream.Position := 0; (* Back to start *)

  FStream.Read(length.l, 1);
  FStream.Read(length.h, 1);

  dec(length.W, 2); (* Now actual Spectrum length *)

  FStream.Read(id_tape, 1);
  CalcCheckSum := id_tape;

  if id_tape = lID then (* Yes, the correct ID, proceed. *)
    with Registers do
    begin
      if (lLength <= length.W) then (* Length <= so OK *)
      begin
        Read_block(lStart, lLength, CalcCheckSum);
        if lLength <> length.W then
          Skip_block(length.W - lLength, CalcCheckSum);

        FStream.Read(CheckSum, 1);
        inc(ix.W, lLength);
        de.W := 0; (* checksum in this case? *)
        result := (CheckSum = CalcCheckSum);
      end
      else (* Too many bytes requested *)
      begin
        Read_block(lStart, length.W, CalcCheckSum);
        FStream.Read(CheckSum, 1);
        inc(ix.W, length.W);
        de.W := 0;
        result := False;
      end;
    end
  else (* Wrong ID! *)
  begin
    Skip_block(length.W, CalcCheckSum);
    FStream.Read(CheckSum, 1);
    result := False;
  end;
end;

constructor TTZXInfo.Create(const AHardware: IHardware);
var
  b: Integer;
  i: Integer;
begin
  inherited;
  b := 1;
  For i := 0 To high(FBitValues) do
  begin
    FBitValues[i] := b;
    b := b shl 1;
  end;
  FIsRealtime := True;
end;

procedure TTZXInfo.GetTZXBlockInfo(lBlockNum: Integer; out lType: Integer;
  out sText: string; out len: Integer);
var
  lPtr: Integer;
  l: Integer;
begin
  lPtr := FTZXOffsets[lBlockNum];
  lType := FBuffer[lPtr];

  case lType of
    $10:
      sText := 'Standard Block';
    $11:
      sText := 'Turbo Block';
    $12:
      sText := 'Pure Tone';
    $13:
      sText := 'Pulse Sequence';
    $14:
      sText := 'Pure Data Block';
    $15:
      sText := 'Direct Recording';
    $16:
      sText := 'C64 Standard Block';
    $17:
      sText := 'C64 Turbo Block';
    $20:
      begin
        // Pause/StopTape
        l := FBuffer[lPtr + 1] + (FBuffer[lPtr + 2] shl 8);
        If l = 0 Then
          sText := 'Stop Tape'
        Else
          sText := 'Pause Tape for ' + inttostr(l) + 'ms';
      End;
    $21:
      sText := 'Group Start';
    $22:
      sText := 'Group End';
    $23:
      sText := 'Jump to Block';
    $24:
      sText := 'Loop Start';
    $25:
      sText := 'Loop End';
    $2A:
      sText := 'Stop Tape if 48K';
    $30:
      begin
        sText := '';
        l := FBuffer[lPtr + 1];
        For l := lPtr + 2 To lPtr + 1 + l do
          sText := sText + chr(FBuffer[l]);
      end;
    $31:
      sText := 'Message Block';
    $32:
      sText := 'Archive Info';
    $33:
      sText := 'Hardware Type';
    $34:
      sText := 'Emulation Info';
    $35:
      sText := 'Custom Info Block';
    $40:
      sText := 'Snapshot Block';
    $5A:
      sText := 'Block Merge Marker';
    $FE:
      sText := 'End of Tape';
  end;
  len := FTZXBlockLength[lBlockNum];
end;

procedure TTZXInfo.StartTape();
begin
  if IsPlaying then
    exit;
  inherited;

  FTZXData.TotalTs := 0;
End;

procedure TTZXInfo.StopTape();
begin
  inherited;
end;

procedure TTZXInfo.CloseTape;
begin
  inherited;
  SetLength(FTZXOffsets, 0);
end;

procedure TTZXInfo.OpenTape(const AFilename: String);
var
  ReadLength: Integer;
  s: string;
  f: Integer;
  BlockLen: Longint;
  BlockID: Longint;

  BlockList, BlockLengths: Array [0 .. 2047] of Longint;
  BlockListNum: Longint;
  BlockLengthsNum: Longint;
  Stream: TStream;

begin
  inherited;

  if not fileexists(AFilename) then
  begin
    CloseTape;
    exit;
  end;

  fillchar(BlockList, sizeof(BlockList), 0);
  fillchar(BlockLengths, sizeof(BlockLengths), 0);

  Stream := TFileStream.Create(AFilename, fmOpenRead);

  ReadLength := Stream.Size;
  If ReadLength = 0 Then
  begin
    Stream.Free;
    CloseTape;
    exit;
  end;

  // Read the TZX file into FBuffer
  FBufferSize := ReadLength + 1;
  GetMem(FBuffer, FBufferSize);
  Stream.Read(FBuffer[0], ReadLength);
  FBuffer[ReadLength] := $FE;
  Stream.Free;

  // Now decode the TZX file into an individual blocks list

  s := '';

  For f := 0 To 6 do
    s := s + chr(FBuffer[f]);

  If s <> 'ZXTape!' Then
  begin
    CloseTape;
    exit;
  end;

  BlockListNum := 0;
  BlockLengthsNum := 0;
  f := 10;

  repeat
    BlockID := FBuffer[f];
    BlockList[BlockListNum] := f;
    BlockListNum := BlockListNum + 1;

    f := f + 1;

    case BlockID of
      $10:
        BlockLen := FBuffer[f + 3] shl 8 + FBuffer[f + 2] + 4;
      $11:
        BlockLen := FBuffer[f + 15] + (FBuffer[f + 16] shl 8) +
          (FBuffer[f + 17] shl 16) + 18;
      $12:
        BlockLen := 4;
      $13:
        BlockLen := 1 + (FBuffer[f] * 2);
      $14:
        BlockLen := FBuffer[f + 7] + (FBuffer[f + 8] shl 8) +
          (FBuffer[f + 9] shl 16) + 10;
      $15:
        BlockLen := FBuffer[f + 5] + (FBuffer[f + 6] shl 8) +
          (FBuffer[f + 7] shl 16) + 8;
      $20:
        BlockLen := 2;
      $21:
        BlockLen := FBuffer[f] + 1;
      $22:
        BlockLen := 0;
      $23:
        BlockLen := 2;
      $24:
        BlockLen := 2;
      $25:
        BlockLen := 0;
      $26:
        BlockLen := (FBuffer[f] + (FBuffer[f + 1] shl 8) * 2) + 2;
      $27:
        BlockLen := 0;
      $28:
        BlockLen := FBuffer[f] + (FBuffer[f + 1] shl 8) + 2;
      $2A:
        BlockLen := 4;
      $30:
        BlockLen := FBuffer[f] + 1;
      $31:
        BlockLen := FBuffer[f + 1] + 2;
      $32:
        BlockLen := FBuffer[f] + (FBuffer[f + 1] shl 8) + 2;
      $33:
        BlockLen := (FBuffer[f] * 3) + 1;
      $34:
        BlockLen := 8;
      $35:
        BlockLen := FBuffer[f + 16] + (FBuffer[f + 17] shl 8) +
          (FBuffer[f + 18] shl 16) + (FBuffer[f + 19] shl 24) + 20;
      $40:
        BlockLen := FBuffer[f + 1] + (FBuffer[f + 2] shl 8) +
          (FBuffer[f + 3] shl 16) + 4;
      $5A:
        BlockLen := 9;
      $FE:
        BlockLen := 0;
      $FF:
        BlockLen := 0;
    Else
      BlockLen := FBuffer[f] + (FBuffer[f + 1] shl 8) + (FBuffer[f + 2] shl 16)
        + (FBuffer[f + 3] shl 24) + 4;
    End;

    f := f + BlockLen;
    BlockLengths[BlockLengthsNum] := BlockLen + 1;
    inc(BlockLengthsNum, 1);
  until f >= FBufferSize;

  SetLength(FTZXOffsets, BlockListNum);
  SetLength(FTZXBlockLength, BlockListNum);

  For f := 0 To BlockListNum - 1 do
  begin
    FTZXOffsets[f] := BlockList[f];
    FTZXBlockLength[f] := BlockLengths[f];
  end;
  SetCurTZXBlock(0);
end;

procedure TTZXInfo.SetCurTZXBlock(const BlockNum: Integer);
var
  f: Integer;

  // lType,len:longint;
  // sText:string;
begin
  with FTZXData do
  begin
    BlockIsStandardTiming := False;
    State := 5;
    AimTStates := 0;
    Pointer := FTZXOffsets[BlockNum];
    CurBlockID := FBuffer[Pointer];
    case CurBlockID of
      $10: // Standard ROM Loader block
        begin
          PulseLen := 2168;
          If FBuffer[Pointer + 5] = $FF Then
            PulseToneLen := 3220
          Else
            PulseToneLen := 8064;
          Sync1Len := 667;
          Sync2Len := 735;
          ZeroLen := 855;
          OneLen := 1710;
          PauseLen := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          DataLen := FTZXBlockLength[BlockNum] + FTZXOffsets[BlockNum];
          ROMDataLen := FBuffer[Pointer + 3] + (FBuffer[Pointer + 4] shl 8);
          UsedBits := 8;
          State := 0; // State 0 - playing Pulse
          AimTStates := PulseLen;
          Byte := 0;
          CurBlock := BlockNum;
          DataPos := Pointer + 5;
          PulsesDone := 2;
          BlockIsStandardTiming := True;
        end;
      $11: // Non-Standard TAP block
        begin
          PulseLen := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          PulseToneLen := FBuffer[Pointer + 11] + (FBuffer[Pointer + 12] shl 8);
          Sync1Len := FBuffer[Pointer + 3] + (FBuffer[Pointer + 4] shl 8);
          Sync2Len := FBuffer[Pointer + 5] + (FBuffer[Pointer + 6] shl 8);
          ZeroLen := FBuffer[Pointer + 7] + (FBuffer[Pointer + 8] shl 8);
          OneLen := FBuffer[Pointer + 9] + (FBuffer[Pointer + 10] shl 8);
          UsedBits := FBuffer[Pointer + 13];
          PauseLen := FBuffer[Pointer + 14] + (FBuffer[Pointer + 15] shl 8);
          State := 0; // State 0 - playing Pulse.
          AimTStates := PulseLen;
          Byte := 0;
          CurBlock := BlockNum;
          DataPos := Pointer + 19;
          DataLen := FTZXBlockLength[BlockNum] + FTZXOffsets[BlockNum];
          ROMDataLen := FBuffer[Pointer + 3] + (FBuffer[Pointer + 4] shl 8);
          If (PulseLen = 2168) And
            ((PulseToneLen = 3220) Or (PulseToneLen = 8064)) And
            (Sync1Len = 667) And (Sync2Len = 735) And (ZeroLen = 855) And
            (OneLen = 1710) Then
            BlockIsStandardTiming := True
          Else
            BlockIsStandardTiming := False
        end;
      $12: // Pure Tone
        begin
          State := 0; // playing a possible pilot tone
          PulseLen := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          PulseToneLen := FBuffer[Pointer + 3] + (FBuffer[Pointer + 4] shl 8);
          AimTStates := PulseLen;
          Byte := 1;
          CurBlock := BlockNum;
        end;
      $13: // Row of Pulses
        begin
          State := 0; // playing a possible pilot tone
          PulseToneLen := FBuffer[Pointer + 1];
          // // NUMBER OF PULSES
          PulseLen := FBuffer[Pointer + 2] + (FBuffer[Pointer + 3] shl 8);
          PulsesDone := 1;
          Byte := Pointer + 4;
          AimTStates := PulseLen;
          CurBlock := BlockNum;
        end;
      $14: // Pure Data block
        begin
          ZeroLen := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          OneLen := FBuffer[Pointer + 3] + (FBuffer[Pointer + 4] shl 8);
          UsedBits := FBuffer[Pointer + 5];
          PauseLen := FBuffer[Pointer + 6] + (FBuffer[Pointer + 7] shl 8);
          DataLen := FTZXBlockLength[BlockNum] + FTZXOffsets[BlockNum];
          State := 3; // Set to DATA Byte(s] output.
          // // CC IN
          DataPos := Pointer + 11;
          // // CC OUT
          Byte := Pointer + 11;
          If (FBuffer[Byte] And 128) > 0 Then
            AimTStates := OneLen
          Else
            AimTStates := ZeroLen;
          PulsesDone := 2;
          If Byte = DataLen - 1 Then
            BitLimit := FBitValues[8 - UsedBits]
          Else
            BitLimit := 1;
          BitCounter := 128;
          CurBlock := BlockNum;
        end;
      $15: // Direct Recording Block
        begin
          OneLen := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          // Length of Sample (Ts]
          PauseLen := FBuffer[Pointer + 3] + (FBuffer[Pointer + 4] shl 8);
          // (ms]
          UsedBits := FBuffer[Pointer + 5];
          // Samples used in last byte
          DataLen := FBuffer[Pointer + 6] + (FBuffer[Pointer + 7] shl 8) +
            FBuffer[Pointer + 8] shl 16;
          // FTZXBlockLength[BlockNum] + Offsets[BlockNum]
          Byte := Pointer + 9;
          State := 3; // Set to DATA bytes output
          AimTStates := OneLen;
          If Byte = DataLen - 1 Then
            BitLimit := FBitValues[8 - UsedBits]
          Else
            BitLimit := 1;
          BitCounter := 128;
          FEarBit := 64 * (FBuffer[Byte] div 128);
          CurBlock := BlockNum;
        end;
      $20: // Pause or STOP tape.
        begin
          CurBlock := BlockNum;
          PauseLen := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          If PauseLen = 0 Then
            If IsPlaying Then
              StopTape
            Else
            begin
              AimTStates := PauseLen * 3500;
              State := 4
              // When the TStates gets past AimStates, the next block will be used
            End;
        end;
      $23: // Jump to block
        begin
          Byte := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          If Byte < 32768 Then
            SetCurTZXBlock(BlockNum + Byte)
          Else
            SetCurTZXBlock(BlockNum - (65536 - Byte));
        end;
      $24: // Loop Start
        begin
          LoopCounter := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8);
          LoopPoint := BlockNum + 1;
          SetCurTZXBlock(BlockNum + 1);
        end;
      $25: // Loop End
        begin
          LoopCounter := LoopCounter - 1;
          If LoopCounter > 0 Then
            SetCurTZXBlock(LoopPoint)
          Else
            SetCurTZXBlock(BlockNum + 1)
        end;
      $26: // Call Sequence
        begin
          NumCalls := FBuffer[Pointer + 1] + (FBuffer[Pointer + 2] shl 8) - 1;
          CallByte := NumCalls;
          CallCounter := 0;
          SetLength(FTZXCallList, NumCalls);
          For f := 0 To NumCalls - 1 do
            FTZXCallList[f] := FBuffer[(Pointer + 4) + (f * 2)] +
              (FBuffer[(Pointer + 5) + (f * 2) + 1] shl 8);
          CallByte := BlockNum;
          Byte := FBuffer[Pointer + 3] + (FBuffer[Pointer + 4] shl 8);
          If Byte < 32768 Then
            SetCurTZXBlock(BlockNum + Byte)
          Else
            SetCurTZXBlock(BlockNum - (65536 - Byte));
        end;
      $27: // CALL Return
        begin
          If CallCounter < NumCalls Then
          begin
            CallCounter := CallCounter + 1;
            Byte := FTZXCallList[CallCounter];
            If Byte < 32768 Then
              SetCurTZXBlock(CallByte + Byte)
            Else
              SetCurTZXBlock(CallByte - (65536 - Byte))
          end;
        end;

      $2A: // Stop tape in 48k Mode
        begin
          If FHardware.GetModel = 0 Then // 48k Speccy?
          begin
            If IsPlaying Then
              StopTape
          End;
          CurBlock := BlockNum
        end;
      $FE: // End of Tape
        begin
          AimTStates := 30;
          CurBlock := BlockNum;
          If IsPlaying Then
            SetCurTZXBlock(0);
          StopTape;
        end;
    Else
      CurBlock := BlockNum
    End;
  end;
End;

procedure TTZXInfo.UpdateState(const TapeTStates: Longint);
begin
  with FTZXData do
  begin
    TotalTs := TotalTs + TapeTStates;
    While (TotalTs >= AimTStates) And IsPlaying do
    begin
      TotalTs := TotalTs - AimTStates;
      case CurBlockID of
        $10, $11, $14:
          begin
            Case State of
              0: // 'Playing Pilot tone.
                begin
                  FEarBit := FEarBit Xor 64;
                  If Byte < PulseToneLen Then
                  // ' Byte holds number of pulses
                  begin
                    AimTStates := PulseLen;
                    Byte := Byte + 1;
                  end
                  Else
                  begin
                    Byte := 0;
                    State := 1; // ' Set to SYNC1 Pulse output
                    AimTStates := Sync1Len;
                  End;
                end;
              1: // ' SYNC 1
                begin
                  FEarBit := FEarBit Xor 64;
                  State := 2; // ' Set to SYNC2 Pulse output
                  AimTStates := Sync2Len;
                end;
              2: // ' SYNC 2
                begin
                  FEarBit := FEarBit Xor 64;
                  State := 3; // ' Set to DATA Byte(s) output
                  Byte := DataPos;
                  If (FBuffer[Byte] And 128) > 0 Then
                    // ' Set next pulse length
                    AimTStates := OneLen
                  Else
                    AimTStates := ZeroLen;
                  PulsesDone := 2;
                  // ' *2* edges per Data BIT, one on, one off
                  BitCounter := 128; // ' Start with the full byte
                  BitLimit := 1;
                end;
              3: // ' DATA Bytes out
                begin
                  FEarBit := FEarBit Xor 64;
                  PulsesDone := PulsesDone - 1;
                  If PulsesDone = 0 Then
                  // ' Done both pulses for this bit?
                  begin
                    If BitCounter > BitLimit Then
                    // ' Done all the bits for this byte?
                    begin
                      BitCounter := BitCounter shr 1; // div 2;
                      // ' Bitcounter counts *down*
                      PulsesDone := 2;
                      If (FBuffer[Byte] And BitCounter) > 0 Then
                        AimTStates := OneLen
                      Else
                        AimTStates := ZeroLen;
                    End
                    Else // ' all bits done, setup for next byte
                    begin
                      Byte := Byte + 1;
                      If Byte < DataLen Then // ' last byte?
                      begin
                        If Byte = DataLen - 1 Then
                          BitLimit := FBitValues[8 - UsedBits]
                          // ' if so, set up the last bits used
                        Else
                          BitLimit := 1; // ' else use full 8 bits
                        BitCounter := 128;
                        PulsesDone := 2;
                        If (FBuffer[Byte] And 128) > 0 Then
                          AimTStates := OneLen
                        Else
                          AimTStates := ZeroLen
                      end
                      Else
                      begin
                        If (PauseLen > 0) Then
                        begin
                          AimTStates := PauseLen * 3500;
                          State := 4; // ' Set to Pause output
                        end
                        Else
                        begin
                          State := 0;
                          SetCurTZXBlock(CurBlock + 1);
                        End;
                      End;
                    End;
                  end
                  Else
                  begin // Not done both pulses, flip the ear bit next time
                    If (FBuffer[Byte] And BitCounter) > 0 Then
                      AimTStates := OneLen
                    Else
                      AimTStates := ZeroLen
                  End;
                end;
              4: // End Pause output
                SetCurTZXBlock(CurBlock + 1)
            end;
          End;

        $12:
          begin
            FEarBit := FEarBit Xor 64;
            If Byte < PulseToneLen Then
            begin
              AimTStates := PulseLen;
              Byte := Byte + 1;
            end
            Else
              SetCurTZXBlock(CurBlock + 1);
          end;
        $13:
          begin
            FEarBit := FEarBit Xor 64;
            If PulsesDone < PulseToneLen Then
            begin
              PulseLen := FBuffer[Byte] + (FBuffer[Byte + 1] shl 8);
              AimTStates := PulseLen;
              Byte := Byte + 2;
              PulsesDone := PulsesDone + 1;
            end
            Else
              SetCurTZXBlock(CurBlock + 1)
          End;
        $15: // *UnTested* - any  actually use a DRB block?
          begin
            // LastEarBit := FEarBit;
            If BitCounter > BitLimit Then
            // ' Done all the bits for this byte?
            begin
              BitCounter := BitCounter div 2;
              // Bitcounter counts *down*
              If (FBuffer[Byte] And BitCounter) > 0 Then
                // Set the ear bit
                FEarBit := 64
              Else
                FEarBit := 0;
              AimTStates := OneLen;
            end
            Else // all bits done, setup for next byte
            begin
              Byte := Byte + 1;
              If Byte < DataLen Then // ' last byte?
              begin
                If Byte = DataLen - 1 Then
                  BitLimit := FBitValues[8 - UsedBits]
                  // ' if so, set up the last bits used
                Else
                  BitLimit := 1; // ' else use full 8 bits
                BitCounter := 128;
                If (FBuffer[Byte] And BitCounter) > 0 Then
                  // ' Set the ear bit
                  FEarBit := 64
                Else
                  FEarBit := 0;
                AimTStates := OneLen
              end
              Else
              begin
                If PauseLen > 0 Then
                begin
                  AimTStates := PauseLen * 3500;
                  State := 4; // ' Set to Pause output
                end
                Else
                begin
                  State := 0;
                  SetCurTZXBlock(CurBlock + 1);
                End;
              End;
            End;
          end;
        $FE:
          StopTape;
      Else
        SetCurTZXBlock(CurBlock + 1)
      end;
    end;
  end;
end;

end.
