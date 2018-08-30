unit Computer.Spectrum.Disk;

interface

uses Classes;

type
  TDisks = array [0 .. 3] of string;

  TBetadisk = class
  private
    FRegStatus: Byte;
    FRegCom: Byte;
    FRegTrack: Integer;
    FRegSect: Byte;
    FRegData: Byte;
    FSystem: Byte;
    StepDirect: Shortint;
    FDskIndex: array [0 .. 4] of Byte;
    FTrackReal: array [0 .. 3] of Byte;
    FDiskBuf: array [0 .. $FF] of Byte;
    FDskFileName: string;

    FStream: TStream;
    FDskIndexCounter: Byte;
    FDskCountDatLost: Byte;
    FCntData: Integer;
    FDskDataSize: Integer;
    FCntReady: Word;
    FDskStatus: Byte;
    FReady: Boolean;
    FReadIndexTrack: Boolean;

    function DiskPosition(Sect: Word): Longint;
    procedure SetStateStep;
    procedure OpCom;
    procedure StepRear;
    procedure OpReadSector;
    procedure OpWriteSector;
  public
    Disks: TDisks;
    procedure DiskVG(OperIO: Byte; var DataIO: Byte);
    constructor create;
  end;

implementation

uses sysutils;

const
  cFormatDatLen = 1;

function TBetadisk.DiskPosition(Sect: Word): Longint;
begin
  DiskPosition := ((FRegTrack shl 5) or (FSystem and $10 xor $10) or
    Sect) shl $08;
end;

procedure TBetadisk.SetStateStep;
begin
  if FRegTrack < 0 then
  begin
    FTrackReal[FSystem and $03] := 0;
    FRegTrack := 0;
  end;
  if FRegTrack > $4F then
  begin
    FTrackReal[FSystem and $03] := $4F;
    FRegTrack := $4F;
  end;

  FRegStatus := FDskStatus and $40;
  if FRegCom and $08 > 0 then
    FRegStatus := FRegStatus or $20;
  if FRegTrack = 0 then
    FRegStatus := FRegStatus or $04;
end;

procedure TBetadisk.OpCom;
begin
  FDskCountDatLost := $00;
  FCntData := $0000;
  FRegStatus := $03;
  if not FReady then
  begin
    FRegStatus := $01;
    FCntReady := $10;
  end;
end;

procedure TBetadisk.StepRear;
begin
  if FRegTrack > 0 then
  begin
    dec(FRegTrack);
    FTrackReal[FSystem and $03] := FRegTrack;
  end;
  StepDirect := -1;
  SetStateStep;
end;

procedure TBetadisk.OpReadSector;
var
  numread: Int64;
  Position: Int64;
begin
  if (FRegSect > $10) or (FRegTrack <> FTrackReal[FSystem and $03]) or
    (FDskStatus and $04 = 0) then
  begin
    FRegStatus := $10; { OpNotFound }
    exit;
  end;

  Position := DiskPosition(FRegSect - 1);

  FStream.Position := Position;
  numread := FStream.Read(FDiskBuf, sizeof(FDiskBuf));

  if Position = 2048 then
    FDiskBuf[227] := 16; { BUG !!! at pos $8e3 must be $10 ?? }

  if numread <> sizeof(FDiskBuf) then
    FRegStatus := $08 { OpContSum }
  else
  begin
    FDskDataSize := $FF;
    FRegData := FDiskBuf[0];
    OpCom;
  end;
end;

procedure TBetadisk.OpWriteSector;
begin
  if FDskStatus and $40 > 0 then
  begin
    FRegStatus := $40; { OpWrProt }
    exit;
  end;
  if (FRegSect > $10) or (FRegTrack <> FTrackReal[FSystem and $03]) or
    (FDskStatus and $04 = 0) then
    FRegStatus := $10 { OpNotFound }
  else
  begin
    FDskDataSize := $0100;
    OpCom;
  end;
end;

procedure TBetadisk.DiskVG(OperIO: Byte; var DataIO: Byte);

var
  numwritten, i, attr: Integer;
  IntrqDrq: Word;
begin
  if FRegStatus and $01 > 0 then
  begin
    dec(FDskCountDatLost);
    if FDskCountDatLost = 0 then
      FRegStatus := $04;
  end;
  if FCntReady > 0 then
  begin
    dec(FCntReady);
    if (FCntReady = 0) then
      FRegStatus := $03;
  end;

  case OperIO of
    $0:
      begin
        DataIO := FRegStatus;
        if FRegCom and $80 > 0 then
          exit;
        dec(FDskIndexCounter);
        if FDskIndexCounter and $E > 0 then
          DataIO := DataIO or $02;
      end;
    $1:
      begin
        if FRegStatus and $01 = 0 then
        begin
          if (DataIO and $F0) = $D0 then
            exit;
          FRegCom := DataIO;
          case FRegCom shr 4 of
            $0:
              begin
                FTrackReal[FSystem and $03] := 0;
                FRegTrack := 0;
                StepDirect := -1;
                SetStateStep;
              end;
            $1:
              begin
                FTrackReal[FSystem and $03] := FRegData;
                FRegTrack := FRegData;
                if FRegData - FRegTrack < 0 then
                  StepDirect := -1;
                if FRegData - FRegTrack > 0 then
                  StepDirect := 1;
                SetStateStep;
              end;
            $2, $3, $4, $5:
              begin
                if ((FRegCom shr 4) in [2, 3]) and (StepDirect = -1) then
                begin
                  StepRear;
                  exit;
                end;
                if FRegTrack > 0 then
                begin
                  inc(FRegTrack);
                  FTrackReal[FSystem and $03] := FRegTrack;
                end;
                StepDirect := 1;
                SetStateStep;
              end;
            $6, $7:
              StepRear;
            $8, $9:
              OpReadSector;
            $A, $B:
              OpWriteSector;
            $C:
              begin
                FDskDataSize := $0005;
                FRegData := 1;
                if FReadIndexTrack then
                  FRegData := FRegTrack;
                OpCom;
              end;
            $E:
              ;
            $F:
              begin
                if FDskStatus and $40 > 0 then
                  FRegStatus := $40 { OpWrProt }
                else
                begin
                  FDskDataSize := cFormatDatLen;
                  OpCom;
                end;
              end;
          end;
        end;
      end;
    $2:
      DataIO := FRegTrack;
    $3:
      if FRegStatus and $01 = 0 then
        FRegTrack := DataIO;
    $4:
      DataIO := FRegSect;
    $5:
      if FRegStatus and $01 = 0 then
        FRegSect := DataIO;
    $6:
      begin
        if FRegStatus and $01 > 0 then
        begin
          case FRegCom shr 4 of
            $8, $9:
              begin
                DataIO := FRegData;
                FDskCountDatLost := $10;
                if FCntData < FDskDataSize then
                begin
                  inc(FCntData);
                  FRegData := FDiskBuf[FCntData];
                end
                else
                begin
                  if FRegCom and $10 > 0 then
                  begin
                    inc(FRegSect);
                    OpReadSector;
                  end
                  else
                    FRegStatus := $00; { OpOk }
                end
              end;
            $C:
              begin
                DataIO := FRegData;
                FDskCountDatLost := $10;
                if FCntData < FDskDataSize then
                begin
                  FRegData := FDskIndex[FCntData];
                  inc(FCntData);
                end
                else
                  FRegStatus := $00; { OpOk }
              end;
          end
        end
        else
          DataIO := FRegData;
      end;
    $7:
      begin
        if (FRegStatus and $01 > 0) then
        begin
          case FRegCom shr 4 of
            $A, $B:
              begin
                FDiskBuf[FCntData] := DataIO;
                FRegData := DataIO;
                inc(FCntData);
                FDskCountDatLost := $10;
                if FCntData >= FDskDataSize then
                begin
                  FStream.Position := DiskPosition(FRegSect - 1);
                  numwritten := FStream.Write(FDiskBuf, sizeof(FDiskBuf));
                  if numwritten <> sizeof(FDiskBuf) then
                  begin
                    FRegStatus := $08; { OpContSum }
                    exit;
                  end;
                  if (FRegCom and $10 > 0) then
                  begin
                    inc(FRegSect);
                    OpWriteSector;
                  end
                  else
                    FRegStatus := $00; { OpOk }
                end
              end;
            $F:
              begin
                FRegData := DataIO;
                FDskCountDatLost := $10;
                inc(FCntData);
                if FCntData >= FDskDataSize then
                begin
                  fillchar(FDiskBuf, sizeof(FDiskBuf), 0);
                  if FDskStatus and $04 > 0 then
                  begin
                    FStream.Position := DiskPosition(0);
                    for i := 0 to $10 - 1 do
                      FStream.Write(FDiskBuf, sizeof(FDiskBuf));
                  end
                  else
                  begin
                    if not fileexists(FDskFileName) then
                    begin
                      FRegStatus := $00; { OpOk }
                      exit;
                    end;

                    FStream := TFileStream.create(FDskFileName,
                      fmOpenReadWrite);
                    filemode := 2;
                    for i := 0 to $A00 - 1 do
                      FStream.Write(FDiskBuf, sizeof(FDiskBuf));
                    FDskStatus := FDskStatus or $4;
                  end;
                  FRegStatus := $00; { OpOk }
                end;
              end;
          end
        end
        else
          FRegData := DataIO;
      end;
    $8:
      begin
        { if RegStatus and 32>0 then DRQ:=$40 else DRQ:=0;
          if Regstatus and 64>0 then Intrq:=$80 else Intrq:=0;
          Dataio:=DRQ or intrq; }
        IntrqDrq := $BF;
        if FRegStatus and $01 > 0 then
          IntrqDrq := $3F;
        if FRegStatus and $02 > 0 then
          IntrqDrq := $7F;
        DataIO := IntrqDrq;
      end;
    $9:
      begin
        if (FRegStatus and $01 > 0) and (FRegCom and $80 > 0) then
        begin
          FRegStatus := $08; { OpContSum }
          exit;
        end;
        FSystem := DataIO;
        if (FSystem xor FDskStatus) and $03 > 0 then
        begin
          DiskVG($B, DataIO);
          DiskVG($A, DataIO);
        end;
      end;
    $A:
      begin
        if FDskStatus and $80 > 0 then
          exit;
        FDskStatus := FSystem and $03;
        FDskFileName := Disks[FDskStatus];
        if FileExists(FDskFileName) then
        begin
          attr := $20 and $01; //TODO
          FDskStatus := FDskStatus or (attr shl 6);
          if not fileexists(FDskFileName) then
          begin
            FDskStatus := FDskStatus or $04;
            exit;
          end;

          FStream := TFileStream.create(FDskFileName, fmOpenReadWrite);
          filemode := 0;
        end;
        FDskStatus := FDskStatus or $80;
      end;
    $B:
      begin
        if FDskStatus and $80 = 0 then
          exit;
        if FDskStatus and $04 > 0 then
          FreeAndNil(FStream);
        FDskStatus := $00;
      end;
    $C:
      begin
        FRegStatus := $24;
        FRegCom := $00;
        FRegTrack := $00;
        FRegSect := $01;
        FRegData := $00;
        FSystem := $3C;
        StepDirect := Shortint($FF);
        if (FDskStatus and $80 > 0) and (FDskStatus and $03 > 0) then
        begin
          DiskVG($B, DataIO);
          DiskVG($A, DataIO);
        end;
      end;
  end;
end;

constructor TBetadisk.create;
var
  i: Byte;
begin
  FReady := true;
  FReadIndexTrack := false;

  FDskIndex[0] := 0;
  FDskIndex[1] := 1;
  FDskIndex[2] := 1;
  FDskIndex[3] := 0;
  FDskIndex[4] := 0;
  for i := low(Disks) to high(Disks) do
    Disks[i] := '';
end;

end.
