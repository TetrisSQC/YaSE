unit UDebugger;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ListBox,
  Computer;

const
  STATE_RUNNING = 0;
  STATE_PAUSED = 1;
  STATE_STEP = 2;
  BP_MAX = 200;

type
  TBreakPoints = Array [0 .. BP_MAX] of Word;

  TfrmDebug = class(TForm)
    lblCaptionAF: TLabel;
    lblAF: TLabel;
    lblBCCaption: TLabel;
    lblBC: TLabel;
    lblDEDescription: TLabel;
    lblDE: TLabel;
    lblHLDescription: TLabel;
    lblHL: TLabel;
    lblIXDescription: TLabel;
    lblIX: TLabel;
    lblIYDescripton: TLabel;
    lblIY: TLabel;
    lblSPDescription: TLabel;
    lblSP: TLabel;
    lblPCDescription: TLabel;
    lblPC: TLabel;
    lblIRDescription: TLabel;
    lblIR: TLabel;
    lblIMMDescription: TLabel;
    lblIMM: TLabel;
    lblIFF1Description: TLabel;
    lblIFF1: TLabel;
    lblIFF2Description: TLabel;
    lblIFF2: TLabel;
    gbFlags: TGroupBox;
    lblSFlag: TLabel;
    lblZFlag: TLabel;
    lblHFlag: TLabel;
    lblPFlag: TLabel;
    lblNFlag: TLabel;
    lblCFlag: TLabel;
    lblUD5Flag: TLabel;
    lblUD3Flag: TLabel;
    btnAddWatch: TButton;
    btnDelWatch: TButton;
    lbWatches: TListBox;
    btnPause: TButton;
    btnPlay: TButton;
    btnStep: TButton;
    gbNextInstr: TGroupBox;
    lblMemory: TLabel;
    lblInstruction: TLabel;
    gbMemory: TGroupBox;
    lblRAM0Description: TLabel;
    lblRAM0: TLabel;
    lblRAM1: TLabel;
    lblRAM1Description: TLabel;
    lblRAM2: TLabel;
    lblRAM2Description: TLabel;
    lblRamVideo: TLabel;
    lblRamVideoDescription: TLabel;
    lblRAM4Description: TLabel;
    lblRAM4: TLabel;
    lblRAM3: TLabel;
    lblRAM3Description: TLabel;
    cbBreak: TComboBox;
    btnAddBreak: TButton;
    btnDelBreak: TButton;
    cbBreakPoint: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure btnAddWatchClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FDebugState: Integer;
    AResult: array [0 .. 31] of String;
    FBreakpoints: TBreakPoints;
    FBreakCount: Word;
    procedure GetBreakpoints;
    procedure SetDebugState(const aState: Integer);
  public
    { Public-Deklarationen }

    procedure ClearList;
    procedure EvalWatches(const Hardware: IHardware);
    property Breakpoints: TBreakPoints read FBreakpoints;
    property BreakCount: Word read FBreakCount;
    property DebugState: Integer read FDebugState write SetDebugState;
  end;

var
  frmDebug: TfrmDebug;

implementation

{$R *.fmx}

uses UAddwatch;

const
  Z80RUNNING = 1;
  Z80STOPPED = 2;

var
  z80runstate: Integer;

procedure TfrmDebug.SetDebugState(const aState: Integer);
begin
  FDebugState := aState;
  case FDebugState of
    STATE_RUNNING:
      begin
        Caption := 'Debug (Running)';
        z80runstate := Z80RUNNING;
      end;
    STATE_STEP:
      begin
        Caption := 'Debug (Step)';
        z80runstate := Z80RUNNING;
      end;
    STATE_PAUSED:
      begin
        Caption := 'Debug (Paused)';
        z80runstate := Z80STOPPED;
      end;
  end;
end;

(*
  if lbWatches.ItemIndex >= 0 then
  begin
  lbWatches.Items.Delete(lbWatches.ItemIndex);
  end
  else
  begin
  ShowMessage('You need to choose a Watch before delete it!');
  end;

  procedure TfrmDebug.sbAddBreakClick(Sender: TObject);
  begin
  frmBreakPoint.SetPos(Left + sbAddBreak.Left + 8, Top + sbAddBreak.Top + 8);
  frmBreakPoint.ShowModal;
  if frmBreakPoint.ModalResult = MROK then
  begin
  cbBreak.ItemIndex := cbBreak.Items.Add(frmBreakPoint.edAddr.Text);
  GetBreakpoints;
  end;
  end;

  procedure TfrmDebug.sbDelBreakClick(Sender: TObject);
  begin
  if cbBreak.ItemIndex >= 0 then
  begin
  cbBreak.Items.Delete(cbBreak.ItemIndex);
  GetBreakpoints;
  if cbBreak.Items.Count > 0 then
  cbBreak.ItemIndex := 0
  else
  cbBreak.ItemIndex := -1
  end;
  end;

*)

procedure TfrmDebug.btnAddWatchClick(Sender: TObject);
var
  cAux: String;
begin
  frmAddWatch.ShowModal;
  if frmAddWatch.ModalResult = MROK then
  begin
    with frmAddWatch do
      cAux := edAddr.Text + ',' + edDesloc.Text + ',' +
       IntToStr(MemoryAddress) + ',' + IntToStr(ViewType) + ',' +
       IntToStr(ValueType) + ',?????';
      lbWatches.Items.Add(cAux);
  end;

end;

procedure TfrmDebug.ClearList;
begin
  lbWatches.Items.Clear;
  lbWatches.Items.Add('0000,00,0,0,0,????');
  lbWatches.Items.Add('0000,00,1,0,0,????');
  lbWatches.Items.Add('0000,00,2,0,0,????');
  lbWatches.Items.Add('0000,00,3,0,0,????');
  lbWatches.Items.Add('0000,00,4,0,0,????');
  lbWatches.Items.Add('0000,00,6,0,0,????');
  lbWatches.Items.Add('0000,00,7,0,1,????');
end;

(*
  procedure TfrmDebug.lbWatchesDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
  var
  I: Integer;
  cItem, cAux: String;
  cItems: array [1 .. 10] of String[7];
  nCont: Integer;
  begin
  with lbWatches.Canvas, Rect do
  begin
  FillRect(Rect);

  cItem := lbWatches.Items[Index];
  nCont := 0;
  cAux := '';
  for I := 1 to Length(cItem) + 1 do
  begin
  if (I > Length(cItem)) or (cItem[I] = ',') then
  begin
  Inc(nCont);
  cItems[nCont] := cAux;
  cAux := '';
  end
  else
  begin
  cAux := cAux + cItem[I];
  end;
  end;

  cAux := '???';
  if cItems[5] <> '' then
  begin
  if cItems[5][1] = '0' then
  begin
  cAux := 'bptr ';
  end
  else
  begin
  cAux := 'wptr ';
  end;
  end;

  if cItems[3] <> '' then
  begin
  case cItems[3][1] of
  '0':
  cAux := cAux + '[BC]';
  '1':
  cAux := cAux + '[DE]';
  '2':
  cAux := cAux + '[HL]';
  '3':
  cAux := cAux + '[IX+' + cItems[2] + ']';
  '4':
  cAux := cAux + '[IY+' + cItems[2] + ']';
  '5':
  cAux := cAux + '[' + cItems[1] + ']';
  '6':
  cAux := cAux + '[PC]';
  '7':
  cAux := cAux + '[SP]';
  end;
  end;

  if not(ODSELECTED in State) then
  Font.Color := CLBLUE;
  TextOut(Left, Top, cAux);

  if not(ODSELECTED in State) then
  Font.Color := CLRED;
  TextOut(Left + 48, Top, AResult[Index]);
  end;
  end; *)

procedure TfrmDebug.EvalWatches(const Hardware: IHardware);
var
  I, nIndex, nAddr, nDesloc: Integer;
  cItem, cAux: String;
  cItems: array [1 .. 10] of String;
  nCont: Integer;
  Regs: TZ80Register;
  valA, valB: Byte;
begin
  for nIndex := 0 to lbWatches.Items.Count - 1 do
  begin
    cItem := lbWatches.Items[nIndex];
    nCont := 0;
    cAux := '';
    for I := 1 to Length(cItem) + 1 do
    begin
      if (I > Length(cItem)) or (cItem[I] = ',') then
      begin
        Inc(nCont);
        cItems[nCont] := cAux;
        cAux := '';
      end
      else
      begin
        cAux := cAux + cItem[I];
      end;
    end;

    nAddr := StrToInt('$' + cItems[1]);
    nDesloc := ShortInt(StrToInt('$' + cItems[2]));
    Regs := Hardware.GetRegister;
    begin
      if cItems[3] <> '' then
      begin
        with Regs do
          case cItems[3][1] of
            '0':
              nAddr := BC.W;
            '1':
              nAddr := DE.W;
            '2':
              nAddr := HL.W;
            '3':
              nAddr := IX.W + nDesloc;
            '4':
              nAddr := IY.W + nDesloc;
            '6':
              nAddr := PC.W;
            '7':
              nAddr := SP.W;
          end;
      end;

      cAux := '????';
      if cItems[5] <> '' then
      begin
        if cItems[5][1] = '0' then
        begin
          Hardware.PeekByte(nAddr, valB);
          cAux := IntToHex(valB, 2);
        end
        else
        begin
          Hardware.PeekByte(nAddr + 1, valA);
          Hardware.PeekByte(nAddr, valB);
          cAux := IntToHex(valA shl 8 + valB, 4);
        end;
      end;
    end;
    AResult[nIndex] := cAux;

  end;
end;

procedure TfrmDebug.FormCreate(Sender: TObject);
begin
  ClearList;
end;

procedure TfrmDebug.GetBreakpoints;
var
  I: Integer;
begin
  fillchar(FBreakpoints, sizeof(FBreakpoints), 0);
  for I := 0 to cbBreak.Items.Count - 1 do
    FBreakpoints[I] := StrToInt('$' + cbBreak.Items[I]);
  FBreakCount := cbBreak.Items.Count;
end;

end.
