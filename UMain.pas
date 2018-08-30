{$DEFINE THREADTEST}
unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Forms3D, FMX.Graphics, FMX.Dialogs,
  Computer, Computer.Spectrum, FMX.Objects, System.Math.Vectors, FMX.Controls3D,
  FMX.Layers3D, FMX.MaterialSources, FMX.Objects3D, UCRTEffect, FMX.Menus,
  System.Actions, FMX.ActnList, FMX.Controls.Presentation, FMX.StdCtrls;

type
  TExecutor = class(TThread)
  protected
    FHardware: IHardware;
    procedure Execute; override;
  public
    constructor Create(const AHardware: IHardware);
    destructor Destroy;override;
  end;

  TfrmMain = class(TForm3D)
    tmrMain: TTimer;
    rectVideo: TRectangle3D;
    mmMain: TMainMenu;
    mnuView: TMenuItem;
    alMain: TActionList;
    mnuDistortion: TMenuItem;
    actDistortion: TAction;
    actFlicker: TAction;
    actVignette: TAction;
    actScanline: TAction;
    mnuScanline: TMenuItem;
    mnuFlicker: TMenuItem;
    mnuVignette: TMenuItem;
    mnuPixelshift: TMenuItem;
    actNoPixelShift: TAction;
    actHylian: TAction;
    actSinus: TAction;
    mnuNone: TMenuItem;
    mnuHylian: TMenuItem;
    mnuSinus: TMenuItem;
    mnuMain: TMenuItem;
    mnuAbout: TMenuItem;
    mnuSeparator: TMenuItem;
    mnuOptions: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuServices: TMenuItem;
    mnuSeparator2: TMenuItem;
    mnuHideApp: TMenuItem;
    mnuHideOthers: TMenuItem;
    mnuShowAll: TMenuItem;
    mnuSeparator3: TMenuItem;
    mnuQuit: TMenuItem;
    mnuHelp: TMenuItem;
    mnuDebugger: TMenuItem;
    mnuFile: TMenuItem;
    mnuOpenSnapshot: TMenuItem;
    mnuSaveSnapshot: TMenuItem;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    MenuItem1: TMenuItem;
    mnuReset: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure tmrMainTimer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure Form3DResize(Sender: TObject);
    procedure actDistortionUpdate(Sender: TObject);
    procedure actDistortionExecute(Sender: TObject);
    procedure actFlickerExecute(Sender: TObject);
    procedure actFlickerUpdate(Sender: TObject);
    procedure actVignetteExecute(Sender: TObject);
    procedure actVignetteUpdate(Sender: TObject);
    procedure actScanlineExecute(Sender: TObject);
    procedure actScanlineUpdate(Sender: TObject);
    procedure actNoPixelShiftUpdate(Sender: TObject);
    procedure actNoPixelShiftExecute(Sender: TObject);
    procedure actHylianExecute(Sender: TObject);
    procedure actHylianUpdate(Sender: TObject);
    procedure actSinusExecute(Sender: TObject);
    procedure actSinusUpdate(Sender: TObject);
    procedure mnuHideAppClick(Sender: TObject);
    procedure mnuHideOthersClick(Sender: TObject);
    procedure mnuShowAllClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure Form3DDestroy(Sender: TObject);
    procedure mnuDebuggerClick(Sender: TObject);
    procedure mnuOpenSnapshotClick(Sender: TObject);
    procedure mnuSaveSnapshotClick(Sender: TObject);
    procedure mnuResetClick(Sender: TObject);
  private
    { Private-Deklarationen }
    FHardware: IHardware;
{$IFDEF THREADTEST}
    FExecutor: TExecutor;
{$ENDIF}
    FMaterial: TCrtTextureMaterialSource;

    procedure DoPaintSpeccy(Sender: TObject; const AImage, ABorder: PByte;
      const AWidth, AHeight, AScanline: Integer);
  public
    { Public-Deklarationen }
  end;

var
  frmMain: TfrmMain;

implementation

uses System.Audio, Computer.Spectrum.Graphic, Computer.Spectrum.Tape
{$IFDEF MACOS}, Macapi.CoreGraphics, FMX.Platform.Mac,
  Macapi.Helpers, Macapi.AppKit
{$ENDIF}, UDebugger;

{$R *.fmx}
{$IFDEF CPUARM}

procedure FillDWord(var Dest; Count: Integer; Value: Cardinal);
var
  pStart, pEnd: ^Cardinal;
begin
  pStart := @Dest;
  pEnd := pStart;
  inc(pEnd, Count);
  while PByte(pStart) < PByte(pEnd) do
  begin
    pStart^ := Value;
    inc(pStart);
  end;
end;

{$ELSE}

procedure FillDWord(var Dest; Count: Integer; Value: DWord); register;
asm
  push edi
  mov edi,eax
  mov eax,ecx
  mov ecx,edx
  rep stosd
  pop edi
end;
{$ENDIF}

procedure FitRect(var r: TRectF; const BoundsRect: TRectF);
var
  aspect_ratio, w, h: double;
begin
  if RectWidth(BoundsRect) * RectHeight(BoundsRect) = 0 then
    Exit;
  if r.EqualsTo(BoundsRect) then
    Exit;

  aspect_ratio := RectWidth(r) / RectHeight(r);

  h := RectHeight(BoundsRect);
  w := round(h * aspect_ratio) and (not 1);
  if (w > RectWidth(BoundsRect)) then
  begin
    w := RectWidth(BoundsRect);
    h := round(w / aspect_ratio) and (not 1);
  end;
  r := RectF(0, 0, w, h);
  RectCenter(r, BoundsRect);
end;

constructor TExecutor.create(const AHardware: IHardware);
begin
  inherited create(false);
  FHardware := AHardware;
end;

destructor TExecutor.Destroy;
begin
  Terminate;
  WaitFor;
  inherited;
end;

procedure TExecutor.Execute;
begin
  while not terminated do
  begin
    FHardware.CycleTick;
    sleep(10);
  end;
end;

procedure TfrmMain.Form3DDestroy(Sender: TObject);
begin
  FHardware := nil;
{$IFDEF THREADTEST}
  FreeAndNil(FExecutor);
{$ENDIF}
end;

procedure TfrmMain.Form3DResize(Sender: TObject);
const
  CONTEXT = 0.99;
var
  ar, w, h: single;
begin
  ar := 320 / 256;

  if ar = 0 then
  begin
    w := CONTEXT * ClientWidth;
    h := CONTEXT * ClientHeight;
  end
  else
  begin
    h := CONTEXT * ClientHeight;
    w := h * ar;
    if w > ClientWidth then
    begin
      w := CONTEXT * ClientWidth;
      h := w / ar;
    end;
  end;

  rectVideo.SetSize(w, h, 0);
  rectVideo.Position.Point := Point3D(ClientWidth / 2, ClientHeight / 2, 0);

  if assigned(FMaterial) then
    with TCrtTextureMaterial(FMaterial.Material) do
    begin
      Width := w;
      Height := h;
    end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
{$IFDEF MACOS}
  WindowHandleToPlatform(Handle).Wnd.setFrameAutosaveName
    (StrToNSStr(ClassName));
  mnuQuit.ShortCut := scCommand or Ord('Q');
  mnuOptions.ShortCut := scCommand or 188; // Ord(',');
{$ENDIF}
  FHardware := TZXSpectrum.create;
  FHardware.SetPaintCallback(DoPaintSpeccy);
  // FHardware.LoadFromFile('The Way Of The Exploding Fist (Dro Soft).tzx'); //
  // FHardware.LoadFromFile('MatthewCranstonBattles.tap');
  // War - Alpha (Winner).tzx');//1999.z80');
  // FHardware.LoadFromFile('ltk48.tap');
  FHardware.SetModel(5);
  caption := FHardware.GetDeviceName;
{$IFDEF THREADTEST}
  FExecutor := TExecutor.create(FHardware);
{$ENDIF}
  FMaterial := TCrtTextureMaterialSource.create(self);
  FMaterial.Parent := self;
  rectVideo.MaterialSource := FMaterial;

  ClientWidth := 320;
  ClientHeight := 256;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  k: Word;
begin
  if Key = 122 then
    FHardware.NMI;

  If ssAlt in Shift Then
    Exit;
  if KeyChar = #0 then
    k := Key
  else
    k := Ord(Upcase(KeyChar));
{$IFDEF MACOS}
  if (CGEventSourceKeyState(0, $38) <> 0) or (CGEventSourceKeyState(0, $3C) <> 0)
  then
    Include(Shift, ssShift);
{$ENDIF}
  FHardware.doKey(true, k, Shift);

   if k=Ord('Q') then FHardware.SetKempstonValue(FHardware.GetKempstonValue or $08);
   if k=Ord('A') then FHardware.SetKempstonValue(FHardware.GetKempstonValue or $04);
   if k=Ord('O') then FHardware.SetKempstonValue(FHardware.GetKempstonValue or $02);
   if k=Ord('P') then FHardware.SetKempstonValue(FHardware.GetKempstonValue or $01);
   if k=32 then FHardware.SetKempstonValue(FHardware.GetKempstonValue or $10);

  if not(ssCommand in Shift) then
    Key := 0;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
  Shift: TShiftState);
var
  k: Word;
begin
  if KeyChar = #0 then
    k := Key
  else
    k := Ord(Upcase(KeyChar));
{$IFDEF MACOS}
  if (CGEventSourceKeyState(0, $38) <> 0) or (CGEventSourceKeyState(0, $3C) <> 0)
  then
    Include(Shift, ssShift);
{$ENDIF}
  FHardware.SetKempstonValue(0);
  FHardware.doKey(false, k, Shift);
end;

procedure TfrmMain.mnuDebuggerClick(Sender: TObject);
begin
 frmDebug.showmodal;
end;

procedure TfrmMain.mnuHideAppClick(Sender: TObject);
begin
{$IFDEF MACOS}
  TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).Hide(nil);
{$ENDIF}
end;

procedure TfrmMain.mnuHideOthersClick(Sender: TObject);
begin
{$IFDEF MACOS}
  TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication)
    .hideOtherApplications(nil);
  mnuHideOthers.Enabled := false;
  mnuShowAll.Enabled := true;
{$ENDIF}
end;

procedure TfrmMain.mnuOpenSnapshotClick(Sender: TObject);
begin
 if dlgOpen.Execute then
  FHardware.LoadFromFile(dlgOpen.FileName);
end;

procedure TfrmMain.mnuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuResetClick(Sender: TObject);
begin
 FHardware.Reset;
end;

procedure TfrmMain.mnuSaveSnapshotClick(Sender: TObject);
begin
  if dlgSave.Execute then
   FHardware.SaveToFile(dlgSave.FileName);
end;

procedure TfrmMain.mnuShowAllClick(Sender: TObject);
begin
{$IFDEF MACOS}
  TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication)
    .unhideAllApplications(nil);
  mnuHideOthers.Enabled := true;
  mnuShowAll.Enabled := false;
{$ENDIF}
end;

procedure TfrmMain.tmrMainTimer(Sender: TObject);
begin
{$IFNDEF THREADTEST}
  FHardware.CycleTick;
{$ENDIF}
end;

{$IFDEF THREADTEST}

procedure TfrmMain.DoPaintSpeccy(Sender: TObject; const AImage, ABorder: PByte;
  const AWidth, AHeight, AScanline: Integer);
begin
  TThread.Synchronize(nil,
    procedure()
    var
      Data: TBitmapData;
      dst: PByte;
      src: PByte;
      y: Integer;
    begin
      with FMaterial.Texture do
      begin
        if (Width <> AWidth) and (Height <> AHeight) then
          Resize(AWidth, AHeight);
        Map(TMapAccess.Write, Data);
        try
          if Data.Pitch = AScanline then
            move(AImage^, Data.Data^, AScanline * Height)
          else
          begin
            src := AImage;
            dst := Data.Data;

            for y := 0 to AHeight - 1 do
            begin
              move(src^, dst^, AScanline);
              inc(dst, Data.Pitch);
              inc(src, AScanline);
            end;
          end;
        finally
          Unmap(Data);
        end;
      end;
    end);
end;
{$ELSE}

procedure TForm3.DoPaintSpeccy(Sender: TObject; const AImage, ABorder: PByte;
const AWidth, AHeight, AScanline: Integer);
var
  Data: TBitmapData;
  dst: PByte;
  src: PByte;
  y: Integer;
begin
  with FMaterial.Texture do
  begin
    if (Width <> AWidth) and (Height <> AHeight) then
      Resize(AWidth, AHeight);

    Map(TMapAccess.Write, Data);
    try
      if Data.Pitch = AScanline then
        move(AImage^, Data.Data^, AScanline * Height)
      else
      begin
        src := AImage;
        dst := Data.Data;

        for y := 0 to AHeight - 1 do
        begin
          move(src^, dst^, AScanline);
          inc(dst, Data.Pitch);
          inc(src, AScanline);
        end;
      end;
    finally
      Unmap(Data);
    end;
  end;
end;
{$ENDIF}

procedure TfrmMain.actDistortionExecute(Sender: TObject);
begin
  with TCrtTextureMaterial(FMaterial.Material) do
    Distortion := not Distortion;
end;

procedure TfrmMain.actDistortionUpdate(Sender: TObject);
begin
  actDistortion.Checked := TCrtTextureMaterial(FMaterial.Material).Distortion;
end;

procedure TfrmMain.actFlickerExecute(Sender: TObject);
begin
  with TCrtTextureMaterial(FMaterial.Material) do
    Flicker := not Flicker;
end;

procedure TfrmMain.actFlickerUpdate(Sender: TObject);
begin
  actFlicker.Checked := TCrtTextureMaterial(FMaterial.Material).Flicker;
end;

procedure TfrmMain.actHylianExecute(Sender: TObject);
begin
  TCrtTextureMaterial(FMaterial.Material).PixelShift := 1;
end;

procedure TfrmMain.actHylianUpdate(Sender: TObject);
begin
  actHylian.Checked := TCrtTextureMaterial(FMaterial.Material).PixelShift = 1;
end;

procedure TfrmMain.actNoPixelShiftExecute(Sender: TObject);
begin
  TCrtTextureMaterial(FMaterial.Material).PixelShift := 0;
end;

procedure TfrmMain.actNoPixelShiftUpdate(Sender: TObject);
begin
  actNoPixelShift.Checked := TCrtTextureMaterial(FMaterial.Material)
    .PixelShift = 0;
end;

procedure TfrmMain.actScanlineExecute(Sender: TObject);
begin
  with TCrtTextureMaterial(FMaterial.Material) do
    Scanlines := not Scanlines;
end;

procedure TfrmMain.actScanlineUpdate(Sender: TObject);
begin
  actScanline.Checked := TCrtTextureMaterial(FMaterial.Material).Scanlines;
end;

procedure TfrmMain.actSinusExecute(Sender: TObject);
begin
  TCrtTextureMaterial(FMaterial.Material).PixelShift := 2;
end;

procedure TfrmMain.actSinusUpdate(Sender: TObject);
begin
  actSinus.Checked := TCrtTextureMaterial(FMaterial.Material).PixelShift = 2;
end;

procedure TfrmMain.actVignetteExecute(Sender: TObject);
begin
  with TCrtTextureMaterial(FMaterial.Material) do
    Vignette := not Vignette;
end;

procedure TfrmMain.actVignetteUpdate(Sender: TObject);
begin
  actVignette.Checked := TCrtTextureMaterial(FMaterial.Material).Vignette;
end;

end.
