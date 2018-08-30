program Yase;

uses
  System.StartUpCopy,
  FMX.Forms,
  UMain in 'UMain.pas' {frmMain},
  Computer in 'Emulator\Computer.pas',
  Computer.CPU.Z80 in 'Emulator\Computer.CPU.Z80.pas',
  Computer.Spectrum.Tape in 'Emulator\Computer.Spectrum.Tape.pas',
  Computer.Spectrum.Disk in 'Emulator\Computer.Spectrum.Disk.pas',
  Computer.Spectrum.Graphic in 'Emulator\Computer.Spectrum.Graphic.pas',
  Computer.Audio.AY8912 in 'Emulator\Computer.Audio.AY8912.pas',
  Computer.Spectrum in 'Emulator\Computer.Spectrum.pas',
  System.Audio in 'Emulator\System.Audio.pas',
  Computer.CPU.Z80.Disassembler in 'Emulator\Computer.CPU.Z80.Disassembler.pas',
  System.Logger in 'System.Logger.pas',
  UCrtEffect in 'UCrtEffect.pas',
  UDebugger in 'UDebugger.pas' {frmDebug},
  UAddwatch in 'UAddwatch.pas' {frmAddWatch};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmDebug, frmDebug);
  Application.CreateForm(TfrmAddWatch, frmAddWatch);
  Application.Run;
end.
