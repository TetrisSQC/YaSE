unit UAddwatch;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit;

type
  TfrmAddWatch = class(TForm)
    gbMemorywatch: TGroupBox;
    rbBC: TRadioButton;
    rbDE: TRadioButton;
    rbHL: TRadioButton;
    rbIY: TRadioButton;
    rbNNNN: TRadioButton;
    rbIX: TRadioButton;
    gbView: TGroupBox;
    rbHexy: TRadioButton;
    rbDecimal: TRadioButton;
    rbBinary: TRadioButton;
    gbType: TGroupBox;
    rbWordPtr: TRadioButton;
    rbBytePtr: TRadioButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    edAddr: TEdit;
    edDesloc: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure edAddrKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }

    function MemoryAddress: Integer;
    function ViewType: Integer;
    function ValueType: Integer;
  end;

var
  frmAddWatch: TfrmAddWatch;

implementation

{$R *.fmx}

procedure TfrmAddWatch.edAddrKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not(KeyChar in [#8, '0' .. '9', 'A' .. 'F', 'a' .. 'f']) then
  begin
    KeyChar := #0;
  end;
end;

function TfrmAddWatch.MemoryAddress: Integer;
begin
  if rbBC.IsChecked then
    result := 0
  else if rbDE.IsChecked then
    result := 1
  else if rbHL.IsChecked then
    result := 2
  else if rbIY.IsChecked then
    result := 3
  else if rbIX.IsPressed then
    result := 4
  else
    result := 5;
end;

function TfrmAddWatch.ValueType: Integer;
begin
  if rbBytePtr.IsChecked then
    result := 0
  else
    result := 1;
end;

function TfrmAddWatch.ViewType: Integer;
begin
  if rbHexy.IsChecked then
    result := 0
  else if rbDecimal.IsChecked then
    result := 1
  else
    result := 2;
end;

end.
