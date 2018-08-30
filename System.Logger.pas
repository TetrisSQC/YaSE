unit System.Logger;

interface

procedure Log(const Line: String);

implementation

uses SysUtils, Classes;

var
  FLogfile: TStringlist;

function GetLogName: String;
begin
  result := extractFilepath(paramstr(0)) + 'speccy.log';
end;

procedure Log(const Line: String);
begin
  if not assigned(FLogfile) then
  begin
    FLogfile := TStringlist.create;
    if FileExists(GetLogName) then
      FLogfile.LoadFromFile(GetLogName);
  end;
  FLogfile.Add(Line);
end;

initialization

finalization

if assigned(FLogfile) then
begin
  FLogfile.SaveToFile(GetLogName);
  FLogfile.Free;
end;

end.
