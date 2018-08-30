unit UConfigDir;

interface

// All functions return a path with delimiter at the end
function GetConfigPath(const Filename: String; DoCopy: Boolean): String;
function GetConfigDir: String;

function GetApplicationDir: string;
function GetResourceDir: string;
function GetMusicDir: string;
function GetMovieDir: string;
function GetTempDir: String;
function GetRecordingDir: String;

implementation

uses System.SysUtils, System.IOUtils
{$IFDEF MACOS}, MacAPI.CoreFoundation, MacAPI.Foundation, MacAPI.ObjectiveC
{$ENDIF}
{$IFDEF MSWINDOWS}, Winapi.Windows {$ENDIF};

function GetApplicationDir: string;
begin
  Result := ExtractFilePath(Paramstr(0));
end;

{$IFDEF PORTABLE}

function GetConfigPath(const Filename: String; DoCopy: Boolean): String;
begin
  Result := GetConfigDir + Filename;
end;

function GetConfigDir: String;
begin
  Result := GetApplicationDir;
end;

function GetResourceDir: string;
begin
  Result := GetApplicationDir;
end;

function GetMusicDir: string;
begin
  Result := GetApplicationDir;
end;

function GetMovieDir: string;
begin
  Result := GetApplicationDir;
end;

function GetTempDir: String;
begin
  Result := GetApplicationDir;
end;
{$ELSE}

var
  FApplicationName: String = '';

{$IFDEF MACOS}
{$IFNDEF IOS}

function CFToDelphiString(const CFStr: CFStringRef): string;
var
  Range: CFRange;
begin
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  if Range.length = 0 then
    Exit;
  CFStringGetCharacters(CFStr, Range, PWideChar(Result));
end;

function NSToDelphiString(const NSStr: NSString): string; inline;
begin
  Result := CFToDelphiString((NSStr as ILocalObject).GetObjectID);
end;
{$ENDIF}
{$ENDIF}
{$IFDEF MACOS}

var
  FResourceDir: String = '';
{$ENDIF}

function GetResourceDir: string;
{$IFDEF MACOS}
var
  NS: NSBundle;
  AutoReleasePool: NSAutoreleasePool;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  Result := GetApplicationDir;
{$ENDIF}
{$IFDEF MACOS}
  if FResourceDir = '' then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      NS := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
      FResourceDir := NSToDelphiString(NS.ResourcePath) + '/';
    finally
      AutoReleasePool.release;
    end;
  end;
  Result := FResourceDir;
{$ENDIF}
end;

function GetConfigPath(const Filename: String; DoCopy: Boolean): String;
var
  SourceDir, TargetDir: String;
  SearchRec: TSearchRec;
  DosError: Integer;
begin
  Result := GetConfigDir + Filename;
  if (DoCopy) and (not FileExists(Result)) then
  begin
    try
      TargetDir := IncludeTrailingPathDelimiter(ExtractFileDir(Result));
      if not DirectoryExists(TargetDir) then
        ForceDirectories(TargetDir);

      SourceDir := IncludeTrailingPathDelimiter
        (GetResourceDir + ExtractFileDir(Filename));
      DosError := FindFirst(GetResourceDir + Filename, faAnyFile, SearchRec);
      while DosError = 0 do
      begin
        if (SearchRec.Attr <> faDirectory) and
          (not FileExists(TargetDir + SearchRec.Name)) then
          TFile.Copy(SourceDir + SearchRec.Name, TargetDir + SearchRec.Name);
        DosError := FindNext(SearchRec);
      end;
      System.SysUtils.FindClose(SearchRec);
    except
    end;
  end;
end;

// -- Platform dependend code --------------------------------------------------

{$IFDEF MACOS}

function CFStringGetValue(const CFStr: CFStringRef): string;
var
  Range: CFRange;
begin
  if CFStr = nil then
    Exit('');
  Range.location := 0;
  Range.length := CFStringGetLength(CFStr);
  SetLength(Result, Range.length);
  CFStringGetCharacters(CFStr, Range, PChar(Result));
end;

function GetBundleIdentifier: String;
begin
end;

{$ENDIF}
{$IFDEF MSWINDOWS}

Function GetFileInfo(out CompanyName, ProductName: String;
  Filename: String = ''): Boolean;
Var
  VersionInfoSize, VerInfoSize, GetInfoSizeJunk: LongWord;
  VersionInfo, Translation, InfoPointer: Pointer;
  VersionValue: String;
Begin
  Result := False;
  ProductName := '';
  CompanyName := '';
  if Filename = '' then
    Filename := Paramstr(0);

  VerInfoSize := GetFileVersionInfoSize(PChar(Filename), GetInfoSizeJunk);
  If VerInfoSize > 0 Then
  Begin
    GetMem(VersionInfo, VerInfoSize);
    Try
      GetFileVersionInfo(PChar(Filename), 0, VerInfoSize, VersionInfo);

      VerQueryValue(VersionInfo, '\\VarFileInfo\\Translation', Translation,
        VerInfoSize);
      VersionValue := '\\StringFileInfo\\' +
        IntToHex((PLongInt(Translation)^ shl 16) or
        (PLongInt(Translation)^ shr 16), 8) + '\\';
      VersionInfoSize := 0;

      VerQueryValue(VersionInfo, PWideChar(VersionValue + 'ProductName'),
        InfoPointer, VersionInfoSize);
      if VersionInfoSize > 0 then
        ProductName := Trim(PWideChar(InfoPointer));

      VerQueryValue(VersionInfo, PWideChar(VersionValue + 'CompanyName'),
        InfoPointer, VersionInfoSize);
      if VersionInfoSize > 0 then
        CompanyName := Trim(PWideChar(InfoPointer));

    Finally
      FreeMem(VersionInfo);
    End;
    Result := (CompanyName <> '') and (ProductName <> '');
  End;
End;
{$ENDIF}

procedure Prepare;
{$IFDEF MACOS}
var
  BundleHandle: CFBundleRef;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Product, Company: String;
{$ENDIF}
begin
{$IFDEF MACOS}
  BundleHandle := CFBundleGetMainBundle;
  FApplicationName := '';
  if BundleHandle <> nil then
    FApplicationName := CFStringGetValue(CFBundleGetIdentifier(BundleHandle));
{$ENDIF}
{$IFDEF MSWINDOWS}
  if GetFileInfo(Company, Product) then
    FApplicationName := Company + PathDelim + Product;
{$ENDIF}
  if FApplicationName = '' then // Fallback
    FApplicationName := ExtractFileName(ChangeFileExt(Paramstr(0), ''));
end;

function GetConfigDir: String;
begin
{$IFDEF MACOS}
{$IFDEF IOS}
  Result := TPath.GetHomePath + PathDelim + 'Documents' + PathDelim;
{$ELSE}
  Result := TPath.GetHomePath + PathDelim + 'Library/Application Support/' +
    FApplicationName + PathDelim;
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  Result := TPath.GetPublicPath + PathDelim + FApplicationName + PathDelim;
{$ENDIF}
{$IFDEF ANDROID}
  Result := TPath.GetHomePath + PathDelim;
{$ENDIF}
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

function GetTempDir: String;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetTempPath);
end;

function GetMusicDir: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetMusicPath);
end;

function GetMovieDir: string;
begin
  Result := IncludeTrailingPathDelimiter(TPath.GetMoviesPath);
end;

function GetRecordingDir: String;
begin
  Result := IncludeTrailingPathDelimiter(TPath.Combine(TPath.GetMoviesPath,
    'SAT2IP Viewer recordings'));
  if not DirectoryExists(Result) then
    ForceDirectories(Result);
end;

initialization

Prepare;

finalization

{$ENDIF}

end.
