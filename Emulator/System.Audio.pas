{$R+}
unit System.Audio;

interface

uses
  SDL, Computer.Audio.AY8912;

type
  TWavBuffer = array [0 .. 48000] of Byte;

  TAudioRenderer = class
  protected
    FAYUpdateCount: integer;
    FWaveOut: TWavBuffer;
    FWavePtr: integer;
    FCounter: integer;

  public
    constructor Create;

    procedure Add(const ts: integer; const glWaveAddTStates: integer;
      const glBeeperVal: integer; const AY8912: TAY8912 = nil);
    procedure Update; virtual;
  end;

  TSDLRenderer = class(TAudioRenderer)
  private
    FHandle: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Update; override;
  end;

function Audio: TAudioRenderer;

implementation

uses SysUtils, Math;

var
  FAudio: TAudioRenderer;

function Audio: TAudioRenderer;
begin
  if not assigned(FAudio) then
    FAudio := TSDLRenderer.Create;
  result := FAudio;
end;

{ TAudioRenderer }

constructor TAudioRenderer.Create;
begin
  fillchar(FWaveOut, sizeof(FWaveOut), 0);
end;

procedure TAudioRenderer.Add(const ts: integer; const glWaveAddTStates: integer;
  const glBeeperVal: integer; const AY8912: TAY8912 = nil);
var
  value: Byte;
begin
  If FAYUpdateCount = 800 Then
  begin
    if assigned(AY8912) then
      AY8912.Update;
    FAYUpdateCount := 0;
  End
  else
    inc(FAYUpdateCount);

  inc(FCounter, ts);

  value := glBeeperVal;

  if assigned(AY8912) then
    value := value + AY8912.RenderByte;

  While (FCounter >= glWaveAddTStates) and (FWavePtr < high(FWaveOut)) do
  begin
    FWaveOut[FWavePtr] := value;
    inc(FWavePtr);
    FWaveOut[FWavePtr] := value;
    inc(FWavePtr);

    FWaveOut[FWavePtr] := value;
    inc(FWavePtr);
    FWaveOut[FWavePtr] := value;
    inc(FWavePtr);

    dec(FCounter, glWaveAddTStates);
  end;
End;

procedure TAudioRenderer.Update;
var
  i: integer;
begin
  if FWavePtr > 0 then
    for i := FWavePtr to high(FWaveOut) do
      FWaveOut[i] := FWaveOut[i - 1];
  FWavePtr := 0;
end;

{ TSDLRenderer }

constructor TSDLRenderer.Create;
var
  wanted, have: TSDL_AudioSpec;
begin
  inherited;
  if SDL_AVAILABLE then
  begin
    SDL_INIT(SDL_INIT_AUDIO);

    wanted.freq := 22050;
    wanted.format := AUDIO_S16;
    wanted.channels := 2;
    wanted.samples := 4096;

    wanted.silence := 0;
    wanted.size := 0;
    wanted.callback := nil;
    wanted.userdata := nil;
    wanted.padding := 0;

    FHandle := SDL_OpenAudioDevice(nil, 0, @wanted, @have, 0);
    SDL_ClearQueuedAudio(FHandle);
    SDL_PauseAudioDevice(FHandle, 0);
  end
  else
    FHandle := 0;
end;

destructor TSDLRenderer.Destroy;
begin
  if FHandle <> 0 then
    SDL_CloseAudioDevice(FHandle);
  inherited;
end;

procedure TSDLRenderer.Update;
begin
  if (FHandle <> 0) and (FWavePtr > 0) then
    SDL_QueueAudio(FHandle, @FWaveOut, FWavePtr);
  FWavePtr := 0;
end;

initialization

finalization

FreeAndNil(FAudio);

end.
