unit sdl;

{$IF defined(CPUARM) and defined(IOS)}
{$DEFINE LINKLIB}
{$ENDIF}

interface

const
{$IFDEF MACOS}
{$IFDEF CPUARM}
  libsdl = 'libSDL2.a';
{$ELSE}
{$IFDEF IOS}
  libsdl = 'libSDL2.dylib';
{$ELSE}
  libsdl = 'libSDL.dylib';
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  libsdl = 'SDL.dll';
{$ENDIF}
{$IFDEF ANDROID}
  libsdl = 'libSDL2.so';
{$ENDIF}

const
  // SDL.h constants
  SDL_INIT_TIMER = $00000001;
  SDL_INIT_AUDIO = $00000010;
  SDL_INIT_VIDEO = $00000020;
  SDL_INIT_CDROM = $00000100;
  SDL_INIT_JOYSTICK = $00000200;
  SDL_INIT_NOPARACHUTE = $00100000; // Don't catch fatal signals
  SDL_INIT_EVENTTHREAD = $01000000; // Not supported on all OS's
  SDL_INIT_EVERYTHING = $0000FFFF;
  // SDL_audio.h constants
  AUDIO_U8 = $0008; // Unsigned 8-bit samples
  AUDIO_S8 = $8008; // Signed 8-bit samples
  AUDIO_U16LSB = $0010; // Unsigned 16-bit samples
  AUDIO_S16LSB = $8010; // Signed 16-bit samples
  AUDIO_U16MSB = $1010; // As above, but big-endian byte order
  AUDIO_S16MSB = $9010; // As above, but big-endian byte order
  AUDIO_U16 = AUDIO_U16LSB;
  AUDIO_S16 = AUDIO_S16LSB;
  // Native audio byte ordering
  AUDIO_U16SYS = AUDIO_U16LSB;
  AUDIO_S16SYS = AUDIO_S16LSB;

type
  TSDL_AudioSpecCallback = procedure(userdata: Pointer; stream: PByte;
    len: Integer); cdecl;

  TSDL_AudioSpec = record
    freq: Integer; // DSP frequency -- samples per second
    format: Word; // Audio data format
    channels: Byte; // Number of channels: 1 mono, 2 stereo
    silence: Byte; // Audio buffer silence value (calculated)
    samples: Word; // Audio buffer size in samples
    padding: Word; // Necessary for some compile environments
    size: Cardinal; // Audio buffer size in bytes (calculated)
    { This function is called when the audio device needs more data.
      'stream' is a pointer to the audio data buffer
      'len' is the length of that buffer in bytes.
      Once the callback returns, the buffer will no longer be valid.
      Stereo samples are stored in a LRLRLR ordering. }
    callback: TSDL_AudioSpecCallback;
    userdata: Pointer;
  end;

  PSDL_AudioSpec = ^TSDL_AudioSpec;

  PSDL_version = ^TSDL_version;

  TSDL_version = record
    major: Byte;
    minor: Byte;
    patch: Byte;
  end;

{$IFDEF LINKLIB}

function SDL_Init(flags: Cardinal): Integer; cdecl; external libsdl;
function SDL_GetError: PByte; cdecl; external libsdl;
procedure SDL_Quit; cdecl; external libsdl;
function SDL_OpenAudioDevice(device: PByte; iscapture: Integer;
  desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec; allowed_changes: Integer)
  : Cardinal; cdecl; external libsdl;
procedure SDL_ClearQueuedAudio(dev: Cardinal); cdecl; external libsdl;
procedure SDL_PauseAudioDevice(dev: Cardinal; pause_on: Integer)cdecl;
  external libsdl;
procedure SDL_CloseAudioDevice(dev: Cardinal)cdecl; external libsdl;
function SDL_QueueAudio(dev: Cardinal; data: Pointer; len: Cardinal): Integer;
  cdecl; external libsdl;
{$ELSE}

var
  SDL_Init: function(flags: Cardinal): Integer; cdecl;
  SDL_GetError: function: PByte; cdecl;
  SDL_Quit: procedure; cdecl;
  SDL_OpenAudioDevice: function(device: PByte; iscapture: Integer;
    desired: PSDL_AudioSpec; obtained: PSDL_AudioSpec; allowed_changes: Integer)
    : Cardinal cdecl;
  SDL_ClearQueuedAudio: procedure(dev: Cardinal); cdecl;
  SDL_PauseAudioDevice: procedure(dev: Cardinal; pause_on: Integer)cdecl;
  SDL_CloseAudioDevice: procedure(dev: Cardinal)cdecl;
  SDL_QueueAudio: function(dev: Cardinal; data: Pointer; len: Cardinal)
    : Integer; cdecl;
{$ENDIF}
function SDL_AVAILABLE: Boolean;

implementation

uses SysUtils
{$IFDEF MSWINDOWS}
  , windows
{$ENDIF};

{$IFDEF LINKLIB}

function SDL_AVAILABLE: Boolean;
begin
  result := true;
end;
{$ELSE}

var
  SDLHandle: THandle;

procedure LoadSDLLibs;
{$IFDEF Android}
var
  PActivity: PANativeActivity;
  res: Integer;
{$ENDIF}
begin
{$IFNDEF IOS}
  if SDLHandle = 0 then
  begin
    SDLHandle := LoadLibrary(libsdl);

    @SDL_Init := GetProcAddress(SDLHandle, 'SDL_Init');
    @SDL_GetError := GetProcAddress(SDLHandle, 'SDL_GetError');
    @SDL_Quit := GetProcAddress(SDLHandle, 'SDL_Quit');

    @SDL_OpenAudioDevice := GetProcAddress(SDLHandle, 'SDL_OpenAudioDevice');
    @SDL_ClearQueuedAudio := GetProcAddress(SDLHandle, 'SDL_ClearQueuedAudio');
    @SDL_PauseAudioDevice := GetProcAddress(SDLHandle, 'SDL_PauseAudioDevice');
    @SDL_CloseAudioDevice := GetProcAddress(SDLHandle, 'SDL_CloseAudioDevice');
    @SDL_QueueAudio := GetProcAddress(SDLHandle, 'SDL_QueueAudio');

{$IFDEF Android}
    @JNI_OnLoad := GetProcAddress(SDLHandle, 'JNI_OnLoad');
    //
    PActivity := PANativeActivity(System.DelphiActivity);
    res := JNI_OnLoad(PActivity^.vm, nil);
    if res < 0 then
      raise Exception.Create('init JNI failed');

    res := av_jni_set_java_vm(PActivity^.vm, nil);
    if res < 0 then
      raise Exception.Create('failed to set JNI activity');
{$ENDIF}
  end;
{$ENDIF}
end;

function SDL_AVAILABLE: Boolean;
begin
  result := SDLHandle <> 0;
end;

initialization

LoadSDLLibs;

finalization

FreeLibrary(SDLHandle);
{$ENDIF}

end.
