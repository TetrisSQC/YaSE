unit Computer.Audio.AY8912;

interface

type
  TRegArray = array [0 .. 15] of integer;
  TVolTableArray = array [0 .. 63] of integer;

  TRegAY8912 = record
    sampleRate: integer;
    register_latch: integer;
    Regs: TRegArray;
    UpdateStep: Double;
    PeriodA: integer;
    PeriodB: integer;
    PeriodC: integer;
    PeriodN: integer;
    PeriodE: integer;
    CountA: integer;
    CountB: integer;
    CountC: integer;
    CountN: integer;
    CountE: integer;
    VolA: integer;
    VolB: integer;
    VolC: integer;
    VolE: integer;
    EnvelopeA: integer;
    EnvelopeB: integer;
    EnvelopeC: integer;
    OutputA: integer;
    OutputB: integer;
    OutputC: integer;
    OutputN: integer;
    CountEnv: integer;
    Hold: integer;
    Alternate: integer;
    Attack: integer;
    Holding: integer;
    VolTable2: TVolTableArray;
  end;

  TAY8912 = class
  private
    fAYPSG: TRegAY8912;
    AY_OutNoise: integer;
    VolA: integer;
    VolB: integer;
    VolC: integer;
    lOut1: integer;
    lOut2: integer;
    lOut3: integer;
    AY_Left: integer;
    AY_NextEvent: integer;
    FRenderByte: Integer;

    procedure set_clock(const clock: Double);
    procedure set_volume(const volume: integer; gain: integer);

    procedure WriteReg(index: integer; const v: Byte);
    function ReadReg(index: integer): Byte;
    procedure Render;

  public
    constructor Create(const clock: Double; const sample_rate: integer;
      const sample_bits: integer);
    procedure Reset;
    procedure Update;

    property RenderByte: Integer read FRenderByte;

    property Regs[Index: integer]: Byte read ReadReg write WriteReg;
  end;

implementation

const
  MAX_OUTPUT = 63;
  AY_STEP: integer = 32768;
  MAXVOL = $1F;

  // AY register ID's
  AY_AFINE = 0;
  AY_ACOARSE = 1;
  AY_BFINE = 2;
  AY_BCOARSE = 3;
  AY_CFINE = 4;
  AY_CCOARSE = 5;
  AY_NOISEPER = 6;
  AY_ENABLE = 7;
  AY_AVOL = 8;
  AY_BVOL = 9;
  AY_CVOL = 10;
  AY_EFINE = 11;
  AY_ECOARSE = 12;
  AY_ESHAPE = 13;
  AY_PORTA = 14;
  AY_PORTB = 15;

procedure TAY8912.Reset;
var
  i: integer;
begin
  with fAYPSG do
  begin
    register_latch := 0;
    OutputA := 0;
    OutputB := 0;
    OutputC := 0;
    OutputN := $FF;
    PeriodA := 0;
    PeriodB := 0;
    PeriodC := 0;
    PeriodN := 0;
    PeriodE := 0;
    CountA := 0;
    CountB := 0;
    CountC := 0;
    CountN := 0;
    CountE := 0;
    VolA := 0;
    VolB := 0;
    VolC := 0;
    VolE := 0;
    EnvelopeA := 0;
    EnvelopeB := 0;
    EnvelopeC := 0;
    CountEnv := 0;
    Hold := 0;
    Alternate := 0;
    Holding := 0;
    Attack := 0;
  end;

  Randomize;
  for i := 0 to AY_PORTA do
  begin
    WriteReg(i, 0); // * AYWriteReg() uses the timer system; we cannot
  end; // * call it at this time because the timer system
  // * has not been initialized.
end;

procedure TAY8912.set_clock(const clock: Double);
var
  t1: Double;
begin

  { ' /* the AY_STEP clock for the tone and noise generators is the chip clock    */
    ' /* divided by 8; for the envelope generator of the AY-3-8912, it is half */
    ' /* that much (clock/16), but the envelope of the YM2149 goes twice as    */
    ' /* fast, therefore again clock/8.                                        */
    ' /* Here we calculate the number of AY_STEPs which happen during one sample  */
    ' /* at the given sample rate. No. of events = sample rate / (clock/8).    */
    ' /* AY_STEP is a multiplier used to turn the fraction into a fixed point     */
    ' /* number. }
  t1 := AY_STEP * fAYPSG.sampleRate * 8.0;
  fAYPSG.UpdateStep := t1 / clock
end;

{ ' /*
  ' ** set output gain
  ' **
  ' ** The gain is expressed in 0.2dB increments, e.g. a gain of 10 is an increase
  ' ** of 2dB. Note that the gain only affects sounds not playing at full volume,
  ' ** since the ones at full volume are already played at the maximum intensity
  ' ** allowed by the sound card.
  ' ** 0x00 is the default.
  ' ** 0xff is the maximum allowed value.
  ' */ }

procedure TAY8912.set_volume(const volume: integer; gain: integer);
var
  i: integer;
  out1: Double;
  out2: Double;
begin
  gain := gain and $FF;

  // increase max output basing on gain (0.2 dB per AY_STEP) */
  out1 := MAX_OUTPUT;
  out2 := MAX_OUTPUT;

  while (gain > 0) do
  begin
    gain := gain - 1;
    out1 := out1 * 1.023292992;
    /// * = (10 ^ (0.2/20)) */
    out2 := out2 * 1.023292992;
  end;

  { ' /* calculate the volume.voltage conversion table */
    ' /* The AY-3-8912 has 16 levels, in a logarithmic scale (3dB per AY_STEP) */
    ' /* The YM2149 still has 16 levels for the tone generators, but 32 for */
    ' /* the envelope generator (1.5dB per AY_STEP). */ }
  for i := 31 downto 0 do
  begin
    // * limit volume to avoid clipping */
    if (out2 > MAX_OUTPUT) then
      fAYPSG.VolTable2[i] := MAX_OUTPUT
    else
      fAYPSG.VolTable2[i] := Round(out2);

    out1 := out1 / 1.188502227; // .188502227 '/* = 10 ^ (1.5/20) = 1.5dB */
    out2 := out2 / 1.188502227 // .188502227
  end;
  fAYPSG.VolTable2[63] := MAX_OUTPUT;
end;

procedure TAY8912.WriteReg(index: integer; const v: Byte);
var
  old: integer;
begin
  fAYPSG.Regs[index] := v;

  { '/* A note about the period of tones, noise and envelope: for speed reasons,*/
    '/* we count down from the period to 0, but careful studies of the chip     */
    '/* output prove that it instead counts up from 0 until the counter becomes */
    '/* greater or equal to the period. This is an important difference when the*/
    '/* program is rapidly changing the period to modulate the sound.           */
    '/* To compensate for the difference, when the period is changed we adjust  */
    '/* our internal counter.                                                   */
    '/* Also, note that period = 0 is the same as period = 1. This is mentioned */
    '/* in the YM2203 data sheets. However, this does NOT apply to the Envelope */
    '/* period. In that case, period = 0 is half as period = 1. */ }
  case index of
    AY_AFINE, AY_ACOARSE:
      begin
        fAYPSG.Regs[AY_ACOARSE] := fAYPSG.Regs[AY_ACOARSE] and $F;

        old := fAYPSG.PeriodA;

        fAYPSG.PeriodA :=
          Round((fAYPSG.Regs[AY_AFINE] + (256 * fAYPSG.Regs[AY_ACOARSE])) *
          fAYPSG.UpdateStep);

        if (fAYPSG.PeriodA = 0) then
          fAYPSG.PeriodA := Round(fAYPSG.UpdateStep);

        fAYPSG.CountA := fAYPSG.CountA + (fAYPSG.PeriodA - old);

        if (fAYPSG.CountA <= 0) then
          fAYPSG.CountA := 1;
      end;
    AY_BFINE, AY_BCOARSE:
      begin
        fAYPSG.Regs[AY_BCOARSE] := fAYPSG.Regs[AY_BCOARSE] and $F;

        old := fAYPSG.PeriodB;

        fAYPSG.PeriodB :=
          Round((fAYPSG.Regs[AY_BFINE] + (256 * fAYPSG.Regs[AY_BCOARSE])) *
          fAYPSG.UpdateStep);

        if (fAYPSG.PeriodB = 0) then
          fAYPSG.PeriodB := Round(fAYPSG.UpdateStep);

        fAYPSG.CountB := fAYPSG.CountB + fAYPSG.PeriodB - old;

        if (fAYPSG.CountB <= 0) then
          fAYPSG.CountB := 1
      end;

    AY_CFINE, AY_CCOARSE:
      begin
        fAYPSG.Regs[AY_CCOARSE] := fAYPSG.Regs[AY_CCOARSE] and $F;

        old := fAYPSG.PeriodC;

        fAYPSG.PeriodC :=
          Round((fAYPSG.Regs[AY_CFINE] + (256 * fAYPSG.Regs[AY_CCOARSE])) *
          fAYPSG.UpdateStep);

        if (fAYPSG.PeriodC = 0) then
          fAYPSG.PeriodC := Round(fAYPSG.UpdateStep);

        fAYPSG.CountC := fAYPSG.CountC + (fAYPSG.PeriodC - old);

        if (fAYPSG.CountC <= 0) then
          fAYPSG.CountC := 1;
      end;

    AY_NOISEPER:
      begin
        fAYPSG.Regs[AY_NOISEPER] := fAYPSG.Regs[AY_NOISEPER] and $1F;

        old := fAYPSG.PeriodN;

        fAYPSG.PeriodN := Round(fAYPSG.Regs[AY_NOISEPER] * fAYPSG.UpdateStep);

        if (fAYPSG.PeriodN = 0) then
          fAYPSG.PeriodN := Round(fAYPSG.UpdateStep);

        fAYPSG.CountN := fAYPSG.CountN + (fAYPSG.PeriodN - old);

        if (fAYPSG.CountN <= 0) then
          fAYPSG.CountN := 1;
      end;

    AY_AVOL:
      begin
        fAYPSG.Regs[AY_AVOL] := fAYPSG.Regs[AY_AVOL] and $1F;

        fAYPSG.EnvelopeA := fAYPSG.Regs[AY_AVOL] and $10;

        if fAYPSG.EnvelopeA <> 0 then
          fAYPSG.VolA := fAYPSG.VolE
        else
        begin
          if fAYPSG.Regs[AY_AVOL] <> 0 then
            fAYPSG.VolA := fAYPSG.VolTable2[fAYPSG.Regs[AY_AVOL] * 2 + 1]
          else
            fAYPSG.VolA := fAYPSG.VolTable2[0];
        end;
      end;

    AY_BVOL:
      begin
        fAYPSG.Regs[AY_BVOL] := fAYPSG.Regs[AY_BVOL] and $1F;

        fAYPSG.EnvelopeB := fAYPSG.Regs[AY_BVOL] and $10;

        if fAYPSG.EnvelopeB <> 0 then
          fAYPSG.VolB := fAYPSG.VolE
        else
        begin
          if fAYPSG.Regs[AY_BVOL] <> 0 then
            fAYPSG.VolB := fAYPSG.VolTable2[fAYPSG.Regs[AY_BVOL] * 2 + 1]
          else
            fAYPSG.VolB := fAYPSG.VolTable2[0];
        end;
      end;

    AY_CVOL:
      begin
        fAYPSG.Regs[AY_CVOL] := fAYPSG.Regs[AY_CVOL] and $1F;

        fAYPSG.EnvelopeC := fAYPSG.Regs[AY_CVOL] and $10;

        if fAYPSG.EnvelopeC <> 0 then
          fAYPSG.VolC := fAYPSG.VolE
        else
        begin
          if fAYPSG.Regs[AY_CVOL] <> 0 then
            fAYPSG.VolC := fAYPSG.VolTable2[fAYPSG.Regs[AY_CVOL] * 2 + 1]
          else
            fAYPSG.VolC := fAYPSG.VolTable2[0];
        end;
      end;

    AY_EFINE, AY_ECOARSE:
      begin
        old := fAYPSG.PeriodE;

        fAYPSG.PeriodE :=
          Round(((fAYPSG.Regs[AY_EFINE] + (256 * fAYPSG.Regs[AY_ECOARSE]))) *
          fAYPSG.UpdateStep);

        if (fAYPSG.PeriodE = 0) then
          fAYPSG.PeriodE := Round(fAYPSG.UpdateStep / 2);

        fAYPSG.CountE := fAYPSG.CountE + (fAYPSG.PeriodE - old);

        if (fAYPSG.CountE <= 0) then
          fAYPSG.CountE := 1
      end;

    AY_ESHAPE:
      begin
        { '/* envelope shapes:
          'C AtAlH
          '0 0 x x  \___
          '
          '0 1 x x  /___
          '
          '1 0 0 0  \\\\
          '
          '1 0 0 1  \___
          '
          '1 0 1 0  \/\/
          '          ___
          '1 0 1 1  \
          '
          '1 1 0 0  ////
          '          ___
          '1 1 0 1  /
          '
          '1 1 1 0  /\/\
          '
          '1 1 1 1  /___
          '
          'The envelope counter on the AY-3-8910 has 16 AY_STEPs. On the YM2149 it
          'has twice the AY_STEPs, happening twice as fast. Since the end result is
          'just a smoother curve, we always use the YM2149 behaviour.
          '*/ }
        if (fAYPSG.Regs[AY_ESHAPE] <> $FF) then
        begin
          fAYPSG.Regs[AY_ESHAPE] := fAYPSG.Regs[AY_ESHAPE] and $F;

          if ((fAYPSG.Regs[AY_ESHAPE] and $4) = $4) then
            fAYPSG.Attack := MAXVOL
          else
            fAYPSG.Attack := $0;

          fAYPSG.Hold := fAYPSG.Regs[AY_ESHAPE] and $1;

          fAYPSG.Alternate := fAYPSG.Regs[AY_ESHAPE] and $2;

          fAYPSG.CountE := fAYPSG.PeriodE;

          fAYPSG.CountEnv := MAXVOL; // &h1f

          fAYPSG.Holding := 0;

          fAYPSG.VolE := fAYPSG.VolTable2[fAYPSG.CountEnv xor fAYPSG.Attack];

          if (fAYPSG.EnvelopeA <> 0) then
            fAYPSG.VolA := fAYPSG.VolE;

          if (fAYPSG.EnvelopeB <> 0) then
            fAYPSG.VolB := fAYPSG.VolE;

          if (fAYPSG.EnvelopeC <> 0) then
            fAYPSG.VolC := fAYPSG.VolE;
        end;
      end;
  end; // case
end;

function TAY8912.ReadReg(index: integer): Byte;
begin
  result := fAYPSG.Regs[index];
end;

constructor TAY8912.Create(const clock: Double; const sample_rate: integer;
  const sample_bits: integer);
begin
  fAYPSG.sampleRate := sample_rate;
  set_clock(clock);
  set_volume(255, 12);
  Reset;
end;

procedure TAY8912.Update;
var
  Buffer_Length: integer;
begin
  Buffer_Length := 400;

  { /* The 8910 has three outputs, each output is the mix of one of the three */
    ' /* tone generators and of the (single) noise generator. The two are mixed */
    ' /* BEFORE going into the DAC. The formula to mix each channel is: */
    ' /* (ToneOn | ToneDisable) & (NoiseOn | NoiseDisable). */
    ' /* Note that this means that if both tone and noise are disabled, the output */
    ' /* is 1, not 0, and can be modulated changing the volume. */

    ' /* if the channels are disabled, set their output to 1, and increase the */
    ' /* counter, if necessary, so they will not be inverted during this update. */
    ' /* Setting the output to 1 is necessary because a disabled channel is locked */
    ' /* into the ON state (see above); and it has no effect if the volume is 0. */
    ' /* if the volume is 0, increase the counter, but don't touch the output. */ }

  if (fAYPSG.Regs[AY_ENABLE] and $1) = $1 then
  begin
    if fAYPSG.CountA <= (Buffer_Length * AY_STEP) then
      fAYPSG.CountA := fAYPSG.CountA + (Buffer_Length * AY_STEP);

    fAYPSG.OutputA := 1;
  end
  else if (fAYPSG.Regs[AY_AVOL] = 0) then
  begin
    { ' /* note that I do count += Buffer_Length, NOT count = Buffer_Length + 1. You might think */
      ' /* it's the same since the volume is 0, but doing the latter could cause */
      ' /* interferencies when the program is rapidly modulating the volume. */ }
    if fAYPSG.CountA <= (Buffer_Length * AY_STEP) then
      fAYPSG.CountA := fAYPSG.CountA + (Buffer_Length * AY_STEP);
  end;

  if (fAYPSG.Regs[AY_ENABLE] and $2) = $2 then
  begin
    if fAYPSG.CountB <= (Buffer_Length * AY_STEP) then
      fAYPSG.CountB := fAYPSG.CountB + (Buffer_Length * AY_STEP);

    fAYPSG.OutputB := 1;
  end
  else if fAYPSG.Regs[AY_BVOL] = 0 then
  begin
    if fAYPSG.CountB <= (Buffer_Length * AY_STEP) then
      fAYPSG.CountB := fAYPSG.CountB + (Buffer_Length * AY_STEP);
  end;

  if (fAYPSG.Regs[AY_ENABLE] and $4) = $4 then
  begin
    if fAYPSG.CountC <= (Buffer_Length * AY_STEP) then
      fAYPSG.CountC := fAYPSG.CountC + (Buffer_Length * AY_STEP);

    fAYPSG.OutputC := 1;
  end
  else if (fAYPSG.Regs[AY_CVOL] = 0) then
  begin
    if fAYPSG.CountC <= (Buffer_Length * AY_STEP) then
      fAYPSG.CountC := fAYPSG.CountC + (Buffer_Length * AY_STEP);
  end;

  { '/* for the noise channel we must not touch OutputN - it's also not necessary */
    '/* since we use AY_OutNoise. */ }
  if ((fAYPSG.Regs[AY_ENABLE] and $38) = $38) then // * all off */
  begin
    if fAYPSG.CountN <= (Buffer_Length * AY_STEP) then
      fAYPSG.CountN := fAYPSG.CountN + (Buffer_Length * AY_STEP);
  end;

  AY_OutNoise := (fAYPSG.OutputN or fAYPSG.Regs[AY_ENABLE]);
  Render;
end;

procedure TAY8912.Render;
begin
    VolA := 0;
    VolB := 0;
    VolC := 0;

    // vola, volb and volc keep track of how long each square wave stays
    // in the 1 position during the sample period.

    AY_Left := AY_STEP;

    repeat
      AY_NextEvent := 0;

      if (fAYPSG.CountN < AY_Left) then
        AY_NextEvent := fAYPSG.CountN
      else
        AY_NextEvent := AY_Left;

      if (AY_OutNoise and $8) = $8 then
      begin
        if (fAYPSG.OutputA = 1) then
          VolA := VolA + fAYPSG.CountA;

        fAYPSG.CountA := fAYPSG.CountA - AY_NextEvent;

        { PeriodA is the half period of the square wave. Here, in each
          loop I add PeriodA twice, so that at the end of the loop the
          square wave is in the same status (0 or 1) it was at the start.
          vola is also incremented by PeriodA, since the wave has been 1
          exactly half of the time, regardless of the initial position.
          If we exit the loop in the middle, OutputA has to be inverted
          and vola incremented only if the exit status of the square
          wave is 1. }

        while (fAYPSG.CountA <= 0) do
        begin
          fAYPSG.CountA := fAYPSG.CountA + fAYPSG.PeriodA;
          if (fAYPSG.CountA > 0) then
          begin
            if (fAYPSG.Regs[AY_ENABLE] and 1) = 0 then
              fAYPSG.OutputA := fAYPSG.OutputA xor 1;
            if (fAYPSG.OutputA <> 0) then
              VolA := VolA + fAYPSG.PeriodA;
            break;
          end;

          fAYPSG.CountA := fAYPSG.CountA + fAYPSG.PeriodA;
          VolA := VolA + fAYPSG.PeriodA;
        end;
        if (fAYPSG.OutputA = 1) then
          VolA := VolA - fAYPSG.CountA;
      end
      else
      begin
        fAYPSG.CountA := fAYPSG.CountA - AY_NextEvent;

        while (fAYPSG.CountA <= 0) do
        begin
          fAYPSG.CountA := fAYPSG.CountA + fAYPSG.PeriodA;
          if (fAYPSG.CountA > 0) then
          begin
            fAYPSG.OutputA := fAYPSG.OutputA xor 1;
            break;
          end;
          fAYPSG.CountA := fAYPSG.CountA + fAYPSG.PeriodA;
        end;
      end;

      if (AY_OutNoise and $10) = $10 then
      begin
        if (fAYPSG.OutputB = 1) then
          VolB := VolB + fAYPSG.CountB;
        fAYPSG.CountB := fAYPSG.CountB - AY_NextEvent;

        while (fAYPSG.CountB <= 0) do
        begin
          fAYPSG.CountB := fAYPSG.CountB + fAYPSG.PeriodB;
          if (fAYPSG.CountB > 0) then
          begin
            if (fAYPSG.Regs[AY_ENABLE] and 2) = 0 then
              fAYPSG.OutputB := fAYPSG.OutputB xor 1;
            if (fAYPSG.OutputB <> 0) then
              VolB := VolB + fAYPSG.PeriodB;
            break;
          end;
          fAYPSG.CountB := fAYPSG.CountB + fAYPSG.PeriodB;
          VolB := VolB + fAYPSG.PeriodB;
        end;
        if (fAYPSG.OutputB = 1) then
          VolB := VolB - fAYPSG.CountB;
      end
      else
      begin
        fAYPSG.CountB := fAYPSG.CountB - AY_NextEvent;

        while (fAYPSG.CountB <= 0) do
        begin
          fAYPSG.CountB := fAYPSG.CountB + fAYPSG.PeriodB;
          if (fAYPSG.CountB > 0) then
          begin
            fAYPSG.OutputB := fAYPSG.OutputB xor 1;
            break;
          end;
          fAYPSG.CountB := fAYPSG.CountB + fAYPSG.PeriodB;
        end;
      end;

      if (AY_OutNoise and $20) = $20 then
      begin
        if (fAYPSG.OutputC = 1) then
          VolC := VolC + fAYPSG.CountC;
        fAYPSG.CountC := fAYPSG.CountC - AY_NextEvent;
        while (fAYPSG.CountC <= 0) do
        begin
          fAYPSG.CountC := fAYPSG.CountC + fAYPSG.PeriodC;
          if (fAYPSG.CountC > 0) then
          begin
            if (fAYPSG.Regs[AY_ENABLE] and 4) = 0 then
              fAYPSG.OutputC := fAYPSG.OutputC xor 1;
            if (fAYPSG.OutputC <> 0) then
              VolC := VolC + fAYPSG.PeriodC;
            break;
          end;

          fAYPSG.CountC := fAYPSG.CountC + fAYPSG.PeriodC;
          VolC := VolC + fAYPSG.PeriodC;
        end;
        if (fAYPSG.OutputC = 1) then
          VolC := VolC - fAYPSG.CountC;
      end
      else
      begin
        fAYPSG.CountC := fAYPSG.CountC - AY_NextEvent;
        while (fAYPSG.CountC <= 0) do
        begin
          fAYPSG.CountC := fAYPSG.CountC + fAYPSG.PeriodC;
          if (fAYPSG.CountC > 0) then
          begin
            fAYPSG.OutputC := fAYPSG.OutputC xor 1;
            break;
          end;
          fAYPSG.CountC := fAYPSG.CountC + fAYPSG.PeriodC;
        end;
      end;

      fAYPSG.CountN := fAYPSG.CountN - AY_NextEvent;
      if (fAYPSG.CountN <= 0) then
      begin
        // Is noise output going to change?
        fAYPSG.OutputN := Round(random(510));
        AY_OutNoise := (fAYPSG.OutputN or fAYPSG.Regs[AY_ENABLE]);
        fAYPSG.CountN := fAYPSG.CountN + fAYPSG.PeriodN;
      end;

      AY_Left := AY_Left - AY_NextEvent;
    until (AY_Left <= 0);

    if (fAYPSG.Holding = 0) then
    begin
      fAYPSG.CountE := fAYPSG.CountE - AY_STEP;
      if (fAYPSG.CountE <= 0) then
      begin
        repeat
          fAYPSG.CountEnv := fAYPSG.CountEnv - 1;
          fAYPSG.CountE := fAYPSG.CountE + fAYPSG.PeriodE;
        until (fAYPSG.CountE > 0);

        // check envelope current position
        if (fAYPSG.CountEnv < 0) then
        begin
          if (fAYPSG.Hold <> 0) then
          begin
            if (fAYPSG.Alternate <> 0) then
            begin
              fAYPSG.Attack := fAYPSG.Attack xor MAXVOL; // $1f
            end;
            fAYPSG.Holding := 1;
            fAYPSG.CountEnv := 0;
          end
          else
          begin
            // if CountEnv has looped an odd number of times (usually 1),
            // invert the output.
            if (fAYPSG.Alternate <> 0) and ((fAYPSG.CountEnv and $20) = $20)
            then
            begin
              fAYPSG.Attack := fAYPSG.Attack xor MAXVOL; // $1f
            end;

            fAYPSG.CountEnv := fAYPSG.CountEnv and MAXVOL; // $1f
          end;
        end;

        fAYPSG.VolE := fAYPSG.VolTable2[fAYPSG.CountEnv xor fAYPSG.Attack];

        // reload volume
        if (fAYPSG.EnvelopeA <> 0) then
          fAYPSG.VolA := fAYPSG.VolE;
        if (fAYPSG.EnvelopeB <> 0) then
          fAYPSG.VolB := fAYPSG.VolE;
        if (fAYPSG.EnvelopeC <> 0) then
          fAYPSG.VolC := fAYPSG.VolE;
      end;
    end;

    lOut1 := (VolA * fAYPSG.VolA) div 65535;
    lOut2 := (VolB * fAYPSG.VolB) div 65535;
    lOut3 := (VolC * fAYPSG.VolC) div 65535;

    FRenderByte := lOut1 + lOut2 + lOut3;
end;

end.
