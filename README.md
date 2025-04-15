# SevenatorAda

To build, use

    gprbuild -P sevenator.gpr

## Yamaha DX7 MIDI System Exclusive Format

[SysEx Documentation](https://github.com/asb2m10/dexed/blob/master/Documentation/sysex-format.txt)

## LFO and operator-specific settings

Note that each operator has AMS or "Amplitude Modulation Sensitivity".
However, there is only one "LFO Pitch Modulation Sensitivity" (LPMS).
So LPMS is an attribute of the LFO, as are LPMD (LFO Pitch Modulation Depth)
and LAMD (LFO Amplitude Modulation Depth).

## Ada notes

### Arrays or containers

Earlier versions of this program used arrays to store the various parts of
the System Exclusive data. This turned out to be quite tedious, because there
were many subtypes of unconstrained array, in some cases also both normal and
packed variants, with different lengths.

After struggling to keep track of all the various array indices I decided to
switch to using containers, or vectors to be more specific. The `Byte_Vector`
type is defined in the Sixten library which this program depends on. This 
makes it a lot easier to emit normal and packed voices (and their normal and
packed components). Each type in the data model will not be able to emit their
data as an array (or even a vector), and at this time that capability is not
even needed. So this should simplify the program considerably.
