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
