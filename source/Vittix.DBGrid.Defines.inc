{ Vittix.DBGrid.Defines.inc - compiler-version feature flags.

  Supported compilers: Delphi 10.3 Rio (CompilerVersion 33.0) and newer.
  Include with the compiler directive dollar-I Vittix.DBGrid.Defines.inc
  Add a flag here (not scattered compiler-version checks) whenever a
  newer RTL/VCL API needs a fallback for older supported compilers. }

{$IF CompilerVersion < 33.0}
  {$MESSAGE FATAL 'Vittix.DBGrid requires Delphi 10.3 Rio or newer.'}
{$IFEND}

{$IF CompilerVersion >= 34.0}
  // Delphi 10.4 Sydney+: StyleServices(AControl) resolves per-control VCL styles.
  {$DEFINE VX_STYLESERVICES_CONTROL}
{$IFEND}
