
//  Microsoft (R) .NET Framework IL Disassembler.  Version 4.6.1055.0
//  Copyright (c) Microsoft Corporation.  All rights reserved.



// Metadata version: v4.0.30319
.assembly extern mscorlib
{
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 )                         // .z\V.4..
  .ver 4:0:0:0
}
.assembly extern FSharp.Core
{
  .publickeytoken = (B0 3F 5F 7F 11 D5 0A 3A )                         // .?_....:
  .ver 4:5:0:0
}
.assembly LiteralValue
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute::.ctor(int32,
                                                                                                      int32,
                                                                                                      int32) = ( 01 00 02 00 00 00 00 00 00 00 00 00 00 00 00 00 ) 

  // --- The following custom attribute is added automatically, do not uncomment -------
  //  .custom instance void [mscorlib]System.Diagnostics.DebuggableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggableAttribute/DebuggingModes) = ( 01 00 01 01 00 00 00 00 ) 

  .hash algorithm 0x00008004
  .ver 0:0:0:0
}
.mresource public FSharpSignatureData.LiteralValue
{
  // Offset: 0x00000000 Length: 0x00000287
}
.mresource public FSharpSignatureDataB.LiteralValue
{
  // Offset: 0x00000290 Length: 0x0000000B
}
.mresource public FSharpOptimizationData.LiteralValue
{
  // Offset: 0x000002A0 Length: 0x0000007F
}
.mresource public FSharpOptimizationDataB.LiteralValue
{
  // Offset: 0x00000328 Length: 0x00000000
}
.module LiteralValue.exe
// MVID: {5BF2DEA8-196E-5E7E-A745-0383A8DEF25B}
.imagebase 0x00400000
.file alignment 0x00000200
.stackreserve 0x00100000
.subsystem 0x0003       // WINDOWS_CUI
.corflags 0x00000001    //  ILONLY
// Image base: 0x016C0000


// =============== CLASS MEMBERS DECLARATION ===================

.class public abstract auto ansi sealed LiteralValue
       extends [mscorlib]System.Object
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
  .field public static literal int32 x = int32(0x00000007)
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.LiteralAttribute::.ctor() = ( 01 00 00 00 ) 
  .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .method public static int32  main(string[] argv) cil managed
  {
    .entrypoint
    .custom instance void [FSharp.Core]Microsoft.FSharp.Core.EntryPointAttribute::.ctor() = ( 01 00 00 00 ) 
    // Code size       2 (0x2)
    .maxstack  8
    .language '{AB4F38C9-B6E6-43BA-BE3B-58080B2CCCE3}', '{994B45C4-E6E9-11D2-903F-00C04FA302A1}', '{5A869D0B-6611-11D3-BD2A-0000F80849BD}'
    .line 6,6 : 5,6 'C:\\GitHub\\dsyme\\visualfsharp\\tests\\fsharpqa\\Source\\CodeGen\\EmittedIL\\LiteralValue\\LiteralValue.fs'
    IL_0000:  ldc.i4.0
    IL_0001:  ret
  } // end of method LiteralValue::main

} // end of class LiteralValue

.class private abstract auto ansi sealed '<StartupCode$LiteralValue>'.$LiteralValue
       extends [mscorlib]System.Object
{
} // end of class '<StartupCode$LiteralValue>'.$LiteralValue


// =============================================================

// *********** DISASSEMBLY COMPLETE ***********************
