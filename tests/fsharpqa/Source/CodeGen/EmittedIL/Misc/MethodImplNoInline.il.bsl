
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
.assembly MethodImplNoInline
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute::.ctor(int32,
                                                                                                      int32,
                                                                                                      int32) = ( 01 00 02 00 00 00 00 00 00 00 00 00 00 00 00 00 ) 
  .hash algorithm 0x00008004
  .ver 0:0:0:0
}
.mresource public FSharpSignatureData.MethodImplNoInline
{
  // Offset: 0x00000000 Length: 0x00000307
}
.mresource public FSharpSignatureDataB.MethodImplNoInline
{
  // Offset: 0x00000310 Length: 0x00000008
}
.mresource public FSharpOptimizationData.MethodImplNoInline
{
  // Offset: 0x00000320 Length: 0x000000F5
}
.mresource public FSharpOptimizationDataB.MethodImplNoInline
{
  // Offset: 0x00000420 Length: 0x00000009
}
.module MethodImplNoInline.exe
// MVID: {5BF2DEA8-4480-09E2-A745-0383A8DEF25B}
.imagebase 0x00400000
.file alignment 0x00000200
.stackreserve 0x00100000
.subsystem 0x0003       // WINDOWS_CUI
.corflags 0x00000001    //  ILONLY
// Image base: 0x00A40000


// =============== CLASS MEMBERS DECLARATION ===================

.class public abstract auto ansi sealed MethodImplNoInline
       extends [mscorlib]System.Object
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
  .method public static void  g() cil managed noinlining
  {
    // Code size       24 (0x18)
    .maxstack  4
    .locals init (class [FSharp.Core]Microsoft.FSharp.Core.PrintfFormat`4<class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [mscorlib]System.IO.TextWriter,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit> V_0)
    IL_0000:  ldstr      "Hey!"
    IL_0005:  newobj     instance void class [FSharp.Core]Microsoft.FSharp.Core.PrintfFormat`5<class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [mscorlib]System.IO.TextWriter,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit>::.ctor(string)
    IL_000a:  stloc.0
    IL_000b:  call       class [mscorlib]System.IO.TextWriter [mscorlib]System.Console::get_Out()
    IL_0010:  ldloc.0
    IL_0011:  call       !!0 [FSharp.Core]Microsoft.FSharp.Core.PrintfModule::PrintFormatLineToTextWriter<class [FSharp.Core]Microsoft.FSharp.Core.Unit>(class [mscorlib]System.IO.TextWriter,
                                                                                                                                                         class [FSharp.Core]Microsoft.FSharp.Core.PrintfFormat`4<!!0,class [mscorlib]System.IO.TextWriter,class [FSharp.Core]Microsoft.FSharp.Core.Unit,class [FSharp.Core]Microsoft.FSharp.Core.Unit>)
    IL_0016:  pop
    IL_0017:  ret
  } // end of method MethodImplNoInline::g

  .method public static void  f() cil managed
  {
    // Code size       8 (0x8)
    .maxstack  8
    IL_0000:  tail.
    IL_0002:  call       void MethodImplNoInline::g()
    IL_0007:  ret
  } // end of method MethodImplNoInline::f

} // end of class MethodImplNoInline

.class private abstract auto ansi sealed '<StartupCode$MethodImplNoInline>'.$MethodImplNoInline
       extends [mscorlib]System.Object
{
  .field static assembly int32 init@
  .custom instance void [mscorlib]System.Diagnostics.DebuggerBrowsableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggerBrowsableState) = ( 01 00 00 00 00 00 00 00 ) 
  .custom instance void [mscorlib]System.Runtime.CompilerServices.CompilerGeneratedAttribute::.ctor() = ( 01 00 00 00 ) 
  .custom instance void [mscorlib]System.Diagnostics.DebuggerNonUserCodeAttribute::.ctor() = ( 01 00 00 00 ) 
  .method public static void  main@() cil managed
  {
    .entrypoint
    // Code size       6 (0x6)
    .maxstack  8
    IL_0000:  call       void MethodImplNoInline::g()
    IL_0005:  ret
  } // end of method $MethodImplNoInline::main@

} // end of class '<StartupCode$MethodImplNoInline>'.$MethodImplNoInline


// =============================================================

// *********** DISASSEMBLY COMPLETE ***********************
