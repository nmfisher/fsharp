// #Conformance #Interop #PInvoke #Structs 

#if TESTS_AS_APP
module Core_csext
#endif

#nowarn "9"
open System
open System.Runtime.InteropServices
open System.Drawing
open System.Runtime.CompilerServices

let failures = ref []

let report_failure (s : string) = 
    stderr.Write" NO: "
    stderr.WriteLine s
    failures := !failures @ [s]

let isLinux = RuntimeInformation.IsOSPlatform(OSPlatform.Linux)

module GetSystemTimeTest = 
    open System
    open System.Runtime.InteropServices

    [<StructLayout(LayoutKind.Explicit, CharSet=CharSet.Ansi)>]
    type LinuxTm =
      struct
          [<FieldOffset(0)>] val tm_sec : int;
          [<FieldOffset(4)>] val tm_min : int;
          [<FieldOffset(8)>] val tm_hour : int;
          [<FieldOffset(12)>] val tm_mday : int;
          [<FieldOffset(16)>] val tm_mon : int;
          [<FieldOffset(20)>] val tm_year : int;
          [<FieldOffset(24)>] val tm_wday : int;
          [<FieldOffset(28)>] val tm_yday : int;
          [<FieldOffset(32)>] val tm_isdst : int;
      end

    [<StructLayout(LayoutKind.Explicit, Size=16, CharSet=CharSet.Ansi)>]
    type MySystemTime = class
       new() = { wYear=0us; wMonth=0us; wDayOfWeek=0us; wDay=0us; wHour=0us; wMinute=0us;wSecond=0us;wMilliseconds=0us }
       [<FieldOffset(0)>] val wYear : uint16; 
       [<FieldOffset(2)>] val wMonth : uint16;
       [<FieldOffset(4)>] val wDayOfWeek : uint16; 
       [<FieldOffset(6)>] val wDay : uint16 ; 
       [<FieldOffset(8)>] val wHour : uint16 ; 
       [<FieldOffset(10)>] val wMinute : uint16 ; 
       [<FieldOffset(12)>] val wSecond : uint16 ; 
       [<FieldOffset(14)>] val wMilliseconds : uint16 ; 
    end

    [<DllImport("kernel32.dll")>]
    extern void GetSystemTime([<MarshalAs(UnmanagedType.LPStruct)>] MySystemTime ct);

    [<DllImport("libc.so.6")>]
    extern int64 time(int64& ct);

    [<DllImport("libc.so.6")>]
    extern IntPtr localtime(int64& time_t);

    do 
      match isLinux with
      | true ->
          let mutable timer = 0L
          let mutable time_t = time(&timer)        
          let tm = Marshal.PtrToStructure<LinuxTm>(localtime(&time_t))
          printf "The System time is %d/%d/%d %d:%d:%d\n"
                             tm.tm_mday
                             (tm.tm_mon + 1)
                             (tm.tm_year + 1900)
                             tm.tm_hour
                             tm.tm_min
                             tm.tm_sec
      | false -> 
          let sysTime = new MySystemTime()
          GetSystemTime(sysTime);
          printf "The System time is %d/%d/%d %d:%d:%d\n" 
                            (int32 sysTime.wDay)
                            (int32 sysTime.wMonth )
                            (int32 sysTime.wYear )
                            (int32 sysTime.wHour )
                            (int32 sysTime.wMinute )
                            (int32 sysTime.wSecond)


module MemoryStatusTest = 
    open System
    open System.Runtime.InteropServices

    [<StructLayout(LayoutKind.Sequential, CharSet=CharSet.Auto)>]
    type MEMORYSTATUSEX = class
        val mutable dwLength : uint32
        val dwMemoryLoad : uint32
        val ullTotalPhys : uint64
        val ullAvailPhys : uint64
        val ullTotalPageFile : uint64
        val ullAvailPageFile : uint64
        val ullTotalVirtual : uint64
        val ullAvailVirtual : uint64
        val ullAvailExtendedVirtual : uint64
        new() = {dwLength = Convert.ToUInt32(Marshal.SizeOf(typeof<MEMORYSTATUSEX>));
                 dwMemoryLoad = 0ul;
                 ullTotalPhys = 0UL;
                 ullAvailPhys = 0UL;
                 ullTotalPageFile = 0UL;
                 ullAvailPageFile = 0UL;
                 ullTotalVirtual = 0UL;
                 ullAvailVirtual = 0UL;
                 ullAvailExtendedVirtual  = 0UL}
    end

    [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern [<MarshalAs(UnmanagedType.Bool)>] bool 
          GlobalMemoryStatusEx( [<In; Out>] MEMORYSTATUSEX lpBuffer);

    let main() =
        match isLinux with 
        | false ->
          let mex = new MEMORYSTATUSEX()
          GlobalMemoryStatusEx(mex) |> ignore
          printf "%A\n" mex
        | true -> 
          printfn "Running on Linux, skipping MemoryStatusTest1"
    main()


module MemoryStatusTest2 = 
    open System
    open System.Runtime.InteropServices

    [<StructLayout(LayoutKind.Sequential, CharSet=CharSet.Auto)>]
    type MEMORYSTATUSEX = struct
        val mutable dwLength : uint32
        val dwMemoryLoad : uint32
        val ullTotalPhys : uint64
        val ullAvailPhys : uint64
        val ullTotalPageFile : uint64
        val ullAvailPageFile : uint64
        val ullTotalVirtual : uint64
        val ullAvailVirtual : uint64
        val ullAvailExtendedVirtual : uint64
        new(dummy:int) = {dwLength = Convert.ToUInt32(Marshal.SizeOf(typeof<MEMORYSTATUSEX>));
                 dwMemoryLoad = 0ul;
                 ullTotalPhys = 0UL;
                 ullAvailPhys = 0UL;
                 ullTotalPageFile = 0UL;
                 ullAvailPageFile = 0UL;
                 ullTotalVirtual = 0UL;
                 ullAvailVirtual = 0UL;
                 ullAvailExtendedVirtual  = 0UL}
    end

    [<DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)>]
    extern [<MarshalAs(UnmanagedType.Bool)>] bool 
          GlobalMemoryStatusEx( [<In; Out>] MEMORYSTATUSEX *lpBuffer);

    let main() =
      match isLinux with 
      | false ->      
          let mutable mex = new MEMORYSTATUSEX(0)
          GlobalMemoryStatusEx(&& mex) |> ignore
          printf "%A\n" mex
      | true ->
          printfn "Running on Linux, skipping MemoryStatusTest2"
    main()

(*--------------------*)  

#if TESTS_AS_APP
let RUN() = !failures
#else
let aa =
  match !failures with 
  | [] -> 
      stdout.WriteLine "Test Passed"
      System.IO.File.WriteAllText("test.ok","ok")
      exit 0
  | messages ->
      printfn "%A" messages
      stdout.WriteLine "Test Failed"
      exit 1
#endif



