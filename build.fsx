// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------

#r "./packages/FAKE/tools/FakeLib.dll"

open Fake
open System

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDir  = "./build/"
let deployDir  = "./deploy/"
let solution = "FSharp.EdIlyin.Core.sln"
let dotnetcliVersion = "2.0.0-preview2-006497"
let mutable dotnetExePath = "dotnet"

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------

let run' timeout cmd args dir =
    if execProcess (fun info ->
        info.FileName <- cmd
        if not (String.IsNullOrWhiteSpace dir) then
            info.WorkingDirectory <- dir
        info.Arguments <- args
    ) timeout |> not then
        failwithf "Error while running '%s' with args: %s" cmd args

let run = run' System.TimeSpan.MaxValue

let runDotnet workingDir args =
    let result =
        ExecProcess (fun info ->
            info.FileName <- dotnetExePath
            info.WorkingDirectory <- workingDir
            info.Arguments <- args) TimeSpan.MaxValue
    if result <> 0 then failwithf "dotnet %s failed" args

// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

Target "Clean" (fun _ ->
    !! "src/**/bin" ++ "src/**/obj" |> CleanDirs
    CleanDirs [buildDir; deployDir]
)

Target "InstallDotNetCLI" (fun _ ->
    dotnetExePath <- DotNetCli.InstallDotNetSDK dotnetcliVersion
)

Target "Restore" (fun _ ->
    runDotnet null "restore"
)

Target "Build" (fun _ ->
    MSBuildDebug buildDir "" [solution] |> ignore
)

Target "BuildRelease" (fun _ ->
    MSBuildDebug deployDir "" [solution] |> ignore
    run msBuildExe "/Target=" null
)

Target "Release" (fun _ ->
    runDotnet null "pack -c Release /p:PackageVersion=0.0.3 -o deployn"
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------

"Clean"
  ==> "InstallDotNetCLI"
  ==> "Restore"
  ==> "Build"

"Clean"
    ==> "BuildRelease"
    ==> "Release"

RunTargetOrDefault "Build"
