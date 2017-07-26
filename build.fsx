#r "packages/FAKE/tools/FakeLib.dll"
open Fake


RestorePackages()
let solution = "AlsoCloud.Swiftpage"
let buildDir = sprintf "./src/%s/bin/Release/net461/" solution
let deployDir = "./deploy/"
Target "CleanBuild" (fun _ -> CleanDir buildDir)
Target "Clean" (fun _ -> CleanDir deployDir)

Target "Deploy" (fun _ ->
    !! (buildDir + "/**/*.*")
    |> Copy (deployDir + solution)
)

Target "Default" ignore

"Clean"
    ==> "Deploy"
    ==> "Default"

RunTargetOrDefault "Default"
