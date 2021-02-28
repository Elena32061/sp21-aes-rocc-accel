# AES RoCC Accelerator/Co-Processor --- EE290C

## Requirements
As the tests depend on the chisel verification library, this generator must be built alongside Chipyard.

### Installing Chipyard
You should already have chipyard installed from lab 1. However, double check that the chipyard version  is >= 1.3. 
If not, please update your chipyard (The chipyard lab repo has a branch `v1p4` for version 1.4. This may be already be merged into master. If unsure, ask.)

### Installing Chisel Verification Library
Note that we start in the chipyard root directory.
```
~/chipyard> cd tools
~/chipyard/tools> git submodule add https://github.com/TsaiAnson/verif.git
```

### Installing the AES RoCC Accelerator Generator
Note that we start in the chipyard root directory.
```
~/chipyard> cd generators
~/chipyard/generators> git submodule add git@bwrcrepo.eecs.berkeley.edu:EE290C_EE194_tstech28/aes.git
```

### Modifying your build.sbt
Add the following snippet to the end of `chipyard/build.sbt`:
```
val directoryLayout = Seq(
  scalaSource in Compile := baseDirectory.value / "src",
  javaSource in Compile := baseDirectory.value / "resources",
  resourceDirectory in Compile := baseDirectory.value / "resources",
  scalaSource in Test := baseDirectory.value / "test",
  javaSource in Test := baseDirectory.value / "resources",
  resourceDirectory in Test := baseDirectory.value / "resources",
)

val verifSettings = Seq(
  resolvers ++= Seq(
    Resolver.sonatypeRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.mavenLocal
  ),
  scalacOptions := Seq("-deprecation", "-unchecked", "-Xsource:2.11", "-language:reflectiveCalls"),
  libraryDependencies += "edu.berkeley.cs" %% "chiseltest" % "0.3.1",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.+" % "test"
)

lazy val verifCore = (project in file("./tools/verif/core"))
  .settings(directoryLayout)
  .sourceDependency(chiselRef, chiselLib)
  .dependsOn(rocketchip, chipyard, dsptools, `rocket-dsptools`)
  .settings(commonSettings)
  .settings(verifSettings)

lazy val verifTL = (project in file("./tools/verif/tilelink"))
  .settings(directoryLayout)
  .sourceDependency(chiselRef, chiselLib)
  .dependsOn(rocketchip, chipyard, dsptools, `rocket-dsptools`, verifCore)
  .settings(commonSettings)
  .settings(verifSettings)

lazy val verifGemmini = (project in file("./tools/verif/cosim"))
  .settings(directoryLayout)
  .sourceDependency(chiselRef, chiselLib)
  .dependsOn(verifCore, verifTL)
  .settings(commonSettings)
  .settings(verifSettings)
  .settings(libraryDependencies += "com.google.protobuf" % "protobuf-java" % "3.14.0")
  .settings(libraryDependencies += "com.google.protobuf" % "protobuf-java-util" % "3.14.0")

lazy val aes = (project in file("generators/aes"))
  .sourceDependency(chiselRef, chiselLib)
  .dependsOn(verifCore, verifTL, verifGemmini)
  .settings(commonSettings)
  .settings(verifSettings)
```

### Compiling and running the tests
Note that we start in the chipyard root directory.
```
~/chipyard> cd sims/verilator
~/chipyard/sims/verilator> make launch-sbt
sbt:chipyardRoot> project aes
sbt:aes> compile                       // If you just want to compile src code
sbt:aes> test:compile                  // If you just want to compile test code
sbt:aes> testOnly aes.dcplrSanityTest  // Compiles all dependencies and runs test
```

## Spec
Please refer to the chip spec [here](https://docs.google.com/document/d/1J9azqokkR0AsUUAkwU-hotsNtb-0KX5duK7d7f_3MhI/edit?usp=sharing) (to avoid having multiple versions).
