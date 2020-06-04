Write-Host "Compiling..."
&scalac solution.scala
Write-Host "Running..."
&scala -J-Xmx2g euler.Solution
