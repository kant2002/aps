<#
Script: run-aps-tests.ps1
Description: Parcourt tous les fichiers .aps dans le dossier fourni (et sous-dossiers)
			 et exécute la commande `dotnet run aps -- -i <fullpath.aps>` pour chacun.
Usage:  .\run-aps-tests.ps1 -TestsFolder "C:\chemin\vers\tests"
#>

[CmdletBinding()]
param(
	[Parameter(Mandatory=$true, Position=0)]
	[string]$TestsFolder
)

if (-not (Test-Path -Path $TestsFolder -PathType Container)) {
	Write-Error "Le dossier spécifié n'existe pas: $TestsFolder"
	exit 2
}

$apsFiles = Get-ChildItem -Path $TestsFolder -Filter '*.ap' -Recurse -File -ErrorAction Stop

if ($apsFiles.Count -eq 0) {
	Write-Host "Aucun fichier .ap trouvé dans '$TestsFolder'."
	exit 0
}

$failures = @()

dotnet build aps

foreach ($file in $apsFiles) {
	Write-Host "--> Exécution: $($file.FullName)"
	$args = @('run','--no-build','--no-launch-profile', '--project','aps','--','-i','"' + $file.FullName.Trim() + '"')
	try {
		$proc = Start-Process -FilePath 'dotnet' -ArgumentList $args -NoNewWindow -Wait -PassThru -ErrorAction Stop
	}
	catch {
		Write-Error "Impossible de démarrer la commande dotnet pour $($file.FullName): $_"
		$failures += $file.FullName
		continue
	}

	if ($proc.ExitCode -ne 0) {
		Write-Host "  [ÉCHEC] ExitCode=$($proc.ExitCode)"
		$failures += $file.FullName
	}
	else {
		Write-Host "  [OK]"
	}
}

if ($failures.Count -gt 0) {
	Write-Error "$($failures.Count) exécution(s) ont échoué."
	foreach ($f in $failures) { Write-Host " - $f" }
	exit 1
}

Write-Host "Toutes les exécutions .aps ont réussi."
exit 0
