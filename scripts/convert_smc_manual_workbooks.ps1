# Convert one legacy BCV SMC workbook to XLSX with Microsoft Excel.

[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$InputPath,

    [Parameter(Mandatory = $true)]
    [string]$OutputPath
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$resolvedInputPath = (Resolve-Path -LiteralPath $InputPath).Path
$outputDirectory = Split-Path -Parent $OutputPath

if (-not (Test-Path -LiteralPath $outputDirectory)) {
    New-Item -ItemType Directory -Path $outputDirectory -Force | Out-Null
}

$temporaryOutputPath = Join-Path $outputDirectory (".{0}_{1}.xlsx" -f [System.IO.Path]::GetFileNameWithoutExtension($OutputPath), [System.Guid]::NewGuid().ToString("N"))
$backupPath = Join-Path $outputDirectory (".{0}_{1}.backup" -f [System.IO.Path]::GetFileName($OutputPath), [System.Guid]::NewGuid().ToString("N"))
$excel = $null
$workbook = $null

try {
    # Excel is required because readxl cannot parse these legacy BCV workbooks.
    $excel = New-Object -ComObject Excel.Application
    $excel.Visible = $false
    $excel.DisplayAlerts = $false
    $workbook = $excel.Workbooks.Open($resolvedInputPath, 0, $true)
    $workbook.SaveAs($temporaryOutputPath, 51)
    $workbook.Close($false)
    $workbook = $null

    if (-not (Test-Path -LiteralPath $temporaryOutputPath)) {
        throw "Excel did not produce a converted workbook: $temporaryOutputPath"
    }

    if (Test-Path -LiteralPath $OutputPath) {
        Move-Item -LiteralPath $OutputPath -Destination $backupPath
    }

    Move-Item -LiteralPath $temporaryOutputPath -Destination $OutputPath

    if (Test-Path -LiteralPath $backupPath) {
        Remove-Item -LiteralPath $backupPath -Force
    }
}
catch {
    if (Test-Path -LiteralPath $temporaryOutputPath) {
        Remove-Item -LiteralPath $temporaryOutputPath -Force
    }

    if ((Test-Path -LiteralPath $backupPath) -and -not (Test-Path -LiteralPath $OutputPath)) {
        Move-Item -LiteralPath $backupPath -Destination $OutputPath
    }

    throw
}
finally {
    if ($null -ne $workbook) {
        $workbook.Close($false)
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($workbook) | Out-Null
    }

    if ($null -ne $excel) {
        $excel.Quit()
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($excel) | Out-Null
    }
}
