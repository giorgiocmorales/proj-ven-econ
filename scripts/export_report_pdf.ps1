# Export the editable report through Word so PDF retains its template formatting.
[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$InputPath,

    [Parameter(Mandatory = $true)]
    [string]$OutputPath
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# Resolve both paths before opening Word and reject an unsupported source format.
$resolvedInputPath = (Resolve-Path -LiteralPath $InputPath).Path
if ([System.IO.Path]::GetExtension($resolvedInputPath) -ne '.docx') {
    throw [System.ArgumentException]::new('The report PDF exporter requires a DOCX input.')
}
$resolvedOutputPath = [System.IO.Path]::GetFullPath($OutputPath)
if ([System.IO.Path]::GetExtension($resolvedOutputPath) -ne '.pdf') {
    throw [System.ArgumentException]::new('The report PDF exporter requires a PDF output path.')
}
$outputDirectory = [System.IO.Path]::GetDirectoryName($resolvedOutputPath)
New-Item -ItemType Directory -Path $outputDirectory -Force | Out-Null
$word = $null
$document = $null

try {
    # Open read-only, paginate, and export; do not rewrite the editable Word report.
    $word = New-Object -ComObject Word.Application
    $word.Visible = $false
    $word.DisplayAlerts = 0
    $document = $word.Documents.Open($resolvedInputPath, $false, $true)
    $document.Repaginate()
    $document.ExportAsFixedFormat($resolvedOutputPath, 17)
    if (!(Test-Path -LiteralPath $resolvedOutputPath) -or
        (Get-Item -LiteralPath $resolvedOutputPath).Length -eq 0) {
        throw [System.IO.IOException]::new('Word did not produce a nonempty report PDF.')
    }
} finally {
    # Close only this exporter instance, including when opening or exporting fails.
    if ($null -ne $document) {
        $document.Close(0)
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($document) | Out-Null
    }
    if ($null -ne $word) {
        $word.Quit()
        [System.Runtime.InteropServices.Marshal]::ReleaseComObject($word) | Out-Null
    }
}
