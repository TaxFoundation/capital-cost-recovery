# Build the Canada immediate-expensing ranking workbook (2026-2030) via Excel COM.
# Every weighted average is computed from asset NPVs by formula; every rank is a
# live RANK() formula. Only the asset NPVs are inputs (from final-data/npv_all_years.csv).
# ASCII only. Numbers written as invariant-culture constant formulas (German-locale safe).

[Threading.Thread]::CurrentThread.CurrentCulture = [Globalization.CultureInfo]::GetCultureInfo('en-US')
$inv = [Globalization.CultureInfo]::InvariantCulture

# Kill orphaned EXCEL processes (no main window) that can wedge automation
Get-Process EXCEL -ErrorAction SilentlyContinue | Where-Object { $_.MainWindowHandle -eq 0 } | Stop-Process -Force -ErrorAction SilentlyContinue

# Resolve paths relative to this script's location (script lives in the repo root)
if ($PSScriptRoot) { $repo = $PSScriptRoot } else { $repo = Split-Path -Parent $MyInvocation.MyCommand.Path }
$src  = Join-Path $repo 'final-data\npv_all_years.csv'
$out  = Join-Path $repo 'final-outputs\canada_expensing_rankings_2026_2030.xlsx'

$excl  = @('BGR','HRV','CYP','MLT','ROU')   # EU-but-not-OECD, excluded from OECD ranking
$years = 2026,2027,2028,2029,2030

# ---- Load and filter source data ----
$rows = Import-Csv $src
$byYearIso = @{}
$names = @{}
foreach ($r in $rows) {
  $y = [int]$r.year
  if ($years -notcontains $y) { continue }
  $iso = $r.iso_3
  if ($excl -contains $iso) { continue }
  if (-not $byYearIso.ContainsKey($y)) { $byYearIso[$y] = @{} }
  $byYearIso[$y][$iso] = [pscustomobject]@{
    country   = $r.country
    buildings = [double]::Parse($r.buildings_cost_recovery, $inv)
    machines  = [double]::Parse($r.machines_cost_recovery, $inv)
    intang    = [double]::Parse($r.intangibles_cost_recovery, $inv)
    waverage  = [double]::Parse($r.waverage, $inv)
  }
  $names[$iso] = $r.country
}

# ---- Fixed country order across all sheets: descending 2026 current-law weighted avg ----
$order = @($byYearIso[2026].Keys | Sort-Object `
  @{Expression = { $byYearIso[2026][$_].waverage }; Descending = $true }, `
  @{Expression = { $names[$_] } })
$nC        = $order.Count
$dataStart = 5
$dataEnd   = $dataStart + $nC - 1
$canRow    = $dataStart + [array]::IndexOf($order, 'CAN')
Write-Host ("Countries: {0}  data rows {1}-{2}  Canada row {3}" -f $nC,$dataStart,$dataEnd,$canRow)

# ---- Start Excel ----
$xl = New-Object -ComObject Excel.Application
$xl.Visible = $false
$xl.DisplayAlerts = $false
$xl.ScreenUpdating = $false
$xl.SheetsInNewWorkbook = 1
$wb = $xl.Workbooks.Add()
$xl.Calculation = -4135   # xlCalculationManual (only settable once a workbook exists)

function Set-Num($cell, $v) {
  # write a number as an invariant-culture constant formula (avoids German-locale Value2 failure)
  $cell.Formula = '=' + [Convert]::ToString([double]$v, $inv)
}

# ---- Create sheets in tab order ----
$sheets = @{}
$first = $wb.Worksheets.Item(1)
$first.Name = 'Canada summary'
$sheets['Canada summary'] = $first
$prev = $first
foreach ($n in @('2026','2027','2028','2029','2030','Consolidated','Notes')) {
  $ws = $wb.Worksheets.Add([System.Reflection.Missing]::Value, $prev)
  $ws.Name = $n
  $sheets[$n] = $ws
  $prev = $ws
}

# ---- Notes sheet (weights + methodology) ----
$nt = $sheets['Notes']
$nt.Cells.Item(1,1).Value2 = 'Notes and sources'
$nt.Cells.Item(3,1).Value2 = 'Asset weights (capital-stock shares; Devereux 2012), matching capital_cost_recovery.R'
$nt.Cells.Item(4,1).Value2 = 'w_machines';    Set-Num $nt.Cells.Item(4,2) 0.4391081
$nt.Cells.Item(5,1).Value2 = 'w_buildings';   Set-Num $nt.Cells.Item(5,2) 0.4116638
$nt.Cells.Item(6,1).Value2 = 'w_intangibles'; Set-Num $nt.Cells.Item(6,2) 0.1492281
$nt.Cells.Item(8,1).Value2  = 'Reform: Canada grants immediate expensing (NPV = 1.0) for machinery and buildings from 2026 onward; intangibles remain on current law.'
$nt.Cells.Item(9,1).Value2  = 'Weighted average = w_machines*machinery + w_buildings*buildings + w_intangibles*intangibles.'
$nt.Cells.Item(10,1).Value2 = 'Ranking: RANK on the weighted average rounded to 4 dp, within the 38 OECD countries, descending, ties assigned the minimum rank (matches R rank(-round(x,4), ties.method="min")).'
$nt.Cells.Item(11,1).Value2 = 'OECD ranking excludes the 5 EU-but-not-OECD countries in the dataset: Bulgaria, Croatia, Cyprus, Malta, Romania.'
$nt.Cells.Item(12,1).Value2 = 'Source data: final-data/npv_all_years.csv (produced by capital_cost_recovery.R). Canada per-asset scenario: final-outputs/canada_expensing_scenario.csv.'
$nt.Cells.Item(13,1).Value2 = 'Current-law forecasts for all 38 countries extend through 2030; the ranking window is 2026-2030 for that reason.'
$nt.Columns.Item(1).ColumnWidth = 60
$nt.Cells.Item(1,1).Font.Bold = $true

# ---- Per-year ranking sheets ----
$hdr = @('Country','ISO','Buildings (current law)','Machinery (current law)','Intangibles (current law)','Weighted avg (current law)','Rank (current law)','Buildings (reform)','Machinery (reform)','Intangibles (reform)','Weighted avg (reform)','Rank (reform)')
foreach ($y in $years) {
  $ws = $sheets["$y"]
  $ws.Cells.Item(1,1).Value2 = ("Net Present Value of Capital Allowances, OECD Ranking {0}: current law vs. Canada immediate-expensing reform" -f $y)
  $ws.Cells.Item(2,1).Value2 = 'Weighted avg = weights (Notes) applied to asset NPVs (source: final-data/npv_all_years.csv). Reform sets Canada machinery and buildings to 1.0; intangibles unchanged. Ranks: RANK on weighted avg rounded to 4 dp, ties=min.'
  for ($c=0; $c -lt $hdr.Count; $c++) { $ws.Cells.Item(4, $c+1).Value2 = $hdr[$c] }
  for ($k=0; $k -lt $nC; $k++) {
    $iso = $order[$k]; $row = $dataStart + $k; $d = $byYearIso[$y][$iso]
    $ws.Cells.Item($row,1).Value2 = $d.country
    $ws.Cells.Item($row,2).Value2 = $iso
    Set-Num $ws.Cells.Item($row,3) $d.buildings
    Set-Num $ws.Cells.Item($row,4) $d.machines
    Set-Num $ws.Cells.Item($row,5) $d.intang
    $ws.Cells.Item($row,6).Formula  = '=ROUND(Notes!$B$4*D{0}+Notes!$B$5*C{0}+Notes!$B$6*E{0},4)' -f $row
    $ws.Cells.Item($row,7).Formula  = '=RANK(F{0},F${1}:F${2},0)' -f $row,$dataStart,$dataEnd
    if ($iso -eq 'CAN') {
      $ws.Cells.Item($row,8).Formula = '=1'
      $ws.Cells.Item($row,9).Formula = '=1'
    } else {
      $ws.Cells.Item($row,8).Formula = '=C{0}' -f $row
      $ws.Cells.Item($row,9).Formula = '=D{0}' -f $row
    }
    $ws.Cells.Item($row,10).Formula = '=E{0}' -f $row
    $ws.Cells.Item($row,11).Formula = '=ROUND(Notes!$B$4*I{0}+Notes!$B$5*H{0}+Notes!$B$6*J{0},4)' -f $row
    $ws.Cells.Item($row,12).Formula = '=RANK(K{0},K${1}:K${2},0)' -f $row,$dataStart,$dataEnd
  }
  $ws.Range(("C{0}:F{1}" -f $dataStart,$dataEnd)).NumberFormat = '0.0000'
  $ws.Range(("H{0}:K{1}" -f $dataStart,$dataEnd)).NumberFormat = '0.0000'
  $ws.Range(("G{0}:G{1}" -f $dataStart,$dataEnd)).NumberFormat = '0'
  $ws.Range(("L{0}:L{1}" -f $dataStart,$dataEnd)).NumberFormat = '0'
  $ws.Range('A4:L4').Font.Bold = $true
  $ws.Cells.Item(1,1).Font.Bold = $true
  $ws.Columns.Item(1).ColumnWidth = 20
  $ws.Range('C:L').ColumnWidth = 13
}

# ---- Canada summary sheet ----
$cs = $sheets['Canada summary']
$cs.Cells.Item(1,1).Value2 = 'Canada: immediate expensing for machinery and buildings - weighted-average NPV and OECD rank, 2026-2030'
$cs.Cells.Item(2,1).Value2 = 'Values link to the per-year sheets. Rank change is positive when the reform improves Canada rank (current-law rank minus reform rank).'
$csh = @('Year','Weighted avg (current law)','Rank (current law)','Weighted avg (reform)','Rank (reform)','Rank change (+ = better)','Weighted-avg change')
for ($c=0; $c -lt $csh.Count; $c++) { $cs.Cells.Item(4, $c+1).Value2 = $csh[$c] }
for ($i=0; $i -lt $years.Count; $i++) {
  $y = $years[$i]; $r = 5 + $i
  Set-Num $cs.Cells.Item($r,1) $y
  $cs.Cells.Item($r,2).Formula = "='{0}'!F{1}" -f $y,$canRow
  $cs.Cells.Item($r,3).Formula = "='{0}'!G{1}" -f $y,$canRow
  $cs.Cells.Item($r,4).Formula = "='{0}'!K{1}" -f $y,$canRow
  $cs.Cells.Item($r,5).Formula = "='{0}'!L{1}" -f $y,$canRow
  $cs.Cells.Item($r,6).Formula = '=C{0}-E{0}' -f $r
  $cs.Cells.Item($r,7).Formula = '=D{0}-B{0}' -f $r
}
$cs.Range('A1').Font.Bold = $true
$cs.Range('A4:G4').Font.Bold = $true
$cs.Range('A5:A9').NumberFormat = '0'
$cs.Range('B5:B9').NumberFormat = '0.0000'
$cs.Range('C5:C9').NumberFormat = '0'
$cs.Range('D5:D9').NumberFormat = '0.0000'
$cs.Range('E5:E9').NumberFormat = '0'
$cs.Range('F5:F9').NumberFormat = '0'
$cs.Range('G5:G9').NumberFormat = '0.0000'
$cs.Columns.Item(1).ColumnWidth = 8
$cs.Range('B:G').ColumnWidth = 22

# ---- Consolidated sheet (all years side by side, links to per-year sheets) ----
$co = $sheets['Consolidated']
$co.Cells.Item(1,1).Value2 = 'Consolidated OECD rankings 2026-2030: current law vs. Canada immediate-expensing reform'
$co.Cells.Item(2,1).Value2 = 'All values link to the per-year sheets. wavg = weighted average NPV; CL = current law.'
$co.Cells.Item(4,1).Value2 = 'Country'
for ($i=0; $i -lt $years.Count; $i++) {
  $y = $years[$i]; $base = 2 + $i*4
  $co.Cells.Item(4,$base).Value2   = ("{0} wavg CL"     -f $y)
  $co.Cells.Item(4,$base+1).Value2 = ("{0} rank CL"     -f $y)
  $co.Cells.Item(4,$base+2).Value2 = ("{0} wavg reform" -f $y)
  $co.Cells.Item(4,$base+3).Value2 = ("{0} rank reform" -f $y)
}
for ($k=0; $k -lt $nC; $k++) {
  $row = $dataStart + $k
  $co.Cells.Item($row,1).Formula = "='2026'!A{0}" -f $row
  for ($i=0; $i -lt $years.Count; $i++) {
    $y = $years[$i]; $base = 2 + $i*4
    $co.Cells.Item($row,$base).Formula   = "='{0}'!F{1}" -f $y,$row
    $co.Cells.Item($row,$base+1).Formula = "='{0}'!G{1}" -f $y,$row
    $co.Cells.Item($row,$base+2).Formula = "='{0}'!K{1}" -f $y,$row
    $co.Cells.Item($row,$base+3).Formula = "='{0}'!L{1}" -f $y,$row
  }
}
$co.Range('A4').Font.Bold = $true
$co.Range('B4:U4').Font.Bold = $true
$co.Cells.Item(1,1).Font.Bold = $true
$co.Columns.Item(1).ColumnWidth = 20
# number formats per column group: weighted averages 4 dp, ranks integer
for ($i=0; $i -lt $years.Count; $i++) {
  $base = 2 + $i*4
  $co.Range($co.Cells.Item($dataStart,$base),   $co.Cells.Item($dataEnd,$base)).NumberFormat   = '0.0000'  # wavg CL
  $co.Range($co.Cells.Item($dataStart,$base+1), $co.Cells.Item($dataEnd,$base+1)).NumberFormat = '0'       # rank CL
  $co.Range($co.Cells.Item($dataStart,$base+2), $co.Cells.Item($dataEnd,$base+2)).NumberFormat = '0.0000'  # wavg reform
  $co.Range($co.Cells.Item($dataStart,$base+3), $co.Cells.Item($dataEnd,$base+3)).NumberFormat = '0'       # rank reform
  $co.Columns.Item($base).ColumnWidth   = 12
  $co.Columns.Item($base+1).ColumnWidth = 10
  $co.Columns.Item($base+2).ColumnWidth = 12
  $co.Columns.Item($base+3).ColumnWidth = 10
}

# ---- Recalculate, error scan, save ----
$xl.Calculation = -4105   # xlCalculationAutomatic
$xl.CalculateFull()

$totErr = 0
foreach ($n in @('Canada summary','2026','2027','2028','2029','2030','Consolidated')) {
  try {
    $e = $sheets[$n].UsedRange.SpecialCells(-4123, 16)   # xlCellTypeFormulas, xlErrors
    if ($e) { $totErr += $e.Count; Write-Host ("ERRORS on {0}: {1}" -f $n,$e.Count) }
  } catch { }  # SpecialCells throws when none found
}
Write-Host ("Total formula error cells: {0}" -f $totErr)

# ---- Verification read-back (computed values) ----
Write-Host "`n--- Canada summary (computed) ---"
Write-Host "Year  wavgCL   rankCL  wavgReform  rankReform  dRank  dWavg"
for ($i=0; $i -lt $years.Count; $i++) {
  $r = 5 + $i
  $vals = @(
    $cs.Cells.Item($r,1).Value2, $cs.Cells.Item($r,2).Value2, $cs.Cells.Item($r,3).Value2,
    $cs.Cells.Item($r,4).Value2, $cs.Cells.Item($r,5).Value2, $cs.Cells.Item($r,6).Value2,
    $cs.Cells.Item($r,7).Value2
  )
  Write-Host ("{0}  {1:N4}  {2}      {3:N4}      {4}          {5}     {6:N4}" -f $vals[0],$vals[1],$vals[2],$vals[3],$vals[4],$vals[5],$vals[6])
}
Write-Host "`n--- 2026 spot-check (rank CL / rank reform) ---"
$w26 = $sheets['2026']
foreach ($iso in @('EST','LVA','CAN','USA','LTU')) {
  $ri = $dataStart + [array]::IndexOf($order,$iso)
  Write-Host ("{0}  wavgCL={1:N4} rankCL={2}  wavgRef={3:N4} rankRef={4}" -f $iso, $w26.Cells.Item($ri,6).Value2, $w26.Cells.Item($ri,7).Value2, $w26.Cells.Item($ri,11).Value2, $w26.Cells.Item($ri,12).Value2)
}

$wb.SaveAs($out, 51)   # xlOpenXMLWorkbook
$wb.Close($true)
$xl.Quit()
[System.Runtime.InteropServices.Marshal]::ReleaseComObject($wb) | Out-Null
[System.Runtime.InteropServices.Marshal]::ReleaseComObject($xl) | Out-Null
[GC]::Collect(); [GC]::WaitForPendingFinalizers()
Write-Host ("`nSaved: {0}" -f $out)
