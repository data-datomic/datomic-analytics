Import-Module BitsTransfer
$ErrorActionPreference = "Stop"

function CheckHash {
 param([string] $file, [string] $hash)

 (Get-FileHash $file -Algorithm SHA256).Hash.ToLower() -eq $hash
}


function Fatal-Error() {
    param([string] $message)
    Write-Error $message
    exit 1
}

function SetupVsEnv() {
    $path = vswhere -latest -products * -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
    if (-not $?) {
        Fatal-Error "vswhere failed"
    }
    if ($path) {
        $path = join-path $path 'Common7\Tools\vsdevcmd.bat'
        Write-Output "Looking for vsdevcmd.bat in $path"
        if (test-path $path) {
            cmd /s /c """$path"" -arch=amd64  && set" | where { $_ -match '(\w+)=(.*)' } | foreach {
                #Write-Output $Matches
                $null = new-item -force -path "Env:\$($Matches[1])" -value $Matches[2]
            }
            if (-not $?) {
                cmd /s /c "set VSCMD_DEBUG=1 && ""$path"" -arch=amd64"
                Fatal-Error "vsdevcmd.bat failed"
            }
            echo "Visual studio path setup done $?"
        } else {
            Fatal-Error "vsdevcmd.bat does not exist"
        }
    } else {
        Fatal-Error "vswhere did not find visual studio"
    }
}


function DownloadAndCheckFile() {
    param([string] $url, [string] $file, [string] $hash)
    $has_file = $false

    if (Test-Path $file -PathType Leaf) {
        if (CheckHash $file $hash) {
            $has_file = $true
        } else {
            $new_hash = (Get-FileHash $file -Algorithm SHA256).Hash.ToLower()
            Write-Output "File exists with different hash: $new_hash expected: $hash"
        }
    } else {
        Write-Output "File $file does not exist, need to download"
    }
    if (-not $has_file) {
        Write-Output "Downloading file $url"
        Start-BitsTransfer -Source $url -Destination $file
    }
    Write-Output "Dowload finished"
    if (-not (CheckHash $file $hash)) {
        Fatal-Error "Downloaded file hash mismatch"
    }
}

SetupVsEnv

$version_base = "5.14"
$version_full = "5.14.2"
#$url = "https://download.qt.io/official_releases/qt/$version_base/$version_full/single/qt-everywhere-src-$version_full.zip"
$url = "http://master.qt.io/archive/qt/$version_base/$version_full/single/qt-everywhere-src-$version_full.zip"
$output = "qt-everywhere-src-$version_full.zip"
$hash_expected = "847f39c5b9db3eeee890a2aee3065ae81032287ab9d5812015ff9b37d19b64d6"
$QT_SRC_DIR = "qt-everywhere-src-$version_full"
$qt_build_dir = "$QT_SRC_DIR/build"
$QT_PREFIX = "$PSScriptRoot/qt"
$BUILD_THREADS = 3
$PACKAGE_FILE = "cutter-deps-qt-win.tar.gz"


# https://download.qt.io/official_releases/jom/jom.zip
$jom_url = "http://master.qt.io/official_releases/jom/jom_1_1_3.zip"
$jom_archive = "jom.zip"
$jom_hash = "128fdd846fe24f8594eed37d1d8929a0ea78df563537c0c1b1861a635013fff8"


DownloadAndCheckFile $url $output $hash_expected

Write-Output "File hash correct"
Write-Output "Extracting"
7z.exe x -bt -aos -bsp1 "$output" `
    -x'!'${QT_SRC_DIR}/qtwebengine `
    -x'!'${QT_SRC_DIR}/qt3d `
    -x'!'${QT_SRC_DIR}/qtcanvas3d `
    -x'!'${QT_SRC_DIR}/qtcharts `
    -x'!'${QT_SRC_DIR}/qtconnectivity `
    -x'!'${QT_SRC_DIR}/qtdeclarative `
    -x'!'${QT_SRC_DIR}/qtdoc `
    -x'!'${QT_SRC_DIR}/qtscript `
    -x'!'${QT_SRC_DIR}/qtdatavis3d `
    -x'!'${QT_SRC_DIR}/qtgamepad `
    -x'!'${QT_SRC_DIR}/qtlocation `
    -x'!'${QT_SRC_DIR}/qtgraphicaleffects `
    -x'!'${QT_SRC_DIR}/qtmultimedia `
    -x'!'${QT_SRC_DIR}/qtpurchasing `
    -x'!'${QT_SRC_DIR}/qtscxml `
    -x'!'${QT_SRC_DIR}/qtsensors `
    -x'!'${QT_SRC_DIR}/qtserialbus `
    -x'!'${QT_SRC_DIR}/qtserialport `
    -x'!'${QT_SRC_DIR}/qtspeech `
    -x'!'${QT_SRC_DIR}/qtvirtualkeyboard `
    -x'!'${QT_SRC_DIR}/qtwebglplugin `
    -x'!'${QT_SRC_DIR}/qtwebsockets `
    -x'!'${QT_SRC_DIR}/qtwebview `
    -x'!'${QT_SRC_DIR}/qtmacextras `
    -x'!'${QT_SRC_DIR}/qtwayland `
    -x'!'${QT_SRC_DIR}/qtquickcontrols `
    -x'!'${QT_SRC_DIR}/qtquickcontrols2 `
    -x'!'${QT_SRC_DIR}/qtx11extras `
    -x'!'${QT_SRC_DIR}/qtandroidextras `
    -x'!'${QT_SRC_DIR}/qtwebchannel
if (-not $?) {
    Fatal-Error "Failed to extract source"
}

DownloadAndCheckFile $jom_url $jom_archive $jom_hash

Write-Output "Extracting jom"
7z.exe x -bt -aos -bsp1 -ojom "$jom_archive"
if (-not $?) {
    Fatal-Error "Failed to extract jom"
}

Write-Output "Building Qt"
New-Item -Path . -Name $qt_build_dir -ItemType Directory
Write-Output "build dir '$qt_build_dir'"
Set-Location -Path $qt_build_dir
Write-Output "Current dir"
Get-Location
cmd /c "..\configure.bat -prefix `"${QT_PREFIX}`" -opensource -confirm-license  -release  -qt-libpng  -qt-libjpeg  -schannel  -no-feature-cups  -no-feature-icu  -no-sql-db2  -no-sql-ibase  -no-sql-mysql  -no-sql-oci  -no-sql-odbc  -no-sql-psql  -no-sql-sqlite2  -no-sql-sqlite  -no-sql-tds  -nomake tests  -nomake examples  -nomake tools  -skip qtwebengine  -skip qt3d  -skip qtcanvas3d  -skip qtcharts  -skip qtconnectivity  -skip qtdeclarative  -skip qtdoc  -skip qtscript  -skip qtdatavis3d  -skip qtgamepad  -skip qtlocation  -skip qtgraphicaleffects  -skip qtmultimedia  -skip qtpurchasing  -skip qtscxml  -skip qtsensors  -skip qtserialbus  -skip qtserialport  -skip qtspeech  -skip qtvirtualkeyboard  -skip qtwebglplugin  -skip qtwebsockets  -skip qtwebview  -skip qtquickcontrols  -skip qtquickcontrols2  -skip qtwayland -skip qtmacextras -skip qtx11extras 2>&1"
if (-not $?) {
     Fatal-Error "Failed to configure qt"
}

Write-Output "Running jom"
cmd /c "`"$PSScriptRoot/jom/jom.exe`" -J $BUILD_THREADS 2>&1"
if (-not $?) {
    Fatal-Error "Qt compilation failed"
}
cmd /c "`"$PSScriptRoot/jom/jom.exe`" install 2>&1"
if (-not $?) {
    Fatal-Error "Qt file installation failed"
}

Set-Location $PSScriptRoot

$TMP_TAR = "cutter-deps-qt-win.tar"
7z a -bsp1 -ttar $TMP_TAR "$QT_PREFIX"
if (-not $?) {
    Fatal-Error "Result compressing failed"
}

7z a -bsp1 $PACKAGE_FILE $TMP_TAR
if (-not $?) {
    Fatal-Error "Result compressing failed"
}

Write-Output "Finished successfully"
