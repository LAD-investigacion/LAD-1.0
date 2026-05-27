@echo off
title LAD - Laboratorio de Análisis Distributivo
cd /d "%~dp0"

set "RSCRIPT=%~dp0runtime\R-Portable\App\R-Portable\bin\Rscript.exe"

if not exist "%RSCRIPT%" (
    echo [ERROR] No se encuentra R-Portable
    pause
    exit /b 1
)

echo ========================================
echo    LABORATORIO DE ANALISIS DISTRIBUTIVO
echo               LAD-MG 1.0
echo ========================================
echo.
echo Instalando paquetes necesarios...
echo.

:: Instalar readxl desde binario (sin compilar)
"%RSCRIPT%" -e "install.packages('readxl', type = 'binary', repos = 'https://cloud.r-project.org')"

echo.
echo Iniciando la aplicacion...
echo.

cd /d "%~dp0App"
"%RSCRIPT%" -e "shiny::runApp('.', launch.browser=TRUE, port=1984, host='127.0.0.1')"

if errorlevel 1 (
    echo.
    echo [ERROR] La aplicacion fallo al iniciar.
    pause
)