# =================================================================
# DB2 Database Seeding Script (PowerShell)
# =================================================================
# This script creates tables and populates them with data from
# the COBOL text files based on the copybook structures
# 
# Author: SPAC-23
# Date: 2025-11-12
# =================================================================

Write-Host "🚀 Starting DB2 Database Seeding..." -ForegroundColor Green
Write-Host "==================================" -ForegroundColor Green

# Function to execute DB2 commands
function Invoke-DB2Command {
    param(
        [string]$SqlCommand,
        [string]$Description
    )
    
    Write-Host "📝 $Description" -ForegroundColor Cyan
    
    try {
        $result = docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null 2>&1 && db2 `"$SqlCommand`"" 2>$null
        if ($LASTEXITCODE -eq 0) {
            Write-Host "   ✅ Success" -ForegroundColor Green
            return $true
        } else {
            Write-Host "   ❌ Failed: $Description" -ForegroundColor Red
            return $false
        }
    }
    catch {
        Write-Host "   ❌ Error: $_" -ForegroundColor Red
        return $false
    }
}

# Function to execute DB2 commands with output
function Invoke-DB2CommandWithOutput {
    param(
        [string]$SqlCommand,
        [string]$Description
    )
    
    if ($Description) {
        Write-Host "📝 $Description" -ForegroundColor Cyan
    }
    
    docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null 2>&1 && db2 `"$SqlCommand`""
}

# Check if Docker is available
try {
    docker --version | Out-Null
} catch {
    Write-Host "❌ Error: Docker is not available!" -ForegroundColor Red
    Write-Host "Please install Docker and try again." -ForegroundColor Yellow
    Read-Host "Press Enter to exit"
    exit 1
}

# Check if DB2 container is running
$containerRunning = docker ps --format "table {{.Names}}" | Select-String "db2server"
if (-not $containerRunning) {
    Write-Host "❌ Error: DB2 container 'db2server' is not running!" -ForegroundColor Red
    Write-Host "Please start the container first with: docker start db2server" -ForegroundColor Yellow
    Read-Host "Press Enter to exit"
    exit 1
}

Write-Host "✅ DB2 container is running" -ForegroundColor Green

Write-Host ""
Write-Host "🗑️  Cleaning up existing tables..." -ForegroundColor Yellow
Write-Host "================================" -ForegroundColor Yellow

# Drop tables if they exist (ignore errors)
docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null 2>&1 && db2 'DROP TABLE KONTI'" 2>$null | Out-Null
docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null 2>&1 && db2 'DROP TABLE KUNDER'" 2>$null | Out-Null
Write-Host "✅ Cleanup completed" -ForegroundColor Green

Write-Host ""
Write-Host "🏗️  Creating database tables..." -ForegroundColor Yellow
Write-Host "===============================" -ForegroundColor Yellow

# Create KUNDER table
$success = Invoke-DB2Command -SqlCommand "CREATE TABLE KUNDER (
    KUNDE_ID CHAR(10) NOT NULL,
    FORNAVN VARCHAR(20),
    EFTERNAVN VARCHAR(20),
    KONTONUMMER VARCHAR(20),
    BALANCE DECIMAL(9,2),
    VALUTAKODE CHAR(3),
    VEJNAVN VARCHAR(30),
    HUSNR CHAR(5),
    ETAGE CHAR(5),
    SIDE CHAR(5),
    CITY VARCHAR(20),
    POSTNR CHAR(4),
    LANDE_KODE CHAR(2),
    TELEFON CHAR(8),
    EMAIL VARCHAR(50),
    PRIMARY KEY (KUNDE_ID)
)" -Description "Creating KUNDER table"

if (-not $success) {
    Read-Host "Press Enter to exit"
    exit 1
}

# Create KONTI table
$success = Invoke-DB2Command -SqlCommand "CREATE TABLE KONTI (
    KUNDE_ID CHAR(10) NOT NULL,
    KONTO_ID CHAR(10) NOT NULL,
    KONTO_TYPE VARCHAR(20),
    BALANCE DECIMAL(9,2),
    VALUTA_KD CHAR(3),
    PRIMARY KEY (KONTO_ID),
    FOREIGN KEY (KUNDE_ID) REFERENCES KUNDER(KUNDE_ID)
)" -Description "Creating KONTI table"

if (-not $success) {
    Read-Host "Press Enter to exit"
    exit 1
}

Write-Host ""
Write-Host "📊 Inserting customer data..." -ForegroundColor Yellow
Write-Host "=============================" -ForegroundColor Yellow

# Customer data array for easier management
$customers = @(
    @{
        ID = "2345678901"; FirstName = "Anna"; LastName = "Nielsen"
        Account = "DK987654321098765"; Balance = 1800.50; Currency = "DKK"
        Street = "Norrebrogade"; HouseNo = "45"; Floor = "2"; Side = "N"
        City = "Copenhagen"; PostCode = "2100"; Country = "DK"
        Phone = "12345678"; Email = "anna.nielsen@email.dk"
    },
    @{
        ID = "3456789012"; FirstName = "Peter"; LastName = "Andersen"
        Account = "DK555666777888999"; Balance = 3201.00; Currency = "DKK"
        Street = "Vesterbrogade"; HouseNo = "12"; Floor = "1"; Side = "TV"
        City = "Aarhus"; PostCode = "8000"; Country = "DK"
        Phone = "87654321"; Email = "peter.andersen@post.dk"
    },
    @{
        ID = "4567890123"; FirstName = "Marie"; LastName = "Pedersen"
        Account = "DK111222333444555"; Balance = 950.25; Currency = "DKK"
        Street = "Ostergade"; HouseNo = "7"; Floor = "3"; Side = "SV"
        City = "Odense"; PostCode = "5000"; Country = "DK"
        Phone = "65432198"; Email = "marie.pedersen@mail.dk"
    },
    @{
        ID = "5678901234"; FirstName = "Johan"; LastName = "Larsen"
        Account = "DK666777888999000"; Balance = 4101.50; Currency = "DKK"
        Street = "Strandvejen"; HouseNo = "23"; Floor = "1"; Side = "TV"
        City = "Aalborg"; PostCode = "9000"; Country = "DK"
        Phone = "98765432"; Email = "johan.larsen@webmail.dk"
    },
    @{
        ID = "1234567890"; FirstName = "Lars"; LastName = "Hansen"
        Account = "DK123445678912345"; Balance = 2500.75; Currency = "DKK"
        Street = "Lautruphojvej"; HouseNo = "1"; Floor = "5"; Side = "TV"
        City = "Copenhagen"; PostCode = "2605"; Country = "DK"
        Phone = "45888888"; Email = "test.test@test.dk"
    }
)

# Insert customers
foreach ($customer in $customers) {
    $sql = "INSERT INTO KUNDER VALUES ('$($customer.ID)', '$($customer.FirstName)', '$($customer.LastName)', '$($customer.Account)', $($customer.Balance), '$($customer.Currency)', '$($customer.Street)', '$($customer.HouseNo)', '$($customer.Floor)', '$($customer.Side)', '$($customer.City)', '$($customer.PostCode)', '$($customer.Country)', '$($customer.Phone)', '$($customer.Email)')"
    
    Invoke-DB2Command -SqlCommand $sql -Description "Inserting $($customer.FirstName) $($customer.LastName)" | Out-Null
}

Write-Host ""
Write-Host "💰 Inserting account data..." -ForegroundColor Yellow
Write-Host "============================" -ForegroundColor Yellow

# Account data array
$accounts = @(
    @{ CustomerID = "2345678901"; AccountID = "ACC0001234"; Type = "Lonkonto"; Balance = 1800.50; Currency = "DKK" },
    @{ CustomerID = "3456789012"; AccountID = "ACC0002345"; Type = "Lonkonto"; Balance = 3201.00; Currency = "DKK" },
    @{ CustomerID = "3456789012"; AccountID = "ACC0006789"; Type = "Budgetkonto"; Balance = 25000.00; Currency = "DKK" },
    @{ CustomerID = "3456789012"; AccountID = "ACC0011111"; Type = "Investeringskonto"; Balance = 12500.75; Currency = "DKK" },
    @{ CustomerID = "4567890123"; AccountID = "ACC0003456"; Type = "Lonkonto"; Balance = 950.25; Currency = "DKK" },
    @{ CustomerID = "4567890123"; AccountID = "ACC0007890"; Type = "Pensionskonto"; Balance = 125000.00; Currency = "DKK" },
    @{ CustomerID = "5678901234"; AccountID = "ACC0004567"; Type = "Lonkonto"; Balance = 4101.50; Currency = "DKK" },
    @{ CustomerID = "5678901234"; AccountID = "ACC0008901"; Type = "Opsparingskonto"; Balance = 75000.25; Currency = "DKK" },
    @{ CustomerID = "5678901234"; AccountID = "ACC0012345"; Type = "Erhvervskonto"; Balance = 8750.00; Currency = "DKK" },
    @{ CustomerID = "5678901234"; AccountID = "ACC0054321"; Type = "Budgetkonto"; Balance = 1250.50; Currency = "DKK" },
    @{ CustomerID = "1234567890"; AccountID = "ACC0005678"; Type = "Lonkonto"; Balance = 2500.75; Currency = "DKK" },
    @{ CustomerID = "1234567890"; AccountID = "ACC0009012"; Type = "Opsparingskonto"; Balance = 50000.00; Currency = "DKK" }
)

# Insert accounts
foreach ($account in $accounts) {
    $sql = "INSERT INTO KONTI VALUES ('$($account.CustomerID)', '$($account.AccountID)', '$($account.Type)', $($account.Balance), '$($account.Currency)')"
    
    Invoke-DB2Command -SqlCommand $sql -Description "$($account.AccountID) ($($account.Type))" | Out-Null
}

Write-Host ""
Write-Host "🔍 Verifying data..." -ForegroundColor Yellow
Write-Host "===================" -ForegroundColor Yellow

Write-Host ""
Write-Host "📋 Customer Summary:" -ForegroundColor Cyan
Invoke-DB2CommandWithOutput -SqlCommand "SELECT KUNDE_ID, FORNAVN, EFTERNAVN, CITY FROM KUNDER ORDER BY FORNAVN"

Write-Host ""
Write-Host "📊 Account Summary by Customer:" -ForegroundColor Cyan
Invoke-DB2CommandWithOutput -SqlCommand "SELECT k.FORNAVN, k.EFTERNAVN, COUNT(a.KONTO_ID) as ANTAL_KONTI, DECIMAL(SUM(a.BALANCE), 10, 2) as TOTAL_BALANCE FROM KUNDER k LEFT JOIN KONTI a ON k.KUNDE_ID = a.KUNDE_ID GROUP BY k.KUNDE_ID, k.FORNAVN, k.EFTERNAVN ORDER BY k.FORNAVN"

Write-Host ""
Write-Host "💳 Detailed Account List:" -ForegroundColor Cyan
Invoke-DB2CommandWithOutput -SqlCommand "SELECT k.FORNAVN, k.EFTERNAVN, a.KONTO_ID, a.KONTO_TYPE, a.BALANCE FROM KUNDER k JOIN KONTI a ON k.KUNDE_ID = a.KUNDE_ID ORDER BY k.FORNAVN, a.KONTO_TYPE"

Write-Host ""
Write-Host "🎉 Database seeding completed successfully!" -ForegroundColor Green
Write-Host "==========================================" -ForegroundColor Green
Write-Host ""
Write-Host "📈 Summary:" -ForegroundColor Yellow
Write-Host "- 5 customers inserted from Kundeoplysninger.txt" -ForegroundColor White
Write-Host "- 12 accounts inserted from KontoOpl.txt" -ForegroundColor White
Write-Host "- All data based on COBOL copybook structures" -ForegroundColor White
Write-Host "- Foreign key relationships established" -ForegroundColor White
Write-Host ""
Write-Host "🚀 Your DB2 database is now ready for COBOL integration!" -ForegroundColor Green
Write-Host ""
Read-Host "Press Enter to exit"