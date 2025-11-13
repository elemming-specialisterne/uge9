#!/bin/bash

# =================================================================
# DB2 Database Seeding Script
# =================================================================
# This script creates tables and populates them with data from
# the COBOL text files based on the copybook structures
# 
# Author: SPAC-23
# Date: 2025-11-12
# =================================================================

echo "🚀 Starting DB2 Database Seeding..."
echo "=================================="

# Check if DB2 container is running
if ! docker ps | grep -q "db2server"; then
    echo "❌ Error: DB2 container 'db2server' is not running!"
    echo "Please start the container first with: docker start db2server"
    exit 1
fi

echo "✅ DB2 container is running"

# Function to execute DB2 commands
execute_db2() {
    local sql_command="$1"
    local description="$2"
    
    echo "📝 $description"
    
    if docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null && db2 \"$sql_command\"" > /dev/null 2>&1; then
        echo "   ✅ Success"
    else
        echo "   ❌ Failed: $description"
        return 1
    fi
}

# Function to execute DB2 commands with output
execute_db2_with_output() {
    local sql_command="$1"
    local description="$2"
    
    echo "📝 $description"
    docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null && db2 \"$sql_command\""
}

echo ""
echo "🗑️  Cleaning up existing tables..."
echo "================================"

# Drop tables if they exist (ignore errors if they don't exist)
docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null && db2 'DROP TABLE KONTI'" 2>/dev/null || true
docker exec db2server su - db2inst1 -c "db2 connect to REPODB > /dev/null && db2 'DROP TABLE KUNDER'" 2>/dev/null || true
echo "✅ Cleanup completed"

echo ""
echo "🏗️  Creating database tables..."
echo "==============================="

# Create KUNDER table based on KUNDER.cpy
execute_db2 "CREATE TABLE KUNDER (
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
)" "Creating KUNDER table"

# Create KONTI table based on KONTOOPL.cpy
execute_db2 "CREATE TABLE KONTI (
    KUNDE_ID CHAR(10) NOT NULL,
    KONTO_ID CHAR(10) NOT NULL,
    KONTO_TYPE VARCHAR(20),
    BALANCE DECIMAL(9,2),
    VALUTA_KD CHAR(3),
    PRIMARY KEY (KONTO_ID),
    FOREIGN KEY (KUNDE_ID) REFERENCES KUNDER(KUNDE_ID)
)" "Creating KONTI table"

echo ""
echo "📊 Inserting customer data..."
echo "============================="

# Insert customers from Kundeoplysninger.txt
execute_db2 "INSERT INTO KUNDER VALUES (
    '2345678901', 'Anna', 'Nielsen', 'DK987654321098765', 1800.50, 'DKK',
    'Norrebrogade', '45', '2', 'N', 'Copenhagen', '2100', 'DK', 
    '12345678', 'anna.nielsen@email.dk'
)" "Inserting Anna Nielsen"

execute_db2 "INSERT INTO KUNDER VALUES (
    '3456789012', 'Peter', 'Andersen', 'DK555666777888999', 3201.00, 'DKK',
    'Vesterbrogade', '12', '1', 'TV', 'Aarhus', '8000', 'DK',
    '87654321', 'peter.andersen@post.dk'
)" "Inserting Peter Andersen"

execute_db2 "INSERT INTO KUNDER VALUES (
    '4567890123', 'Marie', 'Pedersen', 'DK111222333444555', 950.25, 'DKK',
    'Ostergade', '7', '3', 'SV', 'Odense', '5000', 'DK',
    '65432198', 'marie.pedersen@mail.dk'
)" "Inserting Marie Pedersen"

execute_db2 "INSERT INTO KUNDER VALUES (
    '5678901234', 'Johan', 'Larsen', 'DK666777888999000', 4101.50, 'DKK',
    'Strandvejen', '23', '1', 'TV', 'Aalborg', '9000', 'DK',
    '98765432', 'johan.larsen@webmail.dk'
)" "Inserting Johan Larsen"

execute_db2 "INSERT INTO KUNDER VALUES (
    '1234567890', 'Lars', 'Hansen', 'DK123445678912345', 2500.75, 'DKK',
    'Lautruphojvej', '1', '5', 'TV', 'Copenhagen', '2605', 'DK',
    '45888888', 'test.test@test.dk'
)" "Inserting Lars Hansen"

echo ""
echo "💰 Inserting account data..."
echo "============================"

# Insert accounts from KontoOpl.txt

# Anna Nielsen (1 account)
execute_db2 "INSERT INTO KONTI VALUES ('2345678901', 'ACC0001234', 'Lonkonto', 1800.50, 'DKK')" "Anna's Lønkonto"

# Peter Andersen (3 accounts)
execute_db2 "INSERT INTO KONTI VALUES ('3456789012', 'ACC0002345', 'Lonkonto', 3201.00, 'DKK')" "Peter's Lønkonto"
execute_db2 "INSERT INTO KONTI VALUES ('3456789012', 'ACC0006789', 'Budgetkonto', 25000.00, 'DKK')" "Peter's Budgetkonto"
execute_db2 "INSERT INTO KONTI VALUES ('3456789012', 'ACC0011111', 'Investeringskonto', 12500.75, 'DKK')" "Peter's Investeringskonto"

# Marie Pedersen (2 accounts)
execute_db2 "INSERT INTO KONTI VALUES ('4567890123', 'ACC0003456', 'Lonkonto', 950.25, 'DKK')" "Marie's Lønkonto"
execute_db2 "INSERT INTO KONTI VALUES ('4567890123', 'ACC0007890', 'Pensionskonto', 125000.00, 'DKK')" "Marie's Pensionskonto"

# Johan Larsen (4 accounts)
execute_db2 "INSERT INTO KONTI VALUES ('5678901234', 'ACC0004567', 'Lonkonto', 4101.50, 'DKK')" "Johan's Lønkonto"
execute_db2 "INSERT INTO KONTI VALUES ('5678901234', 'ACC0008901', 'Opsparingskonto', 75000.25, 'DKK')" "Johan's Opsparingskonto"
execute_db2 "INSERT INTO KONTI VALUES ('5678901234', 'ACC0012345', 'Erhvervskonto', 8750.00, 'DKK')" "Johan's Erhvervskonto"
execute_db2 "INSERT INTO KONTI VALUES ('5678901234', 'ACC0054321', 'Budgetkonto', 1250.50, 'DKK')" "Johan's Budgetkonto"

# Lars Hansen (2 accounts)
execute_db2 "INSERT INTO KONTI VALUES ('1234567890', 'ACC0005678', 'Lonkonto', 2500.75, 'DKK')" "Lars's Lønkonto"
execute_db2 "INSERT INTO KONTI VALUES ('1234567890', 'ACC0009012', 'Opsparingskonto', 50000.00, 'DKK')" "Lars's Opsparingskonto"

echo ""
echo "🔍 Verifying data..."
echo "==================="

echo ""
echo "📋 Customer Summary:"
execute_db2_with_output "SELECT KUNDE_ID, FORNAVN, EFTERNAVN, CITY FROM KUNDER ORDER BY FORNAVN" ""

echo ""
echo "📊 Account Summary by Customer:"
execute_db2_with_output "SELECT 
    k.FORNAVN, 
    k.EFTERNAVN, 
    COUNT(a.KONTO_ID) as ANTAL_KONTI, 
    DECIMAL(SUM(a.BALANCE), 10, 2) as TOTAL_BALANCE 
FROM KUNDER k 
LEFT JOIN KONTI a ON k.KUNDE_ID = a.KUNDE_ID 
GROUP BY k.KUNDE_ID, k.FORNAVN, k.EFTERNAVN 
ORDER BY k.FORNAVN" ""

echo ""
echo "💳 Detailed Account List:"
execute_db2_with_output "SELECT 
    k.FORNAVN, 
    k.EFTERNAVN, 
    a.KONTO_ID, 
    a.KONTO_TYPE, 
    a.BALANCE 
FROM KUNDER k 
JOIN KONTI a ON k.KUNDE_ID = a.KUNDE_ID 
ORDER BY k.FORNAVN, a.KONTO_TYPE" ""

echo ""
echo "🎉 Database seeding completed successfully!"
echo "=========================================="
echo ""
echo "📈 Summary:"
echo "- 5 customers inserted from Kundeoplysninger.txt"
echo "- 12 accounts inserted from KontoOpl.txt"
echo "- All data based on COBOL copybook structures"
echo "- Foreign key relationships established"
echo ""
echo "🚀 Your DB2 database is now ready for COBOL integration!"